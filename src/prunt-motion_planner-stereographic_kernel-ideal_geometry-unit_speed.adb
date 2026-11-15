--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.
pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  The rounding of one dot product
   ---------------------------------------------------------------------------

   Axis_Count : constant := 4;

   subtype Axis_Tally is Natural range 0 .. Axis_Count;
   --  How many axes a partial dot product covers.

   Sum_Limit : constant Dimensionless := 17.0;
   --  No partial sum of a dot product of two vectors within Largest_Component is larger, rounding included.

   Product_Limit : constant Dimensionless := 5.0;
   --  No rounded product of two components within Largest_Component is larger.

   Step_Error : constant := 2.0**(-48);
   --  More than the rounding which one product and its addition to the sum contribute: 21 * Epsilon + 2 * Eta.

   Dot_Error : constant := 2.0**(-46);
   --  Four times Step_Error: more than the rounding error of a whole dot product.

   function Is_Within_Limit (Vector : Position_Scale) return Boolean
   is (for all Axis in Axis_Name => abs Vector (Axis) <= Largest_Component)
   with Ghost;
   --  Whether no component of Vector is larger than Largest_Component.

   function Partial_Dot (Left, Right : Position_Scale; Tally : Axis_Tally) return Real
   is (case Tally is
         when 0 => Real'(0.0),
         when 1 => R (Left (X_Axis)) * R (Right (X_Axis)),
         when 2 => R (Left (X_Axis)) * R (Right (X_Axis)) + R (Left (Y_Axis)) * R (Right (Y_Axis)),
         when 3 =>
           R (Left (X_Axis)) * R (Right (X_Axis)) + R (Left (Y_Axis)) * R (Right (Y_Axis))
           + R (Left (Z_Axis)) * R (Right (Z_Axis)),
         when 4 => Dot_Real (Left, Right))
   with Ghost;
   --  The exact dot product over the first Tally axes.

   function Accumulated_Error (Tally : Axis_Tally) return Real
   is (case Tally is
         when 0 => Real'(0.0),
         when 1 => Step_Error,
         when 2 => 2.0 * Step_Error,
         when 3 => 3.0 * Step_Error,
         when 4 => Dot_Error)
   with Ghost;
   --  More than the rounding error of the floating-point dot product over the first Tally axes.

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real)
   with Ghost, Pre => abs X <= Limit and then abs Y <= Limit, Post => abs (X * Y) <= Limit * Limit;
   --  |X| ≤ L and |Y| ≤ L give |X·Y| ≤ L².

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real) is
   begin
      pragma Assert (abs X * abs Y <= Limit * abs Y);
      pragma Assert (Limit * abs Y <= Limit * Limit);
   end Lemma_Real_Product_Bound;

   procedure Lemma_Component_Is_Bounded (A : Dimensionless)
   with Ghost, Pre => abs A <= Largest_Component, Post => abs (R (A)) <= 2.0;
   --  Largest_Component, as a bound on the real number.

   procedure Lemma_Component_Is_Bounded (A : Dimensionless) is null;

   procedure Lemma_Component_Product (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs A <= Largest_Component and then abs B <= Largest_Component,
     Post => abs (R (A) * R (B)) <= 4.0;
   --  The exact product of two components is at most four in magnitude.

   procedure Lemma_Component_Product (A, B : Dimensionless) is
   begin
      Lemma_Component_Is_Bounded (A);
      Lemma_Component_Is_Bounded (B);
      Lemma_Real_Product_Bound (R (A), R (B), 2.0);
   end Lemma_Component_Product;

   procedure Lemma_Accumulated_Error (S, P, T, Exact, Term, Error : Real)
   with
     Ghost,
     Pre  =>
       abs (S - Exact) <= Error
       and then Error <= 3.0 * Step_Error
       and then abs Exact <= 12.0
       and then abs Term <= 4.0
       and then abs (P - Term) <= Epsilon * abs Term + Eta
       and then abs (T - (S + P)) <= Epsilon * abs (S + P) + Eta,
     Post => abs (T - (Exact + Term)) <= Error + Step_Error and then abs T <= 17.0;
   --  S approximates Exact, P is the rounded product Term, and T is the rounded sum of S and P. The product is
   --  within 4 * Epsilon + Eta of Term, the sum S + P is within 17 in magnitude, so T is within 17 * Epsilon + Eta
   --  of it: together 21 * Epsilon + 2 * Eta, which is less than Step_Error.

   procedure Lemma_Accumulated_Error (S, P, T, Exact, Term, Error : Real) is
   begin
      pragma Assert (abs (P - Term) <= 4.0 * Epsilon + Eta);
      pragma Assert (abs ((S + P) - (Exact + Term)) <= Error + 4.0 * Epsilon + Eta);
      pragma Assert (abs (S + P) <= 17.0);
      pragma Assert (abs (T - (S + P)) <= 17.0 * Epsilon + Eta);
   end Lemma_Accumulated_Error;

   procedure Lemma_Accumulate (Sum, A, B : Dimensionless; Exact, Error : Real)
   with
     Ghost,
     Pre  =>
       abs Sum <= Sum_Limit
       and then abs A <= Largest_Component
       and then abs B <= Largest_Component
       and then abs Exact <= 12.0
       and then Error <= 3.0 * Step_Error
       and then abs (R (Sum) - Exact) <= Error,
     Post =>
       abs (A * B) <= Product_Limit
       and then abs (Sum + A * B) <= Sum_Limit
       and then abs (R (Sum + A * B) - (Exact + R (A) * R (B))) <= Error + Step_Error;
   --  One step of the floating-point dot product: a rounded multiplication and a rounded addition. Neither
   --  overflows, and the sum drifts from the exact one by at most Step_Error more.

   procedure Lemma_Accumulate (Sum, A, B : Dimensionless; Exact, Error : Real) is
   begin
      Lemma_Component_Product (A, B);
      Float_Lemmas.Lemma_Rounding_Error_Mul (A, B);
      pragma Assert (abs (R (A * B) - R (A) * R (B)) <= 4.0 * Epsilon + Eta);
      pragma Assert (abs (R (A * B)) <= 5.0);
      pragma Assert (abs (A * B) <= Product_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, A * B);
      Lemma_Accumulated_Error (R (Sum), R (A * B), R (Sum + A * B), Exact, R (A) * R (B), Error);
      pragma Assert (abs (R (Sum + A * B)) <= 17.0);
      pragma Assert (abs (Sum + A * B) <= Sum_Limit);
   end Lemma_Accumulate;

   procedure Lemma_Partial_Dot_Step (Left, Right : Position_Scale; Axis : Axis_Name)
   with
     Ghost,
     Post =>
       Partial_Dot (Left, Right, Axis_Name'Pos (Axis) + 1)
       = Partial_Dot (Left, Right, Axis_Name'Pos (Axis)) + R (Left (Axis)) * R (Right (Axis))
       and then Accumulated_Error (Axis_Name'Pos (Axis) + 1) = Accumulated_Error (Axis_Name'Pos (Axis)) + Step_Error;
   --  One more axis adds its term to the exact partial sum, and Step_Error to the error bound.

   procedure Lemma_Partial_Dot_Step (Left, Right : Position_Scale; Axis : Axis_Name) is
      Term_X : constant Real := R (Left (X_Axis)) * R (Right (X_Axis));
      Term_Y : constant Real := R (Left (Y_Axis)) * R (Right (Y_Axis));
      Term_Z : constant Real := R (Left (Z_Axis)) * R (Right (Z_Axis));
      Term_E : constant Real := R (Left (E_Axis)) * R (Right (E_Axis));
   begin
      case Axis is
         when X_Axis =>
            pragma Assert (Partial_Dot (Left, Right, 0) = 0.0);
            pragma Assert (Partial_Dot (Left, Right, 1) = Term_X);
            pragma Assert (Accumulated_Error (0) = 0.0 and then Accumulated_Error (1) = Step_Error);

         when Y_Axis =>
            pragma Assert (Partial_Dot (Left, Right, 1) = Term_X);
            pragma Assert (Partial_Dot (Left, Right, 2) = Term_X + Term_Y);
            pragma Assert (Accumulated_Error (1) = Step_Error and then Accumulated_Error (2) = 2.0 * Step_Error);

         when Z_Axis =>
            pragma Assert (Partial_Dot (Left, Right, 2) = Term_X + Term_Y);
            pragma Assert (Partial_Dot (Left, Right, 3) = Term_X + Term_Y + Term_Z);
            pragma Assert (Accumulated_Error (2) = 2.0 * Step_Error and then Accumulated_Error (3) = 3.0 * Step_Error);

         when E_Axis =>
            pragma Assert (Partial_Dot (Left, Right, 3) = Term_X + Term_Y + Term_Z);
            pragma Assert (Partial_Dot (Left, Right, 4) = Term_X + Term_Y + Term_Z + Term_E);
            pragma Assert (Accumulated_Error (3) = 3.0 * Step_Error and then Accumulated_Error (4) = Dot_Error);
      end case;
   end Lemma_Partial_Dot_Step;

   procedure Lemma_Partial_Dot_Is_Bounded (Left, Right : Position_Scale; Tally : Axis_Tally)
   with
     Ghost,
     Pre  => Is_Within_Limit (Left) and then Is_Within_Limit (Right) and then Tally < Axis_Count,
     Post => abs (Partial_Dot (Left, Right, Tally)) <= 12.0 and then Accumulated_Error (Tally) <= 3.0 * Step_Error;
   --  Up to three terms, each at most four in magnitude.

   procedure Lemma_Partial_Dot_Is_Bounded (Left, Right : Position_Scale; Tally : Axis_Tally) is
   begin
      pragma Assert (abs Left (X_Axis) <= Largest_Component and then abs Right (X_Axis) <= Largest_Component);
      pragma Assert (abs Left (Y_Axis) <= Largest_Component and then abs Right (Y_Axis) <= Largest_Component);
      pragma Assert (abs Left (Z_Axis) <= Largest_Component and then abs Right (Z_Axis) <= Largest_Component);
      Lemma_Component_Product (Left (X_Axis), Right (X_Axis));
      Lemma_Component_Product (Left (Y_Axis), Right (Y_Axis));
      Lemma_Component_Product (Left (Z_Axis), Right (Z_Axis));
      pragma Assert (abs (Partial_Dot (Left, Right, Tally)) <= 12.0);
   end Lemma_Partial_Dot_Is_Bounded;

   procedure Lemma_Dot_Step (Left, Right : Position_Scale; Axis : Axis_Name; Sum : Dimensionless)
   with
     Ghost,
     Pre  =>
       Is_Within_Limit (Left)
       and then Is_Within_Limit (Right)
       and then abs Sum <= Sum_Limit
       and then
         abs (R (Sum) - Partial_Dot (Left, Right, Axis_Name'Pos (Axis))) <= Accumulated_Error (Axis_Name'Pos (Axis)),
     Post =>
       abs (Left (Axis) * Right (Axis)) <= Product_Limit
       and then abs (Sum + Left (Axis) * Right (Axis)) <= Sum_Limit
       and then
         abs (R (Sum + Left (Axis) * Right (Axis)) - Partial_Dot (Left, Right, Axis_Name'Pos (Axis) + 1))
         <= Accumulated_Error (Axis_Name'Pos (Axis) + 1);
   --  The rounding error bound of the floating-point dot product of two vectors within Largest_Component, carried
   --  over one more axis. After the last axis it reads: the computed sum is within Dot_Error of Dot_Real.

   procedure Lemma_Dot_Step (Left, Right : Position_Scale; Axis : Axis_Name; Sum : Dimensionless) is
      Tally : constant Axis_Tally := Axis_Name'Pos (Axis);
   begin
      pragma Assert (abs Left (Axis) <= Largest_Component and then abs Right (Axis) <= Largest_Component);
      Lemma_Partial_Dot_Is_Bounded (Left, Right, Tally);
      Lemma_Accumulate (Sum, Left (Axis), Right (Axis), Partial_Dot (Left, Right, Tally), Accumulated_Error (Tally));
      Lemma_Partial_Dot_Step (Left, Right, Axis);
   end Lemma_Dot_Step;

   procedure Lemma_Difference_Error (S, T, D, Exact : Real)
   with
     Ghost,
     Pre  =>
       abs S <= 17.0
       and then abs T <= 1.0
       and then abs (S - Exact) <= Dot_Error
       and then abs (D - (S - T)) <= Epsilon * abs (S - T) + Eta,
     Post => abs (Exact - T) <= abs D + Defect_Allowance;
   --  D is the rounded difference of S and T, so within 18 * Epsilon + Eta of it; with Dot_Error that is less
   --  than Defect_Allowance.

   procedure Lemma_Difference_Error (S, T, D, Exact : Real) is
   begin
      pragma Assert (abs (S - T) <= 18.0);
      pragma Assert (abs (D - (S - T)) <= 18.0 * Epsilon + Eta);
      pragma Assert (abs (Exact - T) <= abs (S - T) + Dot_Error);
   end Lemma_Difference_Error;

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (abs Value) = abs (R (Value));
   --  The floating-point absolute value is not rounded.

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Defect_Entry (Sum, Target : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => abs Sum <= Sum_Limit and then abs Target <= 1.0 and then abs (R (Sum) - Exact) <= Dot_Error,
     Post => abs (Exact - R (Target)) <= R (abs (Sum - Target)) + Defect_Allowance;
   --  The magnitude of the rounded difference of a computed dot product and its target is within Defect_Allowance
   --  of the magnitude of the exact difference.

   procedure Lemma_Defect_Entry (Sum, Target : Dimensionless; Exact : Real) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Sub (Sum, Target);
      pragma Assert (abs (R (Sum)) <= 17.0);
      pragma Assert (abs (R (Target)) <= 1.0);
      Lemma_Difference_Error (R (Sum), R (Target), R (Sum - Target), Exact);
      Lemma_Abs_Is_Exact (Sum - Target);
   end Lemma_Defect_Entry;

   procedure Lemma_Order_Is_Exact (Low, High : Dimensionless)
   with Ghost, Pre => Low <= High, Post => R (Low) <= R (High);

   procedure Lemma_Order_Is_Exact (Low, High : Dimensionless) is null;

   procedure Lemma_Maximum (A, B : Dimensionless)
   with
     Ghost,
     Post =>
       R (A) <= R (Dimensionless'Max (A, B))
       and then R (B) <= R (Dimensionless'Max (A, B))
       and then (if A >= 0.0 then Dimensionless'Max (A, B) >= 0.0);
   --  The floating-point maximum is at least both operands, as real numbers too.

   procedure Lemma_Maximum (A, B : Dimensionless) is
   begin
      pragma Assert (A <= Dimensionless'Max (A, B));
      pragma Assert (B <= Dimensionless'Max (A, B));
      Lemma_Order_Is_Exact (A, Dimensionless'Max (A, B));
      Lemma_Order_Is_Exact (B, Dimensionless'Max (A, B));
   end Lemma_Maximum;

   procedure Lemma_Worst_Covers_Entry (Sum, Target, Worst : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  =>
       abs Sum <= Sum_Limit
       and then abs Target <= 1.0
       and then abs (R (Sum) - Exact) <= Dot_Error
       and then Worst >= 0.0,
     Post =>
       Dimensionless'Max (Worst, abs (Sum - Target)) >= 0.0
       and then R (Worst) <= R (Dimensionless'Max (Worst, abs (Sum - Target)))
       and then abs (Exact - R (Target)) <= R (Dimensionless'Max (Worst, abs (Sum - Target))) + Defect_Allowance;
   --  What Frame_Defect's update of its running maximum achieves for one pair of frame vectors.

   procedure Lemma_Worst_Covers_Entry (Sum, Target, Worst : Dimensionless; Exact : Real) is
   begin
      Lemma_Defect_Entry (Sum, Target, Exact);
      Lemma_Maximum (Worst, abs (Sum - Target));
   end Lemma_Worst_Covers_Entry;

   ---------------------------------------------------------------------------
   --  The defect bound, pair by pair
   ---------------------------------------------------------------------------

   Frame_Size : constant := 4;

   function Defect (Left, Right : Position_Scale; Is_Diagonal : Boolean) return Real
   is (abs (Dot_Real (Left, Right) - (if Is_Diagonal then Real'(1.0) else Real'(0.0))))
   with Ghost;
   --  How far the exact dot product of two frame vectors is from the identity matrix's entry.

   function Holds_Before (Frame : Frame_Vector_Array; Row, Column : Natural; Bound : Real) return Boolean
   is (for all I in Frame_Component_Index =>
         (if I < Row
          then
            Defect (Frame (I), Frame (0), I = 0) <= Bound
            and then Defect (Frame (I), Frame (1), I = 1) <= Bound
            and then Defect (Frame (I), Frame (2), I = 2) <= Bound
            and then Defect (Frame (I), Frame (3), I = 3) <= Bound
          elsif I = Row
          then
            (if 0 < Column then Defect (Frame (I), Frame (0), I = 0) <= Bound)
            and then (if 1 < Column then Defect (Frame (I), Frame (1), I = 1) <= Bound)
            and then (if 2 < Column then Defect (Frame (I), Frame (2), I = 2) <= Bound)
            and then (if 3 < Column then Defect (Frame (I), Frame (3), I = 3) <= Bound)))
   with Ghost;
   --  Is_Defect_Bound restricted to the pairs which Frame_Defect reaches before (Row, Column): all of the rows
   --  before Row, and the columns of Row before Column. Only the rows are quantified over and the columns are
   --  written out, because the provers instantiate one quantifier on the row vector readily, but two nested
   --  quantifiers only with difficulty.

   function Is_Defect_Bound_Before (Frame : Frame_Vector_Array; Row, Column : Natural; Bound : Real) return Boolean
   with Ghost, Post => Is_Defect_Bound_Before'Result = Holds_Before (Frame, Row, Column, Bound);
   --  Holds_Before, but not an expression function: Frame_Defect carries it as one fact, and only the lemmas
   --  below look inside.

   function Is_Defect_Bound_Before (Frame : Frame_Vector_Array; Row, Column : Natural; Bound : Real) return Boolean is
   begin
      return Holds_Before (Frame, Row, Column, Bound);
   end Is_Defect_Bound_Before;

   procedure Lemma_No_Rows (Frame : Frame_Vector_Array; Bound : Real)
   with Ghost, Post => Is_Defect_Bound_Before (Frame, 0, 0, Bound);
   --  No pair comes before the first.

   procedure Lemma_No_Rows (Frame : Frame_Vector_Array; Bound : Real) is
   begin
      pragma Assert (Holds_Before (Frame, 0, 0, Bound));
   end Lemma_No_Rows;

   procedure Lemma_Relax_Holds (Frame : Frame_Vector_Array; Row, Column : Natural; Old_Bound, New_Bound : Real)
   with
     Ghost,
     Pre  => Holds_Before (Frame, Row, Column, Old_Bound) and then Old_Bound <= New_Bound,
     Post => Holds_Before (Frame, Row, Column, New_Bound);
   --  A larger bound holds wherever a smaller one does.

   procedure Lemma_Relax_Holds (Frame : Frame_Vector_Array; Row, Column : Natural; Old_Bound, New_Bound : Real)
   is null;

   procedure Lemma_Append_Holds (Frame : Frame_Vector_Array; Row, Column : Frame_Component_Index; Bound : Real)
   with
     Ghost,
     Pre  =>
       Holds_Before (Frame, Row, Column, Bound)
       and then
         abs (Dot_Real (Frame (Row), Frame (Column)) - (if Row = Column then Real'(1.0) else Real'(0.0))) <= Bound,
     Post => Holds_Before (Frame, Row, Column + 1, Bound);
   --  The pairs before (Row, Column + 1) are those before (Row, Column), and (Row, Column).

   procedure Lemma_Append_Holds (Frame : Frame_Vector_Array; Row, Column : Frame_Component_Index; Bound : Real)
   is null;

   procedure Lemma_Target_Is_Exact (Is_Diagonal : Boolean)
   with Ghost, Post => R (if Is_Diagonal then 1.0 else 0.0) = (if Is_Diagonal then Real'(1.0) else Real'(0.0));
   --  The identity matrix's entry which Frame_Defect subtracts is the one which Is_Defect_Bound names.

   procedure Lemma_Target_Is_Exact (Is_Diagonal : Boolean) is null;

   procedure Lemma_Extend_Defect_Bound
     (Frame : Frame_Vector_Array; Row, Column : Frame_Component_Index; Old_Bound, New_Bound : Real)
   with
     Ghost,
     Pre  =>
       Is_Defect_Bound_Before (Frame, Row, Column, Old_Bound)
       and then Old_Bound <= New_Bound
       and then abs (Dot_Real (Frame (Row), Frame (Column)) - R (if Row = Column then 1.0 else 0.0)) <= New_Bound,
     Post => Is_Defect_Bound_Before (Frame, Row, Column + 1, New_Bound);
   --  A bound which has grown still holds for the earlier pairs, and now for (Row, Column) as well.

   procedure Lemma_Extend_Defect_Bound
     (Frame : Frame_Vector_Array; Row, Column : Frame_Component_Index; Old_Bound, New_Bound : Real) is
   begin
      Lemma_Target_Is_Exact (Row = Column);
      pragma
        Assert
          (abs (Dot_Real (Frame (Row), Frame (Column)) - (if Row = Column then Real'(1.0) else Real'(0.0)))
           <= New_Bound);
      pragma Assert (Holds_Before (Frame, Row, Column, Old_Bound));
      Lemma_Relax_Holds (Frame, Row, Column, Old_Bound, New_Bound);
      Lemma_Append_Holds (Frame, Row, Column, New_Bound);
      pragma Assert (Holds_Before (Frame, Row, Column + 1, New_Bound));
   end Lemma_Extend_Defect_Bound;

   procedure Lemma_Next_Row_Holds (Frame : Frame_Vector_Array; Row : Frame_Component_Index; Bound : Real)
   with Ghost, Pre => Holds_Before (Frame, Row, Frame_Size, Bound), Post => Holds_Before (Frame, Row + 1, 0, Bound);
   --  A finished row is one of the rows before the next, which has no column yet.

   procedure Lemma_Next_Row_Holds (Frame : Frame_Vector_Array; Row : Frame_Component_Index; Bound : Real) is null;

   procedure Lemma_Next_Row (Frame : Frame_Vector_Array; Row : Frame_Component_Index; Bound : Real)
   with
     Ghost,
     Pre  => Is_Defect_Bound_Before (Frame, Row, Frame_Size, Bound),
     Post => Is_Defect_Bound_Before (Frame, Row + 1, 0, Bound);
   --  The pairs before the end of one row are those before the start of the next.

   procedure Lemma_Next_Row (Frame : Frame_Vector_Array; Row : Frame_Component_Index; Bound : Real) is
   begin
      pragma Assert (Holds_Before (Frame, Row, Frame_Size, Bound));
      Lemma_Next_Row_Holds (Frame, Row, Bound);
      pragma Assert (Holds_Before (Frame, Row + 1, 0, Bound));
   end Lemma_Next_Row;

   procedure Lemma_All_Rows_Hold (Frame : Frame_Vector_Array; Bound : Real)
   with Ghost, Pre => Holds_Before (Frame, Frame_Size, 0, Bound), Post => Is_Defect_Bound (Frame, Bound);
   --  Every row is before the row past the last, and every column of a row is one of the four written out.

   procedure Lemma_All_Rows_Hold (Frame : Frame_Vector_Array; Bound : Real) is null;

   procedure Lemma_All_Rows (Frame : Frame_Vector_Array; Bound : Real)
   with Ghost, Pre => Is_Defect_Bound_Before (Frame, Frame_Size, 0, Bound), Post => Is_Defect_Bound (Frame, Bound);
   --  The pairs before the end of the last row are all of them.

   procedure Lemma_All_Rows (Frame : Frame_Vector_Array; Bound : Real) is
   begin
      pragma Assert (Holds_Before (Frame, Frame_Size, 0, Bound));
      Lemma_All_Rows_Hold (Frame, Bound);
   end Lemma_All_Rows;

   function Frame_Defect (Frame : Frame_Vector_Array) return Dimensionless is
      Worst : Dimensionless := 0.0;
   begin
      for I in Frame_Component_Index loop
         for Axis in Axis_Name loop
            --  The comparison fails for a NaN as well.
            if not (abs Frame (I) (Axis) <= Largest_Component) then
               return Dimensionless'Last;
            end if;
            pragma Loop_Invariant (for all Seen in X_Axis .. Axis => abs Frame (I) (Seen) <= Largest_Component);
         end loop;
         pragma Assert (Is_Within_Limit (Frame (I)));
         pragma Loop_Invariant (for all Seen in 0 .. I => Is_Within_Limit (Frame (Seen)));
      end loop;
      pragma Assert (for all Seen in Frame_Component_Index => Is_Within_Limit (Frame (Seen)));
      Lemma_No_Rows (Frame, R (Worst) + Defect_Allowance);
      for I in Frame_Component_Index loop
         pragma Loop_Invariant (Worst >= 0.0);
         pragma Loop_Invariant (Is_Defect_Bound_Before (Frame, I, 0, R (Worst) + Defect_Allowance));
         for J in Frame_Component_Index loop
            pragma Loop_Invariant (Worst >= 0.0);
            pragma Loop_Invariant (Is_Defect_Bound_Before (Frame, I, J, R (Worst) + Defect_Allowance));
            declare
               Sum          : Dimensionless := 0.0;
               Worst_Before : constant Dimensionless := Worst
               with Ghost;
            begin
               pragma Assert (Is_Within_Limit (Frame (I)));
               pragma Assert (Is_Within_Limit (Frame (J)));
               for Axis in Axis_Name loop
                  pragma Loop_Invariant (abs Sum <= Sum_Limit);
                  pragma
                    Loop_Invariant
                      (abs (R (Sum) - Partial_Dot (Frame (I), Frame (J), Axis_Name'Pos (Axis)))
                       <= Accumulated_Error (Axis_Name'Pos (Axis)));
                  Lemma_Dot_Step (Frame (I), Frame (J), Axis, Sum);
                  Sum := Sum + Frame (I) (Axis) * Frame (J) (Axis);
               end loop;
               pragma Assert (abs Sum <= Sum_Limit);
               pragma Assert (abs (R (Sum) - Dot_Real (Frame (I), Frame (J))) <= Dot_Error);
               Lemma_Worst_Covers_Entry (Sum, (if I = J then 1.0 else 0.0), Worst, Dot_Real (Frame (I), Frame (J)));
               Worst := Dimensionless'Max (Worst, abs (Sum - (if I = J then 1.0 else 0.0)));
               pragma Assert (Worst >= 0.0);
               pragma Assert (R (Worst_Before) <= R (Worst));
               pragma
                 Assert
                   (abs (Dot_Real (Frame (I), Frame (J)) - R (if I = J then 1.0 else 0.0))
                    <= R (Worst) + Defect_Allowance);
               Lemma_Extend_Defect_Bound
                 (Frame, I, J, R (Worst_Before) + Defect_Allowance, R (Worst) + Defect_Allowance);
            end;
         end loop;
         Lemma_Next_Row (Frame, I, R (Worst) + Defect_Allowance);
      end loop;
      Lemma_All_Rows (Frame, R (Worst) + Defect_Allowance);
      return Worst;
   end Frame_Defect;

   ---------------------------------------------------------------------------
   --  The tangent is unit in frame coordinates
   ---------------------------------------------------------------------------

   procedure Lemma_Square_Of_Double (Y, N : Real)
   with Ghost, Pre => N = 2.0 * Y, Post => N * N = 4.0 * (Y * Y);

   procedure Lemma_Square_Of_Double (Y, N : Real) is null;

   procedure Lemma_Projected_Square (S, N, D : Real)
   with Ghost, Pre => N = 1.0 - S and then D = 1.0 + S, Post => N * N + 4.0 * S = D * D;
   --  (1 - S)² + 4·S = (1 + S)².

   procedure Lemma_Projected_Square (S, N, D : Real) is
   begin
      pragma Assert (N * N = 1.0 - 2.0 * S + S * S);
      pragma Assert (D * D = 1.0 + 2.0 * S + S * S);
   end Lemma_Projected_Square;

   procedure Lemma_Stereographic_Identity (Y1, Y2, Y3, N0, N1, N2, N3, D : Real)
   with
     Ghost,
     Pre  =>
       N0 = 1.0 - Y1 * Y1 - (Y2 * Y2 + Y3 * Y3)
       and then N1 = 2.0 * Y1
       and then N2 = 2.0 * Y2
       and then N3 = 2.0 * Y3
       and then D = 1.0 + Y1 * Y1 + (Y2 * Y2 + Y3 * Y3),
     Post => N0 * N0 + N1 * N1 + N2 * N2 + N3 * N3 = D * D;
   --  With S = Y·Y: (1 - S)² + (2·Y₁)² + (2·Y₂)² + (2·Y₃)² = (1 - S)² + 4·S = (1 + S)².

   procedure Lemma_Stereographic_Identity (Y1, Y2, Y3, N0, N1, N2, N3, D : Real) is
      S : constant Real := Y1 * Y1 + Y2 * Y2 + Y3 * Y3;
   begin
      Lemma_Square_Of_Double (Y1, N1);
      Lemma_Square_Of_Double (Y2, N2);
      Lemma_Square_Of_Double (Y3, N3);
      Lemma_Projected_Square (S, N0, D);
      pragma Assert (N1 * N1 + N2 * N2 + N3 * N3 = 4.0 * S);
   end Lemma_Stereographic_Identity;

   procedure Lemma_Frame_Coordinates_Are_Unit (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Stereographic_Identity
        (Y1 => Chart_Value (Coefficients, 1, V),
         Y2 => Chart_Value (Coefficients, 2, V),
         Y3 => Chart_Value (Coefficients, 3, V),
         N0 => Numerator (Coefficients, 0, V),
         N1 => Numerator (Coefficients, 1, V),
         N2 => Numerator (Coefficients, 2, V),
         N3 => Numerator (Coefficients, 3, V),
         D  => Denominator (Coefficients, V));
   end Lemma_Frame_Coordinates_Are_Unit;

   ---------------------------------------------------------------------------
   --  The squared speed as a quadratic form in the frame's dot products
   ---------------------------------------------------------------------------

   procedure Lemma_Scale_Combination (P, N0, N1, N2, N3, F0, F1, F2, F3 : Real)
   with
     Ghost,
     Post =>
       P * (N0 * F0 + N1 * F1 + N2 * F2 + N3 * F3) = N0 * (P * F0) + N1 * (P * F1) + N2 * (P * F2) + N3 * (P * F3);
   --  One axis: a multiple of a combination is the combination of the multiples.

   procedure Lemma_Scale_Combination (P, N0, N1, N2, N3, F0, F1, F2, F3 : Real) is
   begin
      pragma
        Assert
          (P * (N0 * F0 + N1 * F1 + N2 * F2 + N3 * F3)
           = P * (N0 * F0) + P * (N1 * F1) + P * (N2 * F2) + P * (N3 * F3));
      pragma Assert (P * (N0 * F0) = N0 * (P * F0));
      pragma Assert (P * (N1 * F1) = N1 * (P * F1));
      pragma Assert (P * (N2 * F2) = N2 * (P * F2));
      pragma Assert (P * (N3 * F3) = N3 * (P * F3));
   end Lemma_Scale_Combination;

   procedure Lemma_Collect (N, A, B, C, D : Real)
   with Ghost, Post => N * A + N * B + N * C + N * D = N * (A + B + C + D);
   --  One vector: its four axis terms share the factor N.

   procedure Lemma_Collect (N, A, B, C, D : Real) is null;

   procedure Lemma_Regroup
     (P_X, P_Y, P_Z, P_E : Real;
      N0, N1, N2, N3     : Real;
      F0X, F0Y, F0Z, F0E : Real;
      F1X, F1Y, F1Z, F1E : Real;
      F2X, F2Y, F2Z, F2E : Real;
      F3X, F3Y, F3Z, F3E : Real)
   with
     Ghost,
     Post =>
       P_X * (N0 * F0X + N1 * F1X + N2 * F2X + N3 * F3X) + P_Y * (N0 * F0Y + N1 * F1Y + N2 * F2Y + N3 * F3Y)
       + P_Z * (N0 * F0Z + N1 * F1Z + N2 * F2Z + N3 * F3Z)
       + P_E * (N0 * F0E + N1 * F1E + N2 * F2E + N3 * F3E)
       = N0 * (P_X * F0X + P_Y * F0Y + P_Z * F0Z + P_E * F0E) + N1 * (P_X * F1X + P_Y * F1Y + P_Z * F1Z + P_E * F1E)
         + N2 * (P_X * F2X + P_Y * F2Y + P_Z * F2Z + P_E * F2E)
         + N3 * (P_X * F3X + P_Y * F3Y + P_Z * F3Z + P_E * F3E);
   --  A weighted sum over the axes of combinations of four vectors, regrouped by vector:
   --  Σₐ Pₐ·(Σₖ Nₖ·Fₖₐ) = Σₖ Nₖ·(Σₐ Pₐ·Fₖₐ).

   procedure Lemma_Regroup
     (P_X, P_Y, P_Z, P_E : Real;
      N0, N1, N2, N3     : Real;
      F0X, F0Y, F0Z, F0E : Real;
      F1X, F1Y, F1Z, F1E : Real;
      F2X, F2Y, F2Z, F2E : Real;
      F3X, F3Y, F3Z, F3E : Real) is
   begin
      Lemma_Scale_Combination (P_X, N0, N1, N2, N3, F0X, F1X, F2X, F3X);
      Lemma_Scale_Combination (P_Y, N0, N1, N2, N3, F0Y, F1Y, F2Y, F3Y);
      Lemma_Scale_Combination (P_Z, N0, N1, N2, N3, F0Z, F1Z, F2Z, F3Z);
      Lemma_Scale_Combination (P_E, N0, N1, N2, N3, F0E, F1E, F2E, F3E);
      Lemma_Collect (N0, P_X * F0X, P_Y * F0Y, P_Z * F0Z, P_E * F0E);
      Lemma_Collect (N1, P_X * F1X, P_Y * F1Y, P_Z * F1Z, P_E * F1E);
      Lemma_Collect (N2, P_X * F2X, P_Y * F2Y, P_Z * F2Z, P_E * F2E);
      Lemma_Collect (N3, P_X * F3X, P_Y * F3Y, P_Z * F3Z, P_E * F3E);
   end Lemma_Regroup;

   function Tangent_Dot
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; K : Frame_Component_Index; V : Real) return Real
   is (Tangent_Numerator (Frame, Coefficients, X_Axis, V) * R (Frame (K) (X_Axis))
       + Tangent_Numerator (Frame, Coefficients, Y_Axis, V) * R (Frame (K) (Y_Axis))
       + Tangent_Numerator (Frame, Coefficients, Z_Axis, V) * R (Frame (K) (Z_Axis))
       + Tangent_Numerator (Frame, Coefficients, E_Axis, V) * R (Frame (K) (E_Axis)))
   with Ghost;
   --  The tangent numerator at V dotted with one frame vector.

   procedure Lemma_Square_By_Vector
     (T_X, T_Y, T_Z, T_E : Real;
      N0, N1, N2, N3     : Real;
      F0X, F0Y, F0Z, F0E : Real;
      F1X, F1Y, F1Z, F1E : Real;
      F2X, F2Y, F2Z, F2E : Real;
      F3X, F3Y, F3Z, F3E : Real;
      H0, H1, H2, H3, S  : Real)
   with
     Ghost,
     Pre  =>
       T_X = N0 * F0X + N1 * F1X + N2 * F2X + N3 * F3X
       and then T_Y = N0 * F0Y + N1 * F1Y + N2 * F2Y + N3 * F3Y
       and then T_Z = N0 * F0Z + N1 * F1Z + N2 * F2Z + N3 * F3Z
       and then T_E = N0 * F0E + N1 * F1E + N2 * F2E + N3 * F3E
       and then H0 = T_X * F0X + T_Y * F0Y + T_Z * F0Z + T_E * F0E
       and then H1 = T_X * F1X + T_Y * F1Y + T_Z * F1Z + T_E * F1E
       and then H2 = T_X * F2X + T_Y * F2Y + T_Z * F2Z + T_E * F2E
       and then H3 = T_X * F3X + T_Y * F3Y + T_Z * F3Z + T_E * F3E
       and then S = T_X * T_X + T_Y * T_Y + T_Z * T_Z + T_E * T_E,
     Post => S = N0 * H0 + N1 * H1 + N2 * H2 + N3 * H3;
   --  With T = Σₖ Nₖ·Fₖ and Hₖ = T·Fₖ: T·T = Σₖ Nₖ·Hₖ.

   procedure Lemma_Square_By_Vector
     (T_X, T_Y, T_Z, T_E : Real;
      N0, N1, N2, N3     : Real;
      F0X, F0Y, F0Z, F0E : Real;
      F1X, F1Y, F1Z, F1E : Real;
      F2X, F2Y, F2Z, F2E : Real;
      F3X, F3Y, F3Z, F3E : Real;
      H0, H1, H2, H3, S  : Real) is
   begin
      Lemma_Regroup
        (T_X,
         T_Y,
         T_Z,
         T_E,
         N0,
         N1,
         N2,
         N3,
         F0X,
         F0Y,
         F0Z,
         F0E,
         F1X,
         F1Y,
         F1Z,
         F1E,
         F2X,
         F2Y,
         F2Z,
         F2E,
         F3X,
         F3Y,
         F3Z,
         F3E);
      pragma Assert (T_X * T_X = T_X * (N0 * F0X + N1 * F1X + N2 * F2X + N3 * F3X));
      pragma Assert (T_Y * T_Y = T_Y * (N0 * F0Y + N1 * F1Y + N2 * F2Y + N3 * F3Y));
      pragma Assert (T_Z * T_Z = T_Z * (N0 * F0Z + N1 * F1Z + N2 * F2Z + N3 * F3Z));
      pragma Assert (T_E * T_E = T_E * (N0 * F0E + N1 * F1E + N2 * F2E + N3 * F3E));
      pragma Assert (N0 * H0 = N0 * (T_X * F0X + T_Y * F0Y + T_Z * F0Z + T_E * F0E));
      pragma Assert (N1 * H1 = N1 * (T_X * F1X + T_Y * F1Y + T_Z * F1Z + T_E * F1E));
      pragma Assert (N2 * H2 = N2 * (T_X * F2X + T_Y * F2Y + T_Z * F2Z + T_E * F2E));
      pragma Assert (N3 * H3 = N3 * (T_X * F3X + T_Y * F3Y + T_Z * F3Z + T_E * F3E));
      pragma Assert (S = N0 * H0 + N1 * H1 + N2 * H2 + N3 * H3);
   end Lemma_Square_By_Vector;

   procedure Lemma_Speed_By_Frame_Vector (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Post =>
       Speed_Numerator_Square (Frame, Coefficients, V)
       = Numerator (Coefficients, 0, V) * Tangent_Dot (Frame, Coefficients, 0, V)
         + Numerator (Coefficients, 1, V) * Tangent_Dot (Frame, Coefficients, 1, V)
         + Numerator (Coefficients, 2, V) * Tangent_Dot (Frame, Coefficients, 2, V)
         + Numerator (Coefficients, 3, V) * Tangent_Dot (Frame, Coefficients, 3, V);
   --  T·T = Σₖ Nₖ·(T·Fₖ), since T = Σₖ Nₖ·Fₖ.

   procedure Lemma_Speed_By_Frame_Vector (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Square_By_Vector
        (T_X => Tangent_Numerator (Frame, Coefficients, X_Axis, V),
         T_Y => Tangent_Numerator (Frame, Coefficients, Y_Axis, V),
         T_Z => Tangent_Numerator (Frame, Coefficients, Z_Axis, V),
         T_E => Tangent_Numerator (Frame, Coefficients, E_Axis, V),
         N0  => Numerator (Coefficients, 0, V),
         N1  => Numerator (Coefficients, 1, V),
         N2  => Numerator (Coefficients, 2, V),
         N3  => Numerator (Coefficients, 3, V),
         F0X => R (Frame (0) (X_Axis)),
         F0Y => R (Frame (0) (Y_Axis)),
         F0Z => R (Frame (0) (Z_Axis)),
         F0E => R (Frame (0) (E_Axis)),
         F1X => R (Frame (1) (X_Axis)),
         F1Y => R (Frame (1) (Y_Axis)),
         F1Z => R (Frame (1) (Z_Axis)),
         F1E => R (Frame (1) (E_Axis)),
         F2X => R (Frame (2) (X_Axis)),
         F2Y => R (Frame (2) (Y_Axis)),
         F2Z => R (Frame (2) (Z_Axis)),
         F2E => R (Frame (2) (E_Axis)),
         F3X => R (Frame (3) (X_Axis)),
         F3Y => R (Frame (3) (Y_Axis)),
         F3Z => R (Frame (3) (Z_Axis)),
         F3E => R (Frame (3) (E_Axis)),
         H0  => Tangent_Dot (Frame, Coefficients, 0, V),
         H1  => Tangent_Dot (Frame, Coefficients, 1, V),
         H2  => Tangent_Dot (Frame, Coefficients, 2, V),
         H3  => Tangent_Dot (Frame, Coefficients, 3, V),
         S   => Speed_Numerator_Square (Frame, Coefficients, V));
   end Lemma_Speed_By_Frame_Vector;

   procedure Lemma_Tangent_Dot
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; K : Frame_Component_Index; V : Real)
   with
     Ghost,
     Post =>
       Tangent_Dot (Frame, Coefficients, K, V)
       = Numerator (Coefficients, 0, V) * Dot_Real (Frame (K), Frame (0))
         + Numerator (Coefficients, 1, V) * Dot_Real (Frame (K), Frame (1))
         + Numerator (Coefficients, 2, V) * Dot_Real (Frame (K), Frame (2))
         + Numerator (Coefficients, 3, V) * Dot_Real (Frame (K), Frame (3));
   --  T·Fₖ = Σₗ Nₗ·(Fₖ·Fₗ), again since T = Σₗ Nₗ·Fₗ.

   procedure Lemma_Tangent_Dot
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; K : Frame_Component_Index; V : Real)
   is
      P_X : constant Real := R (Frame (K) (X_Axis));
      P_Y : constant Real := R (Frame (K) (Y_Axis));
      P_Z : constant Real := R (Frame (K) (Z_Axis));
      P_E : constant Real := R (Frame (K) (E_Axis));
      T_X : constant Real := Tangent_Numerator (Frame, Coefficients, X_Axis, V);
      T_Y : constant Real := Tangent_Numerator (Frame, Coefficients, Y_Axis, V);
      T_Z : constant Real := Tangent_Numerator (Frame, Coefficients, Z_Axis, V);
      T_E : constant Real := Tangent_Numerator (Frame, Coefficients, E_Axis, V);
   begin
      Lemma_Regroup
        (P_X => P_X,
         P_Y => P_Y,
         P_Z => P_Z,
         P_E => P_E,
         N0  => Numerator (Coefficients, 0, V),
         N1  => Numerator (Coefficients, 1, V),
         N2  => Numerator (Coefficients, 2, V),
         N3  => Numerator (Coefficients, 3, V),
         F0X => R (Frame (0) (X_Axis)),
         F0Y => R (Frame (0) (Y_Axis)),
         F0Z => R (Frame (0) (Z_Axis)),
         F0E => R (Frame (0) (E_Axis)),
         F1X => R (Frame (1) (X_Axis)),
         F1Y => R (Frame (1) (Y_Axis)),
         F1Z => R (Frame (1) (Z_Axis)),
         F1E => R (Frame (1) (E_Axis)),
         F2X => R (Frame (2) (X_Axis)),
         F2Y => R (Frame (2) (Y_Axis)),
         F2Z => R (Frame (2) (Z_Axis)),
         F2E => R (Frame (2) (E_Axis)),
         F3X => R (Frame (3) (X_Axis)),
         F3Y => R (Frame (3) (Y_Axis)),
         F3Z => R (Frame (3) (Z_Axis)),
         F3E => R (Frame (3) (E_Axis)));
      pragma Assert (T_X * P_X = P_X * T_X);
      pragma Assert (T_Y * P_Y = P_Y * T_Y);
      pragma Assert (T_Z * P_Z = P_Z * T_Z);
      pragma Assert (T_E * P_E = P_E * T_E);
      pragma Assert (Tangent_Dot (Frame, Coefficients, K, V) = P_X * T_X + P_Y * T_Y + P_Z * T_Z + P_E * T_E);
   end Lemma_Tangent_Dot;

   ---------------------------------------------------------------------------
   --  The quadratic form of a nearly orthonormal frame
   ---------------------------------------------------------------------------

   procedure Lemma_Term_Bound (N, E, Bound : Real)
   with Ghost, Pre => abs E <= Bound, Post => abs (N * E) <= Bound * abs N;

   procedure Lemma_Term_Bound (N, E, Bound : Real) is
   begin
      pragma Assert (abs (N * E) = abs N * abs E);
      pragma Assert (abs N * abs E <= abs N * Bound);
   end Lemma_Term_Bound;

   procedure Lemma_Weighted_Sum_Bound (N0, N1, N2, N3, E0, E1, E2, E3, Bound : Real)
   with
     Ghost,
     Pre  => abs E0 <= Bound and then abs E1 <= Bound and then abs E2 <= Bound and then abs E3 <= Bound,
     Post => abs (N0 * E0 + N1 * E1 + N2 * E2 + N3 * E3) <= Bound * (abs N0 + abs N1 + abs N2 + abs N3);
   --  |Σₖ Nₖ·Eₖ| ≤ Bound·Σₖ|Nₖ| when every |Eₖ| ≤ Bound.

   procedure Lemma_Weighted_Sum_Bound (N0, N1, N2, N3, E0, E1, E2, E3, Bound : Real) is
   begin
      Lemma_Term_Bound (N0, E0, Bound);
      Lemma_Term_Bound (N1, E1, Bound);
      Lemma_Term_Bound (N2, E2, Bound);
      Lemma_Term_Bound (N3, E3, Bound);
      pragma
        Assert
          (Bound * (abs N0 + abs N1 + abs N2 + abs N3)
           = Bound * abs N0 + Bound * abs N1 + Bound * abs N2 + Bound * abs N3);
   end Lemma_Weighted_Sum_Bound;

   procedure Lemma_Shift_Row (N0, N1, N2, N3, G0, G1, G2, G3, D0, D1, D2, D3 : Real)
   with
     Ghost,
     Post =>
       N0 * (G0 - D0) + N1 * (G1 - D1) + N2 * (G2 - D2) + N3 * (G3 - D3)
       = (N0 * G0 + N1 * G1 + N2 * G2 + N3 * G3) - (N0 * D0 + N1 * D1 + N2 * D2 + N3 * D3);

   procedure Lemma_Shift_Row (N0, N1, N2, N3, G0, G1, G2, G3, D0, D1, D2, D3 : Real) is
   begin
      pragma Assert (N0 * (G0 - D0) = N0 * G0 - N0 * D0);
      pragma Assert (N1 * (G1 - D1) = N1 * G1 - N1 * D1);
      pragma Assert (N2 * (G2 - D2) = N2 * G2 - N2 * D2);
      pragma Assert (N3 * (G3 - D3) = N3 * G3 - N3 * D3);
   end Lemma_Shift_Row;

   procedure Lemma_Twice_Product (X, Y : Real)
   with Ghost, Post => 2.0 * (X * Y) <= X * X + Y * Y;

   procedure Lemma_Twice_Product (X, Y : Real) is
   begin
      pragma Assert ((X - Y) * (X - Y) >= 0.0);
      pragma Assert ((X - Y) * (X - Y) = X * X - 2.0 * (X * Y) + Y * Y);
   end Lemma_Twice_Product;

   procedure Lemma_Square_Of_Sum (A, B, C, D : Real)
   with
     Ghost,
     Post =>
       (A + B + C + D) * (A + B + C + D)
       = A * A + B * B + C * C + D * D + 2.0 * (A * B) + 2.0 * (A * C) + 2.0 * (A * D) + 2.0 * (B * C) + 2.0 * (B * D)
         + 2.0 * (C * D);

   procedure Lemma_Square_Of_Sum (A, B, C, D : Real) is
      S : constant Real := A + B + C + D;
   begin
      pragma Assert (S * S = A * S + B * S + C * S + D * S);
      pragma Assert (A * S = A * A + A * B + A * C + A * D);
      pragma Assert (B * S = B * A + B * B + B * C + B * D);
      pragma Assert (C * S = C * A + C * B + C * C + C * D);
      pragma Assert (D * S = D * A + D * B + D * C + D * D);
      pragma Assert (B * A = A * B);
      pragma Assert (C * A = A * C);
      pragma Assert (C * B = B * C);
      pragma Assert (D * A = A * D);
      pragma Assert (D * B = B * D);
      pragma Assert (D * C = C * D);
   end Lemma_Square_Of_Sum;

   procedure Lemma_Sum_Square_Bound (A, B, C, D : Real)
   with Ghost, Post => (A + B + C + D) * (A + B + C + D) <= 4.0 * (A * A + B * B + C * C + D * D);
   --  2·X·Y ≤ X² + Y² for each of the six pairs.

   procedure Lemma_Sum_Square_Bound (A, B, C, D : Real) is
   begin
      Lemma_Square_Of_Sum (A, B, C, D);
      Lemma_Twice_Product (A, B);
      Lemma_Twice_Product (A, C);
      Lemma_Twice_Product (A, D);
      Lemma_Twice_Product (B, C);
      Lemma_Twice_Product (B, D);
      Lemma_Twice_Product (C, D);
   end Lemma_Sum_Square_Bound;

   procedure Lemma_Abs_Square (X : Real)
   with Ghost, Post => abs X * abs X = X * X;

   procedure Lemma_Abs_Square (X : Real) is null;

   procedure Lemma_Quadratic_Form_Bound (N0, N1, N2, N3, H0, H1, H2, H3, S, Q, Bound : Real)
   with
     Ghost,
     Pre  =>
       Bound >= 0.0
       and then S = N0 * H0 + N1 * H1 + N2 * H2 + N3 * H3
       and then Q = N0 * N0 + N1 * N1 + N2 * N2 + N3 * N3
       and then abs (H0 - N0) <= Bound * (abs N0 + abs N1 + abs N2 + abs N3)
       and then abs (H1 - N1) <= Bound * (abs N0 + abs N1 + abs N2 + abs N3)
       and then abs (H2 - N2) <= Bound * (abs N0 + abs N1 + abs N2 + abs N3)
       and then abs (H3 - N3) <= Bound * (abs N0 + abs N1 + abs N2 + abs N3),
     Post => abs (S - Q) <= 4.0 * Bound * Q;
   --  With A = Σₖ|Nₖ|: |Σₖ Nₖ·(Hₖ - Nₖ)| ≤ Bound·A·A, and A² ≤ 4·Σₖ Nₖ².

   procedure Lemma_Quadratic_Form_Bound (N0, N1, N2, N3, H0, H1, H2, H3, S, Q, Bound : Real) is
      A : constant Real := abs N0 + abs N1 + abs N2 + abs N3;
   begin
      Lemma_Shift_Row (N0, N1, N2, N3, H0, H1, H2, H3, N0, N1, N2, N3);
      pragma Assert (S - Q = N0 * (H0 - N0) + N1 * (H1 - N1) + N2 * (H2 - N2) + N3 * (H3 - N3));
      Lemma_Weighted_Sum_Bound (N0, N1, N2, N3, H0 - N0, H1 - N1, H2 - N2, H3 - N3, Bound * A);
      pragma Assert (abs (S - Q) <= (Bound * A) * A);
      Lemma_Sum_Square_Bound (abs N0, abs N1, abs N2, abs N3);
      Lemma_Abs_Square (N0);
      Lemma_Abs_Square (N1);
      Lemma_Abs_Square (N2);
      Lemma_Abs_Square (N3);
      pragma Assert (A * A <= 4.0 * Q);
      pragma Assert ((Bound * A) * A = Bound * (A * A));
      pragma Assert (Bound * (A * A) <= Bound * (4.0 * Q));
      pragma Assert (Bound * (4.0 * Q) = 4.0 * Bound * Q);
   end Lemma_Quadratic_Form_Bound;

   procedure Lemma_Defect_Instance (Frame : Frame_Vector_Array; Bound : Real; I, J : Frame_Component_Index)
   with
     Ghost,
     Pre  => Is_Defect_Bound (Frame, Bound),
     Post => abs (Dot_Real (Frame (I), Frame (J)) - (if I = J then Real'(1.0) else Real'(0.0))) <= Bound;

   procedure Lemma_Defect_Instance (Frame : Frame_Vector_Array; Bound : Real; I, J : Frame_Component_Index) is null;

   function Absolute_Sum (Coefficients : Bernstein_Chart; V : Real) return Real
   is (abs (Numerator (Coefficients, 0, V))
       + abs (Numerator (Coefficients, 1, V))
       + abs (Numerator (Coefficients, 2, V))
       + abs (Numerator (Coefficients, 3, V)))
   with Ghost;

   procedure Lemma_Row
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Bound : Real; K : Frame_Component_Index; V : Real)
   with
     Ghost,
     Pre  => Is_Defect_Bound (Frame, Bound),
     Post =>
       abs (Tangent_Dot (Frame, Coefficients, K, V) - Numerator (Coefficients, K, V))
       <= Bound * Absolute_Sum (Coefficients, V);
   --  T·Fₖ - Nₖ = Σₗ Nₗ·(Fₖ·Fₗ - δₖₗ), each term of which is within Bound·|Nₗ|.

   procedure Lemma_Row
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Bound : Real; K : Frame_Component_Index; V : Real)
   is
      N0 : constant Real := Numerator (Coefficients, 0, V);
      N1 : constant Real := Numerator (Coefficients, 1, V);
      N2 : constant Real := Numerator (Coefficients, 2, V);
      N3 : constant Real := Numerator (Coefficients, 3, V);
      G0 : constant Real := Dot_Real (Frame (K), Frame (0));
      G1 : constant Real := Dot_Real (Frame (K), Frame (1));
      G2 : constant Real := Dot_Real (Frame (K), Frame (2));
      G3 : constant Real := Dot_Real (Frame (K), Frame (3));
      D0 : constant Real := (if K = 0 then Real'(1.0) else Real'(0.0));
      D1 : constant Real := (if K = 1 then Real'(1.0) else Real'(0.0));
      D2 : constant Real := (if K = 2 then Real'(1.0) else Real'(0.0));
      D3 : constant Real := (if K = 3 then Real'(1.0) else Real'(0.0));
   begin
      Lemma_Tangent_Dot (Frame, Coefficients, K, V);
      pragma Assert (Tangent_Dot (Frame, Coefficients, K, V) = N0 * G0 + N1 * G1 + N2 * G2 + N3 * G3);
      Lemma_Defect_Instance (Frame, Bound, K, 0);
      Lemma_Defect_Instance (Frame, Bound, K, 1);
      Lemma_Defect_Instance (Frame, Bound, K, 2);
      Lemma_Defect_Instance (Frame, Bound, K, 3);
      pragma Assert (abs (G0 - D0) <= Bound);
      pragma Assert (abs (G1 - D1) <= Bound);
      pragma Assert (abs (G2 - D2) <= Bound);
      pragma Assert (abs (G3 - D3) <= Bound);
      Lemma_Weighted_Sum_Bound (N0, N1, N2, N3, G0 - D0, G1 - D1, G2 - D2, G3 - D3, Bound);
      Lemma_Shift_Row (N0, N1, N2, N3, G0, G1, G2, G3, D0, D1, D2, D3);
      pragma Assert (N0 * D0 + N1 * D1 + N2 * D2 + N3 * D3 = Numerator (Coefficients, K, V));
   end Lemma_Row;

   procedure Lemma_Unit_Speed (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Bound, V : Real) is
   begin
      Lemma_Defect_Instance (Frame, Bound, 0, 0);
      pragma Assert (Bound >= 0.0);
      Lemma_Frame_Coordinates_Are_Unit (Coefficients, V);
      Lemma_Speed_By_Frame_Vector (Frame, Coefficients, V);
      Lemma_Row (Frame, Coefficients, Bound, 0, V);
      Lemma_Row (Frame, Coefficients, Bound, 1, V);
      Lemma_Row (Frame, Coefficients, Bound, 2, V);
      Lemma_Row (Frame, Coefficients, Bound, 3, V);
      Lemma_Quadratic_Form_Bound
        (N0    => Numerator (Coefficients, 0, V),
         N1    => Numerator (Coefficients, 1, V),
         N2    => Numerator (Coefficients, 2, V),
         N3    => Numerator (Coefficients, 3, V),
         H0    => Tangent_Dot (Frame, Coefficients, 0, V),
         H1    => Tangent_Dot (Frame, Coefficients, 1, V),
         H2    => Tangent_Dot (Frame, Coefficients, 2, V),
         H3    => Tangent_Dot (Frame, Coefficients, 3, V),
         S     => Speed_Numerator_Square (Frame, Coefficients, V),
         Q     => Denominator (Coefficients, V) * Denominator (Coefficients, V),
         Bound => Bound);
   end Lemma_Unit_Speed;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;
