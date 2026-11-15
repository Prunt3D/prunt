--  Part of the Prunt Motion Controller
--
--  Derived from the reference implementation of LAPACK (https://www.netlib.org/lapack/).
--
--  Copyright (c) 1992-2025 The University of Tennessee and The University
--                          of Tennessee Research Foundation.  All rights
--                          reserved.
--  Copyright (c) 2000-2025 The University of California Berkeley. All
--                          rights reserved.
--  Copyright (c) 2006-2025 The University of Colorado Denver.  All rights
--                          reserved.
--
--  $COPYRIGHT$
--
--  Additional copyrights may follow
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  $HEADER$
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met:
--
--  - Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer listed
--    in this license in the documentation and/or other materials
--    provided with the distribution.
--
--  - Neither the name of the copyright holders nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
--  The copyright holders provide no reassurances that the source code
--  provided does not infringe any patent, copyright, or any other
--  intellectual property rights of third parties.  The copyright holders
--  disclaim any liability to any recipient for claims brought against
--  recipient by any third party for infringement of that parties
--  intellectual property rights.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------

with Prunt.LAPACK.Bounds;
with Prunt.LAPACK.Norms;
with Prunt.LAPACK.Reflectors;

package body Prunt.LAPACK.Hessenberg_Reduction
  with SPARK_Mode
is
   pragma Assertion_Policy (Ignore);

   subtype Scratch_Vector is Real_Vector (Index);
   --  A reflector vector, or the vector w of an application.

   --  Magnitudes. A step of the reduction starts with every element of the block within Balanced_Limit, and the
   --  test at its end shows the same of the block it leaves. In between, the proof only shows that nothing
   --  overflows: it bounds a sum of products by the number of its terms times the bound of one term.

   Right_Term_Limit : constant Real := 2#1.0#E973;
   --  The product of an element of the block and an element of v, which is two at most.

   Right_Sum_Limit : constant Real := 2#1.0#E978;
   --  A sum of Maximum_Order such products at most.

   Right_Work_Limit : constant Real := 2#1.0#E979;
   --  An element of w = A * v.

   Right_Scaled_Limit : constant Real := 2#1.0#E981;
   --  An element of w times Tau and an element of v.

   Right_Limit : constant Real := 2#1.0#E982;
   --  An element of A * H.

   Left_Term_Limit   : constant Real := 2#1.0#E983;
   Left_Sum_Limit    : constant Real := 2#1.0#E988;
   Left_Work_Limit   : constant Real := 2#1.0#E989;
   Left_Scaled_Limit : constant Real := 2#1.0#E990;
   Left_Limit        : constant Real := 2#1.0#E991;
   --  The same for the application from the left to A * H: a product, a sum of products, an element of
   --  w = (A * H)' * v, an element of Tau * w, and an element of Tau * w times an element of v. An element of
   --  H * A * H is within twice the last.

   function Right_Partial_Limit (Terms : Natural) return Real
   is (Real (Terms) * Right_Term_Limit)
   with Ghost, Pre => Terms <= Maximum_Order;

   function Left_Partial_Limit (Terms : Natural) return Real
   is (Real (Terms) * Left_Term_Limit)
   with Ghost, Pre => Terms <= Maximum_Order;
   --  A sum of Terms products.

   procedure Lemma_Right_Count (Terms : Natural; Sum, Term : Real)
   with
     Ghost,
     Pre  =>
       Terms < Maximum_Order
       and then abs Sum <= Real (Terms) * Right_Term_Limit
       and then abs Term <= Right_Term_Limit,
     Post => abs (Sum + Term) <= Real (Terms + 1) * Right_Term_Limit;

   procedure Lemma_Left_Count (Terms : Natural; Sum, Term : Real)
   with
     Ghost,
     Pre  =>
       Terms < Maximum_Order and then abs Sum <= Real (Terms) * Left_Term_Limit and then abs Term <= Left_Term_Limit,
     Post => abs (Sum + Term) <= Real (Terms + 1) * Left_Term_Limit;
   --  One more term keeps a sum within the number of its terms times the bound of a term. The provers settle
   --  this for one number of terms at a time, so the bodies go through every number.

   procedure Lemma_Right_Term (Terms : Natural; Sum, Factor, Element : Real)
   with
     Ghost,
     Pre  =>
       Terms < Maximum_Order
       and then abs Sum <= Right_Partial_Limit (Terms)
       and then abs Factor <= 2.0
       and then abs Element <= Balanced_Limit,
     Post =>
       abs (Factor * Element) <= Right_Term_Limit
       and then abs (Sum + Factor * Element) <= Right_Sum_Limit
       and then abs (Sum + Factor * Element) <= Right_Partial_Limit (Terms + 1);

   procedure Lemma_Left_Term (Terms : Natural; Sum, Element, Factor : Real)
   with
     Ghost,
     Pre  =>
       Terms < Maximum_Order
       and then abs Sum <= Left_Partial_Limit (Terms)
       and then abs Element <= Right_Limit
       and then abs Factor <= 2.0,
     Post =>
       abs (Element * Factor) <= Left_Term_Limit
       and then abs (Sum + Element * Factor) <= Left_Sum_Limit
       and then abs (Sum + Element * Factor) <= Left_Partial_Limit (Terms + 1);
   --  One more product keeps a sum of products within the bound of that many, and of any number of them.

   procedure Lemma_Right_Total (Terms : Natural)
   with Ghost, Pre => Terms <= Maximum_Order, Post => Right_Partial_Limit (Terms) <= Right_Sum_Limit;

   procedure Lemma_Left_Total (Terms : Natural)
   with Ghost, Pre => Terms <= Maximum_Order, Post => Left_Partial_Limit (Terms) <= Left_Sum_Limit;
   --  A sum of Maximum_Order products at most is within the bound of a whole sum.

   procedure Lemma_Sum (X, Y, Limit : Real)
   with
     Ghost,
     Pre  => abs X <= Limit and then abs Y <= Limit and then Limit <= Bounds.Large,
     Post => abs (X + Y) <= 2.0 * Limit;
   --  The sum of two numbers within Limit is within twice Limit.

   procedure Lemma_Update (Element, Small, Big, Small_Limit, Big_Limit, Limit : Real)
   with
     Ghost,
     Pre  =>
       abs Small <= Small_Limit
       and then Small_Limit <= 4.0
       and then abs Big <= Big_Limit
       and then Big_Limit <= Bounds.Large
       and then Limit <= Bounds.Large
       and then Small_Limit * Big_Limit <= Limit
       and then abs Element <= Limit,
     Post =>
       abs (Small * Big) <= Limit
       and then abs (Big * Small) <= Limit
       and then abs (Element + Small * Big) <= 2.0 * Limit
       and then abs (Element + Big * Small) <= 2.0 * Limit;
   --  An element to which a product within Limit is added is within twice Limit.

   procedure Lemma_Scale (Tau, Element, Limit : Real)
   with
     Ghost,
     Pre  => Tau >= 1.0 and then Tau <= 2.0 and then abs Element <= Limit and then Limit <= Bounds.Large,
     Post => abs ((1.0 - Tau) * Element) <= Limit;
   --  A reflector of order one scales by a factor between minus one and zero.

   procedure Multiply_Columns
     (Ilo      : Positive;
      Last_Row : Natural;
      I        : Index;
      Last_V   : Index;
      V        : Scratch_Vector;
      A        : Real_Matrix;
      Work     : in out Scratch_Vector)
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then Last_Row <= A'Last (1)
       and then Last_Row <= Maximum_Order
       and then Last_V >= 2
       and then I + Last_V <= A'Last (2)
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then
         (for all R in Ilo .. Last_Row => (for all C in I + 1 .. I + Last_V => abs A (R, C) <= Balanced_Limit)),
     Post => (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Work_Limit);
   --  Work (Ilo .. Last_Row) := A (Ilo .. Last_Row, I + 1 .. I + Last_V) * v, where v is one followed by
   --  V (1 .. Last_V - 1).

   procedure Update_Columns
     (Ilo      : Positive;
      Ihi      : Index;
      Last_Row : Natural;
      I        : Index;
      Last_V   : Index;
      Tau      : Real;
      V, Work  : Scratch_Vector;
      A        : in out Real_Matrix)
   with
     Pre  =>
       Holds (Ihi, A)
       and then Last_Row <= Ihi
       and then I < Ihi
       and then Last_V in 2 .. Ihi - I
       and then Tau >= 1.0
       and then Tau <= 2.0
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Work_Limit)
       and then (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Balanced_Limit)),
     Post => (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
   --  A (Ilo .. Last_Row, I + 1 .. I + Last_V) := A - Tau * Work * v'.

   procedure Apply_Right
     (Ilo    : Positive;
      Ihi    : Index;
      I      : Index;
      Last_V : Index;
      Tau    : Real;
      V      : Scratch_Vector;
      A      : in out Real_Matrix;
      Work   : in out Scratch_Vector)
   with
     Pre  =>
       Holds (Ihi, A)
       and then I < Ihi
       and then Last_V <= Ihi - I
       and then Tau >= 1.0
       and then Tau <= 2.0
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Balanced_Limit)),
     Post => (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
   --  DLARF1F ('Right'): A (Ilo .. Ihi, I + 1 .. Ihi) := A * H, where H = I - Tau * v * v' and v is one followed by
   --  V (1 .. Last_V - 1).

   procedure Multiply_Rows
     (I           : Index;
      Last_V      : Index;
      Last_Column : Natural;
      V           : Scratch_Vector;
      A           : Real_Matrix;
      Work        : in out Scratch_Vector)
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then Last_V >= 2
       and then I + Last_V <= A'Last (1)
       and then Last_Column <= A'Last (2)
       and then Last_Column <= Maximum_Order
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then
         (for all R in I + 1 .. I + Last_V => (for all C in I + 1 .. Last_Column => abs A (R, C) <= Right_Limit)),
     Post => (for all C in I + 1 .. Last_Column => abs Work (C) <= Left_Work_Limit);
   --  Work (I + 1 .. Last_Column) := A (I + 1 .. I + Last_V, I + 1 .. Last_Column)' * v.

   procedure Update_Rows
     (Ihi         : Index;
      I           : Index;
      Last_V      : Index;
      Last_Column : Natural;
      Tau         : Real;
      V, Work     : Scratch_Vector;
      A           : in out Real_Matrix)
   with
     Pre =>
       Holds (Ihi, A)
       and then Last_Column <= Ihi
       and then I < Ihi
       and then Last_V in 2 .. Ihi - I
       and then Tau >= 1.0
       and then Tau <= 2.0
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then (for all C in I + 1 .. Last_Column => abs Work (C) <= Left_Work_Limit)
       and then (for all R in I + 1 .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
   --  A (I + 1 .. I + Last_V, I + 1 .. Last_Column) := A - Tau * v * Work'.

   procedure Apply_Left
     (N      : Order;
      Ihi    : Index;
      I      : Index;
      Last_V : Index;
      Tau    : Real;
      V      : Scratch_Vector;
      A      : in out Real_Matrix;
      Work   : in out Scratch_Vector)
   with
     Pre =>
       Holds (N, A)
       and then Ihi <= N
       and then I < Ihi
       and then Last_V <= Ihi - I
       and then Tau >= 1.0
       and then Tau <= 2.0
       and then (for all J in 1 .. Last_V - 1 => abs V (J) <= 2.0)
       and then (for all R in I + 1 .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
   --  DLARF1F ('Left'): A (I + 1 .. Ihi, I + 1 .. Ihi) := H * A.

   -----------------------
   -- Lemma_Right_Count --
   -----------------------

   procedure Lemma_Right_Count (Terms : Natural; Sum, Term : Real) is
   begin
      case Terms is
         when 0 => null; when 1 => null; when 2 => null; when 3 => null; when 4 => null; when 5 => null;
         when 6 => null; when 7 => null; when 8 => null; when 9 => null; when 10 => null; when 11 => null;
         when 12 => null; when 13 => null; when 14 => null; when 15 => null; when 16 => null; when 17 => null;
         when 18 => null; when 19 => null; when 20 => null; when 21 => null; when 22 => null; when 23 => null;
         when 24 => null; when 25 => null; when 26 => null; when 27 => null; when 28 => null; when 29 => null;
         when 30 => null; when others => null;
      end case;
      pragma Assert (abs (Sum + Term) <= Real (Terms + 1) * Right_Term_Limit);
   end Lemma_Right_Count;

   ----------------------
   -- Lemma_Left_Count --
   ----------------------

   procedure Lemma_Left_Count (Terms : Natural; Sum, Term : Real) is
   begin
      case Terms is
         when 0 => null; when 1 => null; when 2 => null; when 3 => null; when 4 => null; when 5 => null;
         when 6 => null; when 7 => null; when 8 => null; when 9 => null; when 10 => null; when 11 => null;
         when 12 => null; when 13 => null; when 14 => null; when 15 => null; when 16 => null; when 17 => null;
         when 18 => null; when 19 => null; when 20 => null; when 21 => null; when 22 => null; when 23 => null;
         when 24 => null; when 25 => null; when 26 => null; when 27 => null; when 28 => null; when 29 => null;
         when 30 => null; when others => null;
      end case;
      pragma Assert (abs (Sum + Term) <= Real (Terms + 1) * Left_Term_Limit);
   end Lemma_Left_Count;

   ----------------------
   -- Lemma_Right_Term --
   ----------------------

   procedure Lemma_Right_Term (Terms : Natural; Sum, Factor, Element : Real) is
      Term : constant Real := Factor * Element;
   begin
      Bounds.Lemma_Product (Factor, Element, 2.0, Balanced_Limit);
      pragma Assert (abs Term <= Right_Term_Limit);
      Lemma_Right_Count (Terms, Sum, Term);
      Lemma_Right_Total (Terms + 1);
   end Lemma_Right_Term;

   ---------------------
   -- Lemma_Left_Term --
   ---------------------

   procedure Lemma_Left_Term (Terms : Natural; Sum, Element, Factor : Real) is
      Term : constant Real := Element * Factor;
   begin
      Bounds.Lemma_Product (Factor, Element, 2.0, Right_Limit);
      pragma Assert (abs Term <= Left_Term_Limit);
      Lemma_Left_Count (Terms, Sum, Term);
      Lemma_Left_Total (Terms + 1);
   end Lemma_Left_Term;

   -----------------------
   -- Lemma_Right_Total --
   -----------------------

   procedure Lemma_Right_Total (Terms : Natural) is
   begin
      Bounds.Lemma_Mult_Is_Monotonic (Real (Terms), Real (Maximum_Order), Right_Term_Limit);
   end Lemma_Right_Total;

   ----------------------
   -- Lemma_Left_Total --
   ----------------------

   procedure Lemma_Left_Total (Terms : Natural) is
   begin
      Bounds.Lemma_Mult_Is_Monotonic (Real (Terms), Real (Maximum_Order), Left_Term_Limit);
   end Lemma_Left_Total;

   ---------------
   -- Lemma_Sum --
   ---------------

   procedure Lemma_Sum (X, Y, Limit : Real) is null;

   ------------------
   -- Lemma_Update --
   ------------------

   procedure Lemma_Update (Element, Small, Big, Small_Limit, Big_Limit, Limit : Real) is
      Product : constant Real := Small * Big;
   begin
      Bounds.Lemma_Product (Small, Big, Small_Limit, Big_Limit);
      pragma Assert (abs Product <= Limit);
      Lemma_Sum (Element, Product, Limit);
      pragma Assert (Big * Small = Product);
   end Lemma_Update;

   -----------------
   -- Lemma_Scale --
   -----------------

   procedure Lemma_Scale (Tau, Element, Limit : Real) is
      Factor : constant Real := 1.0 - Tau;
   begin
      pragma Assert (abs Factor <= 1.0);
      Bounds.Lemma_Shrink (Element, Factor);
      pragma Assert (Factor * Element = Element * Factor);
      pragma Assert (abs (Factor * Element) <= Limit);
   end Lemma_Scale;

   ----------------------
   -- Multiply_Columns --
   ----------------------

   procedure Multiply_Columns
     (Ilo      : Positive;
      Last_Row : Natural;
      I        : Index;
      Last_V   : Index;
      V        : Scratch_Vector;
      A        : Real_Matrix;
      Work     : in out Scratch_Vector)
   is
      Temp, Sum, Element : Real;
   begin
      --  w := A (:, I + 2 .. I + Last_V) * v (2 .. Last_V), then w := w + A (:, I + 1).
      for Row in Ilo .. Last_Row loop
         pragma Loop_Invariant (for all R in Ilo .. Row - 1 => Work (R) = 0.0);
         Work (Row) := 0.0;
      end loop;
      pragma Assert (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Partial_Limit (0));
      for J in 1 .. Last_V - 1 loop
         pragma Loop_Invariant (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Partial_Limit (J - 1));
         Temp := V (J);
         for Row in Ilo .. Last_Row loop
            pragma Loop_Invariant (for all R in Ilo .. Row - 1 => abs Work (R) <= Right_Partial_Limit (J));
            pragma Loop_Invariant (for all R in Row .. Last_Row => abs Work (R) <= Right_Partial_Limit (J - 1));
            Sum := Work (Row);
            Element := A (Row, I + 1 + J);
            Lemma_Right_Term (J - 1, Sum, Temp, Element);
            Sum := Sum + Temp * Element;
            pragma
              Assert_And_Cut
                (abs Sum <= Right_Partial_Limit (J)
                   and then (for all R in Ilo .. Row - 1 => abs Work (R) <= Right_Partial_Limit (J))
                   and then (for all R in Row .. Last_Row => abs Work (R) <= Right_Partial_Limit (J - 1)));
            Work (Row) := Sum;
         end loop;
         pragma Assert (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Partial_Limit (J));
      end loop;
      Lemma_Right_Total (Last_V - 1);
      pragma Assert (for all R in Ilo .. Last_Row => abs Work (R) <= Right_Sum_Limit);
      for Row in Ilo .. Last_Row loop
         pragma Loop_Invariant (for all R in Ilo .. Row - 1 => abs Work (R) <= Right_Work_Limit);
         pragma Loop_Invariant (for all R in Row .. Last_Row => abs Work (R) <= Right_Sum_Limit);
         Sum := Work (Row);
         Element := A (Row, I + 1);
         Lemma_Sum (Sum, Element, Right_Sum_Limit);
         Sum := Sum + Element;
         pragma
           Assert_And_Cut
             (abs Sum <= Right_Work_Limit
                and then (for all R in Ilo .. Row - 1 => abs Work (R) <= Right_Work_Limit)
                and then (for all R in Row .. Last_Row => abs Work (R) <= Right_Sum_Limit));
         Work (Row) := Sum;
      end loop;
   end Multiply_Columns;

   --------------------
   -- Update_Columns --
   --------------------

   procedure Update_Columns
     (Ilo      : Positive;
      Ihi      : Index;
      Last_Row : Natural;
      I        : Index;
      Last_V   : Index;
      Tau      : Real;
      V, Work  : Scratch_Vector;
      A        : in out Real_Matrix)
   is
      Temp, Sum, Element : Real;
   begin
      --  A := A - Tau * w * v'.
      for Row in Ilo .. Last_Row loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
         pragma
           Loop_Invariant (for all R in Ilo .. Ihi => (for all C in I + 2 .. Ihi => abs A (R, C) <= Balanced_Limit));
         pragma Loop_Invariant (for all R in Row .. Ihi => abs A (R, I + 1) <= Balanced_Limit);
         Element := A (Row, I + 1);
         Sum := Work (Row);
         Lemma_Update (Element, -Tau, Sum, 2.0, Right_Work_Limit, Right_Scaled_Limit);
         Element := Element + (-Tau) * Sum;
         pragma
           Assert_And_Cut
             (abs Element <= Right_Limit
                and then (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit))
                and then (for all R in Ilo .. Ihi => (for all C in I + 2 .. Ihi => abs A (R, C) <= Balanced_Limit))
                and then (for all R in Row .. Ihi => abs A (R, I + 1) <= Balanced_Limit));
         A (Row, I + 1) := Element;
      end loop;
      for J in 1 .. Last_V - 1 loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
         pragma
           Loop_Invariant
             (for all R in Ilo .. Ihi => (for all C in I + 1 + J .. Ihi => abs A (R, C) <= Balanced_Limit));
         if V (J) /= 0.0 then
            Bounds.Lemma_Product (-Tau, V (J), 2.0, 2.0);
            Temp := (-Tau) * V (J);
            pragma Assert (abs Temp <= 4.0);
            for Row in Ilo .. Last_Row loop
               pragma
                 Loop_Invariant
                   (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
               pragma
                 Loop_Invariant
                   (for all R in Ilo .. Ihi => (for all C in I + 2 + J .. Ihi => abs A (R, C) <= Balanced_Limit));
               pragma Loop_Invariant (for all R in Row .. Ihi => abs A (R, I + 1 + J) <= Balanced_Limit);
               Element := A (Row, I + 1 + J);
               Sum := Work (Row);
               Lemma_Update (Element, Temp, Sum, 4.0, Right_Work_Limit, Right_Scaled_Limit);
               Element := Element + Sum * Temp;
               pragma
                 Assert_And_Cut
                   (abs Element <= Right_Limit
                      and then
                        (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit))
                      and then
                        (for all R in Ilo .. Ihi => (for all C in I + 2 + J .. Ihi => abs A (R, C) <= Balanced_Limit))
                      and then (for all R in Row .. Ihi => abs A (R, I + 1 + J) <= Balanced_Limit));
               A (Row, I + 1 + J) := Element;
            end loop;
         end if;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
         pragma
           Assert (for all R in Ilo .. Ihi => (for all C in I + 2 + J .. Ihi => abs A (R, C) <= Balanced_Limit));
      end loop;
   end Update_Columns;

   -----------------
   -- Apply_Right --
   -----------------

   procedure Apply_Right
     (Ilo    : Positive;
      Ihi    : Index;
      I      : Index;
      Last_V : Index;
      Tau    : Real;
      V      : Scratch_Vector;
      A      : in out Real_Matrix;
      Work   : in out Scratch_Vector)
   is
      Last_Row : Natural;
      Element  : Real;
   begin
      --  ILADLR: the last row which is not zero in columns I + 1 .. I + Last_V.
      if A (Ihi, I + 1) /= 0.0 or else A (Ihi, I + Last_V) /= 0.0 then
         Last_Row := Ihi;
      else
         Last_Row := 0;
         for Column in I + 1 .. I + Last_V loop
            pragma Loop_Invariant (Last_Row <= Ihi);
            for Row in reverse Integer'Max (Ilo, Last_Row + 1) .. Ihi loop
               pragma Loop_Invariant (Last_Row <= Ihi);
               if A (Row, Column) /= 0.0 then
                  Last_Row := Row;
                  exit;
               end if;
            end loop;
         end loop;
      end if;
      if Last_Row < Ilo then
         return;
      end if;

      if Last_V = 1 then
         --  v is the first unit vector: scale the first column.
         for Row in Ilo .. Last_Row loop
            pragma
              Loop_Invariant
                (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Balanced_Limit));
            Element := A (Row, I + 1);
            Lemma_Scale (Tau, Element, Balanced_Limit);
            Element := (1.0 - Tau) * Element;
            pragma
              Assert_And_Cut
                (abs Element <= Balanced_Limit
                   and then Last_Row <= Ihi
                   and then
                     (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Balanced_Limit)));
            A (Row, I + 1) := Element;
         end loop;
      else
         Multiply_Columns (Ilo, Last_Row, I, Last_V, V, A, Work);
         Update_Columns (Ilo, Ihi, Last_Row, I, Last_V, Tau, V, Work, A);
      end if;
   end Apply_Right;

   -------------------
   -- Multiply_Rows --
   -------------------

   procedure Multiply_Rows
     (I           : Index;
      Last_V      : Index;
      Last_Column : Natural;
      V           : Scratch_Vector;
      A           : Real_Matrix;
      Work        : in out Scratch_Vector)
   is
      Temp, Sum, Element, Factor : Real;
   begin
      for Column in I + 1 .. Last_Column loop
         pragma Loop_Invariant (for all C in I + 1 .. Column - 1 => abs Work (C) <= Left_Sum_Limit);

         --  w := A (I + 2 .. I + Last_V, Column)' * v (2 .. Last_V), then w := w + A (I + 1, Column).
         Temp := 0.0;
         for J in 1 .. Last_V - 1 loop
            pragma Loop_Invariant (abs Temp <= Left_Partial_Limit (J - 1));
            Element := A (I + 1 + J, Column);
            Factor := V (J);
            Lemma_Left_Term (J - 1, Temp, Element, Factor);
            Temp := Temp + Element * Factor;
            pragma Assert (abs Temp <= Left_Partial_Limit (J));
         end loop;
         Lemma_Left_Total (Last_V - 1);
         pragma Assert (abs Temp <= Left_Sum_Limit);

         --  The sum is added to a zero, which makes a negative zero positive.
         Sum := 0.0 + Temp;
         pragma
           Assert_And_Cut
             (abs Sum <= Left_Sum_Limit
                and then (for all C in I + 1 .. Column - 1 => abs Work (C) <= Left_Sum_Limit));
         Work (Column) := Sum;
      end loop;
      for Column in I + 1 .. Last_Column loop
         pragma Loop_Invariant (for all C in I + 1 .. Column - 1 => abs Work (C) <= Left_Work_Limit);
         pragma Loop_Invariant (for all C in Column .. Last_Column => abs Work (C) <= Left_Sum_Limit);
         Sum := Work (Column);
         Element := A (I + 1, Column);
         Lemma_Sum (Sum, Element, Left_Sum_Limit);
         Sum := Sum + Element;
         pragma
           Assert_And_Cut
             (abs Sum <= Left_Work_Limit
                and then (for all C in I + 1 .. Column - 1 => abs Work (C) <= Left_Work_Limit)
                and then (for all C in Column .. Last_Column => abs Work (C) <= Left_Sum_Limit));
         Work (Column) := Sum;
      end loop;
   end Multiply_Rows;

   -----------------
   -- Update_Rows --
   -----------------

   procedure Update_Rows
     (Ihi         : Index;
      I           : Index;
      Last_V      : Index;
      Last_Column : Natural;
      Tau         : Real;
      V, Work     : Scratch_Vector;
      A           : in out Real_Matrix)
   is
      Temp, Sum, Element, Factor : Real;
   begin
      --  A := A - Tau * v * w'.
      for Column in I + 1 .. Last_Column loop
         pragma
           Loop_Invariant (for all R in I + 2 .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit));
         pragma Loop_Invariant (for all C in Column .. Ihi => abs A (I + 1, C) <= Right_Limit);
         Element := A (I + 1, Column);
         Sum := Work (Column);
         Lemma_Update (Element, -Tau, Sum, 2.0, Left_Work_Limit, Left_Scaled_Limit);
         Element := Element + (-Tau) * Sum;
         pragma
           Assert_And_Cut
             ((for all R in I + 2 .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Right_Limit))
                and then (for all C in Column .. Ihi => abs A (I + 1, C) <= Right_Limit));
         A (I + 1, Column) := Element;
      end loop;
      for Column in I + 1 .. Last_Column loop
         pragma
           Loop_Invariant (for all R in I + 2 .. Ihi => (for all C in Column .. Ihi => abs A (R, C) <= Right_Limit));
         if Work (Column) /= 0.0 then
            Sum := Work (Column);
            Bounds.Lemma_Product (-Tau, Sum, 2.0, Left_Work_Limit);
            Temp := (-Tau) * Sum;
            pragma Assert (abs Temp <= Left_Scaled_Limit);
            for J in 1 .. Last_V - 1 loop
               pragma
                 Loop_Invariant
                   (for all R in I + 2 .. Ihi => (for all C in Column + 1 .. Ihi => abs A (R, C) <= Right_Limit));
               pragma Loop_Invariant (for all R in I + 1 + J .. Ihi => abs A (R, Column) <= Right_Limit);
               Element := A (I + 1 + J, Column);
               Factor := V (J);
               Lemma_Update (Element, Factor, Temp, 2.0, Left_Scaled_Limit, Left_Limit);
               Element := Element + Factor * Temp;
               pragma
                 Assert_And_Cut
                   ((for all R in I + 2 .. Ihi => (for all C in Column + 1 .. Ihi => abs A (R, C) <= Right_Limit))
                      and then (for all R in I + 1 + J .. Ihi => abs A (R, Column) <= Right_Limit));
               A (I + 1 + J, Column) := Element;
            end loop;
         end if;
         pragma
           Assert (for all R in I + 2 .. Ihi => (for all C in Column + 1 .. Ihi => abs A (R, C) <= Right_Limit));
      end loop;
   end Update_Rows;

   ----------------
   -- Apply_Left --
   ----------------

   procedure Apply_Left
     (N      : Order;
      Ihi    : Index;
      I      : Index;
      Last_V : Index;
      Tau    : Real;
      V      : Scratch_Vector;
      A      : in out Real_Matrix;
      Work   : in out Scratch_Vector)
   is
      Last_Column : Natural;
      Element     : Real;
   begin
      --  ILADLC: the last column which is not zero in rows I + 1 .. I + Last_V. The reference implementation
      --  looks at every column up to N, and transforms those up to the one it finds.
      if A (I + 1, N) /= 0.0 or else A (I + Last_V, N) /= 0.0 then
         Last_Column := N;
      else
         Last_Column := 0;
         Search :
         for Column in reverse I + 1 .. N loop
            pragma Loop_Invariant (Last_Column = 0);
            for Row in I + 1 .. I + Last_V loop
               pragma Loop_Invariant (Last_Column = 0);
               if A (Row, Column) /= 0.0 then
                  Last_Column := Column;
                  exit Search;
               end if;
            end loop;
         end loop Search;
      end if;
      Last_Column := Integer'Min (Last_Column, Ihi);
      if Last_Column <= I then
         return;
      end if;

      if Last_V = 1 then
         --  v is the first unit vector: scale the first row.
         for Column in I + 1 .. Last_Column loop
            pragma Loop_Invariant (for all C in Column .. Ihi => abs A (I + 1, C) <= Right_Limit);
            Element := A (I + 1, Column);
            Lemma_Scale (Tau, Element, Right_Limit);
            Element := (1.0 - Tau) * Element;
            pragma
              Assert_And_Cut
                (Last_Column <= Ihi and then (for all C in Column .. Ihi => abs A (I + 1, C) <= Right_Limit));
            A (I + 1, Column) := Element;
         end loop;
      else
         Multiply_Rows (I, Last_V, Last_Column, V, A, Work);
         Update_Rows (Ihi, I, Last_V, Last_Column, Tau, V, Work, A);
      end if;
   end Apply_Left;

   ------------
   -- Reduce --
   ------------

   procedure Reduce (N : Order; Ilo : Positive; Ihi : Natural; A : in out Real_Matrix; Ok : out Boolean) is

      V : Scratch_Vector := [others => 0.0];
      --  The reflector vector without its leading one: V (J) multiplies row or column I + 1 + J.

      Work : Scratch_Vector := [others => 0.0];

      Alpha, Tau : Real;
      Count      : Positive;
      Last_V     : Positive;
      Bounded    : Boolean;

   begin
      Ok := True;
      for I in Ilo .. Ihi - 1 loop
         pragma Loop_Invariant (Block_Within (A, Ilo, Ihi, Balanced_Limit));

         --  Compute the elementary reflector H (I) which annihilates A (I + 2 .. Ihi, I).
         Count := Ihi - I;
         Alpha := A (I + 1, I);
         for J in 1 .. Count - 1 loop
            pragma Loop_Invariant (for all K in 1 .. J - 1 => abs V (K) <= Balanced_Limit);
            V (J) := A (I + 1 + J, I);
         end loop;
         pragma Assert (abs Alpha <= Balanced_Limit);
         pragma Assert (for all K in 1 .. Count - 1 => abs V (K) <= Balanced_Limit);
         Reflectors.Generate (Count, Alpha, V, Tau, Bounded);
         if not Bounded then
            Ok := False;
            return;
         end if;
         pragma Assert (if Tau = 0.0 then abs Alpha <= Balanced_Limit);
         A (I + 1, I) := Alpha;
         for J in 1 .. Count - 1 loop
            pragma
              Loop_Invariant
                (for all R in Ilo .. Ihi =>
                   (for all C in Ilo .. Ihi => (if C /= I then abs A (R, C) <= Balanced_Limit)));
            pragma Loop_Invariant (for all R in Ilo .. I => abs A (R, I) <= Balanced_Limit);
            pragma Loop_Invariant (if Tau = 0.0 then abs A (I + 1, I) <= Balanced_Limit);
            pragma Loop_Invariant (for all R in I + 2 .. I + J => abs A (R, I) <= 2.0);
            A (I + 1 + J, I) := V (J);
         end loop;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in I + 1 .. Ihi => abs A (R, C) <= Balanced_Limit));
         pragma Assert (for all R in I + 2 .. Ihi => abs A (R, I) <= 2.0);
         pragma Assert (if Tau = 0.0 then Block_Within (A, Ilo, Ihi, Balanced_Limit));

         if Tau /= 0.0 then
            --  Both applications skip the trailing zeros of v.
            Last_V := Count;
            while Last_V > 1 and then V (Last_V - 1) = 0.0 loop
               pragma Loop_Invariant (Last_V in 2 .. Count);
               pragma Loop_Variant (Decreases => Last_V);
               Last_V := Last_V - 1;
            end loop;

            --  Apply H (I) to A (Ilo .. Ihi, I + 1 .. Ihi) from the right, then to A (I + 1 .. Ihi, I + 1 .. Ihi)
            --  from the left.
            Apply_Right (Ilo, Ihi, I, Last_V, Tau, V, A, Work);
            Apply_Left (N, Ihi, I, Last_V, Tau, V, A, Work);

            --  A reflector is orthogonal, so it enlarges no element by more than the square root of the order,
            --  but the proof does not follow that: it only shows that the two applications cannot overflow.
            if Norms.Block_Max (A, Ilo, Ihi) > Balanced_Limit then
               Ok := False;
               return;
            end if;
         end if;
         pragma Assert (Block_Within (A, Ilo, Ihi, Balanced_Limit));
      end loop;
   end Reduce;

end Prunt.LAPACK.Hessenberg_Reduction;
