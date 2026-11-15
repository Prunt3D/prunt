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

pragma Style_Checks ("-s");
--  Lemmas are declared by their bodies, where they are proved: a separate declaration would only repeat them.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Trajectory.Profile_Model.Motion;

package body Prunt.Motion_Planner.Trajectory.Certificates.Validity
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proof goes
   --
   --  Write C for the crackle, Vs for the starting speed, GA for the change of speed of the exact acceleration ramp,
   --  Pk = Vs + GA for the exact coasting speed, G for the loss of speed of the exact deceleration ramp, and TA, Tc
   --  and TB for the durations of the two ramps and of the coast. All are exact reals and none is negative.
   --
   --  The check computes a peak P for Pk, and from it a final speed and a travel. The postconditions of the end
   --  formulas in Trajectory.Profiles put P within Ramp_End_Slack of Pk, and the final speed and the distance
   --  of the deceleration ramp within Ramp_End_Slack of those of the exact ramp which starts at P. That ramp is not
   --  the exact deceleration ramp, which starts at Pk: the difference P - Pk goes into the final speed as it is and
   --  into the distance times TB.
   --
   --  Speed. The computed final speed is within three slacks of Pk and one of G of the exact one, Pk - G, and the
   --  comparison of the check puts it within the checked tolerance of the end speed, relative to the largest of
   --  the speeds. The end speed is not negative, so G is at most Pk plus the error of the exact final speed. That
   --  bounds the error in terms of itself, and so by a little more than the checked tolerance of the top speed,
   --  and it bounds G by little more than Pk.
   --
   --  Distance. Each of the three terms of the computed travel is within a few slacks of its exact term, relative
   --  to the magnitude of that term, and so is their rounded sum. The magnitude of the deceleration term is
   --  (Pk + G / 2) * TB where the term is (Pk - G / 2) * TB, and as G is little more than Pk the former is at most
   --  four times the latter. The sum of the magnitudes is then at most four times the exact distance, and the
   --  comparison of the check puts the exact distance within a little more than the checked tolerance of the
   --  distance asked for.
   --
   --  Every step over reals is a lemma with scalar parameters, each either linear or one product. The absolute
   --  parts of the bounds, which come from products that underflow, are carried along as named numbers.
   ------------------------------------------------------------------------------------------------------------------

   One_Less_Epsilon : constant := 1.0 - 2.0**(-53);
   One_More_Epsilon : constant := 1.0 + 2.0**(-53);
   Three_Epsilon    : constant := 3.0 * 2.0**(-53);
   Three_Eta        : constant := 3.0 * 2.0**(-1075);

   One_More_Slack : constant := 1.0 + Ramp_End_Slack;
   Two_Slacks     : constant := 2.0 * Ramp_End_Slack;
   Three_Slacks   : constant := 3.0 * Ramp_End_Slack;
   Four_Slacks    : constant := 4.0 * Ramp_End_Slack;

   Three_Velocity_Residues : constant := 3.0 * Ramp_End_Velocity_Residue;

   Real_Time_Max    : constant := 2.0**100;
   Real_Crackle_Max : constant := 2.0**80;
   Real_Speed_Max   : constant := 2.0**80;
   --  The reals which the bounds of Is_Certifiable denote.

   Gain_1_Max : constant := Real_Crackle_Max * Real_Time_Max;
   Gain_2_Max : constant := Gain_1_Max * (2.0 * Real_Time_Max);
   Gain_3_Max : constant := Gain_2_Max * (4.0 * Real_Time_Max);
   Gain_Max   : constant := Gain_3_Max * (8.0 * Real_Time_Max);
   --  No ramp within the bounds of Is_Certifiable changes speed by more than Gain_Max, which is 2**486.

   Peak_Max : constant := Real_Speed_Max + Gain_Max;
   --  No exact coasting speed is larger.

   Real_Peak_Bound : constant := 2.0**500;
   --  The real which Rounding_Velocity_Max denotes.

   Duration_Max : constant := 2.0**104;
   --  No ramp lasts longer.

   Tolerance_Above : constant := 1.0E-9 * (1.0 + 2.0**(-53));
   --  The floating-point tolerance of the check, the literal 1.0E-9, denotes no more than this.

   Checked_Slack : constant := 1.0E-9 * (1.0 + 2.0**(-50));
   --  How far apart, relative to its scale, a comparison of the check lets the reals which it compares be: the
   --  tolerance and the rounding of the difference and of the product.

   Internal_Tolerance : constant := 1.001E-9;
   One_More_Tolerance : constant := 1.0 + Internal_Tolerance;
   --  How far, relative to the top speed, the exact final speed is from the end speed.

   Speed_Residue : constant := 2.0**(-698);
   --  How far it is beyond that. This is much less than Certified_Speed_Residue, because the distance needs it
   --  times the duration of the deceleration ramp.

   Coast_Residue     : constant := 2.0**(-599);
   Decel_Residue     : constant := 2.0**(-595);
   Terms_Residue     : constant := 2.0**(-594);
   Travel_Residue    : constant := 2.0**(-593);
   Magnitude_Residue : constant := 2.0**(-592);
   --  The absolute parts of the bounds of the distance, in the order in which the proof reaches them.

   Peak_Bound          : constant Dimensioned_Float := 2#1.0#E500;
   Time_Bound          : constant Dimensioned_Float := 2#1.0#E100;
   Speed_Bound         : constant Dimensioned_Float := 2#1.0#E80;
   Ramp_Distance_Bound : constant Dimensioned_Float := 2#1.0#E970;
   Product_Bound       : constant Dimensioned_Float := 2#1.0#E600;
   Partial_Bound       : constant Dimensioned_Float := 2#1.0#E971;
   Travel_Bound        : constant Dimensioned_Float := 2#1.0#E972;
   Check_Value_Max     : constant Dimensioned_Float := 2#1.0#E1000;
   --  Magnitudes of the floating-point values of the check, without dimensions.

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Product_Monotone (A, B, C, D : Real)
   with Pre => A >= 0.0 and then A <= B and then C >= 0.0 and then C <= D, Post => A * C <= B * D
   is
   begin
      pragma Assert (A * C <= B * C);
      pragma Assert (B * C <= B * D);
   end Lemma_Product_Monotone;

   procedure Lemma_Abs_Product (X, Y : Real) with Pre => Y >= 0.0, Post => abs (X * Y) = abs X * Y is
   begin
      null;
   end Lemma_Abs_Product;

   procedure Lemma_Negated_Product (X, Y : Real) with Post => (0.0 - X) * Y = 0.0 - X * Y is
   begin
      null;
   end Lemma_Negated_Product;

   procedure Lemma_Scaled_Difference (X, Y, Z, Bound : Real)
   with Pre => Z >= 0.0 and then abs (X - Y) <= Bound, Post => abs (X * Z - Y * Z) <= Bound * Z
   is
   begin
      Lemma_Abs_Product (X - Y, Z);
      Lemma_Product_Monotone (abs (X - Y), Bound, Z, Z);
      pragma Assert ((X - Y) * Z = X * Z - Y * Z);
   end Lemma_Scaled_Difference;
   --  A difference times a time.

   ------------------------------------------------------------------------------------------------------------------
   --  The exact ramps
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Gain_Range (C, T1, T2, T3, T4, Gain : Real)
   with
     Pre  =>
       C >= 0.0
       and then C <= Real_Crackle_Max
       and then T1 >= 0.0
       and then T1 <= Real_Time_Max
       and then T2 >= 0.0
       and then T2 <= Real_Time_Max
       and then T3 >= 0.0
       and then T3 <= Real_Time_Max
       and then T4 >= 0.0
       and then T4 <= Real_Time_Max
       and then Gain = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4),
     Post => Gain >= 0.0 and then Gain <= Gain_Max
   is
      P1 : constant Real := C * T1;
      P2 : constant Real := P1 * (T1 + T2);
      P3 : constant Real := P2 * (2.0 * T1 + T2 + T3);
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, T1);
      Lemma_Product_Monotone (C, Real_Crackle_Max, T1, Real_Time_Max);
      pragma Assert (P1 >= 0.0 and then P1 <= Gain_1_Max);
      Lemma_Product_Monotone (0.0, P1, 0.0, T1 + T2);
      Lemma_Product_Monotone (P1, Gain_1_Max, T1 + T2, 2.0 * Real_Time_Max);
      pragma Assert (P2 >= 0.0 and then P2 <= Gain_2_Max);
      Lemma_Product_Monotone (0.0, P2, 0.0, 2.0 * T1 + T2 + T3);
      Lemma_Product_Monotone (P2, Gain_2_Max, 2.0 * T1 + T2 + T3, 4.0 * Real_Time_Max);
      pragma Assert (P3 >= 0.0 and then P3 <= Gain_3_Max);
      Lemma_Product_Monotone (0.0, P3, 0.0, 4.0 * T1 + 2.0 * T2 + T3 + T4);
      Lemma_Product_Monotone (P3, Gain_3_Max, 4.0 * T1 + 2.0 * T2 + T3 + T4, 8.0 * Real_Time_Max);
      pragma Assert (Gain = P3 * (4.0 * T1 + 2.0 * T2 + T3 + T4));
   end Lemma_Gain_Range;
   --  The change of speed of a ramp of nonnegative crackle within the bounds of Is_Certifiable.

   procedure Lemma_Duration_Range (T1, T2, T3, T4, Total : Real)
   with
     Pre  =>
       T1 >= 0.0
       and then T1 <= Real_Time_Max
       and then T2 >= 0.0
       and then T2 <= Real_Time_Max
       and then T3 >= 0.0
       and then T3 <= Real_Time_Max
       and then T4 >= 0.0
       and then T4 <= Real_Time_Max
       and then Total = 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4,
     Post => Total >= 0.0 and then Total <= Duration_Max
   is
   begin
      null;
   end Lemma_Duration_Range;

   procedure Lemma_Negated_Gain (C, T1, T2, T3, T4 : Real)
   with
     Post =>
       (0.0 - C) * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       = 0.0 - C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
   is
      P1 : constant Real := C * T1;
      P2 : constant Real := P1 * (T1 + T2);
      P3 : constant Real := P2 * (2.0 * T1 + T2 + T3);
   begin
      Lemma_Negated_Product (C, T1);
      Lemma_Negated_Product (P1, T1 + T2);
      pragma Assert ((0.0 - C) * T1 * (T1 + T2) = 0.0 - P2);
      Lemma_Negated_Product (P2, 2.0 * T1 + T2 + T3);
      pragma Assert ((0.0 - C) * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) = 0.0 - P3);
      Lemma_Negated_Product (P3, 4.0 * T1 + 2.0 * T2 + T3 + T4);
   end Lemma_Negated_Gain;
   --  A ramp of the opposite crackle changes speed by the opposite amount.

   procedure Lemma_Ramp_Values (Exact : Ramp; C, T1, T2, T3, T4, Vs, Gain, Total : Real)
   with
     Pre  =>
       Exact.T1 = T1
       and then Exact.T2 = T2
       and then Exact.T3 = T3
       and then Exact.T4 = T4
       and then Exact.C = C
       and then Exact.Vs = Vs
       and then Gain = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       and then Total = 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4,
     Post =>
       Velocity_Gain (Exact) = Gain
       and then Total_Time (Exact) = Total
       and then Travel (Exact) = (Vs + Gain / 2.0) * Total
       and then Velocity_Magnitude (Exact) = abs Vs + abs Gain
       and then Travel_Magnitude (Exact) = (abs Vs + abs Gain / 2.0) * Total
   is
   begin
      pragma Assert (Velocity_Gain (Exact) = Gain);
      pragma Assert (Total_Time (Exact) = Total);
   end Lemma_Ramp_Values;
   --  The end of a ramp, with the components of the ramp as scalars.

   procedure Lemma_Half_Negated (Pk, G, TB : Real) with Post => (Pk + (0.0 - G) / 2.0) * TB = Pk * TB - G * TB / 2.0 is
   begin
      null;
   end Lemma_Half_Negated;

   procedure Lemma_Exact_Values
     (Profile                                                  : Feedrate_Profile;
      Max_Crackle                                              : Crackle;
      Start_Vel                                                : Velocity;
      C, Vs, Tc, A1, A2, A3, A4, GA, TA, B1, B2, B3, B4, G, TB : Real)
   with
     Pre  =>
       C = R (Max_Crackle)
       and then Vs = R (Start_Vel)
       and then Tc = R (Profile.Coast)
       and then A1 = R (Profile.Accel (1))
       and then A2 = R (Profile.Accel (2))
       and then A3 = R (Profile.Accel (3))
       and then A4 = R (Profile.Accel (4))
       and then GA = C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4)
       and then TA = 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4
       and then B1 = R (Profile.Decel (1))
       and then B2 = R (Profile.Decel (2))
       and then B3 = R (Profile.Decel (3))
       and then B4 = R (Profile.Decel (4))
       and then G = C * B1 * (B1 + B2) * (2.0 * B1 + B2 + B3) * (4.0 * B1 + 2.0 * B2 + B3 + B4)
       and then TB = 8.0 * B1 + 4.0 * B2 + 2.0 * B3 + B4,
     Post =>
       Exact_Peak (Profile, Max_Crackle, Start_Vel) = Vs + GA
       and then Exact_Finish (Profile, Max_Crackle, Start_Vel) = Vs + GA - G
       and then
         Exact_Distance (Profile, Max_Crackle, Start_Vel)
         = (Vs + GA / 2.0) * TA + (Vs + GA) * Tc + ((Vs + GA) * TB - G * TB / 2.0)
   is
      Accel_Exact : constant Ramp := Accel_Ramp (Profile, Max_Crackle, Start_Vel);
      Decel_Exact : constant Ramp := Decel_Ramp (Profile, Max_Crackle, Start_Vel);
      Pk          : constant Real := Vs + GA;
   begin
      Lemma_Ramp_Values (Accel_Exact, C, A1, A2, A3, A4, Vs, GA, TA);
      pragma Assert (Exact_Peak (Profile, Max_Crackle, Start_Vel) = Pk);
      pragma Assert (Travel (Accel_Exact) = (Vs + GA / 2.0) * TA);
      Lemma_Negated_Gain (C, B1, B2, B3, B4);
      Lemma_Ramp_Values (Decel_Exact, 0.0 - C, B1, B2, B3, B4, Pk, 0.0 - G, TB);
      pragma Assert (Exact_Finish (Profile, Max_Crackle, Start_Vel) = Pk - G);
      Lemma_Half_Negated (Pk, G, TB);
      pragma Assert (Travel (Decel_Exact) = Pk * TB - G * TB / 2.0);
      pragma Assert (Exact_Peak (Profile, Max_Crackle, Start_Vel) * R (Profile.Coast) = Pk * Tc);
      pragma
        Assert
          (Exact_Distance (Profile, Max_Crackle, Start_Vel)
           = Travel (Accel_Exact)
             + Exact_Peak (Profile, Max_Crackle, Start_Vel) * R (Profile.Coast)
             + Travel (Decel_Exact));
      pragma
        Assert
          (Exact_Distance (Profile, Max_Crackle, Start_Vel)
           = (Vs + GA / 2.0) * TA + Pk * Tc + (Pk * TB - G * TB / 2.0));
   end Lemma_Exact_Values;
   --  The exact profile in terms of the reals which its phase times, crackle and starting speed denote.

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point values and the reals which they denote
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Order (F1, F2 : Dimensioned_Float) with Post => (if F1 <= F2 then R (F1) <= R (F2)) is
   begin
      null;
   end Lemma_Order;

   procedure Lemma_Reverse_Order (F1, F2 : Dimensioned_Float) with Post => (if R (F1) <= R (F2) then F1 <= F2) is
   begin
      null;
   end Lemma_Reverse_Order;

   procedure Lemma_Nonnegative (F : Dimensioned_Float) with Pre => F >= 0.0, Post => R (F) >= 0.0 is
   begin
      null;
   end Lemma_Nonnegative;

   procedure Lemma_Magnitude (F : Dimensioned_Float) with Post => R (abs F) = abs R (F) is
   begin
      null;
   end Lemma_Magnitude;

   procedure Lemma_Negation (F : Dimensioned_Float) with Post => R (-F) = 0.0 - R (F) is
   begin
      null;
   end Lemma_Negation;

   procedure Lemma_Time_Range (F : Dimensioned_Float)
   with Pre => F >= 0.0 and then F <= Time_Bound, Post => R (F) >= 0.0 and then R (F) <= Real_Time_Max
   is
   begin
      Lemma_Nonnegative (F);
      Lemma_Order (F, Time_Bound);
      pragma Assert (R (Time_Bound) = Real_Time_Max);
   end Lemma_Time_Range;

   procedure Lemma_Speed_Range (F : Dimensioned_Float)
   with Pre => F >= 0.0 and then F <= Speed_Bound, Post => R (F) >= 0.0 and then R (F) <= Real_Speed_Max
   is
   begin
      Lemma_Nonnegative (F);
      Lemma_Order (F, Speed_Bound);
      pragma Assert (R (Speed_Bound) = Real_Speed_Max);
   end Lemma_Speed_Range;
   --  Also the range of the crackle, which has the same bound.

   procedure Lemma_Within_Peak_Bound (F : Dimensioned_Float)
   with Pre => R (F) <= Real_Peak_Bound, Post => F <= Peak_Bound
   is
   begin
      Lemma_Reverse_Order (F, Peak_Bound);
      pragma Assert (R (Peak_Bound) = Real_Peak_Bound);
   end Lemma_Within_Peak_Bound;

   procedure Lemma_Tolerance_Constant with Post => R (1.0E-9) >= 0.0 and then R (1.0E-9) <= Tolerance_Above is
   begin
      null;
   end Lemma_Tolerance_Constant;
   --  The tolerance of the check as a floating-point number is 1.0E-9 rounded to nearest.

   procedure Lemma_Largest (F1, F2, F3 : Dimensioned_Float; Bound : Real)
   with
     Pre  => R (F1) <= Bound and then R (F2) <= Bound and then R (F3) <= Bound,
     Post => R (Dimensioned_Float'Max (F1, Dimensioned_Float'Max (F2, F3))) <= Bound
   is
   begin
      null;
   end Lemma_Largest;

   procedure Lemma_Largest_Range (F1, F2, F3 : Dimensioned_Float)
   with
     Pre  =>
       F1 >= 0.0
       and then F1 <= Peak_Bound
       and then F2 >= 0.0
       and then F2 <= Peak_Bound
       and then F3 >= 0.0
       and then F3 <= Peak_Bound,
     Post =>
       Dimensioned_Float'Max (F1, Dimensioned_Float'Max (F2, F3)) >= 0.0
       and then Dimensioned_Float'Max (F1, Dimensioned_Float'Max (F2, F3)) <= Peak_Bound
   is
   begin
      null;
   end Lemma_Largest_Range;

   procedure Lemma_Product_Range (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 <= Peak_Bound and then F2 >= 0.0 and then F2 <= Time_Bound,
     Post => F1 * F2 >= 0.0 and then F1 * F2 <= Product_Bound
   is
   begin
      null;
   end Lemma_Product_Range;

   procedure Lemma_Partial_Range (F1, F2 : Dimensioned_Float)
   with
     Pre  => abs F1 <= Ramp_Distance_Bound and then F2 >= 0.0 and then F2 <= Product_Bound,
     Post => abs (F1 + F2) <= Partial_Bound
   is
   begin
      null;
   end Lemma_Partial_Range;

   procedure Lemma_Travel_Range (F1, F2 : Dimensioned_Float)
   with Pre => abs F1 <= Partial_Bound and then abs F2 <= Ramp_Distance_Bound, Post => abs (F1 + F2) <= Travel_Bound
   is
   begin
      null;
   end Lemma_Travel_Range;

   ------------------------------------------------------------------------------------------------------------------
   --  A comparison of the check
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Check (Gap, Diff, Rhs, Prod, Sc : Real)
   with
     Pre  =>
       abs (Diff - Gap) <= Epsilon * abs Gap + Eta
       and then abs Diff <= Rhs
       and then abs (Rhs - Prod) <= Epsilon * abs Prod + Eta
       and then Prod >= 0.0
       and then Prod <= Tolerance_Above * Sc
       and then Sc >= 0.0,
     Post => abs Gap <= Checked_Slack * Sc + Three_Eta
   is
   begin
      pragma Assert (abs Gap * One_Less_Epsilon <= abs Diff + Eta);
      pragma Assert (Rhs <= Prod * One_More_Epsilon + Eta);
      pragma Assert (abs Gap * One_Less_Epsilon <= Tolerance_Above * One_More_Epsilon * Sc + 2.0 * Eta);
   end Lemma_Real_Check;
   --  Gap is the exact difference of the two values compared and Diff its rounding; Prod is the exact product of
   --  the floating-point tolerance and the scale Sc, and Rhs its rounding.

   procedure Lemma_Check (Value, Target, Scale : Dimensioned_Float)
   with
     Pre  =>
       abs Value <= Check_Value_Max
       and then abs Target <= Check_Value_Max
       and then Scale >= 0.0
       and then Scale <= Peak_Bound
       and then abs (Value - Target) <= 1.0E-9 * Scale,
     Post => abs (R (Value) - R (Target)) <= Checked_Slack * R (Scale) + Three_Eta
   is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Sub (Value, Target);
      Lemma_Magnitude (Value - Target);
      Lemma_Order (abs (Value - Target), 1.0E-9 * Scale);
      Float_Lemmas.Lemma_Rounding_Error_Mul (1.0E-9, Scale);
      Lemma_Tolerance_Constant;
      Lemma_Nonnegative (Scale);
      Lemma_Product_Monotone (0.0, R (1.0E-9), 0.0, R (Scale));
      Lemma_Product_Monotone (R (1.0E-9), Tolerance_Above, R (Scale), R (Scale));
      Lemma_Real_Check
        (Gap  => R (Value) - R (Target),
         Diff => R (Value - Target),
         Rhs  => R (1.0E-9 * Scale),
         Prod => R (1.0E-9) * R (Scale),
         Sc   => R (Scale));
   end Lemma_Check;
   --  What a comparison of Profile_Is_Valid, which is made in floating point, says of the reals compared.

   ------------------------------------------------------------------------------------------------------------------
   --  Speed
   --
   --  P and F are the reals which the computed peak and the computed final speed denote.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Peak_Close (P, Pk : Real)
   with
     Pre  => Pk >= 0.0 and then abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue,
     Post => P <= Pk * One_More_Slack + Ramp_End_Velocity_Residue
   is
   begin
      null;
   end Lemma_Peak_Close;

   procedure Lemma_Peak_Bound (P, Pk : Real)
   with
     Pre  =>
       Pk >= 0.0 and then Pk <= Peak_Max and then abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue,
     Post => P <= Real_Peak_Bound
   is
   begin
      null;
   end Lemma_Peak_Bound;
   --  The computed peak is small enough for the rounding of the deceleration ramp's end formulas to be bounded.

   procedure Lemma_Finish_Close (F, P, Pk, G : Real)
   with
     Pre  =>
       Pk >= 0.0
       and then P >= 0.0
       and then G >= 0.0
       and then abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue
       and then abs (F - (P - G)) <= Ramp_End_Slack * (P + G) + Ramp_End_Velocity_Residue,
     Post => abs (F - (Pk - G)) <= Three_Slacks * Pk + Ramp_End_Slack * G + Three_Velocity_Residues
   is
   begin
      pragma Assert (P <= Pk * One_More_Slack + Ramp_End_Velocity_Residue);
      pragma Assert (abs (F - (Pk - G)) <= abs (F - (P - G)) + abs (P - Pk));
   end Lemma_Finish_Close;
   --  The computed final speed is close to the exact one, although it was computed from the computed peak.

   procedure Lemma_Speed_Error (Pk, G, Ve, Top, F, Sc : Real)
   with
     Pre  =>
       Pk >= 0.0
       and then G >= 0.0
       and then Ve >= 0.0
       and then Top >= Pk
       and then Top >= Ve
       and then abs (F - (Pk - G)) <= Three_Slacks * Pk + Ramp_End_Slack * G + Three_Velocity_Residues
       and then abs (F - Ve) <= Checked_Slack * Sc + Three_Eta
       and then Sc <= Top * One_More_Slack + Ramp_End_Velocity_Residue,
     Post => abs (Pk - G - Ve) <= Internal_Tolerance * Top + Speed_Residue
   is
      E : constant Real := abs (Pk - G - Ve);
   begin
      pragma Assert (E <= abs (F - (Pk - G)) + abs (F - Ve));
      pragma Assert (G <= Top + E);
      pragma
        Assert
          (E
           <= Three_Slacks * Top + Ramp_End_Slack * (Top + E) + Three_Velocity_Residues
              + Checked_Slack * (Top * One_More_Slack + Ramp_End_Velocity_Residue)
              + Three_Eta);
   end Lemma_Speed_Error;
   --  The end speed is not negative, so the loss of speed G is at most the peak and the error E of the final
   --  speed; that bounds E in terms of itself, and so in terms of the top speed alone.

   procedure Lemma_Decel_Gain_Bound (Pk, G, Ve, Top : Real)
   with
     Pre  =>
       Pk >= 0.0
       and then Ve >= 0.0
       and then (if Pk >= Ve then Top = Pk else Top = Ve)
       and then abs (Pk - G - Ve) <= Internal_Tolerance * Top + Speed_Residue,
     Post => G <= One_More_Tolerance * Pk + Speed_Residue
   is
   begin
      null;
   end Lemma_Decel_Gain_Bound;
   --  The deceleration ramp loses little more than the whole of the coasting speed.

   procedure Lemma_Speed (Start_Vel, End_Vel, Peak_F, Finish_F : Dimensioned_Float; Pk, G, Top : Real)
   with
     Pre  =>
       Start_Vel >= 0.0
       and then Start_Vel <= Peak_Bound
       and then End_Vel >= 0.0
       and then End_Vel <= Peak_Bound
       and then Peak_F >= 0.0
       and then Peak_F <= Peak_Bound
       and then abs Finish_F <= Check_Value_Max
       and then Pk >= R (Start_Vel)
       and then G >= 0.0
       and then (if Pk >= R (End_Vel) then Top = Pk else Top = R (End_Vel))
       and then abs (R (Peak_F) - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue
       and then abs (R (Finish_F) - (R (Peak_F) - G)) <= Ramp_End_Slack * (R (Peak_F) + G) + Ramp_End_Velocity_Residue
       and then
         abs (Finish_F - End_Vel)
         <= 1.0E-9 * Dimensioned_Float'Max (Start_Vel, Dimensioned_Float'Max (Peak_F, End_Vel)),
     Post =>
       abs (Pk - G - R (End_Vel)) <= Internal_Tolerance * Top + Speed_Residue
       and then G <= One_More_Tolerance * Pk + Speed_Residue
   is
      Scale_F : constant Dimensioned_Float :=
        Dimensioned_Float'Max (Start_Vel, Dimensioned_Float'Max (Peak_F, End_Vel));
      Vs      : constant Real := R (Start_Vel);
      Ve      : constant Real := R (End_Vel);
      P       : constant Real := R (Peak_F);
      F       : constant Real := R (Finish_F);
      Sc      : constant Real := R (Scale_F);
   begin
      Lemma_Nonnegative (Start_Vel);
      Lemma_Nonnegative (End_Vel);
      Lemma_Nonnegative (Peak_F);
      Lemma_Peak_Close (P, Pk);
      Lemma_Finish_Close (F, P, Pk, G);
      pragma Assert (Vs <= Top * One_More_Slack + Ramp_End_Velocity_Residue);
      pragma Assert (P <= Top * One_More_Slack + Ramp_End_Velocity_Residue);
      pragma Assert (Ve <= Top * One_More_Slack + Ramp_End_Velocity_Residue);
      Lemma_Largest (Start_Vel, Peak_F, End_Vel, Top * One_More_Slack + Ramp_End_Velocity_Residue);
      Lemma_Largest_Range (Start_Vel, Peak_F, End_Vel);
      Lemma_Check (Finish_F, End_Vel, Scale_F);
      Lemma_Speed_Error (Pk, G, Ve, Top, F, Sc);
      Lemma_Decel_Gain_Bound (Pk, G, Ve, Top);
   end Lemma_Speed;
   --  What the comparison of the final speed says of the exact profile.

   ------------------------------------------------------------------------------------------------------------------
   --  Distance
   --
   --  AD, M and DD are the reals which the three terms of the computed travel denote: the distance of the
   --  acceleration ramp, the product of the peak and the coast time, and the distance of the deceleration ramp. S1
   --  is the rounded sum of the first two and TF the rounded sum of the three. TrA and KT are the exact first two
   --  terms, and V - W / 2 the third, where V = Pk * TB and W = G * TB.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Coast_Close (M, P, Pk, Tc : Real)
   with
     Pre  =>
       P >= 0.0
       and then Pk >= 0.0
       and then Tc >= 0.0
       and then Tc <= Real_Time_Max
       and then abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue
       and then abs (M - P * Tc) <= Epsilon * abs (P * Tc) + Eta,
     Post => Pk * Tc >= 0.0 and then abs (M - Pk * Tc) <= Two_Slacks * (Pk * Tc) + Coast_Residue
   is
      PT : constant Real := P * Tc;
      KT : constant Real := Pk * Tc;
   begin
      Lemma_Product_Monotone (0.0, P, 0.0, Tc);
      Lemma_Product_Monotone (0.0, Pk, 0.0, Tc);
      Lemma_Scaled_Difference (P, Pk, Tc, Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue);
      pragma
        Assert
          ((Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue) * Tc
           = Ramp_End_Slack * KT + Ramp_End_Velocity_Residue * Tc);
      pragma Assert (abs (PT - KT) <= Ramp_End_Slack * KT + Ramp_End_Velocity_Residue * Real_Time_Max);
      pragma Assert (abs (M - PT) <= Epsilon * PT + Eta);
   end Lemma_Coast_Close;
   --  The computed coasting distance is close to the exact one.

   procedure Lemma_Decel_Close_Linear (DD, U, V, W, Gap : Real)
   with
     Pre  =>
       U >= 0.0
       and then V >= 0.0
       and then W >= 0.0
       and then Gap >= 0.0
       and then Gap <= Ramp_End_Velocity_Residue * Duration_Max
       and then abs (U - V) <= Ramp_End_Slack * V + Gap
       and then abs (DD - (U - W / 2.0)) <= Ramp_End_Slack * (U + W / 2.0) + Ramp_End_Distance_Residue,
     Post => abs (DD - (V - W / 2.0)) <= Three_Slacks * (V + W / 2.0) + Decel_Residue
   is
   begin
      pragma Assert (abs (DD - (V - W / 2.0)) <= abs (DD - (U - W / 2.0)) + abs (U - V));
      pragma Assert (U <= V * One_More_Slack + Gap);
   end Lemma_Decel_Close_Linear;

   procedure Lemma_Decel_Close (DD, P, Pk, G, TB : Real)
   with
     Pre  =>
       P >= 0.0
       and then Pk >= 0.0
       and then G >= 0.0
       and then TB >= 0.0
       and then TB <= Duration_Max
       and then abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue
       and then abs (DD - (P - G / 2.0) * TB) <= Ramp_End_Slack * ((P + G / 2.0) * TB) + Ramp_End_Distance_Residue,
     Post =>
       Pk * TB >= 0.0
       and then G * TB >= 0.0
       and then abs (DD - (Pk * TB - G * TB / 2.0)) <= Three_Slacks * (Pk * TB + G * TB / 2.0) + Decel_Residue
   is
      U : constant Real := P * TB;
      V : constant Real := Pk * TB;
      W : constant Real := G * TB;
   begin
      Lemma_Product_Monotone (0.0, P, 0.0, TB);
      Lemma_Product_Monotone (0.0, Pk, 0.0, TB);
      Lemma_Product_Monotone (0.0, G, 0.0, TB);
      Lemma_Scaled_Difference (P, Pk, TB, Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue);
      pragma
        Assert
          ((Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue) * TB
           = Ramp_End_Slack * V + Ramp_End_Velocity_Residue * TB);
      pragma Assert (abs (U - V) <= Ramp_End_Slack * V + Ramp_End_Velocity_Residue * TB);
      pragma Assert ((P - G / 2.0) * TB = U - W / 2.0);
      pragma Assert ((P + G / 2.0) * TB = U + W / 2.0);
      Lemma_Decel_Close_Linear (DD, U, V, W, Ramp_End_Velocity_Residue * TB);
   end Lemma_Decel_Close;
   --  The computed distance of the deceleration ramp is close to the exact one, although it was computed from the
   --  computed peak.

   procedure Lemma_Terms_Close (AD, M, DD, TrA, KT, V, W : Real)
   with
     Pre  =>
       TrA >= 0.0
       and then KT >= 0.0
       and then V >= 0.0
       and then W >= 0.0
       and then abs (AD - TrA) <= Ramp_End_Slack * TrA + Ramp_End_Distance_Residue
       and then abs (M - KT) <= Two_Slacks * KT + Coast_Residue
       and then abs (DD - (V - W / 2.0)) <= Three_Slacks * (V + W / 2.0) + Decel_Residue,
     Post =>
       abs (AD + M + DD - (TrA + KT + (V - W / 2.0))) <= Three_Slacks * (TrA + KT + (V + W / 2.0)) + Terms_Residue
       and then
         abs AD + abs M + abs DD
         <= (TrA + KT + (V + W / 2.0)) + Three_Slacks * (TrA + KT + (V + W / 2.0)) + Terms_Residue
   is
   begin
      pragma Assert (abs AD <= TrA + Ramp_End_Slack * TrA + Ramp_End_Distance_Residue);
      pragma Assert (abs M <= KT + Two_Slacks * KT + Coast_Residue);
      pragma Assert (abs DD <= (V + W / 2.0) + Three_Slacks * (V + W / 2.0) + Decel_Residue);
   end Lemma_Terms_Close;

   procedure Lemma_Sum_Rounding (AD, M, DD, S1, TF, Mag : Real)
   with
     Pre  =>
       abs AD + abs M + abs DD <= Mag
       and then abs (S1 - (AD + M)) <= Epsilon * abs (AD + M) + Eta
       and then abs (TF - (S1 + DD)) <= Epsilon * abs (S1 + DD) + Eta,
     Post => abs (TF - (AD + M + DD)) <= Three_Epsilon * Mag + Three_Eta
   is
   begin
      pragma Assert (abs (AD + M) <= abs AD + abs M);
      pragma Assert (abs S1 <= (abs AD + abs M) * One_More_Epsilon + Eta);
      pragma Assert (abs (S1 + DD) <= (abs AD + abs M) * One_More_Epsilon + Eta + abs DD);
      pragma Assert (abs (TF - (AD + M + DD)) <= abs (TF - (S1 + DD)) + abs (S1 - (AD + M)));
   end Lemma_Sum_Rounding;
   --  Two rounded sums of three terms.

   procedure Lemma_Travel_Close (AD, M, DD, S1, TF, TrA, KT, V, W : Real)
   with
     Pre  =>
       TrA >= 0.0
       and then KT >= 0.0
       and then V >= 0.0
       and then W >= 0.0
       and then abs (AD - TrA) <= Ramp_End_Slack * TrA + Ramp_End_Distance_Residue
       and then abs (M - KT) <= Two_Slacks * KT + Coast_Residue
       and then abs (DD - (V - W / 2.0)) <= Three_Slacks * (V + W / 2.0) + Decel_Residue
       and then abs (S1 - (AD + M)) <= Epsilon * abs (AD + M) + Eta
       and then abs (TF - (S1 + DD)) <= Epsilon * abs (S1 + DD) + Eta,
     Post => abs (TF - (TrA + KT + (V - W / 2.0))) <= Four_Slacks * (TrA + KT + (V + W / 2.0)) + Travel_Residue
   is
      Exact : constant Real := TrA + KT + (V - W / 2.0);
      Size  : constant Real := TrA + KT + (V + W / 2.0);
      Mag   : constant Real := Size + Three_Slacks * Size + Terms_Residue;
   begin
      Lemma_Terms_Close (AD, M, DD, TrA, KT, V, W);
      Lemma_Sum_Rounding (AD, M, DD, S1, TF, Mag);
      pragma Assert (abs (TF - Exact) <= abs (TF - (AD + M + DD)) + abs (AD + M + DD - Exact));
      pragma Assert (abs (TF - Exact) <= Three_Epsilon * Mag + Three_Eta + Three_Slacks * Size + Terms_Residue);
   end Lemma_Travel_Close;
   --  The computed travel is close to the exact distance, relative to the sizes of its terms.

   procedure Lemma_Magnitude_Bound (TrA, KT, Pk, G, TB : Real)
   with
     Pre  =>
       TrA >= 0.0
       and then KT >= 0.0
       and then Pk >= 0.0
       and then G >= 0.0
       and then TB >= 0.0
       and then TB <= Duration_Max
       and then G <= One_More_Tolerance * Pk + Speed_Residue,
     Post => TrA + KT + (Pk * TB + G * TB / 2.0) <= 4.0 * (TrA + KT + (Pk * TB - G * TB / 2.0)) + Magnitude_Residue
   is
      V : constant Real := Pk * TB;
      W : constant Real := G * TB;
   begin
      Lemma_Product_Monotone (0.0, Pk, 0.0, TB);
      Lemma_Product_Monotone (G, One_More_Tolerance * Pk + Speed_Residue, TB, TB);
      pragma Assert ((One_More_Tolerance * Pk + Speed_Residue) * TB = One_More_Tolerance * V + Speed_Residue * TB);
      pragma Assert (W <= One_More_Tolerance * V + Speed_Residue * Duration_Max);
      pragma Assert (V + W / 2.0 <= 4.0 * (V - W / 2.0) + Magnitude_Residue);
   end Lemma_Magnitude_Bound;
   --  The sizes of the terms of the travel are together at most four times the exact distance, because the
   --  deceleration ramp ends at a speed which is not negative, up to the tolerance of the check.

   procedure Lemma_Distance_Error (Exact, D, TF, Size : Real)
   with
     Pre  =>
       D >= 0.0
       and then abs (TF - Exact) <= Four_Slacks * Size + Travel_Residue
       and then Size <= 4.0 * Exact + Magnitude_Residue
       and then abs (TF - D) <= Checked_Slack * D + Three_Eta,
     Post => abs (Exact - D) <= Certified_Validity_Tolerance * D + Certified_Distance_Residue
   is
      E : constant Real := abs (Exact - D);
   begin
      pragma Assert (E <= abs (TF - Exact) + abs (TF - D));
      pragma Assert (Exact <= D + E);
      pragma
        Assert
          (E <= Four_Slacks * (4.0 * (D + E) + Magnitude_Residue) + Travel_Residue + Checked_Slack * D + Three_Eta);
   end Lemma_Distance_Error;
   --  The exact distance is within the error of the travel, which is relative to the exact distance itself, of the
   --  distance asked for.

   procedure Lemma_Travel_Rounding (Accel_D, Peak_F, Coast, Decel_D : Dimensioned_Float)
   with
     Pre  =>
       abs Accel_D <= Ramp_Distance_Bound
       and then abs Decel_D <= Ramp_Distance_Bound
       and then Peak_F >= 0.0
       and then Peak_F <= Peak_Bound
       and then Coast >= 0.0
       and then Coast <= Time_Bound,
     Post =>
       abs (R (Peak_F * Coast) - R (Peak_F) * R (Coast)) <= Epsilon * abs (R (Peak_F) * R (Coast)) + Eta
       and then
         abs (R (Accel_D + Peak_F * Coast) - (R (Accel_D) + R (Peak_F * Coast)))
         <= Epsilon * abs (R (Accel_D) + R (Peak_F * Coast)) + Eta
       and then
         abs (R (Accel_D + Peak_F * Coast + Decel_D) - (R (Accel_D + Peak_F * Coast) + R (Decel_D)))
         <= Epsilon * abs (R (Accel_D + Peak_F * Coast) + R (Decel_D)) + Eta
       and then abs (Accel_D + Peak_F * Coast + Decel_D) <= Travel_Bound
   is
   begin
      Lemma_Product_Range (Peak_F, Coast);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Peak_F, Coast);
      Lemma_Partial_Range (Accel_D, Peak_F * Coast);
      Float_Lemmas.Lemma_Rounding_Error_Add (Accel_D, Peak_F * Coast);
      Lemma_Travel_Range (Accel_D + Peak_F * Coast, Decel_D);
      Float_Lemmas.Lemma_Rounding_Error_Add (Accel_D + Peak_F * Coast, Decel_D);
   end Lemma_Travel_Rounding;
   --  The rounding of the three operations of the travel, none of which overflows.

   procedure Lemma_Distance (Accel_D, Peak_F, Coast, Decel_D, Distance : Dimensioned_Float; TrA, Pk, G, TB : Real)
   with
     Pre  =>
       abs Accel_D <= Ramp_Distance_Bound
       and then abs Decel_D <= Ramp_Distance_Bound
       and then Peak_F >= 0.0
       and then Peak_F <= Peak_Bound
       and then Coast >= 0.0
       and then Coast <= Time_Bound
       and then Distance >= 0.0
       and then Distance <= Speed_Bound
       and then TrA >= 0.0
       and then Pk >= 0.0
       and then G >= 0.0
       and then TB >= 0.0
       and then TB <= Duration_Max
       and then abs (R (Peak_F) - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue
       and then abs (R (Accel_D) - TrA) <= Ramp_End_Slack * TrA + Ramp_End_Distance_Residue
       and then
         abs (R (Decel_D) - (R (Peak_F) - G / 2.0) * TB)
         <= Ramp_End_Slack * ((R (Peak_F) + G / 2.0) * TB) + Ramp_End_Distance_Residue
       and then G <= One_More_Tolerance * Pk + Speed_Residue
       and then abs (Accel_D + Peak_F * Coast + Decel_D - Distance) <= 1.0E-9 * Distance,
     Post =>
       abs (TrA + Pk * R (Coast) + (Pk * TB - G * TB / 2.0) - R (Distance))
       <= Certified_Validity_Tolerance * R (Distance) + Certified_Distance_Residue
   is
      Tc : constant Real := R (Coast);
      D  : constant Real := R (Distance);
      P  : constant Real := R (Peak_F);
      AD : constant Real := R (Accel_D);
      DD : constant Real := R (Decel_D);
      M  : constant Real := R (Peak_F * Coast);
      S1 : constant Real := R (Accel_D + Peak_F * Coast);
      TF : constant Real := R (Accel_D + Peak_F * Coast + Decel_D);
      KT : constant Real := Pk * Tc;
      V  : constant Real := Pk * TB;
      W  : constant Real := G * TB;
   begin
      Lemma_Travel_Rounding (Accel_D, Peak_F, Coast, Decel_D);
      Lemma_Nonnegative (Peak_F);
      Lemma_Nonnegative (Distance);
      Lemma_Time_Range (Coast);
      Lemma_Coast_Close (M, P, Pk, Tc);
      Lemma_Decel_Close (DD, P, Pk, G, TB);
      Lemma_Travel_Close (AD, M, DD, S1, TF, TrA, KT, V, W);
      Lemma_Magnitude_Bound (TrA, KT, Pk, G, TB);
      Lemma_Check (Accel_D + Peak_F * Coast + Decel_D, Distance, Distance);
      Lemma_Distance_Error (TrA + KT + (V - W / 2.0), D, TF, TrA + KT + (V + W / 2.0));
   end Lemma_Distance;
   --  What the comparison of the travel says of the exact profile.

   ------------------------------------------------------------------------------------------------------------------
   --  The end formulas, with the components of the ramp as scalars
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_End_Velocity
     (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity; C, T1, T2, T3, T4, Vs, Gain : Real)
   with
     Pre  =>
       Rounding_Is_Bounded (Times, Max_Crackle, Start_Vel)
       and then C = R (Max_Crackle)
       and then T1 = R (Times (1))
       and then T2 = R (Times (2))
       and then T3 = R (Times (3))
       and then T4 = R (Times (4))
       and then Vs = R (Start_Vel)
       and then Gain = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4),
     Post =>
       abs (R (Fast_Velocity_At_Max_Time (Times, Max_Crackle, Start_Vel)) - (Vs + Gain))
       <= Ramp_End_Slack * (abs Vs + abs Gain) + Ramp_End_Velocity_Residue
   is
      Exact : constant Ramp := To_Ramp (Times, Max_Crackle, Start_Vel);
   begin
      Lemma_Ramp_Values (Exact, C, T1, T2, T3, T4, Vs, Gain, 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
   end Lemma_End_Velocity;

   procedure Lemma_End_Distance
     (Times                                          : Feedrate_Profile_Times;
      Max_Crackle                                    : Crackle;
      Start_Vel                                      : Velocity;
      C, T1, T2, T3, T4, Vs, Gain, Total, Mean, Size : Real)
   with
     Pre  =>
       Rounding_Is_Bounded (Times, Max_Crackle, Start_Vel)
       and then C = R (Max_Crackle)
       and then T1 = R (Times (1))
       and then T2 = R (Times (2))
       and then T3 = R (Times (3))
       and then T4 = R (Times (4))
       and then Vs = R (Start_Vel)
       and then Gain = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       and then Total = 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4
       and then Mean = Vs + Gain / 2.0
       and then Size = abs Vs + abs Gain / 2.0,
     Post =>
       abs (R (Fast_Distance_At_Max_Time (Times, Max_Crackle, Start_Vel)) - Mean * Total)
       <= Ramp_End_Slack * (Size * Total) + Ramp_End_Distance_Residue
   is
      Exact : constant Ramp := To_Ramp (Times, Max_Crackle, Start_Vel);
   begin
      Lemma_Ramp_Values (Exact, C, T1, T2, T3, T4, Vs, Gain, Total);
      pragma Assert (Travel (Exact) = Mean * Total);
      pragma Assert (Travel_Magnitude (Exact) = Size * Total);
   end Lemma_End_Distance;

   ------------------------------------------------------------------------------------------------------------------
   --  The theorem
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Checks_Passed
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
   with
     Pre  =>
       Is_Certifiable (Profile, Distance, Start_Vel, End_Vel, Max_Crackle)
       and then Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Max_Crackle),
     Post =>
       Passes_Validity_Checks
         (Profile,
          Distance,
          Start_Vel,
          End_Vel,
          Max_Crackle,
          Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
          Fast_Distance_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
          Fast_Distance_At_Max_Time
            (Profile.Decel, -Max_Crackle, Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel)))
   is
   begin
      pragma Assert (Times_In_Range (Profile.Accel));
      pragma Assert (Times_In_Range (Profile.Decel));
      pragma Assert (Validity_Inputs_Are_Bounded (Distance, Start_Vel, End_Vel, Max_Crackle));
      pragma Assert (Profile.Coast <= Phase_Time_Max);
      pragma Assert (Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Max_Crackle));
   end Lemma_Checks_Passed;
   --  The comparisons which Profile_Is_Valid made, as its postcondition states them. This is proved apart from the
   --  theorem because the provers find it far sooner without the theorem's other facts.

   procedure Lemma_Valid_Profile_Is_Exact
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
   is
      C  : constant Real := R (Max_Crackle);
      Vs : constant Real := R (Start_Vel);
      Ve : constant Real := R (End_Vel);
      D  : constant Real := R (Distance);
      Tc : constant Real := R (Profile.Coast);
      A1 : constant Real := R (Profile.Accel (1));
      A2 : constant Real := R (Profile.Accel (2));
      A3 : constant Real := R (Profile.Accel (3));
      A4 : constant Real := R (Profile.Accel (4));
      B1 : constant Real := R (Profile.Decel (1));
      B2 : constant Real := R (Profile.Decel (2));
      B3 : constant Real := R (Profile.Decel (3));
      B4 : constant Real := R (Profile.Decel (4));
      --  The reals which the profile and the request denote.

      GA  : constant Real := C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4);
      TA  : constant Real := 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4;
      G   : constant Real := C * B1 * (B1 + B2) * (2.0 * B1 + B2 + B3) * (4.0 * B1 + 2.0 * B2 + B3 + B4);
      TB  : constant Real := 8.0 * B1 + 4.0 * B2 + 2.0 * B3 + B4;
      Pk  : constant Real := Vs + GA;
      Top : constant Real := (if Pk >= Ve then Pk else Ve);
      TrA : constant Real := (Vs + GA / 2.0) * TA;
      --  The exact profile.

      Peak_F  : constant Velocity := Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel);
      Accel_D : constant Length := Fast_Distance_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel);
      P       : constant Real := R (Peak_F);
      AD      : constant Real := R (Accel_D);
      --  What the check computes for the acceleration ramp.
   begin
      --  The ranges of the reals.
      Lemma_Time_Range (Profile.Accel (1));
      Lemma_Time_Range (Profile.Accel (2));
      Lemma_Time_Range (Profile.Accel (3));
      Lemma_Time_Range (Profile.Accel (4));
      Lemma_Time_Range (Profile.Decel (1));
      Lemma_Time_Range (Profile.Decel (2));
      Lemma_Time_Range (Profile.Decel (3));
      Lemma_Time_Range (Profile.Decel (4));
      Lemma_Time_Range (Profile.Coast);
      Lemma_Speed_Range (Max_Crackle);
      Lemma_Speed_Range (Start_Vel);
      Lemma_Speed_Range (End_Vel);
      Lemma_Nonnegative (Distance);
      Lemma_Gain_Range (C, A1, A2, A3, A4, GA);
      Lemma_Gain_Range (C, B1, B2, B3, B4, G);
      Lemma_Duration_Range (A1, A2, A3, A4, TA);
      Lemma_Duration_Range (B1, B2, B3, B4, TB);
      Lemma_Product_Monotone (0.0, Vs + GA / 2.0, 0.0, TA);
      pragma Assert (TrA >= 0.0);

      --  The acceleration ramp.
      Lemma_End_Velocity (Profile.Accel, Max_Crackle, Start_Vel, C, A1, A2, A3, A4, Vs, GA);
      pragma Assert (abs (P - Pk) <= Ramp_End_Slack * Pk + Ramp_End_Velocity_Residue);
      Lemma_End_Distance
        (Profile.Accel, Max_Crackle, Start_Vel, C, A1, A2, A3, A4, Vs, GA, TA, Vs + GA / 2.0, Vs + GA / 2.0);
      pragma Assert (abs (AD - TrA) <= Ramp_End_Slack * TrA + Ramp_End_Distance_Residue);

      --  The peak which the check computes is one which the deceleration ramp can start from.
      Lemma_Peak_Bound (P, Pk);
      Lemma_Within_Peak_Bound (Peak_F);
      pragma Assert (Peak_F <= Rounding_Velocity_Max);
      Lemma_Checks_Passed (Profile, Distance, Start_Vel, End_Vel, Max_Crackle);
      pragma Assert (Peak_F >= No_Speed);
      Lemma_Nonnegative (Peak_F);
      pragma Assert (Rounding_Is_Bounded (Profile.Decel, -Max_Crackle, Peak_F));

      declare
         Decel_D  : constant Length := Fast_Distance_At_Max_Time (Profile.Decel, -Max_Crackle, Peak_F);
         Finish_F : constant Velocity := Fast_Velocity_At_Max_Time (Profile.Decel, -Max_Crackle, Peak_F);
         DD       : constant Real := R (Decel_D);
         F        : constant Real := R (Finish_F);
         --  What the check computes for the deceleration ramp.
      begin
         --  The deceleration ramp, from the computed peak.
         Lemma_Negation (Max_Crackle);
         Lemma_Negated_Gain (C, B1, B2, B3, B4);
         Lemma_End_Velocity (Profile.Decel, -Max_Crackle, Peak_F, 0.0 - C, B1, B2, B3, B4, P, 0.0 - G);
         pragma Assert (abs (F - (P - G)) <= Ramp_End_Slack * (P + G) + Ramp_End_Velocity_Residue);
         Lemma_End_Distance
           (Profile.Decel, -Max_Crackle, Peak_F, 0.0 - C, B1, B2, B3, B4, P, 0.0 - G, TB, P - G / 2.0, P + G / 2.0);
         pragma
           Assert (abs (DD - (P - G / 2.0) * TB) <= Ramp_End_Slack * ((P + G / 2.0) * TB) + Ramp_End_Distance_Residue);

         --  The two comparisons.
         Lemma_Speed (Start_Vel, End_Vel, Peak_F, Finish_F, Pk, G, Top);
         Lemma_Distance (Accel_D, Peak_F, Profile.Coast, Decel_D, Distance, TrA, Pk, G, TB);
      end;

      --  The exact profile of the specification is the one above.
      Lemma_Exact_Values (Profile, Max_Crackle, Start_Vel, C, Vs, Tc, A1, A2, A3, A4, GA, TA, B1, B2, B3, B4, G, TB);
      pragma Assert (Exact_Peak (Profile, Max_Crackle, Start_Vel) = Pk);
      pragma Assert (Exact_Finish (Profile, Max_Crackle, Start_Vel) = Pk - G);
      pragma Assert (Exact_Top_Speed (Profile, Max_Crackle, Start_Vel, End_Vel) = Top);
      pragma Assert (Exact_Distance (Profile, Max_Crackle, Start_Vel) = TrA + Pk * Tc + (Pk * TB - G * TB / 2.0));
      pragma Assert (Is_Ramp (Accel_Ramp (Profile, Max_Crackle, Start_Vel)));
      pragma Assert (Is_Ramp (Decel_Ramp (Profile, Max_Crackle, Start_Vel)));
      pragma
        Assert
          (abs (Exact_Finish (Profile, Max_Crackle, Start_Vel) - Ve)
           <= Certified_Validity_Tolerance * Top + Certified_Speed_Residue);
      pragma
        Assert
          (abs (Exact_Distance (Profile, Max_Crackle, Start_Vel) - D)
           <= Certified_Validity_Tolerance * D + Certified_Distance_Residue);
   end Lemma_Valid_Profile_Is_Exact;

   procedure Lemma_Speed_At_Every_Instant
     (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity; Stage : Stage_Index; DT : Real)
   is
      Up   : constant Ramp := Accel_Ramp (Profile, Max_Crackle, Start_Vel);
      Down : constant Ramp := Decel_Ramp (Profile, Max_Crackle, Start_Vel);
   begin
      Lemma_Nonnegative (Max_Crackle);
      if In_Stage (Up, Stage, DT) then
         Profile_Model.Motion.Lemma_Velocity_Range (Up, Stage, DT);
      end if;
      if In_Stage (Down, Stage, DT) then
         Profile_Model.Motion.Lemma_Velocity_Range (Down, Stage, DT);
      end if;
   end Lemma_Speed_At_Every_Instant;

   procedure Lemma_Peaks_Are_Respected (C, T1, T2, T3, T4, Vs, A, J, S, Sign : Real)
   with
     Pre  =>
       C >= 0.0
       and then T1 >= 0.0
       and then T2 >= 0.0
       and then T3 >= 0.0
       and then T4 >= 0.0
       and then (Sign = 1.0 or else Sign = -1.0)
       and then C * T1 <= S * (1.0 + Certified_Tolerance)
       and then C * T1 * (T1 + T2) <= J * (1.0 + Certified_Tolerance)
       and then C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) <= A * (1.0 + Certified_Tolerance),
     Post => Limits_Are_Respected ((T1 => T1, T2 => T2, T3 => T3, T4 => T4, C => Sign * C, Vs => Vs), A, J, S, C)
   is
      Exact : constant Ramp := (T1 => T1, T2 => T2, T3 => T3, T4 => T4, C => Sign * C, Vs => Vs);
   begin
      pragma Assert (C * T1 >= 0.0);
      pragma Assert (C * T1 * (T1 + T2) >= 0.0);
      pragma Assert (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) >= 0.0);
      pragma Assert (abs Exact.C = C);
      pragma Assert (Peak_Snap (Exact) = Sign * (C * T1));
      pragma Assert (Peak_Jerk (Exact) = Sign * (C * T1 * (T1 + T2)));
      pragma Assert (Peak_Accel (Exact) = Sign * (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)));
      pragma Assert (abs Peak_Snap (Exact) = C * T1);
      pragma Assert (abs Peak_Jerk (Exact) = C * T1 * (T1 + T2));
      pragma Assert (abs Peak_Accel (Exact) = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3));
   end Lemma_Peaks_Are_Respected;
   --  The peaks of a ramp are those of its phase times and the size of its crackle, whichever way it runs.

   procedure Lemma_Limits_At_Every_Instant_Of_Profile
     (Profile          : Feedrate_Profile;
      Max_Crackle      : Crackle;
      Start_Vel        : Velocity;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Stage            : Stage_Index;
      DT               : Real)
   is
      Up   : constant Ramp := Accel_Ramp (Profile, Max_Crackle, Start_Vel);
      Down : constant Ramp := Decel_Ramp (Profile, Max_Crackle, Start_Vel);
      C    : constant Real := R (Max_Crackle);
      A    : constant Real := R (Acceleration_Max);
      J    : constant Real := R (Jerk_Max);
      S    : constant Real := R (Snap_Max);
   begin
      Lemma_Nonnegative (Max_Crackle);
      pragma Assert (Snap_Peak (Max_Crackle, Profile.Accel (1)) = C * Up.T1);
      pragma Assert (Jerk_Peak (Max_Crackle, Profile.Accel (1), Profile.Accel (2)) = C * Up.T1 * (Up.T1 + Up.T2));
      pragma
        Assert
          (Acceleration_Peak (Max_Crackle, Profile.Accel (1), Profile.Accel (2), Profile.Accel (3))
           = C * Up.T1 * (Up.T1 + Up.T2) * (2.0 * Up.T1 + Up.T2 + Up.T3));
      Lemma_Peaks_Are_Respected (C, Up.T1, Up.T2, Up.T3, Up.T4, Up.Vs, A, J, S, 1.0);
      pragma Assert (Up = (T1 => Up.T1, T2 => Up.T2, T3 => Up.T3, T4 => Up.T4, C => 1.0 * C, Vs => Up.Vs));
      if In_Stage (Up, Stage, DT) then
         Lemma_Limits_At_Every_Instant (Up, A, J, S, C, Stage, DT);
      end if;
      pragma Assert (Snap_Peak (Max_Crackle, Profile.Decel (1)) = C * Down.T1);
      pragma
        Assert (Jerk_Peak (Max_Crackle, Profile.Decel (1), Profile.Decel (2)) = C * Down.T1 * (Down.T1 + Down.T2));
      pragma
        Assert
          (Acceleration_Peak (Max_Crackle, Profile.Decel (1), Profile.Decel (2), Profile.Decel (3))
           = C * Down.T1 * (Down.T1 + Down.T2) * (2.0 * Down.T1 + Down.T2 + Down.T3));
      Lemma_Peaks_Are_Respected (C, Down.T1, Down.T2, Down.T3, Down.T4, Down.Vs, A, J, S, -1.0);
      pragma
        Assert (Down = (T1 => Down.T1, T2 => Down.T2, T3 => Down.T3, T4 => Down.T4, C => (-1.0) * C, Vs => Down.Vs));
      if In_Stage (Down, Stage, DT) then
         Lemma_Limits_At_Every_Instant (Down, A, J, S, C, Stage, DT);
      end if;
   end Lemma_Limits_At_Every_Instant_Of_Profile;

end Prunt.Motion_Planner.Trajectory.Certificates.Validity;
