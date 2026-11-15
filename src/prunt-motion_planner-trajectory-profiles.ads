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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);
pragma Assertion_Policy (Ignore);

pragma Extensions_Allowed (On);

--  The 15-phase bounded-crackle feedrate profile which the motion planner plans: its phase times, and the functions
--  which evaluate crackle through distance at a time within it.

with Prunt.Motion_Planner.Trajectory.Profile_Model;
with Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Trajectory.Rounding.Relative;

package Prunt.Motion_Planner.Trajectory.Profiles
  with SPARK_Mode => On
is

   type Feedrate_Profile_Times_Index is range 1 .. 4;
   type Feedrate_Profile_Times is array (Feedrate_Profile_Times_Index) of Time;
   --  Represents the timings for segments in a 15-phase motion profile. Note that some times are used for multiple
   --  segments. The crackle profile represented by this array is as follows, where Tn represents an item of this array
   --  and C is the set crackle value:
   --
   --  Stage:     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   --  Duration: T₁  T₂  T₁  T₃  T₁  T₂  T₁  T₄  T₁  T₂  T₁  T₃  T₁  T₂  T₁
   --  Crackle:  +C   0  -C   0  -C   0  +C   0  -C   0  +C   0  +C   0  -C
   --
   --  The total time of a profile is therefore:
   --
   --     ΔT = 8 T₁ + 4 T₂ + 2 T₃ + T₄.
   --
   --  For an acceleration profile the crackle is positive and for a deceleration profile the crackle is negative.
   --
   --  Denote the time of any given stage as t and the crackle of that stage as c. If a stage starts with (s, j, a, v,
   --  x) (crackle .. position) then after time t:
   --
   --     s⁺ = s + ct
   --     j⁺ = j + st + ct²/2
   --     a⁺ = a + jt + st²/2 + ct³/6
   --     v⁺ = v + at + jt²/2 + st³/6 + ct⁴/24
   --     x⁺ = x + vt + at²/2 + jt³/6 + st⁴/24 + ct⁵/120
   --
   --  Note that crackle through velocity all start at zero before any motions are executed. By repeatedly applying the
   --  above over a full motion profile it may be observed that snap, jerk, and acceleration will be zero at the end of
   --  a motion profile.
   --
   --  It may also be observed that:
   --
   --     - Maximum snap         = CT₁
   --     - Maximum jerk         = CT₁(T₁ + T₂)
   --     - Maximum acceleration = CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)
   --
   --  Over the entire profile:
   --
   --     ΔV = CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)(4T₁ + 2T₂ + T₃ + T₄)
   --
   --  Thus:
   --
   --     v(ΔT) = Vₛ + ΔV
   --
   --  This is the expression found in Fast_Velocity_At_Max_Time.
   --
   --  Because the acceleration profile is symmetrical around the middle of a profile, velocity has the identity:
   --
   --     v(t) + v(ΔT - t) = 2Vₛ + ΔV
   --
   --  Integrating over [0, ΔT] gives:
   --
   --     ΔX = (Vₛ + ΔV/2)ΔT
   --
   --  Substituting the above definitions of ΔV and ΔX into ΔX gives:
   --
   --     (Vₛ + CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)(4T₁ + 2T₂ + T₃ + T₄) / 2)(8T₁ + 4T₂ + 2T₃ + T₄)
   --
   --  This is the expression found in Fast_Distance_At_Max_Time.

   type Feedrate_Profile is record
      --  Represents the timings for segments in a 31-phase motion profile. Refer to the documentation on
      --  Feedrate_Profile_Times for what the individual parts of this profile are.

      Accel : Feedrate_Profile_Times;
      Coast : Time;
      Decel : Feedrate_Profile_Times;
   end record;

   ------------------------------------------------------------------------------------------------------------------
   --  The proved domain
   --
   --  The functions below are proved free of run-time errors for arguments within these magnitudes, which are far
   --  beyond any that a machine can have. The preconditions are not checked when the program runs: outside them
   --  the functions compute exactly what Prunt's do, including raising Constraint_Error where an operation
   --  overflows, but nothing is proved about that.
   ------------------------------------------------------------------------------------------------------------------

   --  Typed as the quantities they bound, so that the contracts compare against them directly.

   No_Time : constant Time := 0.0;

   No_Velocity : constant Velocity := 0.0;

   Phase_Time_Max : constant Time := Time (2#1.0#E165);
   --  The largest phase time or coast time of the proved domain.

   Ramp_Time_Max : constant Time := Time (2#1.0#E169);
   --  No ramp of the proved domain lasts longer than this.

   Ramp_Time_Sum_Max : constant Time := Time (2#1111.0#E165);
   --  The exact bound of the duration of a ramp of the proved domain: fifteen times Phase_Time_Max.

   Profile_Time_Max : constant Time := Time (2#1.0#E171);
   --  No complete profile of the proved domain lasts longer than this.

   Crackle_Magnitude_Max : constant Crackle := Crackle (2#1.0#E100);
   --  The largest crackle magnitude of the proved domain.

   Start_Velocity_Max : constant Velocity := Velocity (2#1.0#E100);
   --  The largest starting speed of a complete profile in the proved domain.

   Ramp_Velocity_Max : constant Velocity := Velocity (2#1.0#E800);
   --  The largest starting speed of a single ramp in the proved domain, which has to admit any speed that the
   --  acceleration ramp of a complete profile can end at.

   Ramp_Gain_Max : constant Velocity := Velocity (2#1.0#E767);
   --  No ramp of the proved domain changes speed by more than this.

   Evaluated_Gain_Max : constant Velocity := Velocity (2#1.0#E782);
   --  No evaluation of a ramp of the proved domain differs from its starting speed by more than this.

   Ramp_Distance_Max : constant Length := Length (2#1.0#E970);
   --  No ramp of the proved domain covers more than this.

   Evaluated_Travel_Max : constant Length := Length (2#1.0#E950);
   --  No evaluation of a ramp of the proved domain differs from travel at its starting speed by more than this.

   Evaluated_Snap_Max : constant Snap := Snap (2#1.0#E272);
   --  No evaluation of a ramp of the proved domain has a larger snap.

   Evaluated_Jerk_Max : constant Jerk := Jerk (2#1.0#E441);
   --  No evaluation of a ramp of the proved domain has a larger jerk.

   Evaluated_Acceleration_Max : constant Acceleration := Acceleration (2#1.0#E609);
   --  No evaluation of a ramp of the proved domain has a larger acceleration.

   function Times_In_Range (Times : Feedrate_Profile_Times) return Boolean
   is (Times (1) >= No_Time
       and then Times (1) <= Phase_Time_Max
       and then Times (2) >= No_Time
       and then Times (2) <= Phase_Time_Max
       and then Times (3) >= No_Time
       and then Times (3) <= Phase_Time_Max
       and then Times (4) >= No_Time
       and then Times (4) <= Phase_Time_Max);

   ------------------------------------------------------------------------------------------------------------------
   --  The ramp which phase times denote, and how close the end formulas are to it
   --
   --  Fast_Velocity_At_Max_Time and Fast_Distance_At_Max_Time evaluate the end of a ramp in floating point. Their
   --  postconditions bound the difference from the end of the exact ramp of Trajectory.Profile_Model, for
   --  arguments small enough that the rounding lemmas of SPARKlib apply to every operation.
   ------------------------------------------------------------------------------------------------------------------

   use type Prunt.Motion_Planner.Reals.Real;

   function To_Ramp
     (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Profile_Model.Ramp
   is ((T1 => Reals.R (Times (1)),
        T2 => Reals.R (Times (2)),
        T3 => Reals.R (Times (3)),
        T4 => Reals.R (Times (4)),
        C  => Reals.R (Max_Crackle),
        Vs => Reals.R (Start_Vel)))
   with Ghost;
   --  The exact ramp which floating-point phase times, crackle and starting velocity denote.

   Rounding_Time_Max : constant Time := Time (2#1.0#E100);
   --  The largest phase time for which the rounding of the end formulas is bounded.

   Rounding_Crackle_Max : constant Crackle := Crackle (2#1.0#E80);
   --  The largest crackle magnitude for which it is.

   Rounding_Velocity_Max : constant Velocity := Velocity (2#1.0#E500);
   --  The largest starting speed for which it is. The speed which a ramp within these bounds ends at is within
   --  it, so that the deceleration ramp of a profile can start from where its acceleration ramp ended.

   function Rounding_Is_Bounded
     (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Boolean
   is (Times (1) >= No_Time
       and then Times (1) <= Rounding_Time_Max
       and then Times (2) >= No_Time
       and then Times (2) <= Rounding_Time_Max
       and then Times (3) >= No_Time
       and then Times (3) <= Rounding_Time_Max
       and then Times (4) >= No_Time
       and then Times (4) <= Rounding_Time_Max
       and then abs Max_Crackle <= Rounding_Crackle_Max
       and then abs Start_Vel <= Rounding_Velocity_Max);

   Ramp_End_Slack : constant := 2.0**(-44);
   --  How far, relative to the magnitudes of its terms, an end formula is from the end of the exact ramp.

   Ramp_End_Velocity_Residue : constant := 2.0**(-700);
   Ramp_End_Distance_Residue : constant := 2.0**(-600);
   --  How far it is beyond that, in millimetres per second and in millimetres, where a product underflows.

   function Velocity_Magnitude (Exact : Profile_Model.Ramp) return Reals.Real
   is (abs Exact.Vs + abs Profile_Model.Velocity_Gain (Exact))
   with Ghost;
   --  The size of the terms of the final velocity: the starting speed and the change of speed.

   function Travel_Magnitude (Exact : Profile_Model.Ramp) return Reals.Real
   is ((abs Exact.Vs + abs Profile_Model.Velocity_Gain (Exact) / 2.0) * Profile_Model.Total_Time (Exact))
   with Ghost;
   --  The size of the terms of the travel.

   function Times_Are_Zero (Times : Feedrate_Profile_Times) return Boolean
   is (Times (1) = No_Time and then Times (2) = No_Time and then Times (3) = No_Time and then Times (4) = No_Time);
   --  A ramp which takes no time: it covers no distance and leaves the speed as it was.

   function Ramp_In_Range (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Boolean
   is (Times_In_Range (Times)
       and then abs Max_Crackle <= Crackle_Magnitude_Max
       and then abs Start_Vel <= Ramp_Velocity_Max);

   function Profile_In_Range (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Boolean
   is (Times_In_Range (Profile.Accel)
       and then Times_In_Range (Profile.Decel)
       and then Profile.Coast >= No_Time
       and then Profile.Coast <= Phase_Time_Max
       and then abs Max_Crackle <= Crackle_Magnitude_Max
       and then abs Start_Vel <= Start_Velocity_Max);

   ------------------------------------------------------------------------------------------------------------------
   --  The end formulas as functions of the phase times themselves
   --
   --  Distance_Formula and Velocity_Formula are the values which Fast_Distance_At_Max_Time and
   --  Fast_Velocity_At_Max_Time compute, as functions of four phase times instead of an array of them. A search
   --  which changes one phase time states what it found with them: two arrays whose components are the same
   --  cannot be shown to be the same array, but two calls with the same phase times are the same value.
   ------------------------------------------------------------------------------------------------------------------

   function Formula_In_Range (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Boolean
   is (T1 >= No_Time
       and then T1 <= Phase_Time_Max
       and then T2 >= No_Time
       and then T2 <= Phase_Time_Max
       and then T3 >= No_Time
       and then T3 <= Phase_Time_Max
       and then T4 >= No_Time
       and then T4 <= Phase_Time_Max
       and then abs Max_Crackle <= Crackle_Magnitude_Max
       and then abs Start_Vel <= Ramp_Velocity_Max)
   with Ghost;
   --  Ramp_In_Range for phase times which are not in an array.

   function Distance_Formula (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   with
     Ghost,
     Global => null,
     Pre    => Formula_In_Range (T1, T2, T3, T4, Max_Crackle, Start_Vel),
     Post   => abs Distance_Formula'Result <= Ramp_Distance_Max;
   --  (Start_Vel + Max_Crackle * T1 * (T1 + T2) * (2 * T1 + T2 + T3) * (4 * T1 + 2 * T2 + T3 + T4) / 2)
   --  * (8 * T1 + 4 * T2 + 2 * T3 + T4), in floating point, operation for operation as
   --  Fast_Distance_At_Max_Time computes it.

   function Velocity_Formula (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   with
     Ghost,
     Global => null,
     Pre    => Formula_In_Range (T1, T2, T3, T4, Max_Crackle, Start_Vel),
     Post   => abs Velocity_Formula'Result <= abs Start_Vel + Ramp_Gain_Max;
   --  Start_Vel + Max_Crackle * T1 * (T1 + T2) * (2 * T1 + T2 + T3) * (4 * T1 + 2 * T2 + T3 + T4), in floating
   --  point, operation for operation as Fast_Velocity_At_Max_Time computes it.

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   with
     Global => null,
     Pre    => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post   =>
       abs Fast_Distance_At_Max_Time'Result <= Ramp_Distance_Max
       and then
         Fast_Distance_At_Max_Time'Result
         = Distance_Formula (Profile (1), Profile (2), Profile (3), Profile (4), Max_Crackle, Start_Vel)
       and then (if Times_Are_Zero (Profile) then Fast_Distance_At_Max_Time'Result = 0.0 * mm)
       and then
         (if Rounding_Is_Bounded (Profile, Max_Crackle, Start_Vel)
          then
            abs (Reals.R (Fast_Distance_At_Max_Time'Result)
                 - Profile_Model.Travel (To_Ramp (Profile, Max_Crackle, Start_Vel)))
            <= Ramp_End_Slack
               * Travel_Magnitude (To_Ramp (Profile, Max_Crackle, Start_Vel))
               + Ramp_End_Distance_Residue);
   --  Calculates the total distance covered during an acceleration or deceleration phase defined by Profile with a
   --  given starting velocity and maximum crackle.
   --
   --  For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  This function is an optimised version of Distance_At_Time where T is equal to Total_Time (Profile). While
   --  this function is symbolically identical, it may not be numerically identical to Distance_At_Time, but this
   --  does not cause issues with the current design of the motion planner.

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   with
     Global => null,
     Pre    => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post   =>
       abs Fast_Velocity_At_Max_Time'Result <= abs Start_Vel + Ramp_Gain_Max
       and then
         Fast_Velocity_At_Max_Time'Result
         = Velocity_Formula (Profile (1), Profile (2), Profile (3), Profile (4), Max_Crackle, Start_Vel)
       and then (if Times_Are_Zero (Profile) then Fast_Velocity_At_Max_Time'Result = Start_Vel)
       and then
         (if Rounding_Is_Bounded (Profile, Max_Crackle, Start_Vel)
          then
            abs (Reals.R (Fast_Velocity_At_Max_Time'Result)
                 - (Reals.R (Start_Vel) + Profile_Model.Velocity_Gain (To_Ramp (Profile, Max_Crackle, Start_Vel))))
            <= Ramp_End_Slack
               * Velocity_Magnitude (To_Ramp (Profile, Max_Crackle, Start_Vel))
               + Ramp_End_Velocity_Residue);
   --  Calculates the final velocity after an acceleration or deceleration phase defined by Profile with a given
   --  starting velocity and maximum crackle.
   --
   --  For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  This function is an optimised version of Velocity_At_Time where T is equal to Total_Time (Profile). While
   --  this function is symbolically identical, it may not be numerically identical to Velocity_At_Time, but this
   --  does not cause issues with the current design of the motion planner.

   ------------------------------------------------------------------------------------------------------------------
   --  The end formulas to within a relative bound
   --
   --  The bounds in the postconditions above have an absolute part, 2**-700 mm/s and 2**-600 mm, which is what
   --  SPARKlib's allowance for underflow comes to after a chain of products. A solver's accuracy cannot be stated
   --  with it: the quantities of a slow, short ramp are smaller. For a ramp whose first phase time and crackle
   --  are far from underflowing, and whose peak snap is far from overflowing, no product of either formula can
   --  underflow, and the two lemmas below bound the formulas relative to the exact values alone. The exact
   --  values are written with the reals which the arguments denote, so that they are polynomials in a few
   --  unknowns: Lemma_Exact_Ramp_End says that they are the change of speed and the travel of the exact ramp.
   ------------------------------------------------------------------------------------------------------------------

   function Exact_Gain (C, A1, A2, A3, A4 : Reals.Real) return Reals.Real
   is (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4))
   with Ghost;
   --  The change of speed of the exact ramp with crackle C and phase times A1 to A4.

   function Exact_Duration (A1, A2, A3, A4 : Reals.Real) return Reals.Real
   is (8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4)
   with Ghost;

   function Exact_Travel (C, V, A1, A2, A3, A4 : Reals.Real) return Reals.Real
   is ((V + Exact_Gain (C, A1, A2, A3, A4) / 2.0) * Exact_Duration (A1, A2, A3, A4))
   with Ghost;
   --  The travel of that ramp from the speed V.

   procedure Lemma_Exact_Ramp_End (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Post   =>
       Profile_Model.Velocity_Gain (To_Ramp (Times, Max_Crackle, Start_Vel))
       = Exact_Gain
           (Reals.R (Max_Crackle), Reals.R (Times (1)), Reals.R (Times (2)), Reals.R (Times (3)), Reals.R (Times (4)))
       and then
         Profile_Model.Travel (To_Ramp (Times, Max_Crackle, Start_Vel))
         = Exact_Travel
             (Reals.R (Max_Crackle),
              Reals.R (Start_Vel),
              Reals.R (Times (1)),
              Reals.R (Times (2)),
              Reals.R (Times (3)),
              Reals.R (Times (4)))
       and then
         Profile_Model.Total_Time (To_Ramp (Times, Max_Crackle, Start_Vel))
         = Exact_Duration (Reals.R (Times (1)), Reals.R (Times (2)), Reals.R (Times (3)), Reals.R (Times (4)));

   Normal_Time_Min : constant Time := Time (2#1.0#E-170);
   --  A first phase time at least this long is far from underflowing.

   Normal_Crackle_Min : constant Crackle := Crackle (2#1.0#E-90);
   --  A crackle at least this large is.

   Normal_Start_Max : constant Velocity := Velocity (2#1.0#E100);
   --  The largest starting speed for which the travel is relatively bounded.

   Normal_Snap_Max : constant := 2.0**100;
   --  The largest peak snap, the product of crackle and first phase time, for which the formulas are relatively
   --  bounded.

   function Gain_Is_Normal (T1, T2, T3, T4 : Time; Max_Crackle : Crackle) return Boolean
   is (T1 >= Normal_Time_Min
       and then T1 <= Phase_Time_Max
       and then T2 >= No_Time
       and then T2 <= Phase_Time_Max
       and then T3 >= No_Time
       and then T3 <= Phase_Time_Max
       and then T4 >= No_Time
       and then T4 <= Phase_Time_Max
       and then Max_Crackle >= Normal_Crackle_Min
       and then Max_Crackle <= Crackle_Magnitude_Max
       and then Reals.R (Max_Crackle) * Reals.R (T1) <= Normal_Snap_Max)
   with Ghost;
   --  A ramp with positive crackle whose change of speed is relatively bounded.

   function Travel_Is_Normal (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Boolean
   is (Gain_Is_Normal (T1, T2, T3, T4, Max_Crackle)
       and then T1 <= Rounding_Time_Max
       and then T2 <= Rounding_Time_Max
       and then T3 <= Rounding_Time_Max
       and then T4 <= Rounding_Time_Max
       and then Start_Vel >= No_Velocity
       and then Start_Vel <= Normal_Start_Max)
   with Ghost;
   --  One whose travel is, from a starting speed which is not negative.

   Gain_Slack : constant := 2.0**(-47);
   --  How far, relatively, Velocity_Formula from rest is from the exact change of speed: nineteen rounded
   --  operations, where twice a phase time counts for eight.

   Travel_Slack : constant := 2.0**(-46);
   --  How far, relatively, Distance_Formula is from the exact travel: forty-one.

   procedure Lemma_Gain_Is_Near (T1, T2, T3, T4 : Time; Max_Crackle : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Gain_Is_Normal (T1, T2, T3, T4, Max_Crackle),
     Post   =>
       Rounding.Relative.Near
         (Exact_Gain (Reals.R (Max_Crackle), Reals.R (T1), Reals.R (T2), Reals.R (T3), Reals.R (T4)),
          Velocity_Formula (T1, T2, T3, T4, Max_Crackle, No_Velocity),
          Gain_Slack);

   procedure Lemma_Travel_Is_Near (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Travel_Is_Normal (T1, T2, T3, T4, Max_Crackle, Start_Vel),
     Post   =>
       Rounding.Relative.Near
         (Exact_Travel
            (Reals.R (Max_Crackle), Reals.R (Start_Vel), Reals.R (T1), Reals.R (T2), Reals.R (T3), Reals.R (T4)),
          Distance_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel),
          Travel_Slack);

   function Total_Time (Times : Feedrate_Profile_Times) return Time
   with
     Global => null,
     Pre    => Times_In_Range (Times),
     Post   =>
       Total_Time'Result >= No_Time
       and then Total_Time'Result <= Ramp_Time_Max
       and then Total_Time'Result <= Ramp_Time_Sum_Max;
   --  Calculates the total duration of a single acceleration or deceleration phase. This is not equivalent to the sum
   --  of components as some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Crackle_At_Time'Result <= abs Max_Crackle;
   --  Returns the crackle at a specific time T within a single acceleration or deceleration phase. The crackle will
   --  be either +Max_Crackle, -Max_Crackle, or zero. For an acceleration phase Max_Crackle should be positive
   --  and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Snap_At_Time'Result <= Evaluated_Snap_Max;
   --  Returns the snap at a specific time T within a single acceleration or deceleration phase. For an acceleration
   --  phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Jerk_At_Time'Result <= Evaluated_Jerk_Max;
   --  Returns the jerk at a specific time T within a single acceleration or deceleration phase. For an acceleration
   --  phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Acceleration_At_Time'Result <= Evaluated_Acceleration_Max;
   --  Returns the acceleration at a specific time T within a single acceleration or deceleration phase. For an
   --  acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Velocity_At_Time'Result <= abs Start_Vel + Evaluated_Gain_Max;
   --  Returns the velocity at a specific time T within a single acceleration or deceleration phase. For an
   --  acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   with
     Global => null,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Distance_At_Time'Result <= abs Start_Vel * Ramp_Time_Max + Evaluated_Travel_Max;
   --  Returns the distance from the start point at a specific time T within a single acceleration or deceleration
   --  phase. For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle
   --  should be negative.
   --
   --  For a phase whose speeds are not negative the distance never decreases as T increases, to the last bit. A
   --  deceleration phase whose final speed is negative only by rounding is evaluated as ending at rest. One which
   --  really turns back is not, and its return value may be negative.

   type Prepared_Ramp is private;
   --  An acceleration or deceleration phase with everything Distance_At_Time derives from it before it looks at
   --  the time.

   function Is_Prepared (Ramp : Prepared_Ramp) return Boolean
   with Ghost, Global => null;
   --  Ramp holds a ramp of the proved domain, as Prepare returns one.

   function Ramp_Time (Ramp : Prepared_Ramp) return Time
   with Ghost, Global => null;
   --  The duration of the ramp which Ramp was prepared from.

   function Prepare
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Prepared_Ramp
   with
     Global => null,
     Pre    => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post   => Is_Prepared (Prepare'Result) and then Ramp_Time (Prepare'Result) = Total_Time (Profile);

   function Distance_At_Time (Ramp : Prepared_Ramp; T : Time) return Length
   with
     Global => null,
     Pre    => Is_Prepared (Ramp) and then T >= No_Time and then T <= Ramp_Time (Ramp),
     Post   => abs Distance_At_Time'Result <= Ramp_Distance_Max;
   --  Distance_At_Time (Profile, T, Max_Crackle, Start_Vel) for the arguments Ramp was prepared from: the same
   --  value to the last bit, for a fraction of the work. Preparing costs about as much as ten evaluations.

   function Is_Ordered (Ramp : Prepared_Ramp) return Boolean
   with Ghost, Global => null;
   --  The table of Ramp is in order: no stage ends before an earlier one does, each stage starts where the one
   --  before it ends, the distance of each stage is kept between two limits which are in order, and the upper
   --  limit of a stage is at most the lower limit of the next.

   procedure Lemma_Prepared_Is_Ordered (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post   => Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel));
   --  Prepare returns a table which is in order, whatever the rounding of its entries.

   procedure Lemma_Prepared_Distance
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   =>
       Distance_At_Time (Prepare (Profile, Max_Crackle, Start_Vel), T)
       = Distance_At_Time (Profile, T, Max_Crackle, Start_Vel);
   --  The two evaluations agree: every operation of either is an operation of the other, on the same values.

   function Moves_Forward (Ramp : Prepared_Ramp) return Boolean
   with Ghost, Global => null;
   --  Ramp speeds up from a speed which is not negative, or slows down to one: it does not turn back.

   procedure Lemma_Prepared_Moves_Forward
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       Ramp_In_Range (Profile, Max_Crackle, Start_Vel)
       and then
         (if Max_Crackle >= 0.0 * mm / s**5
          then Start_Vel >= No_Velocity
          else Fast_Velocity_At_Max_Time (Profile, Max_Crackle, Start_Vel) >= No_Velocity),
     Post   => Moves_Forward (Prepare (Profile, Max_Crackle, Start_Vel));

   function Stages_Are_Monotone (Ramp : Prepared_Ramp; T1, T2 : Time) return Boolean
   with
     Ghost,
     Global => null,
     Pre    => Is_Prepared (Ramp) and then T1 >= No_Time and then T1 <= T2 and then T2 <= Ramp_Time (Ramp);
   --  The hypothesis of Lemma_Distance_Is_Monotone, which nothing in this package proves or assumes. The distance
   --  at a time is the distance gained by then by the ramp which speeds up from rest with the same phase times,
   --  evaluated at that time, or, for a ramp which slows down, at the time which is left. Within a stage that
   --  gain is a polynomial in the time into the stage, evaluated by a compensated Horner scheme. The hypothesis
   --  is that, if the two times are evaluated in the same stage, the value of that scheme for the earlier of them
   --  is at most its value for the later. It says nothing if they are evaluated in different stages.
   --
   --  docs/spark_trajectory_monotonicity.md argues that the scheme does not decrease over a whole stage. That cannot
   --  be stated here, because a quantified expression cannot range over the floating-point numbers, and it is more
   --  than the lemma needs.

   procedure Lemma_Distance_Is_Monotone (Ramp : Prepared_Ramp; T1, T2 : Time)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       Is_Prepared (Ramp)
       and then Is_Ordered (Ramp)
       and then Moves_Forward (Ramp)
       and then T1 >= No_Time
       and then T1 <= T2
       and then T2 <= Ramp_Time (Ramp)
       and then Stages_Are_Monotone (Ramp, T1, T2),
     Post   => Distance_At_Time (Ramp, T1) <= Distance_At_Time (Ramp, T2);
   --  The distance of a ramp which does not turn back does not decrease from T1 to T2, to the last bit, if the
   --  polynomial of the stage does not (sections 3.5 and 3.6 of docs/spark_trajectory_monotonicity.md).

   Evaluated_Profile_Gain_Max : constant Velocity := Velocity (2#1.0#E784);
   --  No evaluation of a complete profile of the proved domain differs from its starting speed by more than this:
   --  its two ramps each change the speed by at most Evaluated_Gain_Max.

   Evaluated_Profile_Travel_Max : constant Length := Length (2#1.0#E954);
   --  No evaluation of a complete profile of the proved domain differs from travel at its starting speed by more
   --  than this.

   function Total_Time (Profile : Feedrate_Profile) return Time
   with
     Global => null,
     Pre    =>
       Times_In_Range (Profile.Accel)
       and then Times_In_Range (Profile.Decel)
       and then Profile.Coast >= No_Time
       and then Profile.Coast <= Phase_Time_Max,
     Post   =>
       Total_Time'Result >= No_Time
       and then Total_Time'Result <= Profile_Time_Max
       and then Total_Time'Result = Total_Time (Profile.Accel) + Profile.Coast + Total_Time (Profile.Decel);
   --  Calculates the total duration of a complete feedrate profile. This is not equivalent to the sum of components as
   --  some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Crackle_At_Time'Result <= abs Max_Crackle;
   --  Returns the crackle at a specific time T within a feedrate profile. The crackle will be either +Max_Crackle,
   --  -Max_Crackle, or zero.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Snap_At_Time'Result <= Evaluated_Snap_Max;
   --  Returns the snap at a specific time T within a feedrate profile. The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Jerk_At_Time'Result <= Evaluated_Jerk_Max;
   --  Returns the jerk at a specific time T within a feedrate profile. The return value may be negative.

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, No_Velocity) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Acceleration_At_Time'Result <= Evaluated_Acceleration_Max;
   --  Returns the acceleration at a specific time T within a feedrate profile. The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Velocity_At_Time'Result <= abs Start_Vel + Evaluated_Profile_Gain_Max;
   --  Returns the velocity at a specific time T within a feedrate profile. The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   with
     Global => null,
     Pre    =>
       Profile_In_Range (Profile, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Total_Time (Profile),
     Post   => abs Distance_At_Time'Result <= abs Start_Vel * Profile_Time_Max + Evaluated_Profile_Travel_Max;
   --  Returns the distance from the start point at a specific time T within a feedrate profile. The return value may
   --  be negative.

private

   type Feedrate_Profile_Stage_Index is range 1 .. 15;

   type Ramp_Stage is record
      Start, Len : Time := 0.0 * s;
      --  When the stage starts and how long it nominally lasts.
      Vel        : Velocity := 0.0 * mm / s;
      Acc_2      : Acceleration := 0.0 * mm / s**2;
      Jr_6       : Jerk := 0.0 * mm / s**3;
      Sn_24      : Snap := 0.0 * mm / s**4;
      C_120      : Crackle := 0.0 * mm / s**5;
      --  The coefficients of the distance gained within the stage, as a polynomial in the time into it.
      Dist       : Length := 0.0 * mm;
      --  The distance gained before the stage.
      Lo, Hi     : Length := 0.0 * mm;
      --  What the distance gained by a time in the stage is kept between. Hi of a stage is at most Lo of the next.
   end record;
   --  One of the fifteen stages of constant crackle of a ramp which speeds up from rest.

   type Ramp_Stage_Bounds is array (Feedrate_Profile_Stage_Index range 1 .. 14) of Time;
   type Ramp_Stage_Array is array (Feedrate_Profile_Stage_Index) of Ramp_Stage;

   type Ramp_Kind is (Speeds_Up, Slows_Down, Turns_Back);

   type Ramp_Shape is record
      Kind  : Ramp_Kind;
      Vel   : Velocity;
      --  The final speed of a ramp which slows down, and the starting speed of any other.
      Total : Time;
      Whole : Length;
      --  The time of the whole ramp and, for a ramp which slows down, its distance.
   end record;
   --  How the distance of a ramp follows from that of the ramp which speeds up from rest with its phase times.

   type Prepared_Ramp is record
      Shape  : Ramp_Shape;
      Bounds : Ramp_Stage_Bounds;
      Stages : Ramp_Stage_Array;
   end record;

   function Ramp_Time (Ramp : Prepared_Ramp) return Time
   is (Ramp.Shape.Total);

end Prunt.Motion_Planner.Trajectory.Profiles;
