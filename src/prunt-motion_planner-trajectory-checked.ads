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
--  No contract or assertion of this unit is executed, whatever the build profile.

--  Operations which Prunt lets overflow, for the Constraint_Error that the overflow check raises to be the outcome.
--
--  GNATprove proves that a run-time check cannot fail. Where a failing check is the intended behaviour, the
--  operation is made here by a procedure which is specified to raise Constraint_Error, so that the caller's own
--  contract has to admit the exception. The bodies are not seen by GNATprove: each is the single operation, compiled
--  with the overflow check on, and is inlined where it is called.

with Prunt.Motion_Planner.Trajectory.Profiles; use Prunt.Motion_Planner.Trajectory.Profiles;

package Prunt.Motion_Planner.Trajectory.Checked
  with SPARK_Mode => On, Always_Terminates
is

   procedure Divide (Distance : Length; Speed : Velocity; Result : out Time)
   with
     Inline,
     Global            => null,
     Pre               => Speed > 0.0 * mm / s,
     Post              => (if Distance >= 0.0 * mm then Result >= 0.0 * s),
     Exceptional_Cases => (Constraint_Error => True);
   --  Result := Distance / Speed, raising Constraint_Error if the quotient is not representable.

   procedure Total_Time (Profile : Feedrate_Profile; Result : out Time)
   with
     Inline,
     Global            => null,
     Pre               =>
       Times_In_Range (Profile.Accel) and then Times_In_Range (Profile.Decel) and then Profile.Coast >= No_Time,
     Post              => (if Profile.Coast <= Phase_Time_Max then Result = Total_Time (Profile)),
     Exceptional_Cases => (Constraint_Error => Profile.Coast > Phase_Time_Max);
   --  Result := Total_Time (Profile), raising Constraint_Error if the total is not representable. It is the coast
   --  time which a caller cannot bound: for one within the proved domain of Profiles, Total_Time is proved there
   --  not to raise, which is all that the exceptional case states.

   function Travel_Is_Bounded
     (Accel_Distance : Length; Peak : Velocity; Coast : Time; Decel_Distance : Length) return Boolean
   is (abs Accel_Distance <= Ramp_Distance_Max
       and then abs Peak <= Ramp_Velocity_Max
       and then Coast >= No_Time
       and then Coast <= Phase_Time_Max
       and then abs Decel_Distance <= Ramp_Distance_Max);
   --  Arguments for which the sum that Travel forms is proved to be representable, by Bounded_Travel below.

   function Bounded_Travel
     (Accel_Distance : Length; Peak : Velocity; Coast : Time; Decel_Distance : Length) return Length
   is (Accel_Distance + Peak * Coast + Decel_Distance)
   with Ghost, Global => null, Pre => Travel_Is_Bounded (Accel_Distance, Peak, Coast, Decel_Distance);
   --  The expression of Travel where it cannot overflow. GNATprove proves that of this function, which is what
   --  entitles Travel to state that it does not raise there.

   procedure Travel
     (Accel_Distance : Length; Peak : Velocity; Coast : Time; Decel_Distance : Length; Result : out Length)
   with
     Inline,
     Global            => null,
     Post              =>
       (if Travel_Is_Bounded (Accel_Distance, Peak, Coast, Decel_Distance)
        then Result = Bounded_Travel (Accel_Distance, Peak, Coast, Decel_Distance)),
     Exceptional_Cases => (Constraint_Error => not Travel_Is_Bounded (Accel_Distance, Peak, Coast, Decel_Distance));
   --  Result := Accel_Distance + Peak * Coast + Decel_Distance, raising Constraint_Error if the product or a sum
   --  is not representable.

end Prunt.Motion_Planner.Trajectory.Checked;
