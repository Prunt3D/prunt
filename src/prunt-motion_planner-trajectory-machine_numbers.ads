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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

--  Nonnegative machine numbers seen as their bit patterns, which are ordered as the numbers are. The searches of the
--  profile generator count and step through machine numbers this way.
--
--  GNATprove has no model of the bit pattern of a floating-point value: it treats To_Bits as an unknown function, and
--  does not allow an unchecked conversion to a floating-point type at all. So To_Real and To_Time have bodies which
--  GNATprove does not see, and the lemmas below are not proved. They state how IEEE 754 binary64 lays out
--  nonnegative finite numbers, are the only facts about bit patterns which the proofs use, and are checked by
--  running the tests of Prunt.Motion_Planner.Trajectory.Test.

with Ada.Unchecked_Conversion;

package Prunt.Motion_Planner.Trajectory.Machine_Numbers
  with SPARK_Mode => On, Always_Terminates
is

   pragma Compile_Time_Error (Dimensioned_Float'Size /= 64, "The profile searches require IEEE 754 binary64.");

   type Bits is mod 2**64;

   Last_Pattern : constant Bits := 16#7FEF_FFFF_FFFF_FFFF#;
   --  The pattern of Dimensioned_Float'Last. Every pattern up to it is that of a nonnegative finite number.

   function To_Bits is new Ada.Unchecked_Conversion (Dimensioned_Float, Bits);
   --  The bit pattern of a number. An instance of the conversion itself, as in Prunt, so that it compiles to the
   --  same code.

   function To_Real (B : Bits) return Dimensioned_Float
   with
     Inline_Always,
     Global => null,
     Pre    => B <= Last_Pattern,
     Post   => To_Real'Result >= 0.0 and then (B > 0) = (To_Real'Result > 0.0) and then To_Bits (To_Real'Result) = B;
   --  The nonnegative number with bit pattern B.

   function To_Time (B : Bits) return Time
   with
     Inline_Always,
     Global => null,
     Pre    => B <= Last_Pattern,
     Post   =>
       To_Time'Result >= 0.0 * s and then (B > 0) = (To_Time'Result > 0.0 * s) and then To_Bits (To_Time'Result) = B;
   --  To_Real for a result which is a time.

   procedure Lemma_Zero_Pattern
   with Ghost, Global => null, Post => To_Bits (0.0) = 0;
   --  Positive zero has the all-zeros pattern. Negative zero does not, and no lemma here covers it.

   procedure Lemma_Positive_Pattern (X : Dimensioned_Float)
   with Ghost, Global => null, Pre => X > 0.0, Post => To_Bits (X) in 1 .. Last_Pattern;

   procedure Lemma_Round_Trip (X : Dimensioned_Float)
   with Ghost, Global => null, Pre => To_Bits (X) <= Last_Pattern, Post => To_Real (To_Bits (X)) = X;

   procedure Lemma_Order (X, Y : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Pre    => To_Bits (X) <= Last_Pattern and then To_Bits (Y) <= Last_Pattern,
     Post   => (X < Y) = (To_Bits (X) < To_Bits (Y)) and then (X = Y) = (To_Bits (X) = To_Bits (Y));
   --  Nonnegative finite numbers are ordered as their patterns are.

private
   pragma SPARK_Mode (Off);

   function Convert is new Ada.Unchecked_Conversion (Bits, Dimensioned_Float);
   function Convert_Time is new Ada.Unchecked_Conversion (Bits, Time);

   function To_Real (B : Bits) return Dimensioned_Float
   is (Convert (B));

   function To_Time (B : Bits) return Time
   is (Convert_Time (B));

   procedure Lemma_Zero_Pattern is null;
   procedure Lemma_Positive_Pattern (X : Dimensioned_Float) is null;
   procedure Lemma_Round_Trip (X : Dimensioned_Float) is null;
   procedure Lemma_Order (X, Y : Dimensioned_Float) is null;

end Prunt.Motion_Planner.Trajectory.Machine_Numbers;
