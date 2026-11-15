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

--  Rounding bounds which are relative only, for values which are not negative and not tiny.
--
--  The bounds of the parent package carry an absolute part, because SPARKlib allows every rounded operation an
--  error of Eta whatever its result. That part is what a solver's accuracy cannot be stated with: it does not
--  shrink with the quantities. Where the exact result of an operation is at least Normal_Min, Eta is a negligible
--  fraction of it, and the bound of the operation is a relative one. The solvers work with such values: quotients,
--  products and roots of limits within the proved domain.
--
--  Near (X, F, U) says that the floating-point value F, which is not negative, is within U * X of the exact value
--  X, which is not negative. There is one lemma for each kind of operation. Each rounded operation adds Unit to
--  the relative part.
--
--  A difference has no relative bound of its own: Within (X, F, M, U) bounds the error by U times a reference
--  magnitude M, which the terms do not exceed, and a difference whose value is known to be a fraction of M gets
--  its relative bound back from Lemma_Near_From_Within.
--
--  The roots have no lemma in SPARKlib. Their bounds come from the two statements about the library which
--  Trajectory.Elementary takes on trust, with a slightly smaller and a slightly larger machine number as
--  the brackets which those statements speak of.

with Prunt.Motion_Planner.Trajectory.Elementary;      use Prunt.Motion_Planner.Trajectory.Elementary;
with Prunt.Motion_Planner.Trajectory.Machine_Numbers; use Prunt.Motion_Planner.Trajectory.Machine_Numbers;

package Prunt.Motion_Planner.Trajectory.Rounding.Relative
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   Unit : constant := 2.0**(-52);
   --  What one rounded operation adds to a relative bound: twice Epsilon.

   Relative_Max : constant := 2.0**(-28);
   --  The largest relative part which the lemmas take.

   Normal_Min : constant := 2.0**(-960);
   --  An exact result at least this large is so far above Eta that the rounding of the operation is relative.

   Root_Slack : constant := 2.0**(-46);
   --  What a square root adds to a relative bound.

   Cube_Root_Step : constant := 2.0**(-36);
   --  What a cube root adds to a relative bound: the 2**-40 which is taken on trust about the library's cube
   --  root, and the distance of the brackets which the proof gives it.

   Operand_Max : constant Dimensioned_Float := 2#1.0#E511;
   --  The largest operand of a product or quotient which the lemmas take, and the largest radicand.

   Divisor_Min : constant Dimensioned_Float := 2#1.0#E-511;
   --  The smallest divisor.

   function Near (X : Real; F : Dimensioned_Float; U : Real) return Boolean
   is (X >= 0.0 and then F >= 0.0 and then abs (R (F) - X) <= U * X);
   --  F is within the fraction U of the exact value X, neither being negative.

   function Within (X : Real; F : Dimensioned_Float; M, U : Real) return Boolean
   is (abs (R (F) - X) <= U * M and then abs X <= M);
   --  F is within the fraction U of the reference magnitude M from the exact value X, which M bounds.

   ------------------------------------------------------------------------------------------------------------------
   --  Relative bounds through the operations
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Near_Exact (F : Dimensioned_Float)
   with Pre => F >= 0.0, Post => Near (R (F), F, 0.0);
   --  A floating-point value is the real which it denotes.

   procedure Lemma_Near_Weaken (X : Real; F : Dimensioned_Float; U, V : Real)
   with Pre => Near (X, F, U) and then U <= V, Post => Near (X, F, V);

   procedure Lemma_Near_Same (X : Real; F, G : Dimensioned_Float; U : Real)
   with Pre => Near (X, F, U) and then F = G, Post => Near (X, G, U);
   --  Equal floating-point values are near the same reals.

   procedure Lemma_Near_Product (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real)
   with
     Pre  =>
       Near (X1, F1, U1)
       and then Near (X2, F2, U2)
       and then F1 <= Operand_Max
       and then F2 <= Operand_Max
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then X1 * X2 >= Normal_Min
       and then U >= U1 + U2 + Unit,
     Post => Near (X1 * X2, F1 * F2, U);

   procedure Lemma_Near_Quotient (X1, X2, Q : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real)
   with
     Pre  =>
       Near (X1, F1, U1)
       and then Near (X2, F2, U2)
       and then F1 <= Operand_Max
       and then F2 >= Divisor_Min
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then X2 > 0.0
       and then Q * X2 = X1
       and then Q >= Normal_Min
       and then U >= U1 + U2 + Unit,
     Post => Near (Q, F1 / F2, U);
   --  Q is the exact quotient of X1 by X2, stated as a product so that the provers have no division to consider.

   procedure Lemma_Near_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real)
   with
     Pre  =>
       Near (X1, F1, U1)
       and then Near (X2, F2, U2)
       and then F1 <= Operand_Max
       and then F2 <= Operand_Max
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then X1 + X2 >= Normal_Min
       and then U >= U1 + Unit
       and then U >= U2 + Unit,
     Post => Near (X1 + X2, F1 + F2, U);

   procedure Lemma_Near_Square_Root (X, S : Real; F : Dimensioned_Float; U, US : Real)
   with
     Pre  =>
       Near (X, F, U)
       and then F <= Operand_Max
       and then U >= 0.0
       and then U <= Relative_Max
       and then X >= Normal_Min
       and then S >= 0.0
       and then S * S = X
       and then US >= U + Root_Slack,
     Post => Near (S, Dimensionless_Math.Sqrt (F), US);
   --  S is the exact square root of X.

   procedure Lemma_Near_Cube_Root (X, S : Real; F : Dimensioned_Float; U, US : Real)
   with
     Pre  =>
       Near (X, F, U)
       and then F <= Operand_Max
       and then U >= 0.0
       and then U <= Relative_Max
       and then X >= Normal_Min
       and then S >= 0.0
       and then S * S * S = X
       and then US >= U + Cube_Root_Step,
     Post => Near (S, Cube_Root (F), US);
   --  S is the exact cube root of X.

   Cubed_Step : constant := 2.0**(-35);
   --  What a cube root adds to the relative bound of its cube.

   procedure Lemma_Root_Squared (X : Real; F : Dimensioned_Float; U, U2 : Real)
   with
     Pre  =>
       Near (X, F, U)
       and then F <= Operand_Max
       and then U >= 0.0
       and then U <= Relative_Max
       and then X >= Normal_Min
       and then U2 >= U + Root_Slack,
     Post =>
       Dimensionless_Math.Sqrt (F) >= 0.0
       and then R (Dimensionless_Math.Sqrt (F)) >= 0.0
       and then abs (R (Dimensionless_Math.Sqrt (F)) * R (Dimensionless_Math.Sqrt (F)) - X) <= U2 * X;
   --  The square of the square root of a value near X is near X. This is how a root is spoken of where the
   --  exact root is not a rational number.

   procedure Lemma_Root_Cubed (X : Real; F : Dimensioned_Float; U, U3 : Real)
   with
     Pre  =>
       Near (X, F, U)
       and then F <= Operand_Max
       and then U >= 0.0
       and then U <= Relative_Max
       and then X >= Normal_Min
       and then U3 >= U + Cubed_Step,
     Post =>
       Cube_Root (F) >= 0.0
       and then R (Cube_Root (F)) >= 0.0
       and then abs (R (Cube_Root (F)) * R (Cube_Root (F)) * R (Cube_Root (F)) - X) <= U3 * X;
   --  The cube of the cube root of a value near X is near X.

   ------------------------------------------------------------------------------------------------------------------
   --  Terms which may be tiny
   --
   --  A phase time may be zero or tiny, and twice or four times it is then no product which Lemma_Near_Product
   --  takes. Almost (X, F, U) allows such a term an absolute error too, far below anything which the sum it
   --  goes into could notice.
   ------------------------------------------------------------------------------------------------------------------

   Residue : constant := 2.0**(-1500);
   --  The absolute part of the bound of a term which may be tiny.

   Double_Slack    : constant := 8.0 * Unit;
   Quadruple_Slack : constant := 16.0 * Unit;
   --  What the parent package proves of twice and four times a value.

   Scaled_Max : constant Dimensioned_Float := 2#1.0#E500;
   --  The largest value which is doubled or quadrupled.

   function Almost (X : Real; F : Dimensioned_Float; U : Real) return Boolean
   is (X >= 0.0 and then F >= 0.0 and then abs (R (F) - X) <= U * X + Residue);

   procedure Lemma_Almost_From_Near (X : Real; F : Dimensioned_Float; U : Real)
   with Pre => Near (X, F, U), Post => Almost (X, F, U);

   procedure Lemma_Almost_Double (F : Dimensioned_Float)
   with
     Pre  => F >= 0.0 and then F <= Scaled_Max,
     Post => 2.0 * F >= 0.0 and then 2.0 * F <= Operand_Max and then Almost (2.0 * R (F), 2.0 * F, Double_Slack);

   procedure Lemma_Almost_Quadruple (F : Dimensioned_Float)
   with
     Pre  => F >= 0.0 and then F <= Scaled_Max,
     Post => 4.0 * F >= 0.0 and then 4.0 * F <= Operand_Max and then Almost (4.0 * R (F), 4.0 * F, Quadruple_Slack);

   procedure Lemma_Near_Sum_Almost (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real)
   with
     Pre  =>
       Almost (X1, F1, U1)
       and then Almost (X2, F2, U2)
       and then F1 <= Operand_Max
       and then F2 <= Operand_Max
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then X1 + X2 >= Normal_Min
       and then U >= U1 + Unit
       and then U >= U2 + Unit,
     Post => Near (X1 + X2, F1 + F2, U);
   --  A sum which is not tiny, of terms which may be.

   ------------------------------------------------------------------------------------------------------------------
   --  Bounds relative to a reference magnitude, for differences
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Within_From_Near (X : Real; F : Dimensioned_Float; M, U : Real)
   with Pre => Near (X, F, U) and then U >= 0.0 and then X <= M, Post => Within (X, F, M, U);

   procedure Lemma_Within_Difference (X1, X2 : Real; F1, F2 : Dimensioned_Float; M, U1, U2, U : Real)
   with
     Pre  =>
       Within (X1, F1, M, U1)
       and then Within (X2, F2, M, U2)
       and then X1 >= 0.0
       and then X2 >= 0.0
       and then abs F1 <= Operand_Max
       and then abs F2 <= Operand_Max
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then M >= Normal_Min
       and then U >= U1 + U2 + 2.0 * Unit,
     Post => Within (X1 - X2, F1 - F2, M, U);

   procedure Lemma_Within_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; M, U1, U2, U : Real)
   with
     Pre  =>
       Within (X1, F1, M, U1)
       and then Within (X2, F2, M, U2)
       and then abs (X1 + X2) <= M
       and then abs F1 <= Operand_Max
       and then abs F2 <= Operand_Max
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then M >= Normal_Min
       and then U >= U1 + U2 + 2.0 * Unit,
     Post => Within (X1 + X2, F1 + F2, M, U);
   --  A sum of values of either sign which the reference magnitude still bounds.

   procedure Lemma_Near_From_Within (X : Real; F : Dimensioned_Float; M, U, K, UK : Real)
   with
     Pre  =>
       Within (X, F, M, U)
       and then F >= 0.0
       and then X >= 0.0
       and then U >= 0.0
       and then K >= 0.0
       and then M <= K * X
       and then UK >= K * U,
     Post => Near (X, F, UK);
   --  A value which is at least the fraction 1 / K of its reference magnitude is relatively bounded.

   ------------------------------------------------------------------------------------------------------------------
   --  What a relative bound says
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Near_Order (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2 : Real)
   with Pre => Near (X1, F1, U1) and then Near (X2, F2, U2) and then F1 <= F2, Post => X1 - U1 * X1 <= X2 + U2 * X2;
   --  The exact values of two ordered floating-point values are ordered, to within their bounds.

   procedure Lemma_Near_Bounded (X : Real; F, Limit : Dimensioned_Float; U : Real)
   with Pre => Near (X, F, U) and then U <= 1.0 and then 2.0 * X <= R (Limit), Post => F <= Limit;
   --  A value near an exact one which is at most half of a limit is within the limit.

   procedure Lemma_Near_Bounds (X : Real; F : Dimensioned_Float; U : Real)
   with Pre => Near (X, F, U), Post => R (F) <= X + U * X and then R (F) >= X - U * X and then R (F) >= 0.0;
   --  A relative bound as two inequalities.

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic which the users of these lemmas need as well
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Scale_Monotone (A, B, C : Real)
   with Pre => A <= B and then C >= 0.0, Post => A * C <= B * C;

   procedure Lemma_Product_Monotone (A, B, C, D : Real)
   with Pre => A >= 0.0 and then A <= B and then C >= 0.0 and then C <= D, Post => A * C <= B * D;

   procedure Lemma_Product_Cancel (A, B, C : Real)
   with Pre => A * C <= B * C and then C > 0.0, Post => A <= B;
   --  A common positive factor of both sides of an inequality may be dropped.

   procedure Lemma_Squares_Ordered (A, B : Real)
   with Pre => A >= 0.0 and then B >= 0.0 and then A * A <= B * B, Post => A <= B;

   procedure Lemma_Cubes_Ordered (A, B : Real)
   with Pre => A >= 0.0 and then B >= 0.0 and then A * A * A <= B * B * B, Post => A <= B;

   ------------------------------------------------------------------------------------------------------------------
   --  Relative bounds between reals
   --
   --  About (X, Y, U) says that X is within the fraction U of Y, and At_Most (X, Y, U) that X is at most Y and
   --  the fraction U of it. Y is not negative. The lemmas take such bounds through products, sums and chains, so
   --  that an argument about exact values which are each known to a relative bound is a sequence of calls. Every
   --  fraction is at most Relative_Max, and a result's fraction is a named number which the caller chooses: it
   --  has to exceed the sum of the operands' fractions by Second_Order, which is more than their product.
   ------------------------------------------------------------------------------------------------------------------

   Second_Order : constant := 2.0**(-54);
   --  More than the products of the fractions which a step combines.

   function About (X, Y, U : Real) return Boolean
   is (Y >= 0.0 and then abs (X - Y) <= U * Y);

   function At_Most (X, Y, U : Real) return Boolean
   is (Y >= 0.0 and then X <= Y + U * Y);

   procedure Lemma_About_From_Near (X : Real; F : Dimensioned_Float; U : Real)
   with Pre => Near (X, F, U), Post => About (R (F), X, U) and then R (F) >= 0.0;

   procedure Lemma_About_Same (X : Real)
   with Pre => X >= 0.0, Post => About (X, X, 0.0);

   procedure Lemma_About_Weaken (X, Y, U, V : Real)
   with Pre => About (X, Y, U) and then U <= V, Post => About (X, Y, V);

   procedure Lemma_About_Bounds (X, Y, U : Real)
   with
     Pre  => About (X, Y, U) and then U >= 0.0 and then U <= Relative_Max,
     Post => X <= Y + U * Y and then X >= Y - U * Y and then X >= 0.0 and then At_Most (X, Y, U);

   procedure Lemma_About_Symmetric (X, Y, U, V : Real)
   with
     Pre  => About (X, Y, U) and then U >= 0.0 and then U <= Relative_Max and then V >= U + Second_Order,
     Post => About (Y, X, V);

   procedure Lemma_About_Chain (X, Y, Z, U, V, W : Real)
   with
     Pre  =>
       About (X, Y, U)
       and then About (Y, Z, V)
       and then U >= 0.0
       and then U <= Relative_Max
       and then V >= 0.0
       and then V <= Relative_Max
       and then W >= U + V + Second_Order,
     Post => About (X, Z, W);

   procedure Lemma_About_Product (X1, Y1, X2, Y2, U1, U2, U : Real)
   with
     Pre  =>
       About (X1, Y1, U1)
       and then About (X2, Y2, U2)
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then U >= U1 + U2 + Second_Order,
     Post => About (X1 * X2, Y1 * Y2, U);

   procedure Lemma_About_Sum (X1, Y1, X2, Y2, U1, U2, U : Real)
   with
     Pre  => About (X1, Y1, U1) and then About (X2, Y2, U2) and then U1 <= U and then U2 <= U,
     Post => About (X1 + X2, Y1 + Y2, U);

   procedure Lemma_About_Scaled (X, Y, K, U : Real)
   with Pre => About (X, Y, U) and then K >= 0.0, Post => About (K * X, K * Y, U);

   procedure Lemma_About_Cancel (X, Y, K, U : Real)
   with Pre => K > 0.0 and then Y >= 0.0 and then About (K * X, K * Y, U), Post => About (X, Y, U);
   --  A common positive factor of both values may be dropped.

   procedure Lemma_About_Quotient (X1, Y1, X2, Y2, QX, QY, U1, U2, U : Real)
   with
     Pre  =>
       About (X1, Y1, U1)
       and then About (X2, Y2, U2)
       and then Y2 > 0.0
       and then QX * X2 = X1
       and then QY * Y2 = Y1
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then U >= U1 + U2 + Second_Order,
     Post => About (QX, QY, U);
   --  QX and QY are the quotients of X1 by X2 and of Y1 by Y2, stated as products.

   procedure Lemma_At_Most_Chain (X, Y, Z, U, V, W : Real)
   with
     Pre  =>
       At_Most (X, Y, U)
       and then At_Most (Y, Z, V)
       and then U >= 0.0
       and then U <= Relative_Max
       and then V >= 0.0
       and then V <= Relative_Max
       and then W >= U + V + Second_Order,
     Post => At_Most (X, Z, W);

   procedure Lemma_At_Most_Product (X1, Y1, X2, Y2, U1, U2, U : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 >= 0.0
       and then At_Most (X1, Y1, U1)
       and then At_Most (X2, Y2, U2)
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then U >= U1 + U2 + Second_Order,
     Post => At_Most (X1 * X2, Y1 * Y2, U);

   procedure Lemma_At_Most_Sum (X1, Y1, X2, Y2, U1, U2, U : Real)
   with
     Pre  => At_Most (X1, Y1, U1) and then At_Most (X2, Y2, U2) and then U1 <= U and then U2 <= U,
     Post => At_Most (X1 + X2, Y1 + Y2, U);

   procedure Lemma_At_Most_Cancel (X, Y, K, U : Real)
   with Pre => K > 0.0 and then Y >= 0.0 and then At_Most (K * X, K * Y, U), Post => At_Most (X, Y, U);

   procedure Lemma_At_Most_Scaled (X, Y, K, U : Real)
   with Pre => At_Most (X, Y, U) and then K >= 0.0, Post => At_Most (K * X, K * Y, U);

   procedure Lemma_At_Most_From_About (X, Y, U : Real)
   with Pre => About (X, Y, U), Post => At_Most (X, Y, U);

   procedure Lemma_At_Most_Weaken (X, Y, U, V : Real)
   with Pre => At_Most (X, Y, U) and then U <= V, Post => At_Most (X, Y, V);

   procedure Lemma_At_Most_Root (X, Y, U : Real)
   with
     Pre  => X >= 0.0 and then Y >= 0.0 and then U >= 0.0 and then At_Most (X * X, Y * Y, U),
     Post => At_Most (X, Y, U);
   --  Square roots keep an order to within the same fraction, and in fact half of it.

   procedure Lemma_At_Most_Cube_Root (X, Y, U : Real)
   with
     Pre  => X >= 0.0 and then Y >= 0.0 and then U >= 0.0 and then At_Most (X * X * X, Y * Y * Y, U),
     Post => At_Most (X, Y, U);

   procedure Lemma_At_Most_Fourth_Root (X, Y, U : Real)
   with
     Pre  => X >= 0.0 and then Y >= 0.0 and then U >= 0.0 and then At_Most ((X * X) * (X * X), (Y * Y) * (Y * Y), U),
     Post => At_Most (X, Y, U);

   ------------------------------------------------------------------------------------------------------------------
   --  The next machine number
   ------------------------------------------------------------------------------------------------------------------

   Next_Slack : constant := 2.0**(-51);
   --  How far, relatively, the next machine number is above a value which is not tiny.

   Next_Residue : constant := 2.0**(-1000);
   --  The next machine number above a smaller value is no larger than this.

   procedure Lemma_Next_Is_Close (X : Dimensioned_Float)
   with
     Pre  => X >= 0.0 and then X <= Operand_Max and then To_Bits (X) < Last_Pattern,
     Post =>
       To_Real (To_Bits (X) + 1) > X
       and then R (To_Real (To_Bits (X) + 1)) <= R (X) + Next_Slack * R (X) + Next_Residue;
   --  Machine numbers are dense: the one after X is within one part in 2**51 of it, or tiny. This follows from
   --  the order of bit patterns alone, because a slightly larger product of X is a machine number above it.

end Prunt.Motion_Planner.Trajectory.Rounding.Relative;
