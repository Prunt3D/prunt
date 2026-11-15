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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);
pragma Assertion_Policy (Ignore);

with Prunt.Motion_Planner.Trajectory.Rounding;

package body Prunt.Motion_Planner.Trajectory.Profiles
  with SPARK_Mode => On
is

   pragma Extensions_Allowed (On);

   ------------------------------------------------------------------------------------------------------------------
   --  Proof support
   --
   --  Everything from here to the first function body is ghost.
   --
   --  A check on a floating-point operation is proved in a fraction of a second when its operands are two unknowns
   --  with a bound each, and not at all when they are subexpressions of a long formula: the provers then spend their
   --  time on the formula.  So each kind of operation is proved once, in the generic packages below, for operands
   --  bounded by the generic formals, and each statement of the evaluators is preceded by its mirror: the same
   --  expression written with these functions (Mul_100_169.F (Cm, DT) for Cm * DT).  Evaluating the mirror checks
   --  the operand bounds, which are the guaranteed bounds of the inner calls, and leaves for each operation of the
   --  statement the fact that its result is finite and bounded.  The checks of the statement itself then follow by
   --  matching, without arithmetic.  For the matching to need no unfolding either, every bound is stated with the
   --  same few predicates (Within, Within_Nonneg, Symmetric, Near), in the contracts as in the operations.
   --
   --  A mirror follows the expression tree of its statement operation by operation, and each bound is the bound of
   --  the operation on the bounds of its operands, rounded up to four significant bits.  The bounds are named by
   --  their value: P165 is 2**165 and P165x3 is 3 * 2**165, whatever the dimension.  An instance is named by the
   --  bounds of its operands: Mul_100_169 multiplies a value within P100 by one within P169.
   ------------------------------------------------------------------------------------------------------------------

   subtype Raw is Dimensioned_Float;
   --  A value of any dimension, as the bounded operations see it.

   function Within (X, Bound : Raw) return Boolean
   is (abs X <= Bound)
   with Ghost;

   function Within_Nonneg (X, Bound : Raw) return Boolean
   is (X >= 0.0 and then X <= Bound)
   with Ghost;

   function Symmetric (X, Bound : Raw) return Boolean
   is (X >= -Bound and then X <= Bound)
   with Ghost;
   --  Within, as the two inequalities in which the relative bounds below are stated.

   P100    : constant Raw := 2#1.0#E100
   with Ghost;
   P165    : constant Raw := 2#1.0#E165
   with Ghost;
   P166    : constant Raw := 2#1.0#E166
   with Ghost;
   P165x3  : constant Raw := 2#11.0#E165
   with Ghost;
   P167    : constant Raw := 2#1.0#E167
   with Ghost;
   P165x5  : constant Raw := 2#101.0#E165
   with Ghost;
   P166x3  : constant Raw := 2#11.0#E166
   with Ghost;
   P165x7  : constant Raw := 2#111.0#E165
   with Ghost;
   P168    : constant Raw := 2#1.0#E168
   with Ghost;
   P165x9  : constant Raw := 2#1001.0#E165
   with Ghost;
   P166x5  : constant Raw := 2#101.0#E166
   with Ghost;
   P165x11 : constant Raw := 2#1011.0#E165
   with Ghost;
   P167x3  : constant Raw := 2#11.0#E167
   with Ghost;
   P165x13 : constant Raw := 2#1101.0#E165
   with Ghost;
   P166x7  : constant Raw := 2#111.0#E166
   with Ghost;
   P165x15 : constant Raw := 2#1111.0#E165
   with Ghost;
   P169    : constant Raw := 2#1.0#E169
   with Ghost;
   P166x9  : constant Raw := 2#1001.0#E166
   with Ghost;
   P167x5  : constant Raw := 2#101.0#E167
   with Ghost;
   P168x3  : constant Raw := 2#11.0#E168
   with Ghost;
   P167x7  : constant Raw := 2#111.0#E167
   with Ghost;
   P166x15 : constant Raw := 2#1111.0#E166
   with Ghost;
   P167x9  : constant Raw := 2#1001.0#E167
   with Ghost;
   P169x3  : constant Raw := 2#11.0#E169
   with Ghost;
   P167x15 : constant Raw := 2#1111.0#E167
   with Ghost;
   P170x3  : constant Raw := 2#11.0#E170
   with Ghost;
   P168x15 : constant Raw := 2#1111.0#E168
   with Ghost;
   P171x3  : constant Raw := 2#11.0#E171
   with Ghost;
   P171x5  : constant Raw := 2#101.0#E171
   with Ghost;
   P257x11 : constant Raw := 2#1011.0#E257
   with Ghost;
   P259x9  : constant Raw := 2#1001.0#E259
   with Ghost;
   P259x11 : constant Raw := 2#1011.0#E259
   with Ghost;
   P261x5  : constant Raw := 2#101.0#E261
   with Ghost;
   P264    : constant Raw := 2#1.0#E264
   with Ghost;
   P261x9  : constant Raw := 2#1001.0#E261
   with Ghost;
   P265    : constant Raw := 2#1.0#E265
   with Ghost;
   P264x3  : constant Raw := 2#11.0#E264
   with Ghost;
   P263x7  : constant Raw := 2#111.0#E263
   with Ghost;
   P266    : constant Raw := 2#1.0#E266
   with Ghost;
   P265x3  : constant Raw := 2#11.0#E265
   with Ghost;
   P264x7  : constant Raw := 2#111.0#E264
   with Ghost;
   P267    : constant Raw := 2#1.0#E267
   with Ghost;
   P269    : constant Raw := 2#1.0#E269
   with Ghost;
   P270    : constant Raw := 2#1.0#E270
   with Ghost;
   P269x3  : constant Raw := 2#11.0#E269
   with Ghost;
   P271    : constant Raw := 2#1.0#E271
   with Ghost;
   P269x5  : constant Raw := 2#101.0#E269
   with Ghost;
   P270x3  : constant Raw := 2#11.0#E270
   with Ghost;
   P269x7  : constant Raw := 2#111.0#E269
   with Ghost;
   P272    : constant Raw := 2#1.0#E272
   with Ghost;
   P330    : constant Raw := 2#1.0#E330
   with Ghost;
   P331    : constant Raw := 2#1.0#E331
   with Ghost;
   P330x3  : constant Raw := 2#11.0#E330
   with Ghost;
   P332    : constant Raw := 2#1.0#E332
   with Ghost;
   P330x5  : constant Raw := 2#101.0#E330
   with Ghost;
   P331x3  : constant Raw := 2#11.0#E331
   with Ghost;
   P330x7  : constant Raw := 2#111.0#E330
   with Ghost;
   P333    : constant Raw := 2#1.0#E333
   with Ghost;
   P330x9  : constant Raw := 2#1001.0#E330
   with Ghost;
   P331x5  : constant Raw := 2#101.0#E331
   with Ghost;
   P330x11 : constant Raw := 2#1011.0#E330
   with Ghost;
   P332x3  : constant Raw := 2#11.0#E332
   with Ghost;
   P331x7  : constant Raw := 2#111.0#E331
   with Ghost;
   P330x15 : constant Raw := 2#1111.0#E330
   with Ghost;
   P334    : constant Raw := 2#1.0#E334
   with Ghost;
   P331x9  : constant Raw := 2#1001.0#E331
   with Ghost;
   P331x11 : constant Raw := 2#1011.0#E331
   with Ghost;
   P333x3  : constant Raw := 2#11.0#E333
   with Ghost;
   P331x13 : constant Raw := 2#1101.0#E331
   with Ghost;
   P332x7  : constant Raw := 2#111.0#E332
   with Ghost;
   P331x15 : constant Raw := 2#1111.0#E331
   with Ghost;
   P335    : constant Raw := 2#1.0#E335
   with Ghost;
   P332x9  : constant Raw := 2#1001.0#E332
   with Ghost;
   P333x5  : constant Raw := 2#101.0#E333
   with Ghost;
   P332x11 : constant Raw := 2#1011.0#E332
   with Ghost;
   P334x3  : constant Raw := 2#11.0#E334
   with Ghost;
   P332x13 : constant Raw := 2#1101.0#E332
   with Ghost;
   P333x7  : constant Raw := 2#111.0#E333
   with Ghost;
   P332x15 : constant Raw := 2#1111.0#E332
   with Ghost;
   P333x9  : constant Raw := 2#1001.0#E333
   with Ghost;
   P334x5  : constant Raw := 2#101.0#E334
   with Ghost;
   P333x11 : constant Raw := 2#1011.0#E333
   with Ghost;
   P335x3  : constant Raw := 2#11.0#E335
   with Ghost;
   P333x15 : constant Raw := 2#1111.0#E333
   with Ghost;
   P334x9  : constant Raw := 2#1001.0#E334
   with Ghost;
   P336x3  : constant Raw := 2#11.0#E336
   with Ghost;
   P338    : constant Raw := 2#1.0#E338
   with Ghost;
   P335x9  : constant Raw := 2#1001.0#E335
   with Ghost;
   P336x5  : constant Raw := 2#101.0#E336
   with Ghost;
   P335x11 : constant Raw := 2#1011.0#E335
   with Ghost;
   P337x3  : constant Raw := 2#11.0#E337
   with Ghost;
   P335x13 : constant Raw := 2#1101.0#E335
   with Ghost;
   P336x7  : constant Raw := 2#111.0#E336
   with Ghost;
   P339    : constant Raw := 2#1.0#E339
   with Ghost;
   P336x9  : constant Raw := 2#1001.0#E336
   with Ghost;
   P337x5  : constant Raw := 2#101.0#E337
   with Ghost;
   P336x11 : constant Raw := 2#1011.0#E336
   with Ghost;
   P338x3  : constant Raw := 2#11.0#E338
   with Ghost;
   P336x13 : constant Raw := 2#1101.0#E336
   with Ghost;
   P337x7  : constant Raw := 2#111.0#E337
   with Ghost;
   P336x15 : constant Raw := 2#1111.0#E336
   with Ghost;
   P340    : constant Raw := 2#1.0#E340
   with Ghost;
   P426x9  : constant Raw := 2#1001.0#E426
   with Ghost;
   P428x7  : constant Raw := 2#111.0#E428
   with Ghost;
   P431    : constant Raw := 2#1.0#E431
   with Ghost;
   P428x9  : constant Raw := 2#1001.0#E428
   with Ghost;
   P429x7  : constant Raw := 2#111.0#E429
   with Ghost;
   P429x9  : constant Raw := 2#1001.0#E429
   with Ghost;
   P430x5  : constant Raw := 2#101.0#E430
   with Ghost;
   P429x11 : constant Raw := 2#1011.0#E429
   with Ghost;
   P429x15 : constant Raw := 2#1111.0#E429
   with Ghost;
   P430x9  : constant Raw := 2#1001.0#E430
   with Ghost;
   P430x11 : constant Raw := 2#1011.0#E430
   with Ghost;
   P430x13 : constant Raw := 2#1101.0#E430
   with Ghost;
   P434    : constant Raw := 2#1.0#E434
   with Ghost;
   P437    : constant Raw := 2#1.0#E437
   with Ghost;
   P434x9  : constant Raw := 2#1001.0#E434
   with Ghost;
   P438    : constant Raw := 2#1.0#E438
   with Ghost;
   P435x9  : constant Raw := 2#1001.0#E435
   with Ghost;
   P435x13 : constant Raw := 2#1101.0#E435
   with Ghost;
   P436x7  : constant Raw := 2#111.0#E436
   with Ghost;
   P437x5  : constant Raw := 2#101.0#E437
   with Ghost;
   P438x3  : constant Raw := 2#11.0#E438
   with Ghost;
   P436x13 : constant Raw := 2#1101.0#E436
   with Ghost;
   P440    : constant Raw := 2#1.0#E440
   with Ghost;
   P437x9  : constant Raw := 2#1001.0#E437
   with Ghost;
   P438x5  : constant Raw := 2#101.0#E438
   with Ghost;
   P439x3  : constant Raw := 2#11.0#E439
   with Ghost;
   P441    : constant Raw := 2#1.0#E441
   with Ghost;
   P495    : constant Raw := 2#1.0#E495
   with Ghost;
   P497    : constant Raw := 2#1.0#E497
   with Ghost;
   P497x3  : constant Raw := 2#11.0#E497
   with Ghost;
   P498x3  : constant Raw := 2#11.0#E498
   with Ghost;
   P497x7  : constant Raw := 2#111.0#E497
   with Ghost;
   P496x15 : constant Raw := 2#1111.0#E496
   with Ghost;
   P499x3  : constant Raw := 2#11.0#E499
   with Ghost;
   P497x13 : constant Raw := 2#1101.0#E497
   with Ghost;
   P497x15 : constant Raw := 2#1111.0#E497
   with Ghost;
   P498x9  : constant Raw := 2#1001.0#E498
   with Ghost;
   P499x5  : constant Raw := 2#101.0#E499
   with Ghost;
   P500x3  : constant Raw := 2#11.0#E500
   with Ghost;
   P498x15 : constant Raw := 2#1111.0#E498
   with Ghost;
   P499x11 : constant Raw := 2#1011.0#E499
   with Ghost;
   P501x3  : constant Raw := 2#11.0#E501
   with Ghost;
   P499x13 : constant Raw := 2#1101.0#E499
   with Ghost;
   P500x9  : constant Raw := 2#1001.0#E500
   with Ghost;
   P501x5  : constant Raw := 2#101.0#E501
   with Ghost;
   P502x3  : constant Raw := 2#11.0#E502
   with Ghost;
   P500x13 : constant Raw := 2#1101.0#E500
   with Ghost;
   P502x5  : constant Raw := 2#101.0#E502
   with Ghost;
   P505    : constant Raw := 2#1.0#E505
   with Ghost;
   P502x9  : constant Raw := 2#1001.0#E502
   with Ghost;
   P504x3  : constant Raw := 2#11.0#E504
   with Ghost;
   P503x7  : constant Raw := 2#111.0#E503
   with Ghost;
   P503x9  : constant Raw := 2#1001.0#E503
   with Ghost;
   P503x11 : constant Raw := 2#1011.0#E503
   with Ghost;
   P505x3  : constant Raw := 2#11.0#E505
   with Ghost;
   P503x13 : constant Raw := 2#1101.0#E503
   with Ghost;
   P504x7  : constant Raw := 2#111.0#E504
   with Ghost;
   P503x15 : constant Raw := 2#1111.0#E503
   with Ghost;
   P507    : constant Raw := 2#1.0#E507
   with Ghost;
   P504x9  : constant Raw := 2#1001.0#E504
   with Ghost;
   P505x5  : constant Raw := 2#101.0#E505
   with Ghost;
   P504x11 : constant Raw := 2#1011.0#E504
   with Ghost;
   P506x3  : constant Raw := 2#11.0#E506
   with Ghost;
   P504x13 : constant Raw := 2#1101.0#E504
   with Ghost;
   P505x7  : constant Raw := 2#111.0#E505
   with Ghost;
   P504x15 : constant Raw := 2#1111.0#E504
   with Ghost;
   P508    : constant Raw := 2#1.0#E508
   with Ghost;
   P595x3  : constant Raw := 2#11.0#E595
   with Ghost;
   P595x5  : constant Raw := 2#101.0#E595
   with Ghost;
   P594x11 : constant Raw := 2#1011.0#E594
   with Ghost;
   P596x3  : constant Raw := 2#11.0#E596
   with Ghost;
   P598    : constant Raw := 2#1.0#E598
   with Ghost;
   P596x5  : constant Raw := 2#101.0#E596
   with Ghost;
   P595x11 : constant Raw := 2#1011.0#E595
   with Ghost;
   P597x5  : constant Raw := 2#101.0#E597
   with Ghost;
   P596x11 : constant Raw := 2#1011.0#E596
   with Ghost;
   P600    : constant Raw := 2#1.0#E600
   with Ghost;
   P598x5  : constant Raw := 2#101.0#E598
   with Ghost;
   P597x11 : constant Raw := 2#1011.0#E597
   with Ghost;
   P598x15 : constant Raw := 2#1111.0#E598
   with Ghost;
   P599x9  : constant Raw := 2#1001.0#E599
   with Ghost;
   P600x5  : constant Raw := 2#101.0#E600
   with Ghost;
   P600x9  : constant Raw := 2#1001.0#E600
   with Ghost;
   P601x5  : constant Raw := 2#101.0#E601
   with Ghost;
   P601x11 : constant Raw := 2#1011.0#E601
   with Ghost;
   P603x3  : constant Raw := 2#11.0#E603
   with Ghost;
   P602x7  : constant Raw := 2#111.0#E602
   with Ghost;
   P601x15 : constant Raw := 2#1111.0#E601
   with Ghost;
   P602x15 : constant Raw := 2#1111.0#E602
   with Ghost;
   P606    : constant Raw := 2#1.0#E606
   with Ghost;
   P603x11 : constant Raw := 2#1011.0#E603
   with Ghost;
   P605x3  : constant Raw := 2#11.0#E605
   with Ghost;
   P607    : constant Raw := 2#1.0#E607
   with Ghost;
   P604x9  : constant Raw := 2#1001.0#E604
   with Ghost;
   P605x5  : constant Raw := 2#101.0#E605
   with Ghost;
   P604x11 : constant Raw := 2#1011.0#E604
   with Ghost;
   P604x13 : constant Raw := 2#1101.0#E604
   with Ghost;
   P605x7  : constant Raw := 2#111.0#E605
   with Ghost;
   P608    : constant Raw := 2#1.0#E608
   with Ghost;
   P605x9  : constant Raw := 2#1001.0#E605
   with Ghost;
   P606x5  : constant Raw := 2#101.0#E606
   with Ghost;
   P609    : constant Raw := 2#1.0#E609
   with Ghost;
   P676    : constant Raw := 2#1.0#E676
   with Ghost;
   P758x11 : constant Raw := 2#1011.0#E758
   with Ghost;
   P762x7  : constant Raw := 2#111.0#E762
   with Ghost;
   P765    : constant Raw := 2#1.0#E765
   with Ghost;
   P763x5  : constant Raw := 2#101.0#E763
   with Ghost;
   P763x7  : constant Raw := 2#111.0#E763
   with Ghost;
   P766    : constant Raw := 2#1.0#E766
   with Ghost;
   P764x5  : constant Raw := 2#101.0#E764
   with Ghost;
   P763x15 : constant Raw := 2#1111.0#E763
   with Ghost;
   P767    : constant Raw := 2#1.0#E767
   with Ghost;
   P765x5  : constant Raw := 2#101.0#E765
   with Ghost;
   P764x13 : constant Raw := 2#1101.0#E764
   with Ghost;
   P765x11 : constant Raw := 2#1011.0#E765
   with Ghost;
   P765x13 : constant Raw := 2#1101.0#E765
   with Ghost;
   P766x11 : constant Raw := 2#1011.0#E766
   with Ghost;
   P766x13 : constant Raw := 2#1101.0#E766
   with Ghost;
   P767x7  : constant Raw := 2#111.0#E767
   with Ghost;
   P768x5  : constant Raw := 2#101.0#E768
   with Ghost;
   P767x11 : constant Raw := 2#1011.0#E767
   with Ghost;
   P768x9  : constant Raw := 2#1001.0#E768
   with Ghost;
   P769x5  : constant Raw := 2#101.0#E769
   with Ghost;
   P768x11 : constant Raw := 2#1011.0#E768
   with Ghost;
   P770x3  : constant Raw := 2#11.0#E770
   with Ghost;
   P769x7  : constant Raw := 2#111.0#E769
   with Ghost;
   P772    : constant Raw := 2#1.0#E772
   with Ghost;
   P769x11 : constant Raw := 2#1011.0#E769
   with Ghost;
   P769x15 : constant Raw := 2#1111.0#E769
   with Ghost;
   P771x5  : constant Raw := 2#101.0#E771
   with Ghost;
   P771x7  : constant Raw := 2#111.0#E771
   with Ghost;
   P774    : constant Raw := 2#1.0#E774
   with Ghost;
   P771x9  : constant Raw := 2#1001.0#E771
   with Ghost;
   P771x13 : constant Raw := 2#1101.0#E771
   with Ghost;
   P771x15 : constant Raw := 2#1111.0#E771
   with Ghost;
   P773x5  : constant Raw := 2#101.0#E773
   with Ghost;
   P772x11 : constant Raw := 2#1011.0#E772
   with Ghost;
   P772x13 : constant Raw := 2#1101.0#E772
   with Ghost;
   P773x7  : constant Raw := 2#111.0#E773
   with Ghost;
   P776    : constant Raw := 2#1.0#E776
   with Ghost;
   P773x9  : constant Raw := 2#1001.0#E773
   with Ghost;
   P774x5  : constant Raw := 2#101.0#E774
   with Ghost;
   P773x11 : constant Raw := 2#1011.0#E773
   with Ghost;
   P775x3  : constant Raw := 2#11.0#E775
   with Ghost;
   P782    : constant Raw := 2#1.0#E782
   with Ghost;
   P800    : constant Raw := 2#1.0#E800
   with Ghost;
   P797x9  : constant Raw := 2#1001.0#E797
   with Ghost;
   P922x15 : constant Raw := 2#1111.0#E922
   with Ghost;
   P927x11 : constant Raw := 2#1011.0#E927
   with Ghost;
   P931    : constant Raw := 2#1.0#E931
   with Ghost;
   P931x11 : constant Raw := 2#1011.0#E931
   with Ghost;
   P933x3  : constant Raw := 2#11.0#E933
   with Ghost;
   P939    : constant Raw := 2#1.0#E939
   with Ghost;
   P938x5  : constant Raw := 2#101.0#E938
   with Ghost;
   P937x11 : constant Raw := 2#1011.0#E937
   with Ghost;
   P938x11 : constant Raw := 2#1011.0#E938
   with Ghost;
   P942    : constant Raw := 2#1.0#E942
   with Ghost;
   P943    : constant Raw := 2#1.0#E943
   with Ghost;
   P950    : constant Raw := 2#1.0#E950
   with Ghost;
   P969    : constant Raw := 2#1.0#E969
   with Ghost;
   P966x9  : constant Raw := 2#1001.0#E966
   with Ghost;
   P970    : constant Raw := 2#1.0#E970
   with Ghost;
   P1000   : constant Raw := 2#1.0#E1000
   with Ghost;

   function Near (R, X, B : Raw) return Boolean
   is (abs X <= P1000 and then abs B <= P1000 and then R <= X + B and then R >= -(X + B))
   with Ghost;
   --  R is within B of the interval -X .. X (X and B being small enough for their sum to exist).  A bound of this
   --  form survives the rounding of a sum only when it has some slack, so each relative step below doubles the
   --  bound of what it adds.

   procedure Witness (X : Raw) is null with Ghost;
   --  Evaluates a mirror.

   generic
      A, B, C : Raw;
   package Bounded_Product with Ghost is
      function F (X, Y : Raw) return Raw
      is (X * Y)
      with Pre => Within (X, A) and then Within (Y, B), Post => Within (F'Result, C);
   end Bounded_Product;

   generic
      K, A, C : Raw;
   package Bounded_Scale with Ghost is
      function F (X : Raw) return Raw
      is (K * X)
      with Pre => Within (X, A), Post => Within (F'Result, C);
   end Bounded_Scale;

   generic
      A, B, C : Raw;
   package Bounded_Sum with Ghost is
      function Add (X, Y : Raw) return Raw
      is (X + Y)
      with Pre => Within (X, A) and then Within (Y, B), Post => Within (Add'Result, C);

      function Sub (X, Y : Raw) return Raw
      is (X - Y)
      with Pre => Within (X, A) and then Within (Y, B), Post => Within (Sub'Result, C);
   end Bounded_Sum;

   generic
      A, C : Raw;
   package Bounded_Square with Ghost is
      function F (X : Raw) return Raw
      is (X**2)
      with Pre => Within (X, A), Post => Within (F'Result, C);
   end Bounded_Square;

   generic
      A, C : Raw;
   package Bounded_Cube with Ghost is
      function F (X : Raw) return Raw
      is (X**3)
      with Pre => Within (X, A), Post => Within (F'Result, C);
   end Bounded_Cube;

   generic
      A, C : Raw;
   package Bounded_Fourth_Power with Ghost is
      function F (X : Raw) return Raw
      is ((X**2)**2)
      with Pre => Within (X, A), Post => Within (F'Result, C);
      --  Written as the square of a square, as the code is: GNATprove gives no meaning to a fourth power of a
      --  floating-point value, because the language leaves the order of its multiplications open.
   end Bounded_Fourth_Power;

   generic
      A, K, C : Raw;
   package Bounded_Quotient with Ghost is
      function F (X : Raw) return Raw
      is (X / K)
      with Pre => Within (X, A), Post => Within (F'Result, C);
   end Bounded_Quotient;

   generic
      A : Raw;
   package Bounded_Negation with Ghost is
      function F (X : Raw) return Raw
      is (-X)
      with Pre => Within (X, A), Post => Within (F'Result, A);
   end Bounded_Negation;

   generic
      C, D : Raw;
   package Wider_Bound with Ghost is
      function F (X : Raw) return Raw
      is (X)
      with Pre => Within (X, C), Post => Within (F'Result, D);
   end Wider_Bound;

   generic
      C, D : Raw;
   package Final_Bound with Ghost is
      function F (X : Raw) return Raw
      is (X)
      with Pre => Within (X, C), Post => Within (F'Result, C) and then abs F'Result <= D;
      --  X, with the bound which the postcondition of an evaluator states.
   end Final_Bound;

   generic
      A, B, C : Raw;
   package Nonnegative_Product with Ghost is
      function F (X, Y : Raw) return Raw
      is (X * Y)
      with Pre => Within_Nonneg (X, A) and then Within_Nonneg (Y, B), Post => Within_Nonneg (F'Result, C);
   end Nonnegative_Product;

   generic
      K, A, C : Raw;
   package Nonnegative_Scale with Ghost is
      function F (X : Raw) return Raw
      is (K * X)
      with Pre => Within_Nonneg (X, A), Post => Within_Nonneg (F'Result, C);
   end Nonnegative_Scale;

   generic
      A, B, C : Raw;
   package Nonnegative_Sum with Ghost is
      function F (X, Y : Raw) return Raw
      is (X + Y)
      with Pre => Within_Nonneg (X, A) and then Within_Nonneg (Y, B), Post => Within_Nonneg (F'Result, C);
   end Nonnegative_Sum;

   generic
      C, L0, L1, L2, L3 : Raw;
   package Leveled_Bound with Ghost is
      function F (X : Raw) return Raw
      is (X)
      with
        Pre  => Within (X, C),
        Post =>
          Within (F'Result, L0)
          and then Within (F'Result, L1)
          and then Within (F'Result, L2)
          and then Within (F'Result, L3);
      --  X, within the bounds of each of four nested levels.
   end Leveled_Bound;

   generic
      L0, L1, L2, L3, U : Raw;
   package Leveled_Sum with Ghost is
      function F (Old, I : Raw) return Raw
      is (Old + I)
      with
        Pre  => Within (Old, L2) and then Within (I, U),
        Post =>
          (if Within (Old, L0) then Within (F'Result, L1))
          and then (if Within (Old, L1) then Within (F'Result, L2))
          and then Within (F'Result, L3);
      --  One more increment within U takes a value from each level to the next.
   end Leveled_Sum;

   generic
      A : Raw;
   package Symmetric_Bound with Ghost is
      function F (X : Raw) return Raw
      is (X)
      with Pre => Within (X, A), Post => Symmetric (F'Result, A);
   end Symmetric_Bound;

   generic
      Limit, U, BN, Final : Raw;
   package Relative_Start with Ghost is
      function F (V, I : Raw) return Raw
      is (V + I)
      with
        Pre  => Within (V, Limit) and then Symmetric (I, U),
        Post => Near (F'Result, abs V, BN) and then abs F'Result <= abs V + Final;
   end Relative_Start;

   generic
      Limit, BP, U, BN, Final : Raw;
   package Relative_Step with Ghost is
      function F (X, R, I : Raw) return Raw
      is (R + I)
      with
        Pre  => Within_Nonneg (X, Limit) and then Near (R, X, BP) and then Symmetric (I, U),
        Post => Near (F'Result, X, BN) and then abs F'Result <= X + Final;
   end Relative_Step;

   generic
      A : Raw;
   package Bounded_Offset with Ghost is
      function F (T, S : Raw) return Raw
      is (T - S)
      with Pre => Within_Nonneg (T, P169) and then Within_Nonneg (S, A), Post => Within (F'Result, P169);
      --  The offset of a time of a ramp from the start of a stage.
   end Bounded_Offset;

   package Advance_Acc is new Leveled_Sum (P598, P597x5, P600, P597x11, P595x11);
   package Advance_Jr is new Leveled_Sum (P431, P429x11, P430x9, P430x13, P429x7);
   package Advance_Sn is new Leveled_Sum (P265, P266, P265x3, P267, P265);
   package Advance_Vel is new Leveled_Sum (P764x5, P763x15, P765x5, P764x13, P763x5);
   package Cub_165 is new Bounded_Cube (P165, P495);
   package Cub_169 is new Bounded_Cube (P169, P507);
   package Div_265_By_2 is new Bounded_Quotient (P265, 2.0, P264);
   package Div_265_By_24 is new Bounded_Quotient (P265, 24.0, P257x11);
   package Div_265_By_6 is new Bounded_Quotient (P265, 6.0, P259x11);
   package Div_265x3_By_2 is new Bounded_Quotient (P265x3, 2.0, P264x3);
   package Div_265x3_By_6 is new Bounded_Quotient (P265x3, 6.0, P264);
   package Div_267_By_24 is new Bounded_Quotient (P267, 24.0, P259x11);
   package Div_269_By_120 is new Bounded_Quotient (P269, 120.0, P259x9);
   package Div_430x13_By_6 is new Bounded_Quotient (P430x13, 6.0, P428x9);
   package Div_430x9_By_2 is new Bounded_Quotient (P430x9, 2.0, P429x9);
   package Div_435x9_By_2 is new Bounded_Quotient (P435x9, 2.0, P434x9);
   package Div_438_By_2 is new Bounded_Quotient (P438, 2.0, P437);
   package Div_596x3_By_2 is new Bounded_Quotient (P596x3, 2.0, P595x3);
   package Div_596x5_By_2 is new Bounded_Quotient (P596x5, 2.0, P595x5);
   package Div_597x11_By_2 is new Bounded_Quotient (P597x11, 2.0, P596x11);
   package Div_600x9_By_2 is new Bounded_Quotient (P600x9, 2.0, P599x9);
   package Div_601x5_By_2 is new Bounded_Quotient (P601x5, 2.0, P600x5);
   package Div_604x11_By_6 is new Bounded_Quotient (P604x11, 6.0, P601x15);
   package Div_604x9_By_6 is new Bounded_Quotient (P604x9, 6.0, P603x3);
   package Div_607_By_6 is new Bounded_Quotient (P607, 6.0, P601x11);
   package Div_763x7_By_2 is new Bounded_Quotient (P763x7, 2.0, P762x7);
   package Div_765_By_12 is new Bounded_Quotient (P765, 12.0, P758x11);
   package Div_766_By_2 is new Bounded_Quotient (P766, 2.0, P765);
   package Div_766x11_By_2 is new Bounded_Quotient (P766x11, 2.0, P765x11);
   package Div_766x13_By_2 is new Bounded_Quotient (P766x13, 2.0, P765x13);
   package Div_767x7_By_12 is new Bounded_Quotient (P767x7, 12.0, P764x5);
   package Div_771x5_By_12 is new Bounded_Quotient (P771x5, 12.0, P767x7);
   package Div_771x7_By_12 is new Bounded_Quotient (P771x7, 12.0, P768x5);
   package Div_773x9_By_24 is new Bounded_Quotient (P773x9, 24.0, P770x3);
   package Div_774_By_12 is new Bounded_Quotient (P774, 12.0, P767x11);
   package Div_774x5_By_24 is new Bounded_Quotient (P774x5, 24.0, P769x7);
   package Div_775x3_By_24 is new Bounded_Quotient (P775x3, 24.0, P772);
   package Div_776_By_24 is new Bounded_Quotient (P776, 24.0, P768x11);
   package Div_927x11_By_24 is new Bounded_Quotient (P927x11, 24.0, P922x15);
   package Div_933x3_By_12 is new Bounded_Quotient (P933x3, 12.0, P931);
   package Div_939_By_24 is new Bounded_Quotient (P939, 24.0, P931x11);
   package Final_269_272 is new Final_Bound (P269, P272);
   package Final_269x3_272 is new Final_Bound (P269x3, P272);
   package Final_269x5_272 is new Final_Bound (P269x5, P272);
   package Final_269x7_272 is new Final_Bound (P269x7, P272);
   package Final_270_272 is new Final_Bound (P270, P272);
   package Final_270x3_272 is new Final_Bound (P270x3, P272);
   package Final_271_272 is new Final_Bound (P271, P272);
   package Final_272_272 is new Final_Bound (P272, P272);
   package Final_434x9_441 is new Final_Bound (P434x9, P441);
   package Final_435x13_441 is new Final_Bound (P435x13, P441);
   package Final_435x9_441 is new Final_Bound (P435x9, P441);
   package Final_436x13_441 is new Final_Bound (P436x13, P441);
   package Final_436x7_441 is new Final_Bound (P436x7, P441);
   package Final_437_441 is new Final_Bound (P437, P441);
   package Final_437x5_441 is new Final_Bound (P437x5, P441);
   package Final_437x9_441 is new Final_Bound (P437x9, P441);
   package Final_438x3_441 is new Final_Bound (P438x3, P441);
   package Final_438x5_441 is new Final_Bound (P438x5, P441);
   package Final_439x3_441 is new Final_Bound (P439x3, P441);
   package Final_440_441 is new Final_Bound (P440, P441);
   package Final_601x11_609 is new Final_Bound (P601x11, P609);
   package Final_602x15_609 is new Final_Bound (P602x15, P609);
   package Final_602x7_609 is new Final_Bound (P602x7, P609);
   package Final_603x11_609 is new Final_Bound (P603x11, P609);
   package Final_604x11_609 is new Final_Bound (P604x11, P609);
   package Final_604x13_609 is new Final_Bound (P604x13, P609);
   package Final_605x3_609 is new Final_Bound (P605x3, P609);
   package Final_605x5_609 is new Final_Bound (P605x5, P609);
   package Final_605x7_609 is new Final_Bound (P605x7, P609);
   package Final_605x9_609 is new Final_Bound (P605x9, P609);
   package Final_606_609 is new Final_Bound (P606, P609);
   package Final_606x5_609 is new Final_Bound (P606x5, P609);
   package Final_607_609 is new Final_Bound (P607, P609);
   package Final_608_609 is new Final_Bound (P608, P609);
   package Fth_169 is new Bounded_Fourth_Power (P169, P676);
   package Mul_100_165 is new Bounded_Product (P100, P165, P265);
   package Mul_100_169 is new Bounded_Product (P100, P169, P269);
   package Mul_100_338 is new Bounded_Product (P100, P338, P438);
   package Mul_100_507 is new Bounded_Product (P100, P507, P607);
   package Mul_100_676 is new Bounded_Product (P100, P676, P776);
   package Mul_165_100 is new Bounded_Product (P165, P100, P265);
   package Mul_165_165 is new Bounded_Product (P165, P165, P330);
   package Mul_165_261x9 is new Bounded_Product (P165, P261x9, P426x9);
   package Mul_165_263x7 is new Bounded_Product (P165, P263x7, P428x7);
   package Mul_165_264x7 is new Bounded_Product (P165, P264x7, P429x7);
   package Mul_165_429x11 is new Bounded_Product (P165, P429x11, P594x11);
   package Mul_165_430x11 is new Bounded_Product (P165, P430x11, P595x11);
   package Mul_165_598x5 is new Bounded_Product (P165, P598x5, P763x5);
   package Mul_165x15_165 is new Bounded_Product (P165x15, P165, P330x15);
   package Mul_165x3_165 is new Bounded_Product (P165x3, P165, P330x3);
   package Mul_165x3_165x3 is new Bounded_Product (P165x3, P165x3, P330x9);
   package Mul_165x7_165 is new Bounded_Product (P165x7, P165, P330x7);
   package Mul_166_165 is new Bounded_Product (P166, P165, P331);
   package Mul_166x15_165 is new Bounded_Product (P166x15, P165, P331x15);
   package Mul_166x15_330 is new Bounded_Product (P166x15, P330, P496x15);
   package Mul_166x3_165 is new Bounded_Product (P166x3, P165, P331x3);
   package Mul_166x3_166 is new Bounded_Product (P166x3, P166, P332x3);
   package Mul_166x7_165 is new Bounded_Product (P166x7, P165, P331x7);
   package Mul_166x9_165 is new Bounded_Product (P166x9, P165, P331x9);
   package Mul_167_330x7 is new Bounded_Product (P167, P330x7, P497x7);
   package Mul_167_332x11 is new Bounded_Product (P167, P332x11, P499x11);
   package Mul_167_332x13 is new Bounded_Product (P167, P332x13, P499x13);
   package Mul_167x15_165 is new Bounded_Product (P167x15, P165, P332x15);
   package Mul_167x15_330 is new Bounded_Product (P167x15, P330, P497x15);
   package Mul_167x3_165 is new Bounded_Product (P167x3, P165, P332x3);
   package Mul_167x3_330 is new Bounded_Product (P167x3, P330, P497x3);
   package Mul_167x3_331x3 is new Bounded_Product (P167x3, P331x3, P498x9);
   package Mul_167x3_331x5 is new Bounded_Product (P167x3, P331x5, P498x15);
   package Mul_167x7_165 is new Bounded_Product (P167x7, P165, P332x7);
   package Mul_167x9_165 is new Bounded_Product (P167x9, P165, P332x9);
   package Mul_168x15_165 is new Bounded_Product (P168x15, P165, P333x15);
   package Mul_168x3_165 is new Bounded_Product (P168x3, P165, P333x3);
   package Mul_168x3_330 is new Bounded_Product (P168x3, P330, P498x3);
   package Mul_168x3_333 is new Bounded_Product (P168x3, P333, P501x3);
   package Mul_169_100 is new Bounded_Product (P169, P100, P269);
   package Mul_169_166 is new Bounded_Product (P169, P166, P335);
   package Mul_169_261x5 is new Bounded_Product (P169, P261x5, P430x5);
   package Mul_169_429x15 is new Bounded_Product (P169, P429x15, P598x15);
   package Mul_169_599x9 is new Bounded_Product (P169, P599x9, P768x9);
   package Mul_169_769x5 is new Bounded_Product (P169, P769x5, P938x5);
   package Mul_169x3_165 is new Bounded_Product (P169x3, P165, P334x3);
   package Mul_169x3_165x3 is new Bounded_Product (P169x3, P165x3, P334x9);
   package Mul_170x3_165 is new Bounded_Product (P170x3, P165, P335x3);
   package Mul_170x3_330 is new Bounded_Product (P170x3, P330, P500x3);
   package Mul_171x3_165 is new Bounded_Product (P171x3, P165, P336x3);
   package Mul_171x5_165 is new Bounded_Product (P171x5, P165, P336x5);
   package Mul_171x5_330 is new Bounded_Product (P171x5, P330, P501x5);
   package Mul_265_166 is new Bounded_Product (P265, P166, P431);
   package Mul_269_165 is new Bounded_Product (P269, P165, P434);
   package Mul_269_166x9 is new Bounded_Product (P269, P166x9, P435x9);
   package Mul_269_335x11 is new Bounded_Product (P269, P335x11, P604x11);
   package Mul_269_335x9 is new Bounded_Product (P269, P335x9, P604x9);
   package Mul_269_504x9 is new Bounded_Product (P269, P504x9, P773x9);
   package Mul_269_505x5 is new Bounded_Product (P269, P505x5, P774x5);
   package Mul_269_506x3 is new Bounded_Product (P269, P506x3, P775x3);
   package Mul_332x15_165 is new Bounded_Product (P332x15, P165, P497x15);
   package Mul_332x3_165 is new Bounded_Product (P332x3, P165, P497x3);
   package Mul_333x15_165 is new Bounded_Product (P333x15, P165, P498x15);
   package Mul_334x3_165 is new Bounded_Product (P334x3, P165, P499x3);
   package Mul_334x5_165 is new Bounded_Product (P334x5, P165, P499x5);
   package Mul_335x3_165x3 is new Bounded_Product (P335x3, P165x3, P500x9);
   package Mul_336x3_165 is new Bounded_Product (P336x3, P165, P501x3);
   package Mul_336x3_166 is new Bounded_Product (P336x3, P166, P502x3);
   package Mul_336x5_165 is new Bounded_Product (P336x5, P165, P501x5);
   package Mul_337x5_165 is new Bounded_Product (P337x5, P165, P502x5);
   package Mul_340_165 is new Bounded_Product (P340, P165, P505);
   package Mul_431_165x3 is new Bounded_Product (P431, P165x3, P596x3);
   package Mul_431_165x5 is new Bounded_Product (P431, P165x5, P596x5);
   package Mul_431_167 is new Bounded_Product (P431, P167, P598);
   package Mul_431_334 is new Bounded_Product (P431, P334, P765);
   package Mul_431_336x7 is new Bounded_Product (P431, P336x7, P767x7);
   package Mul_431_508 is new Bounded_Product (P431, P508, P939);
   package Mul_434_166 is new Bounded_Product (P434, P166, P600);
   package Mul_434_166x9 is new Bounded_Product (P434, P166x9, P600x9);
   package Mul_434_167x5 is new Bounded_Product (P434, P167x5, P601x5);
   package Mul_434_332x11 is new Bounded_Product (P434, P332x11, P766x11);
   package Mul_434_332x13 is new Bounded_Product (P434, P332x13, P766x13);
   package Mul_434_333 is new Bounded_Product (P434, P333, P767);
   package Mul_434_337x5 is new Bounded_Product (P434, P337x5, P771x5);
   package Mul_434_337x7 is new Bounded_Product (P434, P337x7, P771x7);
   package Mul_434_340 is new Bounded_Product (P434, P340, P774);
   package Mul_596x3_330x7 is new Bounded_Product (P596x3, P330x7, P927x11);
   package Mul_598_165x7 is new Bounded_Product (P598, P165x7, P763x7);
   package Mul_598_168 is new Bounded_Product (P598, P168, P766);
   package Mul_598_335x3 is new Bounded_Product (P598, P335x3, P933x3);
   package Mul_797x9_165x15 is new Bounded_Product (P797x9, P165x15, P966x9);
   package Neg_169 is new Bounded_Negation (P169);
   package Neg_335 is new Bounded_Negation (P335);
   package Neg_338 is new Bounded_Negation (P338);
   package Neg_339 is new Bounded_Negation (P339);
   package Neg_431 is new Bounded_Negation (P431);
   package Neg_507 is new Bounded_Negation (P507);
   package Pos_Scale_2_165 is new Nonnegative_Scale (2.0, P165, P166);
   package Pos_Scale_3_165 is new Nonnegative_Scale (3.0, P165, P165x3);
   package Pos_Scale_4_165 is new Nonnegative_Scale (4.0, P165, P167);
   package Pos_Scale_5_165 is new Nonnegative_Scale (5.0, P165, P165x5);
   package Pos_Scale_6_165 is new Nonnegative_Scale (6.0, P165, P166x3);
   package Pos_Scale_7_165 is new Nonnegative_Scale (7.0, P165, P165x7);
   package Pos_Sum_165_165 is new Nonnegative_Sum (P165, P165, P166);
   package Pos_Sum_165x11_165 is new Nonnegative_Sum (P165x11, P165, P167x3);
   package Pos_Sum_165x11_166 is new Nonnegative_Sum (P165x11, P166, P165x13);
   package Pos_Sum_165x13_165 is new Nonnegative_Sum (P165x13, P165, P166x7);
   package Pos_Sum_165x3_165 is new Nonnegative_Sum (P165x3, P165, P167);
   package Pos_Sum_165x3_166 is new Nonnegative_Sum (P165x3, P166, P165x5);
   package Pos_Sum_165x5_165 is new Nonnegative_Sum (P165x5, P165, P166x3);
   package Pos_Sum_165x5_165x3 is new Nonnegative_Sum (P165x5, P165x3, P168);
   package Pos_Sum_165x5_166 is new Nonnegative_Sum (P165x5, P166, P165x7);
   package Pos_Sum_165x7_165 is new Nonnegative_Sum (P165x7, P165, P168);
   package Pos_Sum_165x7_165x3 is new Nonnegative_Sum (P165x7, P165x3, P166x5);
   package Pos_Sum_165x7_167 is new Nonnegative_Sum (P165x7, P167, P165x11);
   package Pos_Sum_165x9_165 is new Nonnegative_Sum (P165x9, P165, P166x5);
   package Pos_Sum_165x9_166 is new Nonnegative_Sum (P165x9, P166, P165x11);
   package Pos_Sum_166_165 is new Nonnegative_Sum (P166, P165, P165x3);
   package Pos_Sum_166x3_165 is new Nonnegative_Sum (P166x3, P165, P165x7);
   package Pos_Sum_166x3_165x3 is new Nonnegative_Sum (P166x3, P165x3, P165x9);
   package Pos_Sum_166x5_165 is new Nonnegative_Sum (P166x5, P165, P165x11);
   package Pos_Sum_166x5_166 is new Nonnegative_Sum (P166x5, P166, P167x3);
   package Pos_Sum_167_165 is new Nonnegative_Sum (P167, P165, P165x5);
   package Pos_Sum_167_166 is new Nonnegative_Sum (P167, P166, P166x3);
   package Pos_Sum_167x3_165 is new Nonnegative_Sum (P167x3, P165, P165x13);
   package Pos_Sum_168_165 is new Nonnegative_Sum (P168, P165, P165x9);
   package Scale_11_330 is new Bounded_Scale (11.0, P330, P330x11);
   package Scale_120_165 is new Bounded_Scale (120.0, P165, P168x15);
   package Scale_12_165 is new Bounded_Scale (12.0, P165, P167x3);
   package Scale_12_169 is new Bounded_Scale (12.0, P169, P171x3);
   package Scale_12_330 is new Bounded_Scale (12.0, P330, P332x3);
   package Scale_14_165 is new Bounded_Scale (14.0, P165, P166x7);
   package Scale_15_165 is new Bounded_Scale (15.0, P165, P165x15);
   package Scale_18_165 is new Bounded_Scale (18.0, P165, P166x9);
   package Scale_192_165 is new Bounded_Scale (192.0, P165, P171x3);
   package Scale_192_330 is new Bounded_Scale (192.0, P330, P336x3);
   package Scale_22_330 is new Bounded_Scale (22.0, P330, P331x11);
   package Scale_24_165 is new Bounded_Scale (24.0, P165, P168x3);
   package Scale_27_165 is new Bounded_Scale (27.0, P165, P167x7);
   package Scale_27_330 is new Bounded_Scale (27.0, P330, P332x7);
   package Scale_293_165 is new Bounded_Scale (293.0, P165, P171x5);
   package Scale_2_165 is new Bounded_Scale (2.0, P165, P166);
   package Scale_2_330 is new Bounded_Scale (2.0, P330, P331);
   package Scale_2_338 is new Bounded_Scale (2.0, P338, P339);
   package Scale_302_165 is new Bounded_Scale (302.0, P165, P171x5);
   package Scale_302_330 is new Bounded_Scale (302.0, P330, P336x5);
   package Scale_30_165 is new Bounded_Scale (30.0, P165, P166x15);
   package Scale_36_165 is new Bounded_Scale (36.0, P165, P167x9);
   package Scale_390_495 is new Bounded_Scale (390.0, P495, P500x13);
   package Scale_3_165 is new Bounded_Scale (3.0, P165, P165x3);
   package Scale_3_169 is new Bounded_Scale (3.0, P169, P169x3);
   package Scale_3_330 is new Bounded_Scale (3.0, P330, P330x3);
   package Scale_48_330 is new Bounded_Scale (48.0, P330, P334x3);
   package Scale_49_495 is new Bounded_Scale (49.0, P495, P497x13);
   package Scale_4_165 is new Bounded_Scale (4.0, P165, P167);
   package Scale_4_338 is new Bounded_Scale (4.0, P338, P340);
   package Scale_4_495 is new Bounded_Scale (4.0, P495, P497);
   package Scale_585_330 is new Bounded_Scale (585.0, P330, P337x5);
   package Scale_60_165 is new Bounded_Scale (60.0, P165, P167x15);
   package Scale_6_165 is new Bounded_Scale (6.0, P165, P166x3);
   package Scale_6_169 is new Bounded_Scale (6.0, P169, P170x3);
   package Scale_6_330 is new Bounded_Scale (6.0, P330, P331x3);
   package Scale_76_330 is new Bounded_Scale (76.0, P330, P334x5);
   package Scale_7_165 is new Bounded_Scale (7.0, P165, P165x7);
   package Scale_7_330 is new Bounded_Scale (7.0, P330, P330x7);
   package Scale_89_165 is new Bounded_Scale (89.0, P165, P170x3);
   package Scale_89_330 is new Bounded_Scale (89.0, P330, P335x3);
   package Scale_8_165 is new Bounded_Scale (8.0, P165, P168);
   package Sqr_165 is new Bounded_Square (P165, P330);
   package Sqr_169 is new Bounded_Square (P169, P338);
   package Start_766 is new Relative_Start (P800, P766, P767, P767);
   package Start_768x11 is new Relative_Start (P800, P768x11, P769x11, P782);
   package Step_769x11_767x7 is new Relative_Step (P800, P769x11, P767x7, P769x15, P782);
   package Step_769x15_772 is new Relative_Step (P800, P769x15, P772, P774, P782);
   package Step_771x13_767x11 is new Relative_Step (P800, P771x13, P767x11, P771x15, P782);
   package Step_771x15_772 is new Relative_Step (P800, P771x15, P772, P773x5, P782);
   package Step_771x9_769x7 is new Relative_Step (P800, P771x9, P769x7, P771x13, P782);
   package Step_772x11_770x3 is new Relative_Step (P800, P772x11, P770x3, P772x13, P782);
   package Step_772x13_768x5 is new Relative_Step (P800, P772x13, P768x5, P773x7, P782);
   package Step_773x11_772 is new Relative_Step (P800, P773x11, P772, P775x3, P782);
   package Step_773x5_767 is new Relative_Step (P800, P773x5, P767, P772x11, P782);
   package Step_773x7_772 is new Relative_Step (P800, P773x7, P772, P776, P782);
   package Step_773x9_769x7 is new Relative_Step (P800, P773x9, P769x7, P774x5, P782);
   package Step_774_765x11 is new Relative_Step (P800, P774, P765x11, P771x9, P782);
   package Step_774x5_768x5 is new Relative_Step (P800, P774x5, P768x5, P773x11, P782);
   package Step_776_765x13 is new Relative_Step (P800, P776, P765x13, P773x9, P782);
   package Sum_165_165 is new Bounded_Sum (P165, P165, P166);
   package Sum_165_166 is new Bounded_Sum (P165, P166, P165x3);
   package Sum_165x3_165 is new Bounded_Sum (P165x3, P165, P167);
   package Sum_165x3_166 is new Bounded_Sum (P165x3, P166, P165x5);
   package Sum_165x7_165 is new Bounded_Sum (P165x7, P165, P168);
   package Sum_166_165 is new Bounded_Sum (P166, P165, P165x3);
   package Sum_166x3_165 is new Bounded_Sum (P166x3, P165, P165x7);
   package Sum_166x7_165 is new Bounded_Sum (P166x7, P165, P165x15);
   package Sum_166x9_166 is new Bounded_Sum (P166x9, P166, P167x5);
   package Sum_167_166 is new Bounded_Sum (P167, P166, P166x3);
   package Sum_167x3_166 is new Bounded_Sum (P167x3, P166, P166x7);
   package Sum_168_167 is new Bounded_Sum (P168, P167, P167x3);
   package Sum_169_165 is new Bounded_Sum (P169, P165, P166x9);
   package Sum_169_166 is new Bounded_Sum (P169, P166, P166x9);
   package Sum_259x11_259x9 is new Bounded_Sum (P259x11, P259x9, P261x5);
   package Sum_264_257x11 is new Bounded_Sum (P264, P257x11, P261x9);
   package Sum_264x3_259x11 is new Bounded_Sum (P264x3, P259x11, P263x7);
   package Sum_265x3_264 is new Bounded_Sum (P265x3, P264, P264x7);
   package Sum_269_269 is new Bounded_Sum (P269, P269, P270);
   package Sum_269x3_269 is new Bounded_Sum (P269x3, P269, P271);
   package Sum_269x5_269 is new Bounded_Sum (P269x5, P269, P270x3);
   package Sum_269x7_269 is new Bounded_Sum (P269x7, P269, P272);
   package Sum_270_269 is new Bounded_Sum (P270, P269, P269x3);
   package Sum_270x3_269 is new Bounded_Sum (P270x3, P269, P269x7);
   package Sum_271_269 is new Bounded_Sum (P271, P269, P269x5);
   package Sum_330_330x3 is new Bounded_Sum (P330, P330x3, P332);
   package Sum_330x11_330x15 is new Bounded_Sum (P330x11, P330x15, P331x13);
   package Sum_330x11_331x9 is new Bounded_Sum (P330x11, P331x9, P331x15);
   package Sum_330x3_330x3 is new Bounded_Sum (P330x3, P330x3, P331x3);
   package Sum_330x5_330 is new Bounded_Sum (P330x5, P330, P331x3);
   package Sum_330x5_331 is new Bounded_Sum (P330x5, P331, P330x7);
   package Sum_330x7_330 is new Bounded_Sum (P330x7, P330, P333);
   package Sum_330x7_330x7 is new Bounded_Sum (P330x7, P330x7, P331x7);
   package Sum_331_330x3 is new Bounded_Sum (P331, P330x3, P330x5);
   package Sum_331x13_331x3 is new Bounded_Sum (P331x13, P331x3, P335);
   package Sum_331x15_331x3 is new Bounded_Sum (P331x15, P331x3, P332x9);
   package Sum_331x3_330 is new Bounded_Sum (P331x3, P330, P330x7);
   package Sum_331x7_331 is new Bounded_Sum (P331x7, P331, P334);
   package Sum_332_330x3 is new Bounded_Sum (P332, P330x3, P330x7);
   package Sum_332x11_330 is new Bounded_Sum (P332x11, P330, P334x3);
   package Sum_332x11_331x3 is new Bounded_Sum (P332x11, P331x3, P332x13);
   package Sum_332x7_332x7 is new Bounded_Sum (P332x7, P332x7, P333x7);
   package Sum_332x9_330x3 is new Bounded_Sum (P332x9, P330x3, P333x5);
   package Sum_332x9_331x3 is new Bounded_Sum (P332x9, P331x3, P332x11);
   package Sum_333_331 is new Bounded_Sum (P333, P331, P331x5);
   package Sum_333x11_331 is new Bounded_Sum (P333x11, P331, P335x3);
   package Sum_333x5_330 is new Bounded_Sum (P333x5, P330, P332x11);
   package Sum_333x5_331 is new Bounded_Sum (P333x5, P331, P332x11);
   package Sum_333x7_331x7 is new Bounded_Sum (P333x7, P331x7, P333x9);
   package Sum_333x9_330x7 is new Bounded_Sum (P333x9, P330x7, P334x5);
   package Sum_334x3_331 is new Bounded_Sum (P334x3, P331, P332x13);
   package Sum_334x5_330x7 is new Bounded_Sum (P334x5, P330x7, P333x11);
   package Sum_335_330x3 is new Bounded_Sum (P335, P330x3, P332x9);
   package Sum_335_331 is new Bounded_Sum (P335, P331, P332x9);
   package Sum_335x11_332x3 is new Bounded_Sum (P335x11, P332x3, P337x3);
   package Sum_335x13_332x3 is new Bounded_Sum (P335x13, P332x3, P336x7);
   package Sum_335x3_335x3 is new Bounded_Sum (P335x3, P335x3, P336x3);
   package Sum_335x9_331x11 is new Bounded_Sum (P335x9, P331x11, P336x5);
   package Sum_336x11_331 is new Bounded_Sum (P336x11, P331, P338x3);
   package Sum_336x11_331x11 is new Bounded_Sum (P336x11, P331x11, P338x3);
   package Sum_336x11_332x3 is new Bounded_Sum (P336x11, P332x3, P338x3);
   package Sum_336x13_331x3 is new Bounded_Sum (P336x13, P331x3, P337x7);
   package Sum_336x13_332x3 is new Bounded_Sum (P336x13, P332x3, P337x7);
   package Sum_336x15_332x3 is new Bounded_Sum (P336x15, P332x3, P340);
   package Sum_336x3_332x15 is new Bounded_Sum (P336x3, P332x15, P338);
   package Sum_336x5_330x3 is new Bounded_Sum (P336x5, P330x3, P335x11);
   package Sum_336x5_330x9 is new Bounded_Sum (P336x5, P330x9, P335x11);
   package Sum_336x5_331x15 is new Bounded_Sum (P336x5, P331x15, P335x11);
   package Sum_336x9_331 is new Bounded_Sum (P336x9, P331, P337x5);
   package Sum_336x9_331x11 is new Bounded_Sum (P336x9, P331x11, P337x5);
   package Sum_337x3_331x3 is new Bounded_Sum (P337x3, P331x3, P335x13);
   package Sum_337x5_332x9 is new Bounded_Sum (P337x5, P332x9, P336x11);
   package Sum_337x7_331x3 is new Bounded_Sum (P337x7, P331x3, P336x15);
   package Sum_338_332x3 is new Bounded_Sum (P338, P332x3, P335x9);
   package Sum_338_333x3 is new Bounded_Sum (P338, P333x3, P335x9);
   package Sum_338_334x3 is new Bounded_Sum (P338, P334x3, P336x5);
   package Sum_338x3_331x15 is new Bounded_Sum (P338x3, P331x15, P336x13);
   package Sum_338x3_331x3 is new Bounded_Sum (P338x3, P331x3, P336x13);
   package Sum_338x3_332x3 is new Bounded_Sum (P338x3, P332x3, P336x13);
   package Sum_339_334x3 is new Bounded_Sum (P339, P334x3, P336x9);
   package Sum_339_334x9 is new Bounded_Sum (P339, P334x9, P336x11);
   package Sum_428x9_430x5 is new Bounded_Sum (P428x9, P430x5, P429x15);
   package Sum_429x9_426x9 is new Bounded_Sum (P429x9, P426x9, P429x11);
   package Sum_430x9_428x7 is new Bounded_Sum (P430x9, P428x7, P430x11);
   package Sum_434x9_434x9 is new Bounded_Sum (P434x9, P434x9, P435x9);
   package Sum_435x13_434 is new Bounded_Sum (P435x13, P434, P436x7);
   package Sum_435x9_437 is new Bounded_Sum (P435x9, P437, P435x13);
   package Sum_436x13_434x9 is new Bounded_Sum (P436x13, P434x9, P440);
   package Sum_436x7_434x9 is new Bounded_Sum (P436x7, P434x9, P437x5);
   package Sum_437_434 is new Bounded_Sum (P437, P434, P434x9);
   package Sum_437x5_437 is new Bounded_Sum (P437x5, P437, P438x3);
   package Sum_437x9_434 is new Bounded_Sum (P437x9, P434, P438x5);
   package Sum_438x3_434 is new Bounded_Sum (P438x3, P434, P436x13);
   package Sum_438x5_434x9 is new Bounded_Sum (P438x5, P434x9, P439x3);
   package Sum_440_437 is new Bounded_Sum (P440, P437, P437x9);
   package Sum_500x13_502x5 is new Bounded_Sum (P500x13, P502x5, P502x9);
   package Sum_502x9_501x5 is new Bounded_Sum (P502x9, P501x5, P504x3);
   package Sum_503x11_501x3 is new Bounded_Sum (P503x11, P501x3, P505x3);
   package Sum_503x13_498x15 is new Bounded_Sum (P503x13, P498x15, P504x7);
   package Sum_503x15_497x13 is new Bounded_Sum (P503x15, P497x13, P507);
   package Sum_503x7_501x5 is new Bounded_Sum (P503x7, P501x5, P503x9);
   package Sum_503x9_501x5 is new Bounded_Sum (P503x9, P501x5, P503x11);
   package Sum_504x11_497 is new Bounded_Sum (P504x11, P497, P506x3);
   package Sum_504x11_497x15 is new Bounded_Sum (P504x11, P497x15, P506x3);
   package Sum_504x11_497x7 is new Bounded_Sum (P504x11, P497x7, P506x3);
   package Sum_504x11_499x11 is new Bounded_Sum (P504x11, P499x11, P506x3);
   package Sum_504x11_499x13 is new Bounded_Sum (P504x11, P499x13, P506x3);
   package Sum_504x13_497 is new Bounded_Sum (P504x13, P497, P505x7);
   package Sum_504x15_497x3 is new Bounded_Sum (P504x15, P497x3, P508);
   package Sum_504x3_501x3 is new Bounded_Sum (P504x3, P501x3, P503x7);
   package Sum_504x7_498x3 is new Bounded_Sum (P504x7, P498x3, P503x15);
   package Sum_504x9_498x15 is new Bounded_Sum (P504x9, P498x15, P505x5);
   package Sum_504x9_498x9 is new Bounded_Sum (P504x9, P498x9, P505x5);
   package Sum_504x9_499x3 is new Bounded_Sum (P504x9, P499x3, P505x5);
   package Sum_505x3_497x15 is new Bounded_Sum (P505x3, P497x15, P503x13);
   package Sum_505x5_496x15 is new Bounded_Sum (P505x5, P496x15, P504x11);
   package Sum_505x5_500x3 is new Bounded_Sum (P505x5, P500x3, P504x11);
   package Sum_505x5_500x9 is new Bounded_Sum (P505x5, P500x9, P504x11);
   package Sum_505x7_497x3 is new Bounded_Sum (P505x7, P497x3, P504x15);
   package Sum_506x3_497x3 is new Bounded_Sum (P506x3, P497x3, P504x13);
   package Sum_507_499x5 is new Bounded_Sum (P507, P499x5, P504x9);
   package Sum_507_501x3 is new Bounded_Sum (P507, P501x3, P504x9);
   package Sum_507_502x3 is new Bounded_Sum (P507, P502x3, P504x9);
   package Sum_507_505 is new Bounded_Sum (P507, P505, P505x5);
   package Sum_596x11_598x15 is new Bounded_Sum (P596x11, P598x15, P599x9);
   package Sum_600_594x11 is new Bounded_Sum (P600, P594x11, P598x5);
   package Sum_601x11_599x9 is new Bounded_Sum (P601x11, P599x9, P602x7);
   package Sum_602x15_600 is new Bounded_Sum (P602x15, P600, P606);
   package Sum_602x7_601x15 is new Bounded_Sum (P602x7, P601x15, P602x15);
   package Sum_603x11_600x5 is new Bounded_Sum (P603x11, P600x5, P605x3);
   package Sum_604x11_601x15 is new Bounded_Sum (P604x11, P601x15, P604x13);
   package Sum_604x13_600 is new Bounded_Sum (P604x13, P600, P605x7);
   package Sum_605x3_601x15 is new Bounded_Sum (P605x3, P601x15, P607);
   package Sum_605x5_599x9 is new Bounded_Sum (P605x5, P599x9, P604x11);
   package Sum_605x7_603x3 is new Bounded_Sum (P605x7, P603x3, P608);
   package Sum_605x9_601x15 is new Bounded_Sum (P605x9, P601x15, P606x5);
   package Sum_606_603x3 is new Bounded_Sum (P606, P603x3, P603x11);
   package Sum_607_601x11 is new Bounded_Sum (P607, P601x11, P605x5);
   package Sum_608_600x5 is new Bounded_Sum (P608, P600x5, P605x9);
   package Sum_764x13_768x9 is new Bounded_Sum (P764x13, P768x9, P769x5);
   package Sum_800_765 is new Bounded_Sum (P800, P765, P797x9);
   package Sym_765x11 is new Symmetric_Bound (P765x11);
   package Sym_765x13 is new Symmetric_Bound (P765x13);
   package Sym_766 is new Symmetric_Bound (P766);
   package Sym_767 is new Symmetric_Bound (P767);
   package Sym_767x11 is new Symmetric_Bound (P767x11);
   package Sym_767x7 is new Symmetric_Bound (P767x7);
   package Sym_768x11 is new Symmetric_Bound (P768x11);
   package Sym_768x5 is new Symmetric_Bound (P768x5);
   package Sym_769x7 is new Symmetric_Bound (P769x7);
   package Sym_770x3 is new Symmetric_Bound (P770x3);
   package Sym_772 is new Symmetric_Bound (P772);
   package Widen_966x9_970 is new Wider_Bound (P966x9, P970);
   package Widen_Acc_595x3 is new Leveled_Bound (P595x3, P598, P597x5, P600, P597x11);
   package Widen_Acc_595x5 is new Leveled_Bound (P595x5, P598, P597x5, P600, P597x11);
   package Widen_Acc_598 is new Leveled_Bound (P598, P598, P597x5, P600, P597x11);
   package Widen_Dist_922x15 is new Leveled_Bound (P922x15, P931x11, P937x11, P938x11, P942);
   package Widen_Dist_931 is new Leveled_Bound (P931, P931x11, P937x11, P938x11, P942);
   package Widen_Dist_931x11 is new Leveled_Bound (P931x11, P931x11, P937x11, P938x11, P942);
   package Widen_Jr_431 is new Leveled_Bound (P431, P431, P429x11, P430x9, P430x13);
   package Widen_Vel_758x11 is new Leveled_Bound (P758x11, P764x5, P763x15, P765x5, P764x13);
   package Widen_Vel_762x7 is new Leveled_Bound (P762x7, P764x5, P763x15, P765x5, P764x13);
   package Widen_Vel_764x5 is new Leveled_Bound (P764x5, P764x5, P763x15, P765x5, P764x13);
   package Offset_165 is new Bounded_Offset (P165);
   package Offset_165x11 is new Bounded_Offset (P165x11);
   package Offset_165x13 is new Bounded_Offset (P165x13);
   package Offset_165x3 is new Bounded_Offset (P165x3);
   package Offset_165x5 is new Bounded_Offset (P165x5);
   package Offset_165x7 is new Bounded_Offset (P165x7);
   package Offset_165x9 is new Bounded_Offset (P165x9);
   package Offset_166 is new Bounded_Offset (P166);
   package Offset_166x3 is new Bounded_Offset (P166x3);
   package Offset_166x5 is new Bounded_Offset (P166x5);
   package Offset_166x7 is new Bounded_Offset (P166x7);
   package Offset_167 is new Bounded_Offset (P167);
   package Offset_167x3 is new Bounded_Offset (P167x3);
   package Offset_168 is new Bounded_Offset (P168);

   function Ramp_Inputs_OK (T1, T2, T3, T4, Cm : Raw) return Boolean
   is (Within (T1, P165)
       and then Within (T2, P165)
       and then Within (T3, P165)
       and then Within (T4, P165)
       and then Within (T1, P169)
       and then Within (T2, P169)
       and then Within (T3, P169)
       and then Within (T4, P169)
       and then Within (Cm, P100))
   with Ghost;
   --  The domain of a ramp as the mirrors of the stage functions need it: each phase time within Phase_Time_Max,
   --  and within Ramp_Time_Max because a stage function passes it on as an offset, and the crackle within
   --  Crackle_Magnitude_Max.

   function Ramp_Times_OK (T, T1, T2, T3, T4 : Raw) return Boolean
   is (Within_Nonneg (T, P169)
       and then Within (T, P169)
       and then Within_Nonneg (T1, P165)
       and then Within_Nonneg (T2, P165)
       and then Within_Nonneg (T3, P165)
       and then Within_Nonneg (T4, P165))
   with Ghost;
   --  The same domain as the mirror of the stage selection needs it.

   function Ramp_Reach (V : Raw) return Raw
   is (abs V * P169)
   with Ghost, Pre => Within (V, P800), Post => Within_Nonneg (Ramp_Reach'Result, P969);
   --  The distance at speed V over the longest ramp.

   function Ramp_Travel (V, T : Raw) return Raw
   is (V * T)
   with
     Ghost,
     Pre  => Within (V, P800) and then Within_Nonneg (T, P165x15),
     Post => Symmetric (Ramp_Travel'Result, abs V * P169);
   --  The distance at speed V over a time within a ramp.

   function Travel_Reach (V : Raw) return Raw
   is (abs V * P169 + P950)
   with Ghost, Pre => Within (V, P800);
   --  The bound of the postcondition of Distance_At_Time, whose evaluation has checks of its own.

   function Gain_Reach (V : Raw) return Raw
   is (abs V + P782)
   with Ghost, Pre => Within (V, P800);
   --  The bound of the postcondition of Velocity_At_Time.

   procedure Witness_Stage_Starts (T1, T2, T3, T4 : Raw)
   with Ghost;
   --  The mirror of the sums with which the evaluators of a ramp compare the time to find its stage.  It has no
   --  contract, so that GNATprove analyses it in the context of each call and its facts reach the caller.

   procedure Witness_Stage_Starts (T1, T2, T3, T4 : Raw) is
   begin
      Witness (Pos_Sum_165_165.F (T1, T2));
      Witness (Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2));
      Witness (Pos_Sum_165x3_165.F (Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2), T3));
      Witness (Pos_Sum_167_165.F (Pos_Sum_165x3_165.F (Pos_Scale_3_165.F (T1), T2), T3));
      Witness (Pos_Sum_165x5_165.F (Pos_Sum_165x3_166.F (Pos_Scale_3_165.F (T1), Pos_Scale_2_165.F (T2)), T3));
      Witness (Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3));
      Witness
        (Pos_Sum_165x7_165.F
           (Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4));
      Witness
        (Pos_Sum_168_165.F
           (Pos_Sum_165x7_165.F (Pos_Sum_165x5_166.F (Pos_Scale_5_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4));
      Witness
        (Pos_Sum_165x9_165.F
           (Pos_Sum_168_165.F (Pos_Sum_165x5_165x3.F (Pos_Scale_5_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4));
      Witness
        (Pos_Sum_166x5_165.F
           (Pos_Sum_165x9_165.F (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4));
      Witness
        (Pos_Sum_165x11_165.F
           (Pos_Sum_165x9_166.F
              (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
            T4));
      Witness
        (Pos_Sum_167x3_165.F
           (Pos_Sum_166x5_166.F
              (Pos_Sum_165x7_165x3.F (Pos_Scale_7_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
            T4));
      Witness
        (Pos_Sum_165x13_165.F
           (Pos_Sum_165x11_166.F
              (Pos_Sum_165x7_167.F (Pos_Scale_7_165.F (T1), Pos_Scale_4_165.F (T2)), Pos_Scale_2_165.F (T3)),
            T4));
   end Witness_Stage_Starts;

   function Phase_Times_OK (T1, T2, T3, T4 : Raw) return Boolean
   is (Within_Nonneg (T1, P165)
       and then Within_Nonneg (T2, P165)
       and then Within_Nonneg (T3, P165)
       and then Within_Nonneg (T4, P165))
   with Ghost;

   procedure Lemma_Stage_Starts (T1, T2, T3, T4 : Raw)
   with
     Ghost,
     Pre  => Phase_Times_OK (T1, T2, T3, T4),
     Post =>
       Within_Nonneg (T1 + T2, P166)
       and then Within_Nonneg (2.0 * T1, P166)
       and then Within_Nonneg (2.0 * T1 + T2, P165x3)
       and then Within_Nonneg (2.0 * T1 + T2 + T3, P167)
       and then Within_Nonneg (3.0 * T1, P165x3)
       and then Within_Nonneg (3.0 * T1 + T2, P167)
       and then Within_Nonneg (3.0 * T1 + T2 + T3, P165x5)
       and then Within_Nonneg (2.0 * T2, P166)
       and then Within_Nonneg (3.0 * T1 + 2.0 * T2, P165x5)
       and then Within_Nonneg (3.0 * T1 + 2.0 * T2 + T3, P166x3)
       and then Within_Nonneg (4.0 * T1, P167)
       and then Within_Nonneg (4.0 * T1 + 2.0 * T2, P166x3)
       and then Within_Nonneg (4.0 * T1 + 2.0 * T2 + T3, P165x7)
       and then Within_Nonneg (4.0 * T1 + 2.0 * T2 + T3 + T4, P168)
       and then Within_Nonneg (5.0 * T1, P165x5)
       and then Within_Nonneg (5.0 * T1 + 2.0 * T2, P165x7)
       and then Within_Nonneg (5.0 * T1 + 2.0 * T2 + T3, P168)
       and then Within_Nonneg (5.0 * T1 + 2.0 * T2 + T3 + T4, P165x9)
       and then Within_Nonneg (3.0 * T2, P165x3)
       and then Within_Nonneg (5.0 * T1 + 3.0 * T2, P168)
       and then Within_Nonneg (5.0 * T1 + 3.0 * T2 + T3, P165x9)
       and then Within_Nonneg (5.0 * T1 + 3.0 * T2 + T3 + T4, P166x5)
       and then Within_Nonneg (6.0 * T1, P166x3)
       and then Within_Nonneg (6.0 * T1 + 3.0 * T2, P165x9)
       and then Within_Nonneg (6.0 * T1 + 3.0 * T2 + T3, P166x5)
       and then Within_Nonneg (6.0 * T1 + 3.0 * T2 + T3 + T4, P165x11)
       and then Within_Nonneg (2.0 * T3, P166)
       and then Within_Nonneg (6.0 * T1 + 3.0 * T2 + 2.0 * T3, P165x11)
       and then Within_Nonneg (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4, P167x3)
       and then Within_Nonneg (7.0 * T1, P165x7)
       and then Within_Nonneg (7.0 * T1 + 3.0 * T2, P166x5)
       and then Within_Nonneg (7.0 * T1 + 3.0 * T2 + 2.0 * T3, P167x3)
       and then Within_Nonneg (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4, P165x13)
       and then Within_Nonneg (4.0 * T2, P167)
       and then Within_Nonneg (7.0 * T1 + 4.0 * T2, P165x11)
       and then Within_Nonneg (7.0 * T1 + 4.0 * T2 + 2.0 * T3, P165x13)
       and then Within_Nonneg (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4, P166x7);
   --  The bound of every sum with which the evaluators of a ramp compare the time, and of every operation
   --  in it.  Unlike a mirror, this leaves the caller a flat list of bounds: Crackle_At_Time, whose
   --  postcondition is proved over its fifteen returns at once, needs the shorter context.

   procedure Lemma_Stage_Starts (T1, T2, T3, T4 : Raw) is
   begin
      Witness_Stage_Starts (T1, T2, T3, T4);
   end Lemma_Stage_Starts;

   procedure Witness_Stage_Selection (T, T1, T2, T3, T4 : Raw)
   with Ghost;
   --  The mirror of the comparisons by which the evaluators of a ramp find the stage of T, and of the offsets of T
   --  within each stage.  It has no contract either.

   procedure Witness_Stage_Selection (T, T1, T2, T3, T4 : Raw) is
   begin
      Witness (Offset_165.F (T, T1));
      Witness (Offset_166.F (T, Pos_Sum_165_165.F (T1, T2)));
      Witness (Offset_165x3.F (T, Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2)));
      Witness (Offset_167.F (T, Pos_Sum_165x3_165.F (Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2), T3)));
      Witness (Offset_165x5.F (T, Pos_Sum_167_165.F (Pos_Sum_165x3_165.F (Pos_Scale_3_165.F (T1), T2), T3)));
      Witness
        (Offset_166x3.F
           (T, Pos_Sum_165x5_165.F (Pos_Sum_165x3_166.F (Pos_Scale_3_165.F (T1), Pos_Scale_2_165.F (T2)), T3)));
      Witness
        (Offset_165x7.F
           (T, Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3)));
      Witness
        (Offset_168.F
           (T,
            Pos_Sum_165x7_165.F
              (Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4)));
      Witness
        (Offset_165x9.F
           (T,
            Pos_Sum_168_165.F
              (Pos_Sum_165x7_165.F (Pos_Sum_165x5_166.F (Pos_Scale_5_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4)));
      Witness
        (Offset_166x5.F
           (T,
            Pos_Sum_165x9_165.F
              (Pos_Sum_168_165.F (Pos_Sum_165x5_165x3.F (Pos_Scale_5_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4)));
      Witness
        (Offset_165x11.F
           (T,
            Pos_Sum_166x5_165.F
              (Pos_Sum_165x9_165.F (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4)));
      Witness
        (Offset_167x3.F
           (T,
            Pos_Sum_165x11_165.F
              (Pos_Sum_165x9_166.F
                 (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
               T4)));
      Witness
        (Offset_165x13.F
           (T,
            Pos_Sum_167x3_165.F
              (Pos_Sum_166x5_166.F
                 (Pos_Sum_165x7_165x3.F (Pos_Scale_7_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
               T4)));
      Witness
        (Offset_166x7.F
           (T,
            Pos_Sum_165x13_165.F
              (Pos_Sum_165x11_166.F
                 (Pos_Sum_165x7_167.F (Pos_Scale_7_165.F (T1), Pos_Scale_4_165.F (T2)), Pos_Scale_2_165.F (T3)),
               T4)));
   end Witness_Stage_Selection;

   ------------------------------------------------------------------------------------------------------------------
   --  How close the end formulas are to the exact ramp
   --
   --  Still ghost. The end formulas are a chain of rounded sums and products of the phase times, the crackle and
   --  the starting velocity. The lemmas of Trajectory.Rounding take a bound of the rounding through one
   --  operation of such a chain at a time: each operation adds Rounding.Step to the part of the bound which is
   --  relative to the magnitude of the terms, and each product carries the absolute part of a factor's bound,
   --  which is where a product was too small to be normal, through the largest magnitude of the other factor. The
   --  lemmas below apply them operation by operation: to the three sums of phase times which the change of speed
   --  is a product of, to that product, to the duration, and then to each formula as a whole. The velocity is
   --  fourteen operations deep and the distance twenty-two, which is within Ramp_End_Slack, thirty-two times
   --  Rounding.Step.
   --
   --  The formulas are stated as their mirrors, so that the lemmas have the mirrors' proofs of absence of
   --  overflow, and the reals which the inputs denote are parameters of the lemmas, so that the exact values are
   --  polynomials in a few unknowns.
   ------------------------------------------------------------------------------------------------------------------

   P80  : constant Raw := 2#1.0#E80
   with Ghost;
   P500 : constant Raw := 2#1.0#E500
   with Ghost;
   --  Rounding_Crackle_Max and Rounding_Velocity_Max, without their dimensions. P100 is Rounding_Time_Max.

   package Ramp_End_Numbers
     with Ghost
   is
      Time_Limit     : constant := 2.0**100;
      Crackle_Limit  : constant := 2.0**80;
      Velocity_Limit : constant := 2.0**500;
      --  The reals which P100, P80 and P500 denote.

      Times_2  : constant := 2.0 * Time_Limit;
      Times_3  : constant := 3.0 * Time_Limit;
      Times_4  : constant := 4.0 * Time_Limit;
      Times_6  : constant := 6.0 * Time_Limit;
      Times_7  : constant := 7.0 * Time_Limit;
      Times_8  : constant := 8.0 * Time_Limit;
      Times_12 : constant := 12.0 * Time_Limit;
      Times_14 : constant := 14.0 * Time_Limit;
      Times_15 : constant := 15.0 * Time_Limit;
      --  The largest sums of so many phase times.

      Snap_Limit         : constant := 2.0**180;
      Jerk_Limit         : constant := 2.0**281;
      Acceleration_Limit : constant := 2.0**383;
      Gain_Limit         : constant := 2.0**486;
      Half_Gain_Limit    : constant := 2.0**485;
      Mean_Limit         : constant := 2.0**501;
      --  The largest magnitudes of the partial products of the change of speed, of half of it, and of the mean
      --  speed.

      Sum_Residue_1 : constant := 2.0**(-1563);
      Sum_Residue_2 : constant := 2.0**(-1560);
      Sum_Residue_3 : constant := 2.0**(-1557);
      --  The absolute part of the bound of a sum of phase times which is so many sums beyond a multiple of a
      --  phase time, whose own is Scale_Residue.

      Snap_Residue         : constant := 2.0**(-1074);
      Jerk_Residue         : constant := 2.0**(-971);
      Acceleration_Residue : constant := 2.0**(-867);
      Gain_Residue         : constant := 2.0**(-762);
      Velocity_Residue     : constant := 2.0**(-760);
      Half_Residue         : constant := 2.0**(-761);
      Mean_Residue         : constant := 2.0**(-759);
      Distance_Residue     : constant := 2.0**(-653);
      --  The absolute parts of the bounds of the partial products, of the final velocity, of half the change of
      --  speed, of the mean speed and of the distance: each more than twice the absolute part of each operand
      --  times the largest magnitude of the other.
   end Ramp_End_Numbers;

   use Ramp_End_Numbers;
   use Prunt.Motion_Planner.Trajectory.Rounding;
   --  A named number stands for a real only where it is named directly.

   function Rounding_Times_OK (T1, T2, T3, T4 : Raw) return Boolean
   is (Within_Nonneg (T1, P100)
       and then Within_Nonneg (T2, P100)
       and then Within_Nonneg (T3, P100)
       and then Within_Nonneg (T4, P100))
   with Ghost;

   function Rounding_Inputs_OK (T1, T2, T3, T4, Cm, Vs : Raw) return Boolean
   is (Rounding_Times_OK (T1, T2, T3, T4) and then Within (Cm, P80) and then Within (Vs, P500))
   with Ghost;
   --  Rounding_Is_Bounded, as the lemmas need it.

   function Mirrored_Gain (T1, T2, T3, T4, Cm : Raw) return Raw
   is (Mul_598_168.F
         (Mul_431_167.F
            (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
             Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)),
          Sum_165x7_165.Add (Sum_166x3_165.Add (Sum_167_166.Add (Scale_4_165.F (T1), Scale_2_165.F (T2)), T3), T4)))
   with Ghost, Pre => Ramp_Inputs_OK (T1, T2, T3, T4, Cm), Post => Within (Mirrored_Gain'Result, P766);
   --  The change of speed, as the mirrors of both end formulas have it.

   function Mirrored_Duration (T1, T2, T3, T4 : Raw) return Raw
   is (Sum_166x7_165.Add
         (Sum_167x3_166.Add (Sum_168_167.Add (Scale_8_165.F (T1), Scale_4_165.F (T2)), Scale_2_165.F (T3)), T4))
   with
     Ghost,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165) and then Within (T4, P165),
     Post => Within (Mirrored_Duration'Result, P165x15);
   --  The duration, as the mirror of Fast_Distance_At_Max_Time has it.

   procedure Lemma_Rounding_Limits
   with
     Ghost,
     Always_Terminates,
     Post =>
       Reals.R (P100) = Time_Limit and then Reals.R (P80) = Crackle_Limit and then Reals.R (P500) = Velocity_Limit
   is
   begin
      null;
   end Lemma_Rounding_Limits;

   procedure Lemma_Rounding_Time (T : Raw; A : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (T, P100) and then A = Reals.R (T),
     Post => A >= 0.0 and then A <= Time_Limit and then Rounding.Close (A, T, A, 0.0, 0.0)
   is
   begin
      Lemma_Rounding_Limits;
      Rounding.Lemma_Nonnegative (T);
      Rounding.Lemma_Order (T, P100);
      Rounding.Lemma_Exact (T, A);
   end Lemma_Rounding_Time;
   --  A phase time is exact, and is its own magnitude.

   procedure Lemma_Rounding_Input (F, Limit : Raw; X, Bound : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (F, Limit) and then X = Reals.R (F) and then Bound = Reals.R (Limit),
     Post => abs X <= Bound and then Rounding.Close (X, F, abs X, 0.0, 0.0)
   is
   begin
      Rounding.Lemma_Bounded (F, Limit);
      Rounding.Lemma_Exact (F, abs X);
   end Lemma_Rounding_Input;
   --  The crackle and the starting velocity are exact, and may have either sign.

   procedure Lemma_Factor_2 (T1, T2 : Raw; A1, A2 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (T1, P100)
       and then Within_Nonneg (T2, P100)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2),
     Post =>
       A1 >= 0.0
       and then A2 >= 0.0
       and then A1 + A2 <= Times_2
       and then Rounding.Close (A1 + A2, T1 + T2, A1 + A2, Slack_1, Scale_Residue)
   is
   begin
      Lemma_Rounding_Time (T1, A1);
      Lemma_Rounding_Time (T2, A2);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => A1,
         X2 => A2,
         F1 => T1,
         F2 => T2,
         M1 => A1,
         M2 => A2,
         B1 => Time_Limit,
         B2 => Time_Limit,
         U1 => 0.0,
         U2 => 0.0,
         A1 => 0.0,
         A2 => 0.0,
         U  => Slack_1,
         A  => Scale_Residue);
   end Lemma_Factor_2;
   --  T1 + T2, which the peak of jerk is the peak of snap times.

   procedure Lemma_Factor_3 (T1, T2, T3 : Raw; A1, A2, A3 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (T1, P100)
       and then Within_Nonneg (T2, P100)
       and then Within_Nonneg (T3, P100)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3),
     Post =>
       2.0 * A1 + A2 + A3 >= 0.0
       and then 2.0 * A1 + A2 + A3 <= Times_4
       and then Rounding.In_Range (2.0 * T1)
       and then Rounding.In_Range (2.0 * T1 + T2)
       and then Rounding.Close (2.0 * A1 + A2 + A3, 2.0 * T1 + T2 + T3, 2.0 * A1 + A2 + A3, Slack_3, Sum_Residue_2)
   is
   begin
      Lemma_Rounding_Time (T1, A1);
      Lemma_Rounding_Time (T2, A2);
      Lemma_Rounding_Time (T3, A3);
      Rounding.Lemma_Double (T1, Time_Limit);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 2.0 * A1,
         X2 => A2,
         F1 => 2.0 * T1,
         F2 => T2,
         M1 => 2.0 * A1,
         M2 => A2,
         B1 => Times_2,
         B2 => Time_Limit,
         U1 => Slack_1,
         U2 => 0.0,
         A1 => Scale_Residue,
         A2 => 0.0,
         U  => Slack_2,
         A  => Sum_Residue_1);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 2.0 * A1 + A2,
         X2 => A3,
         F1 => 2.0 * T1 + T2,
         F2 => T3,
         M1 => 2.0 * A1 + A2,
         M2 => A3,
         B1 => Times_3,
         B2 => Time_Limit,
         U1 => Slack_2,
         U2 => 0.0,
         A1 => Sum_Residue_1,
         A2 => 0.0,
         U  => Slack_3,
         A  => Sum_Residue_2);
   end Lemma_Factor_3;
   --  2 T1 + T2 + T3, which the peak of acceleration is the peak of jerk times.

   procedure Lemma_Factor_4 (T1, T2, T3, T4 : Raw; A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Rounding_Times_OK (T1, T2, T3, T4)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4),
     Post =>
       4.0 * A1 + 2.0 * A2 + A3 + A4 >= 0.0
       and then 4.0 * A1 + 2.0 * A2 + A3 + A4 <= Times_8
       and then Rounding.In_Range (4.0 * T1)
       and then Rounding.In_Range (2.0 * T2)
       and then Rounding.In_Range (4.0 * T1 + 2.0 * T2)
       and then Rounding.In_Range (4.0 * T1 + 2.0 * T2 + T3)
       and then
         Rounding.Close
           (4.0 * A1 + 2.0 * A2 + A3 + A4,
            4.0 * T1 + 2.0 * T2 + T3 + T4,
            4.0 * A1 + 2.0 * A2 + A3 + A4,
            Slack_5,
            Sum_Residue_3)
   is
   begin
      Lemma_Rounding_Time (T1, A1);
      Lemma_Rounding_Time (T2, A2);
      Lemma_Rounding_Time (T3, A3);
      Lemma_Rounding_Time (T4, A4);
      Rounding.Lemma_Quadruple (T1, Time_Limit);
      Rounding.Lemma_Double (T2, Time_Limit);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 4.0 * A1,
         X2 => 2.0 * A2,
         F1 => 4.0 * T1,
         F2 => 2.0 * T2,
         M1 => 4.0 * A1,
         M2 => 2.0 * A2,
         B1 => Times_4,
         B2 => Times_2,
         U1 => Slack_2,
         U2 => Slack_1,
         A1 => Scale_Residue,
         A2 => Scale_Residue,
         U  => Slack_3,
         A  => Sum_Residue_1);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 4.0 * A1 + 2.0 * A2,
         X2 => A3,
         F1 => 4.0 * T1 + 2.0 * T2,
         F2 => T3,
         M1 => 4.0 * A1 + 2.0 * A2,
         M2 => A3,
         B1 => Times_6,
         B2 => Time_Limit,
         U1 => Slack_3,
         U2 => 0.0,
         A1 => Sum_Residue_1,
         A2 => 0.0,
         U  => Slack_4,
         A  => Sum_Residue_2);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 4.0 * A1 + 2.0 * A2 + A3,
         X2 => A4,
         F1 => 4.0 * T1 + 2.0 * T2 + T3,
         F2 => T4,
         M1 => 4.0 * A1 + 2.0 * A2 + A3,
         M2 => A4,
         B1 => Times_7,
         B2 => Time_Limit,
         U1 => Slack_4,
         U2 => 0.0,
         A1 => Sum_Residue_2,
         A2 => 0.0,
         U  => Slack_5,
         A  => Sum_Residue_3);
   end Lemma_Factor_4;
   --  4 T1 + 2 T2 + T3 + T4, which the change of speed is the peak of acceleration times.

   procedure Lemma_Duration (T1, T2, T3, T4 : Raw; A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Rounding_Times_OK (T1, T2, T3, T4)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4),
     Post =>
       8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4 >= 0.0
       and then 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4 <= Times_15
       and then Rounding.In_Range (8.0 * T1)
       and then Rounding.In_Range (4.0 * T2)
       and then Rounding.In_Range (8.0 * T1 + 4.0 * T2)
       and then Rounding.In_Range (2.0 * T3)
       and then Rounding.In_Range (8.0 * T1 + 4.0 * T2 + 2.0 * T3)
       and then
         Rounding.Close
           (8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4,
            8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4,
            8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4,
            Slack_6,
            Sum_Residue_3)
   is
   begin
      Lemma_Rounding_Time (T1, A1);
      Lemma_Rounding_Time (T2, A2);
      Lemma_Rounding_Time (T3, A3);
      Lemma_Rounding_Time (T4, A4);
      Rounding.Lemma_Octuple (T1, Time_Limit);
      Rounding.Lemma_Quadruple (T2, Time_Limit);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 8.0 * A1,
         X2 => 4.0 * A2,
         F1 => 8.0 * T1,
         F2 => 4.0 * T2,
         M1 => 8.0 * A1,
         M2 => 4.0 * A2,
         B1 => Times_8,
         B2 => Times_4,
         U1 => Slack_3,
         U2 => Slack_2,
         A1 => Scale_Residue,
         A2 => Scale_Residue,
         U  => Slack_4,
         A  => Sum_Residue_1);
      Rounding.Lemma_Double (T3, Time_Limit);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 8.0 * A1 + 4.0 * A2,
         X2 => 2.0 * A3,
         F1 => 8.0 * T1 + 4.0 * T2,
         F2 => 2.0 * T3,
         M1 => 8.0 * A1 + 4.0 * A2,
         M2 => 2.0 * A3,
         B1 => Times_12,
         B2 => Times_2,
         U1 => Slack_4,
         U2 => Slack_1,
         A1 => Sum_Residue_1,
         A2 => Scale_Residue,
         U  => Slack_5,
         A  => Sum_Residue_2);
      Rounding.Lemma_Sum_Nonnegative
        (X1 => 8.0 * A1 + 4.0 * A2 + 2.0 * A3,
         X2 => A4,
         F1 => 8.0 * T1 + 4.0 * T2 + 2.0 * T3,
         F2 => T4,
         M1 => 8.0 * A1 + 4.0 * A2 + 2.0 * A3,
         M2 => A4,
         B1 => Times_14,
         B2 => Time_Limit,
         U1 => Slack_5,
         U2 => 0.0,
         A1 => Sum_Residue_2,
         A2 => 0.0,
         U  => Slack_6,
         A  => Sum_Residue_3);
   end Lemma_Duration;
   --  8 T1 + 4 T2 + 2 T3 + T4, the duration.

   procedure Lemma_Gain_Rounding (T1, T2, T3, T4, Cm : Raw; C, A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Ramp_Inputs_OK (T1, T2, T3, T4, Cm)
       and then Rounding_Times_OK (T1, T2, T3, T4)
       and then Within (Cm, P80)
       and then C = Reals.R (Cm)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4),
     Post =>
       A1 >= 0.0
       and then A2 >= 0.0
       and then A3 >= 0.0
       and then A4 >= 0.0
       and then abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4) <= Gain_Limit
       and then
         Rounding.Close
           (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4),
            Mirrored_Gain (T1, T2, T3, T4, Cm),
            abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4),
            Slack_13,
            Gain_Residue)
   is
      Snap_Peak  : constant Raw := Mul_100_165.F (Cm, T1);
      Factor_2   : constant Raw := Sum_165_165.Add (T1, T2);
      Jerk_Peak  : constant Raw := Mul_265_166.F (Snap_Peak, Factor_2);
      Factor_3   : constant Raw := Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3);
      Accel_Peak : constant Raw := Mul_431_167.F (Jerk_Peak, Factor_3);
      Factor_4   : constant Raw :=
        Sum_165x7_165.Add (Sum_166x3_165.Add (Sum_167_166.Add (Scale_4_165.F (T1), Scale_2_165.F (T2)), T3), T4);
      --  The values which the mirror computes on the way: products of the crackle and sums of phase times.
   begin
      Lemma_Rounding_Limits;
      Lemma_Rounding_Input (Cm, P80, C, Crackle_Limit);
      Lemma_Rounding_Time (T1, A1);
      Lemma_Rounding_Time (T2, A2);
      Lemma_Rounding_Time (T3, A3);
      Lemma_Rounding_Time (T4, A4);
      Rounding.Lemma_Product
        (X1 => C,
         X2 => A1,
         F1 => Cm,
         F2 => T1,
         M1 => abs C,
         M2 => A1,
         B1 => Crackle_Limit,
         B2 => Time_Limit,
         U1 => 0.0,
         U2 => 0.0,
         A1 => 0.0,
         A2 => 0.0,
         U  => Slack_1,
         A  => Snap_Residue);
      pragma Assert (Rounding.Close (C * A1, Snap_Peak, abs C * A1, Slack_1, Snap_Residue));

      Lemma_Factor_2 (T1, T2, A1, A2);
      pragma Assert (Rounding.Close (A1 + A2, Factor_2, A1 + A2, Slack_1, Scale_Residue));
      Rounding.Lemma_Product
        (X1 => C * A1,
         X2 => A1 + A2,
         F1 => Snap_Peak,
         F2 => Factor_2,
         M1 => abs C * A1,
         M2 => A1 + A2,
         B1 => Snap_Limit,
         B2 => Times_2,
         U1 => Slack_1,
         U2 => Slack_1,
         A1 => Snap_Residue,
         A2 => Scale_Residue,
         U  => Slack_3,
         A  => Jerk_Residue);
      pragma Assert (Rounding.Close (C * A1 * (A1 + A2), Jerk_Peak, abs C * A1 * (A1 + A2), Slack_3, Jerk_Residue));

      Lemma_Factor_3 (T1, T2, T3, A1, A2, A3);
      pragma Assert (Rounding.Close (2.0 * A1 + A2 + A3, Factor_3, 2.0 * A1 + A2 + A3, Slack_3, Sum_Residue_2));
      Rounding.Lemma_Product
        (X1 => C * A1 * (A1 + A2),
         X2 => 2.0 * A1 + A2 + A3,
         F1 => Jerk_Peak,
         F2 => Factor_3,
         M1 => abs C * A1 * (A1 + A2),
         M2 => 2.0 * A1 + A2 + A3,
         B1 => Jerk_Limit,
         B2 => Times_4,
         U1 => Slack_3,
         U2 => Slack_3,
         A1 => Jerk_Residue,
         A2 => Sum_Residue_2,
         U  => Slack_7,
         A  => Acceleration_Residue);
      pragma
        Assert
          (Rounding.Close
             (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3),
              Accel_Peak,
              abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3),
              Slack_7,
              Acceleration_Residue));

      Lemma_Factor_4 (T1, T2, T3, T4, A1, A2, A3, A4);
      pragma
        Assert
          (Rounding.Close
             (4.0 * A1 + 2.0 * A2 + A3 + A4, Factor_4, 4.0 * A1 + 2.0 * A2 + A3 + A4, Slack_5, Sum_Residue_3));
      Rounding.Lemma_Product
        (X1 => C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3),
         X2 => 4.0 * A1 + 2.0 * A2 + A3 + A4,
         F1 => Accel_Peak,
         F2 => Factor_4,
         M1 => abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3),
         M2 => 4.0 * A1 + 2.0 * A2 + A3 + A4,
         B1 => Acceleration_Limit,
         B2 => Times_8,
         U1 => Slack_7,
         U2 => Slack_5,
         A1 => Acceleration_Residue,
         A2 => Sum_Residue_3,
         U  => Slack_13,
         A  => Gain_Residue);
      pragma Assert (Mirrored_Gain (T1, T2, T3, T4, Cm) = Accel_Peak * Factor_4);
      Rounding.Lemma_Same (Mirrored_Gain (T1, T2, T3, T4, Cm), Accel_Peak * Factor_4);
   end Lemma_Gain_Rounding;
   --  The change of speed: each product of what came before and a sum of phase times.

   procedure Lemma_Abs_Scaled (X, Y : Reals.Real)
   with Ghost, Always_Terminates, Pre => Y >= 0.0, Post => abs (X * Y) = abs X * Y
   is
   begin
      null;
   end Lemma_Abs_Scaled;

   procedure Lemma_Gain_Magnitude (C, A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => A1 >= 0.0 and then A2 >= 0.0 and then A3 >= 0.0 and then A4 >= 0.0,
     Post =>
       abs (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4))
       = abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4)
   is
   begin
      Lemma_Abs_Scaled (C, A1);
      Lemma_Abs_Scaled (C * A1, A1 + A2);
      Lemma_Abs_Scaled (C * A1 * (A1 + A2), 2.0 * A1 + A2 + A3);
      Lemma_Abs_Scaled (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3), 4.0 * A1 + 2.0 * A2 + A3 + A4);
   end Lemma_Gain_Magnitude;

   procedure Lemma_Ramp_End_Model (Exact : Profile_Model.Ramp; C, A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Exact.C = C
       and then Exact.T1 = A1
       and then Exact.T2 = A2
       and then Exact.T3 = A3
       and then Exact.T4 = A4
       and then A1 >= 0.0
       and then A2 >= 0.0
       and then A3 >= 0.0
       and then A4 >= 0.0,
     Post =>
       Profile_Model.Velocity_Gain (Exact)
       = C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4)
       and then
         abs Profile_Model.Velocity_Gain (Exact)
         = abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4)
       and then Profile_Model.Total_Time (Exact) = 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4
   is
   begin
      Lemma_Gain_Magnitude (C, A1, A2, A3, A4);
      pragma
        Assert
          (Profile_Model.Velocity_Gain (Exact)
           = C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4));
   end Lemma_Ramp_End_Model;
   --  The change of speed and the duration of the exact ramp, in terms of what its components are.

   procedure Lemma_Travel_Model (Exact : Profile_Model.Ramp; V, Gain, Magnitude, Ramp_Duration : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Exact.Vs = V
       and then Profile_Model.Velocity_Gain (Exact) = Gain
       and then abs Profile_Model.Velocity_Gain (Exact) = Magnitude
       and then Profile_Model.Total_Time (Exact) = Ramp_Duration,
     Post =>
       Profile_Model.Travel (Exact) = (V + Gain / 2.0) * Ramp_Duration
       and then Travel_Magnitude (Exact) = (abs V + Magnitude / 2.0) * Ramp_Duration
   is
   begin
      null;
   end Lemma_Travel_Model;
   --  The travel of the exact ramp and the magnitude of its terms, in terms of what its change of speed and
   --  duration are.

   procedure Lemma_Velocity_Rounding (T1, T2, T3, T4, Cm, Vs, Result : Raw; Exact : Profile_Model.Ramp)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Ramp_Inputs_OK (T1, T2, T3, T4, Cm)
       and then Within (Vs, P800)
       and then Result = Start_766.F (Vs, Sym_766.F (Mirrored_Gain (T1, T2, T3, T4, Cm)))
       and then Exact.T1 = Reals.R (T1)
       and then Exact.T2 = Reals.R (T2)
       and then Exact.T3 = Reals.R (T3)
       and then Exact.T4 = Reals.R (T4)
       and then Exact.C = Reals.R (Cm)
       and then Exact.Vs = Reals.R (Vs),
     Post =>
       (if Rounding_Inputs_OK (T1, T2, T3, T4, Cm, Vs)
        then
          abs (Reals.R (Result) - (Reals.R (Vs) + Profile_Model.Velocity_Gain (Exact)))
          <= Ramp_End_Slack * Velocity_Magnitude (Exact) + Ramp_End_Velocity_Residue)
   is
      C  : constant Reals.Real := Reals.R (Cm);
      A1 : constant Reals.Real := Reals.R (T1);
      A2 : constant Reals.Real := Reals.R (T2);
      A3 : constant Reals.Real := Reals.R (T3);
      A4 : constant Reals.Real := Reals.R (T4);
      V  : constant Reals.Real := Reals.R (Vs);
      --  The reals which the inputs denote.

      Exact_Gain     : constant Reals.Real :=
        C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4);
      Gain_Magnitude : constant Reals.Real :=
        abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4);

      Gain : constant Raw := Mirrored_Gain (T1, T2, T3, T4, Cm);
   begin
      if not Rounding_Inputs_OK (T1, T2, T3, T4, Cm, Vs) then
         return;
      end if;

      Lemma_Rounding_Limits;
      Lemma_Gain_Rounding (T1, T2, T3, T4, Cm, C, A1, A2, A3, A4);
      Lemma_Rounding_Input (Vs, P500, V, Velocity_Limit);
      Rounding.Lemma_Sum
        (X1 => V,
         X2 => Exact_Gain,
         F1 => Vs,
         F2 => Gain,
         M1 => abs V,
         M2 => Gain_Magnitude,
         B1 => Velocity_Limit,
         B2 => Gain_Limit,
         U1 => 0.0,
         U2 => Slack_13,
         A1 => 0.0,
         A2 => Gain_Residue,
         U  => Slack_14,
         A  => Velocity_Residue);
      pragma Assert (Result = Vs + Gain);
      Rounding.Lemma_Same (Result, Vs + Gain);
      Rounding.Lemma_Meets
        (X       => V + Exact_Gain,
         F       => Result,
         M       => abs V + Gain_Magnitude,
         U       => Slack_14,
         A       => Velocity_Residue,
         Slack   => Ramp_End_Slack,
         Residue => Ramp_End_Velocity_Residue);
      Lemma_Ramp_End_Model (Exact, C, A1, A2, A3, A4);
      pragma Assert (Velocity_Magnitude (Exact) = abs V + Gain_Magnitude);
   end Lemma_Velocity_Rounding;
   --  Result is the final velocity as the mirror of Fast_Velocity_At_Max_Time computes it, and Exact the ramp
   --  which the inputs denote.

   procedure Lemma_Distance_Rounding (T1, T2, T3, T4, Cm, Vs, Result : Raw; Exact : Profile_Model.Ramp)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Ramp_Inputs_OK (T1, T2, T3, T4, Cm)
       and then Within (Vs, P800)
       and then
         Result
         = Mul_797x9_165x15.F
             (Sum_800_765.Add (Vs, Div_766_By_2.F (Mirrored_Gain (T1, T2, T3, T4, Cm))),
              Mirrored_Duration (T1, T2, T3, T4))
       and then Exact.T1 = Reals.R (T1)
       and then Exact.T2 = Reals.R (T2)
       and then Exact.T3 = Reals.R (T3)
       and then Exact.T4 = Reals.R (T4)
       and then Exact.C = Reals.R (Cm)
       and then Exact.Vs = Reals.R (Vs),
     Post =>
       (if Rounding_Inputs_OK (T1, T2, T3, T4, Cm, Vs)
        then
          abs (Reals.R (Result) - Profile_Model.Travel (Exact))
          <= Ramp_End_Slack * Travel_Magnitude (Exact) + Ramp_End_Distance_Residue)
   is
      C  : constant Reals.Real := Reals.R (Cm);
      A1 : constant Reals.Real := Reals.R (T1);
      A2 : constant Reals.Real := Reals.R (T2);
      A3 : constant Reals.Real := Reals.R (T3);
      A4 : constant Reals.Real := Reals.R (T4);
      V  : constant Reals.Real := Reals.R (Vs);
      --  The reals which the inputs denote.

      Exact_Gain     : constant Reals.Real :=
        C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4);
      Gain_Magnitude : constant Reals.Real :=
        abs C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4);
      Exact_Duration : constant Reals.Real := 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4;

      Gain          : constant Raw := Mirrored_Gain (T1, T2, T3, T4, Cm);
      Ramp_Duration : constant Raw := Mirrored_Duration (T1, T2, T3, T4);
   begin
      if not Rounding_Inputs_OK (T1, T2, T3, T4, Cm, Vs) then
         return;
      end if;

      Lemma_Rounding_Limits;
      Lemma_Gain_Rounding (T1, T2, T3, T4, Cm, C, A1, A2, A3, A4);
      Rounding.Lemma_In_Range (Exact_Gain, Gain, Gain_Magnitude, Gain_Limit, Slack_13, Gain_Residue);
      Rounding.Lemma_Half
        (X  => Exact_Gain,
         F  => Gain,
         M  => Gain_Magnitude,
         B  => Gain_Limit,
         U  => Slack_13,
         A  => Gain_Residue,
         UH => Slack_14,
         AH => Half_Residue);
      Lemma_Rounding_Input (Vs, P500, V, Velocity_Limit);
      Rounding.Lemma_Sum
        (X1 => V,
         X2 => Exact_Gain / 2.0,
         F1 => Vs,
         F2 => Gain / 2.0,
         M1 => abs V,
         M2 => Gain_Magnitude / 2.0,
         B1 => Velocity_Limit,
         B2 => Half_Gain_Limit,
         U1 => 0.0,
         U2 => Slack_14,
         A1 => 0.0,
         A2 => Half_Residue,
         U  => Slack_15,
         A  => Mean_Residue);
      Lemma_Duration (T1, T2, T3, T4, A1, A2, A3, A4);
      pragma Assert (Rounding.Close (Exact_Duration, Ramp_Duration, Exact_Duration, Slack_6, Sum_Residue_3));
      Rounding.Lemma_Product
        (X1 => V + Exact_Gain / 2.0,
         X2 => Exact_Duration,
         F1 => Vs + Gain / 2.0,
         F2 => Ramp_Duration,
         M1 => abs V + Gain_Magnitude / 2.0,
         M2 => Exact_Duration,
         B1 => Mean_Limit,
         B2 => Times_15,
         U1 => Slack_15,
         U2 => Slack_6,
         A1 => Mean_Residue,
         A2 => Sum_Residue_3,
         U  => Slack_22,
         A  => Distance_Residue);
      pragma Assert (Result = (Vs + Gain / 2.0) * Ramp_Duration);
      Rounding.Lemma_Same (Result, (Vs + Gain / 2.0) * Ramp_Duration);
      Rounding.Lemma_Meets
        (X       => (V + Exact_Gain / 2.0) * Exact_Duration,
         F       => Result,
         M       => (abs V + Gain_Magnitude / 2.0) * Exact_Duration,
         U       => Slack_22,
         A       => Distance_Residue,
         Slack   => Ramp_End_Slack,
         Residue => Ramp_End_Distance_Residue);
      Lemma_Ramp_End_Model (Exact, C, A1, A2, A3, A4);
      Lemma_Travel_Model (Exact, V, Exact_Gain, Gain_Magnitude, Exact_Duration);
   end Lemma_Distance_Rounding;
   --  Result is the distance as the mirror of Fast_Distance_At_Max_Time computes it, and Exact the ramp which
   --  the inputs denote.

   function Distance_Formula (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is (Widen_966x9_970.F
         (Mul_797x9_165x15.F
            (Sum_800_765.Add (Start_Vel, Div_766_By_2.F (Mirrored_Gain (T1, T2, T3, T4, Max_Crackle))),
             Mirrored_Duration (T1, T2, T3, T4)))
       * mm);

   function Velocity_Formula (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is (Start_766.F (Start_Vel, Sym_766.F (Mirrored_Gain (T1, T2, T3, T4, Max_Crackle))) * (mm / s));
   --  The mirrors of the two end formulas, which is what makes them free of overflow. A mirror has no dimension,
   --  and the product with the unit, which is one, gives it its own: a conversion cannot.

   procedure Lemma_Unit (X : Raw) with Ghost, Always_Terminates, Post => X * 1.0 = X is
   begin
      null;
   end Lemma_Unit;
   --  A product with a unit is the value itself.

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
      Vs : constant Velocity := Start_Vel;

      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Within (Vs, P800));
      Mirror : constant Raw :=
        Widen_966x9_970.F
          (Mul_797x9_165x15.F
             (Sum_800_765.Add
                (Vs,
                 Div_766_By_2.F
                   (Mul_598_168.F
                      (Mul_431_167.F
                         (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                          Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)),
                       Sum_165x7_165.Add
                         (Sum_166x3_165.Add (Sum_167_166.Add (Scale_4_165.F (T1), Scale_2_165.F (T2)), T3), T4)))),
              Sum_166x7_165.Add
                (Sum_167x3_166.Add (Sum_168_167.Add (Scale_8_165.F (T1), Scale_4_165.F (T2)), Scale_2_165.F (T3)),
                 T4)))
      with Ghost;
      pragma Assert (Within (Mirror, P970));

      pragma Suppress (Overflow_Check);
      --  Every operation below reaches the result through sums and products only, so one which overflows, or meets
      --  a NaN, leaves the result an infinity or a NaN. Testing the result once therefore raises for exactly the
      --  inputs for which a check on each operation would. Every ramp certificate evaluates this.
      Result : constant Length :=
        (Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4) / 2.0)
        * (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
      --  Symbolically equivalent to: Distance_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   begin
      Lemma_Distance_Rounding (T1, T2, T3, T4, Cm, Vs, Result, To_Ramp (Profile, Max_Crackle, Start_Vel));
      Lemma_Unit (Mirror);
      pragma Assert (Result = Distance_Formula (T1, T2, T3, T4, Cm, Vs));
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Fast_Distance_At_Max_Time;

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
      Vs : constant Velocity := Start_Vel;

      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Within (Vs, P800));
      Mirror : constant Raw :=
        Start_766.F
          (Vs,
           Sym_766.F
             (Mul_598_168.F
                (Mul_431_167.F
                   (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                    Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)),
                 Sum_165x7_165.Add
                   (Sum_166x3_165.Add (Sum_167_166.Add (Scale_4_165.F (T1), Scale_2_165.F (T2)), T3), T4))))
      with Ghost;
      pragma Assert (Near (Mirror, abs Vs, P767));

      pragma Suppress (Overflow_Check);
      --  As in Fast_Distance_At_Max_Time.
      Result : constant Velocity := Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4);
      --  Symbolically equivalent to: Velocity_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   begin
      Lemma_Velocity_Rounding (T1, T2, T3, T4, Cm, Vs, Result, To_Ramp (Profile, Max_Crackle, Start_Vel));
      Lemma_Unit (Mirror);
      pragma Assert (Result = Velocity_Formula (T1, T2, T3, T4, Cm, Vs));
      if not (abs Result <= Velocity'Last) then
         raise Constraint_Error with "Unrepresentable ramp velocity.";
      end if;
      return Result;
   end Fast_Velocity_At_Max_Time;

   ------------------------------------------------------------------------------------------------------------------
   --  The end formulas to within a relative bound
   --
   --  Still ghost. The same chains as above, taken through the lemmas of Trajectory.Rounding.Relative: each
   --  partial product of the change of speed is at least the product of the least crackle and the least first
   --  phase time of a normal ramp, so that no product underflows, and each bound is relative.
   ------------------------------------------------------------------------------------------------------------------

   PM170 : constant Raw := 2#1.0#E-170
   with Ghost;
   PM90  : constant Raw := 2#1.0#E-90
   with Ghost;
   --  Normal_Time_Min and Normal_Crackle_Min, without their dimensions.

   L101 : constant Raw := 2#1.0#E101
   with Ghost;
   L267 : constant Raw := 2#1.0#E267
   with Ghost;
   L434 : constant Raw := 2#1.0#E434
   with Ghost;
   L407 : constant Raw := 2#1.0#E407
   with Ghost;
   --  Limits which the partial products of a normal ramp are within.

   package Normal_Numbers
     with Ghost
   is
      Time_Floor    : constant := 2.0**(-170);
      Crackle_Floor : constant := 2.0**(-90);
      Snap_Floor    : constant := Crackle_Floor * Time_Floor;
      Jerk_Floor    : constant := Snap_Floor * Time_Floor;
      Accel_Floor   : constant := Jerk_Floor * 2.0 * Time_Floor;
      Gain_Floor    : constant := Accel_Floor * 4.0 * Time_Floor;
      Half_Floor    : constant := Gain_Floor / 2.0;
      Span_Floor    : constant := 8.0 * Time_Floor;
      --  The least exact values of the crackle, the first phase time, the partial products of the change of
      --  speed, half of it, and the duration.

      Time_Top    : constant := 2.0**165;
      Crackle_Top : constant := 2.0**100;
      Snap_Top    : constant := 2.0**100;
      Jerk_Top    : constant := Snap_Top * 2.0**166;
      Accel_Top   : constant := Jerk_Top * 2.0**167;
      --  The largest ones, for phase times up to Phase_Time_Max.

      Short_Top       : constant := 2.0**100;
      Short_Jerk_Top  : constant := Snap_Top * 2.0**101;
      Short_Accel_Top : constant := Short_Jerk_Top * 2.0**102;
      Short_Gain_Top  : constant := Short_Accel_Top * 2.0**103;
      --  The largest ones for phase times up to Rounding_Time_Max.

      Real_L101 : constant := 2.0**101;
      Real_L267 : constant := 2.0**267;
      Real_L434 : constant := 2.0**434;
      Real_L407 : constant := 2.0**407;

      Units_1  : constant := 1.0 * 2.0**(-52);
      Units_2  : constant := 2.0 * 2.0**(-52);
      Units_3  : constant := 3.0 * 2.0**(-52);
      Units_7  : constant := 7.0 * 2.0**(-52);
      Units_8  : constant := 8.0 * 2.0**(-52);
      Units_9  : constant := 9.0 * 2.0**(-52);
      Units_10 : constant := 10.0 * 2.0**(-52);
      Units_11 : constant := 11.0 * 2.0**(-52);
      Units_16 : constant := 16.0 * 2.0**(-52);
      Units_17 : constant := 17.0 * 2.0**(-52);
      Units_18 : constant := 18.0 * 2.0**(-52);
      Units_19 : constant := 19.0 * 2.0**(-52);
      Units_20 : constant := 20.0 * 2.0**(-52);
      Units_21 : constant := 21.0 * 2.0**(-52);
      Units_41 : constant := 41.0 * 2.0**(-52);
      --  The relative part of a bound after so many times Relative.Unit.
   end Normal_Numbers;

   use Normal_Numbers;

   function Normal_Inputs_OK (T1, T2, T3, T4, Cm : Raw) return Boolean
   is (Ramp_Inputs_OK (T1, T2, T3, T4, Cm)
       and then T1 >= PM170
       and then T2 >= 0.0
       and then T3 >= 0.0
       and then T4 >= 0.0
       and then Cm >= PM90)
   with Ghost;
   --  Gain_Is_Normal, but for the peak snap, as the lemmas need it.

   function Short_Inputs_OK (T1, T2, T3, T4, Vs : Raw) return Boolean
   is (T1 <= P100 and then T2 <= P100 and then T3 <= P100 and then T4 <= P100 and then Vs >= 0.0 and then Vs <= P100)
   with Ghost;
   --  What Travel_Is_Normal adds.

   procedure Lemma_Normal_Constants
   with
     Ghost,
     Always_Terminates,
     Post =>
       Reals.R (PM170) = Time_Floor
       and then Reals.R (PM90) = Crackle_Floor
       and then Reals.R (P165) = Time_Top
       and then Reals.R (P100) = Short_Top
       and then Reals.R (L101) = Real_L101
       and then Reals.R (L267) = Real_L267
       and then Reals.R (L434) = Real_L434
       and then Reals.R (L407) = Real_L407
       and then Reals.R (2.0) = 2.0
       and then Reals.R (4.0) = 4.0
       and then Reals.R (8.0) = 8.0
   is
   begin
      null;
   end Lemma_Normal_Constants;

   procedure Lemma_Normal_Inputs (T1, T2, T3, T4, Cm : Raw; C, A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Normal_Inputs_OK (T1, T2, T3, T4, Cm)
       and then C = Reals.R (Cm)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4),
     Post =>
       C >= Crackle_Floor
       and then C <= Crackle_Top
       and then A1 >= Time_Floor
       and then A1 <= Time_Top
       and then A2 >= 0.0
       and then A2 <= Time_Top
       and then A3 >= 0.0
       and then A3 <= Time_Top
       and then A4 >= 0.0
       and then A4 <= Time_Top
       and then Relative.Near (C, Cm, 0.0)
       and then Relative.Near (A1, T1, 0.0)
       and then Relative.Near (A2, T2, 0.0)
       and then Relative.Near (A3, T3, 0.0)
       and then Relative.Near (A4, T4, 0.0)
   is
   begin
      Lemma_Normal_Constants;
      Rounding.Lemma_Order (PM90, Cm);
      Rounding.Lemma_Order (Cm, P100);
      Rounding.Lemma_Order (PM170, T1);
      Rounding.Lemma_Order (T1, P165);
      Rounding.Lemma_Order (T2, P165);
      Rounding.Lemma_Order (T3, P165);
      Rounding.Lemma_Order (T4, P165);
      pragma Assert (Reals.R (P100) = Crackle_Top);
      Relative.Lemma_Near_Exact (Cm);
      Relative.Lemma_Near_Exact (T1);
      Relative.Lemma_Near_Exact (T2);
      Relative.Lemma_Near_Exact (T3);
      Relative.Lemma_Near_Exact (T4);
   end Lemma_Normal_Inputs;
   --  The inputs of a normal ramp are exact and within the floors and tops above.

   function Same (X, Y : Raw) return Boolean
   is (X = Y)
   with Ghost;
   --  Equality of two values whatever their dimensions.

   procedure Lemma_Zero_Start (X : Raw) with Ghost, Always_Terminates, Post => 0.0 + X = X is
   begin
      null;
   end Lemma_Zero_Start;
   --  A sum with a starting speed of zero is the value itself.

   procedure Lemma_Gain_Chain (T1, T2, T3, T4, Cm : Raw; C, A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Normal_Inputs_OK (T1, T2, T3, T4, Cm)
       and then C = Reals.R (Cm)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4)
       and then C * A1 <= Snap_Top,
     Post =>
       Relative.Near
         (C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4),
          Mirrored_Gain (T1, T2, T3, T4, Cm),
          Units_19)
       and then C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4) >= Gain_Floor
       and then
         (if A1 <= Short_Top and then A2 <= Short_Top and then A3 <= Short_Top and then A4 <= Short_Top
          then C * A1 * (A1 + A2) * (2.0 * A1 + A2 + A3) * (4.0 * A1 + 2.0 * A2 + A3 + A4) <= Short_Gain_Top)
   is
      Snap_Peak  : constant Raw := Mul_100_165.F (Cm, T1);
      Factor_2   : constant Raw := Sum_165_165.Add (T1, T2);
      Jerk_Peak  : constant Raw := Mul_265_166.F (Snap_Peak, Factor_2);
      Twice_1    : constant Raw := Scale_2_165.F (T1);
      Sum_112    : constant Raw := Sum_166_165.Add (Twice_1, T2);
      Factor_3   : constant Raw := Sum_165x3_165.Add (Sum_112, T3);
      Accel_Peak : constant Raw := Mul_431_167.F (Jerk_Peak, Factor_3);
      Four_1     : constant Raw := Scale_4_165.F (T1);
      Twice_2    : constant Raw := Scale_2_165.F (T2);
      Sum_1122   : constant Raw := Sum_167_166.Add (Four_1, Twice_2);
      Sum_11223  : constant Raw := Sum_166x3_165.Add (Sum_1122, T3);
      Factor_4   : constant Raw := Sum_165x7_165.Add (Sum_11223, T4);
      --  The values which the mirror computes on the way.

      B  : constant Reals.Real := A1 + A2;
      Cc : constant Reals.Real := 2.0 * A1 + A2 + A3;
      D  : constant Reals.Real := 4.0 * A1 + 2.0 * A2 + A3 + A4;
      --  The exact sums of phase times.

      S : constant Reals.Real := C * A1;
      J : constant Reals.Real := S * B;
      A : constant Reals.Real := J * Cc;
      --  The exact peaks.
   begin
      Lemma_Normal_Constants;
      Lemma_Normal_Inputs (T1, T2, T3, T4, Cm, C, A1, A2, A3, A4);

      --  Snap.
      Relative.Lemma_Product_Monotone (Crackle_Floor, C, Time_Floor, A1);
      pragma Assert (S >= Snap_Floor);
      Relative.Lemma_Near_Product (C, A1, Cm, T1, 0.0, 0.0, Units_1);
      pragma Assert (Relative.Near (S, Snap_Peak, Units_1));
      Relative.Lemma_Near_Bounded (S, Snap_Peak, L101, Units_1);

      --  Jerk: the sum of the first two phase times, and its product with the peak of snap.
      Relative.Lemma_Near_Sum (A1, A2, T1, T2, 0.0, 0.0, Units_1);
      pragma Assert (Relative.Near (B, Factor_2, Units_1));
      Relative.Lemma_Product_Monotone (Snap_Floor, S, Time_Floor, B);
      pragma Assert (J >= Jerk_Floor);
      Relative.Lemma_Near_Product (S, B, Snap_Peak, Factor_2, Units_1, Units_1, Units_3);
      pragma Assert (Relative.Near (J, Jerk_Peak, Units_3));
      Relative.Lemma_Product_Monotone (S, Snap_Top, B, 2.0 * Time_Top);
      pragma Assert (J <= Jerk_Top);
      Relative.Lemma_Near_Bounded (J, Jerk_Peak, L267, Units_3);

      --  Acceleration: twice the first phase time, two sums, and the product with the peak of jerk.
      Relative.Lemma_Near_Exact (2.0);
      Relative.Lemma_Near_Product (2.0, A1, 2.0, T1, 0.0, 0.0, Units_1);
      pragma Assert (Relative.Near (2.0 * A1, Twice_1, Units_1));
      Relative.Lemma_Near_Sum (2.0 * A1, A2, Twice_1, T2, Units_1, 0.0, Units_2);
      pragma Assert (Relative.Near (2.0 * A1 + A2, Sum_112, Units_2));
      Relative.Lemma_Near_Sum (2.0 * A1 + A2, A3, Sum_112, T3, Units_2, 0.0, Units_3);
      pragma Assert (Relative.Near (Cc, Factor_3, Units_3));
      Relative.Lemma_Product_Monotone (Jerk_Floor, J, 2.0 * Time_Floor, Cc);
      pragma Assert (A >= Accel_Floor);
      Relative.Lemma_Near_Product (J, Cc, Jerk_Peak, Factor_3, Units_3, Units_3, Units_7);
      pragma Assert (Relative.Near (A, Accel_Peak, Units_7));
      Relative.Lemma_Product_Monotone (J, Jerk_Top, Cc, 4.0 * Time_Top);
      pragma Assert (A <= Accel_Top);
      Relative.Lemma_Near_Bounded (A, Accel_Peak, L434, Units_7);

      --  The last factor: four times the first phase time, twice the second, which may be tiny, and two sums.
      Relative.Lemma_Near_Exact (4.0);
      Relative.Lemma_Near_Product (4.0, A1, 4.0, T1, 0.0, 0.0, Units_1);
      pragma Assert (Relative.Near (4.0 * A1, Four_1, Units_1));
      Relative.Lemma_Almost_From_Near (4.0 * A1, Four_1, Units_1);
      Relative.Lemma_Almost_Double (T2);
      pragma Assert (Relative.Almost (2.0 * A2, Twice_2, Units_8));
      Relative.Lemma_Near_Sum_Almost (4.0 * A1, 2.0 * A2, Four_1, Twice_2, Units_1, Units_8, Units_9);
      pragma Assert (Relative.Near (4.0 * A1 + 2.0 * A2, Sum_1122, Units_9));
      Relative.Lemma_Near_Sum (4.0 * A1 + 2.0 * A2, A3, Sum_1122, T3, Units_9, 0.0, Units_10);
      pragma Assert (Relative.Near (4.0 * A1 + 2.0 * A2 + A3, Sum_11223, Units_10));
      Relative.Lemma_Near_Sum (4.0 * A1 + 2.0 * A2 + A3, A4, Sum_11223, T4, Units_10, 0.0, Units_11);
      pragma Assert (Relative.Near (D, Factor_4, Units_11));

      --  The change of speed.
      Relative.Lemma_Product_Monotone (Accel_Floor, A, 4.0 * Time_Floor, D);
      pragma Assert (A * D >= Gain_Floor);
      Relative.Lemma_Near_Product (A, D, Accel_Peak, Factor_4, Units_7, Units_11, Units_19);
      pragma Assert (Mirrored_Gain (T1, T2, T3, T4, Cm) = Accel_Peak * Factor_4);
      Relative.Lemma_Near_Same (A * D, Accel_Peak * Factor_4, Mirrored_Gain (T1, T2, T3, T4, Cm), Units_19);

      if A1 <= Short_Top and then A2 <= Short_Top and then A3 <= Short_Top and then A4 <= Short_Top then
         Relative.Lemma_Product_Monotone (S, Snap_Top, B, 2.0 * Short_Top);
         pragma Assert (J <= Short_Jerk_Top);
         Relative.Lemma_Product_Monotone (J, Short_Jerk_Top, Cc, 4.0 * Short_Top);
         pragma Assert (A <= Short_Accel_Top);
         Relative.Lemma_Product_Monotone (A, Short_Accel_Top, D, 8.0 * Short_Top);
         pragma Assert (A * D <= Short_Gain_Top);
      end if;
   end Lemma_Gain_Chain;
   --  The change of speed of a normal ramp: each product of what came before and a sum of phase times.

   procedure Lemma_Duration_Chain (T1, T2, T3, T4 : Raw; A1, A2, A3, A4 : Reals.Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (T1, P100)
       and then T1 >= PM170
       and then Within_Nonneg (T2, P100)
       and then Within_Nonneg (T3, P100)
       and then Within_Nonneg (T4, P100)
       and then A1 = Reals.R (T1)
       and then A2 = Reals.R (T2)
       and then A3 = Reals.R (T3)
       and then A4 = Reals.R (T4),
     Post =>
       Relative.Near (8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4, Mirrored_Duration (T1, T2, T3, T4), Units_19)
       and then 8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4 >= Span_Floor
   is
      Eight_1 : constant Raw := Scale_8_165.F (T1);
      Four_2  : constant Raw := Scale_4_165.F (T2);
      Sum_12  : constant Raw := Sum_168_167.Add (Eight_1, Four_2);
      Twice_3 : constant Raw := Scale_2_165.F (T3);
      Sum_123 : constant Raw := Sum_167x3_166.Add (Sum_12, Twice_3);
   begin
      Lemma_Normal_Constants;
      Rounding.Lemma_Order (PM170, T1);
      Rounding.Lemma_Nonnegative (T2);
      Rounding.Lemma_Nonnegative (T3);
      Rounding.Lemma_Nonnegative (T4);
      Relative.Lemma_Near_Exact (T1);
      Relative.Lemma_Near_Exact (T4);
      Relative.Lemma_Near_Exact (8.0);
      Relative.Lemma_Near_Product (8.0, A1, 8.0, T1, 0.0, 0.0, Units_1);
      pragma Assert (Relative.Near (8.0 * A1, Eight_1, Units_1));
      Relative.Lemma_Almost_From_Near (8.0 * A1, Eight_1, Units_1);
      Relative.Lemma_Almost_Quadruple (T2);
      pragma Assert (Relative.Almost (4.0 * A2, Four_2, Units_16));
      Relative.Lemma_Near_Sum_Almost (8.0 * A1, 4.0 * A2, Eight_1, Four_2, Units_1, Units_16, Units_17);
      pragma Assert (Relative.Near (8.0 * A1 + 4.0 * A2, Sum_12, Units_17));
      Relative.Lemma_Almost_From_Near (8.0 * A1 + 4.0 * A2, Sum_12, Units_17);
      Relative.Lemma_Almost_Double (T3);
      pragma Assert (Relative.Almost (2.0 * A3, Twice_3, Units_8));
      Relative.Lemma_Near_Sum_Almost (8.0 * A1 + 4.0 * A2, 2.0 * A3, Sum_12, Twice_3, Units_17, Units_8, Units_18);
      pragma Assert (Relative.Near (8.0 * A1 + 4.0 * A2 + 2.0 * A3, Sum_123, Units_18));
      Relative.Lemma_Near_Sum (8.0 * A1 + 4.0 * A2 + 2.0 * A3, A4, Sum_123, T4, Units_18, 0.0, Units_19);
      pragma Assert (Mirrored_Duration (T1, T2, T3, T4) = Sum_123 + T4);
      Relative.Lemma_Near_Same
        (8.0 * A1 + 4.0 * A2 + 2.0 * A3 + A4, Sum_123 + T4, Mirrored_Duration (T1, T2, T3, T4), Units_19);
   end Lemma_Duration_Chain;
   --  The duration of a normal ramp.

   procedure Lemma_Normal_Is_OK (T1, T2, T3, T4 : Time; Max_Crackle : Crackle)
   with
     Ghost,
     Always_Terminates,
     Pre  => Gain_Is_Normal (T1, T2, T3, T4, Max_Crackle),
     Post => Normal_Inputs_OK (T1, T2, T3, T4, Max_Crackle)
   is
   begin
      pragma Assert (Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165));
      pragma Assert (Within (T4, P165) and then Within (Max_Crackle, P100));
      pragma Assert (Within (T1, P169) and then Within (T2, P169) and then Within (T3, P169));
      pragma Assert (Within (T4, P169));
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Max_Crackle));
   end Lemma_Normal_Is_OK;
   --  A normal ramp is within the domain of the mirrors.

   procedure Lemma_Exact_Ramp_End (Times : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) is
   begin
      null;
   end Lemma_Exact_Ramp_End;

   procedure Lemma_Gain_Is_Near (T1, T2, T3, T4 : Time; Max_Crackle : Crackle) is
      C  : constant Reals.Real := Reals.R (Max_Crackle);
      A1 : constant Reals.Real := Reals.R (T1);
      A2 : constant Reals.Real := Reals.R (T2);
      A3 : constant Reals.Real := Reals.R (T3);
      A4 : constant Reals.Real := Reals.R (T4);

      Gain : constant Raw := Mirrored_Gain (T1, T2, T3, T4, Max_Crackle);
   begin
      Lemma_Normal_Is_OK (T1, T2, T3, T4, Max_Crackle);
      Lemma_Gain_Chain (T1, T2, T3, T4, Max_Crackle, C, A1, A2, A3, A4);
      Lemma_Zero_Start (Gain);
      Lemma_Unit (0.0 + Gain);
      pragma Assert (Same (Velocity_Formula (T1, T2, T3, T4, Max_Crackle, No_Velocity), (0.0 + Gain) * 1.0));
      Relative.Lemma_Near_Same
        (Exact_Gain (C, A1, A2, A3, A4), Gain, Velocity_Formula (T1, T2, T3, T4, Max_Crackle, No_Velocity), Units_19);
      Relative.Lemma_Near_Weaken
        (Exact_Gain (C, A1, A2, A3, A4),
         Velocity_Formula (T1, T2, T3, T4, Max_Crackle, No_Velocity),
         Units_19,
         Gain_Slack);
   end Lemma_Gain_Is_Near;

   procedure Lemma_Travel_Is_Near (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity) is
      C  : constant Reals.Real := Reals.R (Max_Crackle);
      V  : constant Reals.Real := Reals.R (Start_Vel);
      A1 : constant Reals.Real := Reals.R (T1);
      A2 : constant Reals.Real := Reals.R (T2);
      A3 : constant Reals.Real := Reals.R (T3);
      A4 : constant Reals.Real := Reals.R (T4);

      Exact         : constant Reals.Real := Exact_Gain (C, A1, A2, A3, A4);
      Span          : constant Reals.Real := Exact_Duration (A1, A2, A3, A4);
      Gain          : constant Raw := Mirrored_Gain (T1, T2, T3, T4, Max_Crackle);
      Half          : constant Raw := Div_766_By_2.F (Gain);
      Mean          : constant Raw := Sum_800_765.Add (Start_Vel, Half);
      Ramp_Duration : constant Raw := Mirrored_Duration (T1, T2, T3, T4);
   begin
      Lemma_Normal_Is_OK (T1, T2, T3, T4, Max_Crackle);
      pragma Assert (Short_Inputs_OK (T1, T2, T3, T4, Start_Vel));
      Lemma_Normal_Constants;
      Rounding.Lemma_Order (T1, P100);
      Rounding.Lemma_Order (T2, P100);
      Rounding.Lemma_Order (T3, P100);
      Rounding.Lemma_Order (T4, P100);
      Rounding.Lemma_Order (Start_Vel, P100);
      Rounding.Lemma_Nonnegative (Start_Vel);
      Lemma_Gain_Chain (T1, T2, T3, T4, Max_Crackle, C, A1, A2, A3, A4);
      pragma Assert (Exact >= Gain_Floor and then Exact <= Short_Gain_Top);
      Relative.Lemma_Near_Bounded (Exact, Gain, L407, Units_19);

      --  Half of the change of speed, and the mean speed.
      Relative.Lemma_Near_Exact (2.0);
      Relative.Lemma_Near_Quotient (Exact, 2.0, Exact / 2.0, Gain, 2.0, Units_19, 0.0, Units_20);
      pragma Assert (Relative.Near (Exact / 2.0, Half, Units_20));
      Relative.Lemma_Near_Bounded (Exact / 2.0, Half, L407, Units_20);
      Relative.Lemma_Near_Exact (Start_Vel);
      Relative.Lemma_Near_Sum (V, Exact / 2.0, Start_Vel, Half, 0.0, Units_20, Units_21);
      pragma Assert (Relative.Near (V + Exact / 2.0, Mean, Units_21));
      Relative.Lemma_Near_Bounded (V + Exact / 2.0, Mean, L407, Units_21);

      --  The duration, and the travel.
      Lemma_Duration_Chain (T1, T2, T3, T4, A1, A2, A3, A4);
      Relative.Lemma_Product_Monotone (Half_Floor, V + Exact / 2.0, Span_Floor, Span);
      Relative.Lemma_Near_Product (V + Exact / 2.0, Span, Mean, Ramp_Duration, Units_21, Units_19, Units_41);
      Lemma_Unit (Mean * Ramp_Duration);
      pragma Assert (Same (Distance_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel), (Mean * Ramp_Duration) * 1.0));
      Relative.Lemma_Near_Same
        (Exact_Travel (C, V, A1, A2, A3, A4),
         Mean * Ramp_Duration,
         Distance_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel),
         Units_41);
      Relative.Lemma_Near_Weaken
        (Exact_Travel (C, V, A1, A2, A3, A4),
         Distance_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel),
         Units_41,
         Travel_Slack);
   end Lemma_Travel_Is_Near;

   function Total_Time (Times : Feedrate_Profile_Times) return Time is
   begin
      return 8.0 * Times (1) + 4.0 * Times (2) + 2.0 * Times (3) + Times (4);
   end Total_Time;

   function Total_Time (Profile : Feedrate_Profile) return Time is
   begin
      return Total_Time (Profile.Accel) + Profile.Coast + Total_Time (Profile.Decel);
   end Total_Time;

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Ramp_Times_OK (T, T1, T2, T3, T4));
      pragma Assert (abs Cm <= abs Max_Crackle);
      pragma Assert (abs (-Cm) <= abs Max_Crackle);
      pragma Assert (abs (0.0 * mm / s**5) <= abs Max_Crackle);
      pragma Assert (Phase_Times_OK (T1, T2, T3, T4));
      Lemma_Stage_Starts (T1, T2, T3, T4);

      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Cm;
      elsif T < T1 + T2 then
         return 0.0 * mm / s**5;
      elsif T < 2.0 * T1 + T2 then
         return -Cm;
      elsif T < 2.0 * T1 + T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 3.0 * T1 + T2 + T3 then
         return -Cm;
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Cm;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return -Cm;
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Cm;
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Cm;
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      else
         return -Cm;
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Snap_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Snap
      with
        Subprogram_Variant => (Decreases => Stage),
        Pre                => Ramp_Inputs_OK (T1, T2, T3, T4, Cm) and then Within (DT, P169),
        Contract_Cases     =>
          (Stage = 1  => Within (Snap_At_Stage'Result, P269) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 2  => Within (Snap_At_Stage'Result, P269) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 3  => Within (Snap_At_Stage'Result, P270) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 4  => Within (Snap_At_Stage'Result, P270) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 5  => Within (Snap_At_Stage'Result, P269x3) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 6  => Within (Snap_At_Stage'Result, P269x3) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 7  => Within (Snap_At_Stage'Result, P271) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 8  => Within (Snap_At_Stage'Result, P271) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 9  => Within (Snap_At_Stage'Result, P269x5) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 10 => Within (Snap_At_Stage'Result, P269x5) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 11 => Within (Snap_At_Stage'Result, P270x3) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 12 => Within (Snap_At_Stage'Result, P270x3) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 13 => Within (Snap_At_Stage'Result, P269x7) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 14 => Within (Snap_At_Stage'Result, P269x7) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max,
           Stage = 15 => Within (Snap_At_Stage'Result, P272) and then abs Snap_At_Stage'Result <= Evaluated_Snap_Max);
      --  Return snap at offset DT within a feedrate profile stage.

      function Snap_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Snap is
      begin
         case Stage is
            when 1  =>
               Witness (Final_269_272.F (Mul_100_169.F (Cm, DT)));
               return Cm * DT;

            when 2  =>
               return Snap_At_Stage (T1, 1);

            when 3  =>
               Witness (Final_270_272.F (Sum_269_269.Sub (Snap_At_Stage (T2, 2), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T2, 2) - Cm * DT;

            when 4  =>
               return Snap_At_Stage (T1, 3);

            when 5  =>
               Witness (Final_269x3_272.F (Sum_270_269.Sub (Snap_At_Stage (T3, 4), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T3, 4) - Cm * DT;

            when 6  =>
               return Snap_At_Stage (T1, 5);

            when 7  =>
               Witness (Final_271_272.F (Sum_269x3_269.Add (Snap_At_Stage (T2, 6), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T2, 6) + Cm * DT;

            when 8  =>
               return Snap_At_Stage (T1, 7);

            when 9  =>
               Witness (Final_269x5_272.F (Sum_271_269.Sub (Snap_At_Stage (T4, 8), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T4, 8) - Cm * DT;

            when 10 =>
               return Snap_At_Stage (T1, 9);

            when 11 =>
               Witness (Final_270x3_272.F (Sum_269x5_269.Add (Snap_At_Stage (T2, 10), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T2, 10) + Cm * DT;

            when 12 =>
               return Snap_At_Stage (T1, 11);

            when 13 =>
               Witness (Final_269x7_272.F (Sum_270x3_269.Add (Snap_At_Stage (T3, 12), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T3, 12) + Cm * DT;

            when 14 =>
               return Snap_At_Stage (T1, 13);

            when 15 =>
               Witness (Final_272_272.F (Sum_269x7_269.Sub (Snap_At_Stage (T2, 14), Mul_100_169.F (Cm, DT))));
               return Snap_At_Stage (T2, 14) - Cm * DT;
         end case;
      end Snap_At_Stage;

   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Ramp_Times_OK (T, T1, T2, T3, T4));
      Witness_Stage_Selection (T, T1, T2, T3, T4);

      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Snap_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Snap_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Snap_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Snap_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Jerk_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Jerk
      with
        Subprogram_Variant => (Decreases => Stage),
        Pre                => Ramp_Inputs_OK (T1, T2, T3, T4, Cm) and then Within (DT, P169),
        Contract_Cases     =>
          (Stage = 1  => Within (Jerk_At_Stage'Result, P437) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 2  => Within (Jerk_At_Stage'Result, P434x9) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 3  => Within (Jerk_At_Stage'Result, P435x9) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 4  => Within (Jerk_At_Stage'Result, P435x9) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 5  =>
             Within (Jerk_At_Stage'Result, P435x13) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 6  => Within (Jerk_At_Stage'Result, P436x7) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 7  => Within (Jerk_At_Stage'Result, P437x5) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 8  => Within (Jerk_At_Stage'Result, P437x5) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 9  => Within (Jerk_At_Stage'Result, P438x3) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 10 =>
             Within (Jerk_At_Stage'Result, P436x13) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 11 => Within (Jerk_At_Stage'Result, P440) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 12 => Within (Jerk_At_Stage'Result, P440) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 13 => Within (Jerk_At_Stage'Result, P437x9) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 14 => Within (Jerk_At_Stage'Result, P438x5) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max,
           Stage = 15 =>
             Within (Jerk_At_Stage'Result, P439x3) and then abs Jerk_At_Stage'Result <= Evaluated_Jerk_Max);
      --  Return jerk at offset DT within a feedrate profile stage.

      function Jerk_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Jerk is
      begin
         case Stage is
            when 1  =>
               Witness (Final_437_441.F (Div_438_By_2.F (Mul_100_338.F (Cm, Sqr_169.F (DT)))));
               return Cm * DT**2 / 2.0;

            when 2  =>
               Witness
                 (Final_434x9_441.F
                    (Sum_437_434.Add (Jerk_At_Stage (T1, 1), Mul_269_165.F (Mul_100_169.F (Cm, DT), T1))));
               return Jerk_At_Stage (T1, 1) + Cm * DT * T1;

            when 3  =>
               Witness
                 (Final_435x9_441.F
                    (Sum_434x9_434x9.Add
                       (Jerk_At_Stage (T2, 2),
                        Div_435x9_By_2.F
                          (Mul_269_166x9.F
                             (Mul_100_169.F (Cm, DT), Sum_169_166.Add (Neg_169.F (DT), Scale_2_165.F (T1)))))));
               return Jerk_At_Stage (T2, 2) + Cm * DT * (-DT + 2.0 * T1) / 2.0;

            when 4  =>
               return Jerk_At_Stage (T1, 3);

            when 5  =>
               Witness
                 (Final_435x13_441.F
                    (Sum_435x9_437.Sub (Jerk_At_Stage (T3, 4), Div_438_By_2.F (Mul_100_338.F (Cm, Sqr_169.F (DT))))));
               return Jerk_At_Stage (T3, 4) - Cm * DT**2 / 2.0;

            when 6  =>
               Witness
                 (Final_436x7_441.F
                    (Sum_435x13_434.Sub (Jerk_At_Stage (T1, 5), Mul_269_165.F (Mul_100_169.F (Cm, DT), T1))));
               return Jerk_At_Stage (T1, 5) - Cm * DT * T1;

            when 7  =>
               Witness
                 (Final_437x5_441.F
                    (Sum_436x7_434x9.Add
                       (Jerk_At_Stage (T2, 6),
                        Div_435x9_By_2.F
                          (Mul_269_166x9.F (Mul_100_169.F (Cm, DT), Sum_169_166.Sub (DT, Scale_2_165.F (T1)))))));
               return Jerk_At_Stage (T2, 6) + Cm * DT * (DT - 2.0 * T1) / 2.0;

            when 8  =>
               return Jerk_At_Stage (T1, 7);

            when 9  =>
               Witness
                 (Final_438x3_441.F
                    (Sum_437x5_437.Sub (Jerk_At_Stage (T4, 8), Div_438_By_2.F (Mul_100_338.F (Cm, Sqr_169.F (DT))))));
               return Jerk_At_Stage (T4, 8) - Cm * DT**2 / 2.0;

            when 10 =>
               Witness
                 (Final_436x13_441.F
                    (Sum_438x3_434.Sub (Jerk_At_Stage (T1, 9), Mul_269_165.F (Mul_100_169.F (Cm, DT), T1))));
               return Jerk_At_Stage (T1, 9) - Cm * DT * T1;

            when 11 =>
               Witness
                 (Final_440_441.F
                    (Sum_436x13_434x9.Add
                       (Jerk_At_Stage (T2, 10),
                        Div_435x9_By_2.F
                          (Mul_269_166x9.F (Mul_100_169.F (Cm, DT), Sum_169_166.Sub (DT, Scale_2_165.F (T1)))))));
               return Jerk_At_Stage (T2, 10) + Cm * DT * (DT - 2.0 * T1) / 2.0;

            when 12 =>
               return Jerk_At_Stage (T1, 11);

            when 13 =>
               Witness
                 (Final_437x9_441.F
                    (Sum_440_437.Add (Jerk_At_Stage (T3, 12), Div_438_By_2.F (Mul_100_338.F (Cm, Sqr_169.F (DT))))));
               return Jerk_At_Stage (T3, 12) + Cm * DT**2 / 2.0;

            when 14 =>
               Witness
                 (Final_438x5_441.F
                    (Sum_437x9_434.Add (Jerk_At_Stage (T1, 13), Mul_269_165.F (Mul_100_169.F (Cm, DT), T1))));
               return Jerk_At_Stage (T1, 13) + Cm * DT * T1;

            when 15 =>
               Witness
                 (Final_439x3_441.F
                    (Sum_438x5_434x9.Add
                       (Jerk_At_Stage (T2, 14),
                        Div_435x9_By_2.F
                          (Mul_269_166x9.F
                             (Mul_100_169.F (Cm, DT), Sum_169_166.Add (Neg_169.F (DT), Scale_2_165.F (T1)))))));
               return Jerk_At_Stage (T2, 14) + Cm * DT * (-DT + 2.0 * T1) / 2.0;
         end case;
      end Jerk_At_Stage;

   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Ramp_Times_OK (T, T1, T2, T3, T4));
      Witness_Stage_Selection (T, T1, T2, T3, T4);

      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Jerk_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Jerk_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Jerk_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Jerk_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Acceleration_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Acceleration
      with
        Subprogram_Variant => (Decreases => Stage),
        Pre                => Ramp_Inputs_OK (T1, T2, T3, T4, Cm) and then Within (DT, P169),
        Contract_Cases     =>
          (Stage = 1  =>
             Within (Acceleration_At_Stage'Result, P601x11)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 2  =>
             Within (Acceleration_At_Stage'Result, P602x7)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 3  =>
             Within (Acceleration_At_Stage'Result, P602x15)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 4  =>
             Within (Acceleration_At_Stage'Result, P606)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 5  =>
             Within (Acceleration_At_Stage'Result, P603x11)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 6  =>
             Within (Acceleration_At_Stage'Result, P605x3)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 7  =>
             Within (Acceleration_At_Stage'Result, P607)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 8  =>
             Within (Acceleration_At_Stage'Result, P607)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 9  =>
             Within (Acceleration_At_Stage'Result, P605x5)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 10 =>
             Within (Acceleration_At_Stage'Result, P604x11)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 11 =>
             Within (Acceleration_At_Stage'Result, P604x13)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 12 =>
             Within (Acceleration_At_Stage'Result, P605x7)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 13 =>
             Within (Acceleration_At_Stage'Result, P608)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 14 =>
             Within (Acceleration_At_Stage'Result, P605x9)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max,
           Stage = 15 =>
             Within (Acceleration_At_Stage'Result, P606x5)
             and then abs Acceleration_At_Stage'Result <= Evaluated_Acceleration_Max);
      --  Return acceleration at offset DT within a feedrate profile stage.

      function Acceleration_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Acceleration is
      begin
         case Stage is
            when 1  =>
               Witness (Final_601x11_609.F (Div_607_By_6.F (Mul_100_507.F (Cm, Cub_169.F (DT)))));
               return Cm * DT**3 / 6.0;

            when 2  =>
               Witness
                 (Final_602x7_609.F
                    (Sum_601x11_599x9.Add
                       (Acceleration_At_Stage (T1, 1),
                        Div_600x9_By_2.F
                          (Mul_434_166x9.F (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1), Sum_169_165.Add (DT, T1))))));
               return Acceleration_At_Stage (T1, 1) + Cm * DT * T1 * (DT + T1) / 2.0;

            when 3  =>
               Witness
                 (Final_602x15_609.F
                    (Sum_602x7_601x15.Add
                       (Acceleration_At_Stage (T2, 2),
                        Div_604x11_By_6.F
                          (Mul_269_335x11.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_336x5_330x9.Add
                                (Sum_338_334x3.Add
                                   (Neg_338.F (Sqr_169.F (DT)), Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                 Mul_165x3_165x3.F
                                   (Scale_3_165.F (T1), Sum_165_166.Add (T1, Scale_2_165.F (T2)))))))));
               return
                 Acceleration_At_Stage (T2, 2) + Cm * DT * (-DT**2 + 3.0 * DT * T1 + 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;

            when 4  =>
               Witness
                 (Final_606_609.F
                    (Sum_602x15_600.Add
                       (Acceleration_At_Stage (T1, 3),
                        Mul_434_166.F (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1), Sum_165_165.Add (T1, T2)))));
               return Acceleration_At_Stage (T1, 3) + Cm * DT * T1 * (T1 + T2);

            when 5  =>
               Witness
                 (Final_603x11_609.F
                    (Sum_606_603x3.Add
                       (Acceleration_At_Stage (T3, 4),
                        Div_604x9_By_6.F
                          (Mul_269_335x9.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_338_332x3.Add
                                (Neg_338.F (Sqr_169.F (DT)),
                                 Mul_166x3_166.F (Scale_6_165.F (T1), Sum_165_165.Add (T1, T2))))))));
               return Acceleration_At_Stage (T3, 4) + Cm * DT * (-DT**2 + 6.0 * T1 * (T1 + T2)) / 6.0;

            when 6  =>
               Witness
                 (Final_605x3_609.F
                    (Sum_603x11_600x5.Add
                       (Acceleration_At_Stage (T1, 5),
                        Div_601x5_By_2.F
                          (Mul_434_167x5.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_166x9_166.Add (Sum_169_165.Add (Neg_169.F (DT), T1), Scale_2_165.F (T2)))))));
               return Acceleration_At_Stage (T1, 5) + Cm * DT * T1 * (-DT + T1 + 2.0 * T2) / 2.0;

            when 7  =>
               Witness
                 (Final_607_609.F
                    (Sum_605x3_601x15.Add
                       (Acceleration_At_Stage (T2, 6),
                        Div_604x11_By_6.F
                          (Mul_269_335x11.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_336x5_330x3.Add
                                (Sum_338_334x3.Sub (Sqr_169.F (DT), Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                 Scale_3_330.F (Sqr_165.F (T1))))))));
               return Acceleration_At_Stage (T2, 6) + Cm * DT * (DT**2 - 3.0 * DT * T1 + 3.0 * T1**2) / 6.0;

            when 8  =>
               return Acceleration_At_Stage (T1, 7);

            when 9  =>
               Witness
                 (Final_605x5_609.F
                    (Sum_607_601x11.Sub
                       (Acceleration_At_Stage (T4, 8), Div_607_By_6.F (Mul_100_507.F (Cm, Cub_169.F (DT))))));
               return Acceleration_At_Stage (T4, 8) - Cm * DT**3 / 6.0;

            when 10 =>
               Witness
                 (Final_604x11_609.F
                    (Sum_605x5_599x9.Add
                       (Acceleration_At_Stage (T1, 9),
                        Div_600x9_By_2.F
                          (Mul_434_166x9.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1), Sum_169_165.Sub (Neg_169.F (DT), T1))))));
               return Acceleration_At_Stage (T1, 9) + Cm * DT * T1 * (-DT - T1) / 2.0;

            when 11 =>
               Witness
                 (Final_604x13_609.F
                    (Sum_604x11_601x15.Add
                       (Acceleration_At_Stage (T2, 10),
                        Div_604x11_By_6.F
                          (Mul_269_335x11.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_336x5_330x9.Sub
                                (Sum_338_334x3.Sub (Sqr_169.F (DT), Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                 Mul_165x3_165x3.F
                                   (Scale_3_165.F (T1), Sum_165_166.Add (T1, Scale_2_165.F (T2)))))))));
               return
                 Acceleration_At_Stage (T2, 10) + Cm * DT * (DT**2 - 3.0 * DT * T1 - 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;

            when 12 =>
               Witness
                 (Final_605x7_609.F
                    (Sum_604x13_600.Sub
                       (Acceleration_At_Stage (T1, 11),
                        Mul_434_166.F (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1), Sum_165_165.Add (T1, T2)))));
               return Acceleration_At_Stage (T1, 11) - Cm * DT * T1 * (T1 + T2);

            when 13 =>
               Witness
                 (Final_608_609.F
                    (Sum_605x7_603x3.Add
                       (Acceleration_At_Stage (T3, 12),
                        Div_604x9_By_6.F
                          (Mul_269_335x9.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_338_332x3.Sub
                                (Sqr_169.F (DT), Mul_166x3_166.F (Scale_6_165.F (T1), Sum_165_165.Add (T1, T2))))))));
               return Acceleration_At_Stage (T3, 12) + Cm * DT * (DT**2 - 6.0 * T1 * (T1 + T2)) / 6.0;

            when 14 =>
               Witness
                 (Final_605x9_609.F
                    (Sum_608_600x5.Add
                       (Acceleration_At_Stage (T1, 13),
                        Div_601x5_By_2.F
                          (Mul_434_167x5.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_166x9_166.Sub (Sum_169_165.Sub (DT, T1), Scale_2_165.F (T2)))))));
               return Acceleration_At_Stage (T1, 13) + Cm * DT * T1 * (DT - T1 - 2.0 * T2) / 2.0;

            when 15 =>
               Witness
                 (Final_606x5_609.F
                    (Sum_605x9_601x15.Add
                       (Acceleration_At_Stage (T2, 14),
                        Div_604x11_By_6.F
                          (Mul_269_335x11.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_336x5_330x3.Sub
                                (Sum_338_334x3.Add
                                   (Neg_338.F (Sqr_169.F (DT)), Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                 Scale_3_330.F (Sqr_165.F (T1))))))));
               return Acceleration_At_Stage (T2, 14) + Cm * DT * (-DT**2 + 3.0 * DT * T1 - 3.0 * T1**2) / 6.0;
         end case;
      end Acceleration_At_Stage;

   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Ramp_Times_OK (T, T1, T2, T3, T4));
      Witness_Stage_Selection (T, T1, T2, T3, T4);

      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Acceleration_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Acceleration_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Acceleration_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Acceleration_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Velocity_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Velocity
      with
        Subprogram_Variant => (Decreases => Stage),
        Pre                =>
          Ramp_Inputs_OK (T1, T2, T3, T4, Cm) and then Within (DT, P169) and then Within_Nonneg (abs Start_Vel, P800),
        Contract_Cases     =>
          (Stage = 1  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P769x11)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 2  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P769x15)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 3  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P774)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 4  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P771x9)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 5  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P771x13)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 6  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P771x15)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 7  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P773x5)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 8  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P772x11)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 9  =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P772x13)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 10 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P773x7)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 11 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P776)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 12 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P773x9)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 13 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P774x5)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 14 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P773x11)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max,
           Stage = 15 =>
             Near (Velocity_At_Stage'Result, abs Start_Vel, P775x3)
             and then abs Velocity_At_Stage'Result <= abs Start_Vel + Evaluated_Gain_Max);
      --  Return velocity at offset DT within a feedrate profile stage.

      function Velocity_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Velocity is
      begin
         case Stage is
            when 1  =>
               Witness
                 (Start_768x11.F (Start_Vel, Sym_768x11.F (Div_776_By_24.F (Mul_100_676.F (Cm, Fth_169.F (DT))))));
               return Start_Vel + Cm * (DT**2)**2 / 24.0;

            when 2  =>
               Witness
                 (Step_769x11_767x7.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 1),
                     Sym_767x7.F
                       (Div_771x5_By_12.F
                          (Mul_434_337x5.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_336x9_331.Add
                                (Sum_339_334x3.Add
                                   (Scale_2_338.F (Sqr_169.F (DT)), Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                 Scale_2_330.F (Sqr_165.F (T1))))))));
               return Velocity_At_Stage (T1, 1) + Cm * DT * T1 * (2.0 * DT**2 + 3.0 * DT * T1 + 2.0 * T1**2) / 12.0;

            when 3  =>
               Witness
                 (Step_769x15_772.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T2, 2),
                     Sym_772.F
                       (Div_775x3_By_24.F
                          (Mul_269_506x3.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x11_497x7.Add
                                (Sum_505x5_500x9.Add
                                   (Sum_507_505.Add
                                      (Neg_507.F (Cub_169.F (DT)), Mul_340_165.F (Scale_4_338.F (Sqr_169.F (DT)), T1)),
                                    Mul_335x3_165x3.F
                                      (Mul_170x3_165.F (Scale_6_169.F (DT), T1),
                                       Sum_165_166.Add (T1, Scale_2_165.F (T2)))),
                                 Mul_167_330x7.F
                                   (Scale_4_165.F (T1),
                                    Sum_332_330x3.Add
                                      (Sum_330_330x3.Add (Sqr_165.F (T1), Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                       Scale_3_330.F (Sqr_165.F (T2))))))))));
               return
                 Velocity_At_Stage (T2, 2)
                 + Cm
                   * DT
                   * (-DT**3 + 4.0 * DT**2 * T1 + 6.0 * DT * T1 * (T1 + 2.0 * T2)
                      + 4.0 * T1 * (T1**2 + 3.0 * T1 * T2 + 3.0 * T2**2))
                   / 24.0;

            when 4  =>
               Witness
                 (Step_774_765x11.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 3),
                     Sym_765x11.F
                       (Div_766x11_By_2.F
                          (Mul_434_332x11.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_333x5_330.Add
                                (Sum_332x9_330x3.Add
                                   (Sum_335_331.Add
                                      (Mul_169_166.F (DT, Sum_165_165.Add (T1, T2)), Scale_2_330.F (Sqr_165.F (T1))),
                                    Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                 Sqr_165.F (T2)))))));
               return
                 Velocity_At_Stage (T1, 3)
                 + Cm * DT * T1 * (DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 2.0;

            when 5  =>
               Witness
                 (Step_771x9_769x7.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T3, 4),
                     Sym_769x7.F
                       (Div_774x5_By_24.F
                          (Mul_269_505x5.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x9_498x15.Add
                                (Sum_507_502x3.Add
                                   (Neg_507.F (Cub_169.F (DT)),
                                    Mul_336x3_166.F
                                      (Mul_171x3_165.F (Scale_12_169.F (DT), T1), Sum_165_165.Add (T1, T2))),
                                 Mul_167x3_331x5.F
                                   (Scale_12_165.F (T1),
                                    Sum_333_331.Add
                                      (Sum_330x7_330.Add
                                         (Sum_330x5_331.Add
                                            (Sum_331_330x3.Add
                                               (Scale_2_330.F (Sqr_165.F (T1)),
                                                Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                             Mul_166_165.F (Scale_2_165.F (T1), T3)),
                                          Sqr_165.F (T2)),
                                       Mul_166_165.F (Scale_2_165.F (T2), T3)))))))));
               return
                 Velocity_At_Stage (T3, 4)
                 + Cm
                   * DT
                   * (-DT**3 + 12.0 * DT * T1 * (T1 + T2)
                      + 12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3))
                   / 24.0;

            when 6  =>
               Witness
                 (Step_771x13_767x11.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 5),
                     Sym_767x11.F
                       (Div_774_By_12.F
                          (Mul_434_340.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_336x15_332x3.Add
                                (Sum_337x7_331x3.Add
                                   (Sum_336x13_332x3.Add
                                      (Sum_338x3_331x15.Add
                                         (Sum_336x11_331x11.Add
                                            (Sum_339_334x9.Add
                                               (Neg_339.F (Scale_2_338.F (Sqr_169.F (DT))),
                                                Mul_169x3_165x3.F
                                                  (Scale_3_169.F (DT), Sum_165_166.Add (T1, Scale_2_165.F (T2)))),
                                             Scale_22_330.F (Sqr_165.F (T1))),
                                          Mul_166x15_165.F (Scale_30_165.F (T1), T2)),
                                       Mul_167x3_165.F (Scale_12_165.F (T1), T3)),
                                    Scale_6_330.F (Sqr_165.F (T2))),
                                 Mul_167x3_165.F (Scale_12_165.F (T2), T3)))))));
               return
                 Velocity_At_Stage (T1, 5)
                 + Cm
                   * DT
                   * T1
                   * (-2.0 * DT**2 + 3.0 * DT * (T1 + 2.0 * T2) + 22.0 * T1**2 + 30.0 * T1 * T2 + 12.0 * T1 * T3
                      + 6.0 * T2**2
                      + 12.0 * T2 * T3)
                   / 12.0;

            when 7  =>
               Witness
                 (Step_771x15_772.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T2, 6),
                     Sym_772.F
                       (Div_775x3_By_24.F
                          (Mul_269_506x3.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x11_499x13.Add
                                (Sum_505x5_500x3.Add
                                   (Sum_507_505.Sub
                                      (Cub_169.F (DT), Mul_340_165.F (Scale_4_338.F (Sqr_169.F (DT)), T1)),
                                    Mul_170x3_330.F (Scale_6_169.F (DT), Sqr_165.F (T1))),
                                 Mul_167_332x13.F
                                   (Scale_4_165.F (T1),
                                    Sum_332x11_331x3.Add
                                      (Sum_332x9_331x3.Add
                                         (Sum_331x15_331x3.Add
                                            (Sum_330x11_331x9.Add
                                               (Scale_11_330.F (Sqr_165.F (T1)),
                                                Mul_166x9_165.F (Scale_18_165.F (T1), T2)),
                                             Mul_166x3_165.F (Scale_6_165.F (T1), T3)),
                                          Scale_6_330.F (Sqr_165.F (T2))),
                                       Mul_166x3_165.F (Scale_6_165.F (T2), T3)))))))));
               return
                 Velocity_At_Stage (T2, 6)
                 + Cm
                   * DT
                   * (DT**3 - 4.0 * DT**2 * T1 + 6.0 * DT * T1**2
                      + 4.0 * T1 * (11.0 * T1**2 + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * T2**2 + 6.0 * T2 * T3))
                   / 24.0;

            when 8  =>
               Witness
                 (Step_773x5_767.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 7),
                     Sym_767.F
                       (Mul_434_333.F
                          (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                           Sum_330x7_330.Add
                             (Sum_331x3_330.Add
                                (Sum_330x5_330.Add
                                   (Sum_331_330x3.Add
                                      (Scale_2_330.F (Sqr_165.F (T1)), Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                    Mul_165_165.F (T1, T3)),
                                 Sqr_165.F (T2)),
                              Mul_165_165.F (T2, T3))))));
               return
                 Velocity_At_Stage (T1, 7) + Cm * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3);

            when 9  =>
               Witness
                 (Step_772x11_770x3.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T4, 8),
                     Sym_770x3.F
                       (Div_773x9_By_24.F
                          (Mul_269_504x9.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_507_501x3.Add
                                (Neg_507.F (Cub_169.F (DT)),
                                 Mul_168x3_333.F
                                   (Scale_24_165.F (T1),
                                    Sum_330x7_330.Add
                                      (Sum_331x3_330.Add
                                         (Sum_330x5_330.Add
                                            (Sum_331_330x3.Add
                                               (Scale_2_330.F (Sqr_165.F (T1)),
                                                Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                             Mul_165_165.F (T1, T3)),
                                          Sqr_165.F (T2)),
                                       Mul_165_165.F (T2, T3)))))))));
               return
                 Velocity_At_Stage (T4, 8)
                 + Cm * DT * (-DT**3 + 24.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3)) / 24.0;

            when 10 =>
               Witness
                 (Step_772x13_768x5.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 9),
                     Sym_768x5.F
                       (Div_771x7_By_12.F
                          (Mul_434_337x7.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_336x13_332x3.Add
                                (Sum_338x3_332x3.Add
                                   (Sum_336x11_332x3.Add
                                      (Sum_337x5_332x9.Add
                                         (Sum_336x9_331x11.Add
                                            (Sum_339_334x3.Sub
                                               (Neg_339.F (Scale_2_338.F (Sqr_169.F (DT))),
                                                Mul_169x3_165.F (Scale_3_169.F (DT), T1)),
                                             Scale_22_330.F (Sqr_165.F (T1))),
                                          Mul_167x9_165.F (Scale_36_165.F (T1), T2)),
                                       Mul_167x3_165.F (Scale_12_165.F (T1), T3)),
                                    Scale_12_330.F (Sqr_165.F (T2))),
                                 Mul_167x3_165.F (Scale_12_165.F (T2), T3)))))));
               return
                 Velocity_At_Stage (T1, 9)
                 + Cm
                   * DT
                   * T1
                   * (-2.0 * DT**2 - 3.0 * DT * T1 + 22.0 * T1**2 + 36.0 * T1 * T2 + 12.0 * T1 * T3 + 12.0 * T2**2
                      + 12.0 * T2 * T3)
                   / 12.0;

            when 11 =>
               Witness
                 (Step_773x7_772.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T2, 10),
                     Sym_772.F
                       (Div_775x3_By_24.F
                          (Mul_269_506x3.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x11_499x11.Add
                                (Sum_505x5_500x9.Sub
                                   (Sum_507_505.Sub
                                      (Cub_169.F (DT), Mul_340_165.F (Scale_4_338.F (Sqr_169.F (DT)), T1)),
                                    Mul_335x3_165x3.F
                                      (Mul_170x3_165.F (Scale_6_169.F (DT), T1),
                                       Sum_165_166.Add (T1, Scale_2_165.F (T2)))),
                                 Mul_167_332x11.F
                                   (Scale_4_165.F (T1),
                                    Sum_332x9_331x3.Add
                                      (Sum_335_330x3.Add
                                         (Sum_331x13_331x3.Add
                                            (Sum_330x11_330x15.Add
                                               (Scale_11_330.F (Sqr_165.F (T1)),
                                                Mul_165x15_165.F (Scale_15_165.F (T1), T2)),
                                             Mul_166x3_165.F (Scale_6_165.F (T1), T3)),
                                          Scale_3_330.F (Sqr_165.F (T2))),
                                       Mul_166x3_165.F (Scale_6_165.F (T2), T3)))))))));
               return
                 Velocity_At_Stage (T2, 10)
                 + Cm
                   * DT
                   * (DT**3 - 4.0 * DT**2 * T1 - 6.0 * DT * T1 * (T1 + 2.0 * T2)
                      + 4.0 * T1 * (11.0 * T1**2 + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * T2**2 + 6.0 * T2 * T3))
                   / 24.0;

            when 12 =>
               Witness
                 (Step_776_765x13.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 11),
                     Sym_765x13.F
                       (Div_766x13_By_2.F
                          (Mul_434_332x13.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_334x3_331.Add
                                (Sum_332x11_330.Add
                                   (Sum_333x5_331.Add
                                      (Sum_332x9_330x3.Add
                                         (Sum_335_331.Add
                                            (Neg_335.F (Mul_169_166.F (DT, Sum_165_165.Add (T1, T2))),
                                             Scale_2_330.F (Sqr_165.F (T1))),
                                          Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                       Mul_166_165.F (Scale_2_165.F (T1), T3)),
                                    Sqr_165.F (T2)),
                                 Mul_166_165.F (Scale_2_165.F (T2), T3)))))));
               return
                 Velocity_At_Stage (T1, 11)
                 + Cm
                   * DT
                   * T1
                   * (-DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3)
                   / 2.0;

            when 13 =>
               Witness
                 (Step_773x9_769x7.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T3, 12),
                     Sym_769x7.F
                       (Div_774x5_By_24.F
                          (Mul_269_505x5.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x9_498x9.Add
                                (Sum_507_502x3.Sub
                                   (Cub_169.F (DT),
                                    Mul_336x3_166.F
                                      (Mul_171x3_165.F (Scale_12_169.F (DT), T1), Sum_165_165.Add (T1, T2))),
                                 Mul_167x3_331x3.F
                                   (Scale_12_165.F (T1),
                                    Sum_330x5_330.Add
                                      (Sum_331_330x3.Add
                                         (Scale_2_330.F (Sqr_165.F (T1)), Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                                       Sqr_165.F (T2)))))))));
               return
                 Velocity_At_Stage (T3, 12)
                 + Cm
                   * DT
                   * (DT**3 - 12.0 * DT * T1 * (T1 + T2) + 12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T2**2))
                   / 24.0;

            when 14 =>
               Witness
                 (Step_774x5_768x5.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T1, 13),
                     Sym_768x5.F
                       (Div_771x7_By_12.F
                          (Mul_434_337x7.F
                             (Mul_269_165.F (Mul_100_169.F (Cm, DT), T1),
                              Sum_336x13_331x3.Add
                                (Sum_338x3_331x3.Add
                                   (Sum_336x11_331.Add
                                      (Sum_339_334x9.Sub
                                         (Scale_2_338.F (Sqr_169.F (DT)),
                                          Mul_169x3_165x3.F
                                            (Scale_3_169.F (DT), Sum_165_166.Add (T1, Scale_2_165.F (T2)))),
                                       Scale_2_330.F (Sqr_165.F (T1))),
                                    Mul_166x3_165.F (Scale_6_165.F (T1), T2)),
                                 Scale_6_330.F (Sqr_165.F (T2))))))));
               return
                 Velocity_At_Stage (T1, 13)
                 + Cm
                   * DT
                   * T1
                   * (2.0 * DT**2 - 3.0 * DT * (T1 + 2.0 * T2) + 2.0 * T1**2 + 6.0 * T1 * T2 + 6.0 * T2**2)
                   / 12.0;

            when 15 =>
               Witness
                 (Step_773x11_772.F
                    (abs Start_Vel,
                     Velocity_At_Stage (T2, 14),
                     Sym_772.F
                       (Div_775x3_By_24.F
                          (Mul_269_506x3.F
                             (Mul_100_169.F (Cm, DT),
                              Sum_504x11_497.Add
                                (Sum_505x5_500x3.Sub
                                   (Sum_507_505.Add
                                      (Neg_507.F (Cub_169.F (DT)), Mul_340_165.F (Scale_4_338.F (Sqr_169.F (DT)), T1)),
                                    Mul_170x3_330.F (Scale_6_169.F (DT), Sqr_165.F (T1))),
                                 Scale_4_495.F (Cub_165.F (T1))))))));
               return
                 Velocity_At_Stage (T2, 14)
                 + Cm * DT * (-DT**3 + 4.0 * DT**2 * T1 - 6.0 * DT * T1**2 + 4.0 * T1**3) / 24.0;
         end case;
      end Velocity_At_Stage;

   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Ramp_Times_OK (T, T1, T2, T3, T4));
      pragma Assert (Within (Start_Vel, P800));
      pragma Assert (Within_Nonneg (abs Start_Vel, P800));
      Witness (Gain_Reach (Start_Vel));
      Witness_Stage_Selection (T, T1, T2, T3, T4);

      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Velocity_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Velocity_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Velocity_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Velocity_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Velocity_At_Time;

   ------------------------------------------------------------------------------------------------------------------
   --  Distance along a ramp
   --
   --  The distance of a ramp whose speeds are not negative never decreases as the time increases, to the last bit.
   --  Rounding alone does not give that: adjacent times round differently wherever two expressions meet or large
   --  terms cancel. So the evaluation is arranged as follows.
   --
   --  A ramp which slows down to a speed which is not negative is evaluated backwards: as the distance of the whole
   --  ramp less the distance, in the time which is left, of the ramp which speeds up from the final speed. Nothing
   --  large is then subtracted from Start_Vel * T. The price is that the error of such a distance is a few units in
   --  the last place of the whole ramp's distance at every time, the start of the ramp included, where the forward
   --  evaluation had an error in proportion to the distance.
   --
   --  The distance of a ramp which speeds up is Start_Vel * T plus a gain, and the gain is that of the same ramp
   --  from rest. Within a stage the gain is the gain at the start of the stage plus a polynomial in the time into
   --  the stage. The polynomial is evaluated to twice the working precision and rounded once: its exact value grows
   --  by at least half a unit in its last place between adjacent times, so the rounded value does not decrease. The
   --  time into the stage is at most the stage's length, and the gain is kept between the gain at the start of the
   --  stage and the gain at the start of the next, which is what makes the stages join. The first, fourth, eighth
   --  and twelfth stages start from closed forms, which avoid the rounding of the stages before them, and the
   --  stages between start from the end of the stage before. Each such group of stages is kept between its own
   --  closed form and that of the next group, which can otherwise lie below it when a stage is shorter than the
   --  spacing of the times around it.
   --
   --  Distance_At_Time derives all of that from the ramp for every time. A Prepared_Ramp holds it for all fifteen
   --  stages. The two must agree to the last bit, because the planner places the members of a run with one and
   --  execution evaluates the run with the other. So every operation which rounds is in a subprogram both of them
   --  call, and those are not inlined.
   --
   --  All of this requires every product and every sum to be rounded on its own. A compiler which fuses a product
   --  and a sum into one operation makes the error terms of the polynomial wrong, and the distance then decreases
   --  between some adjacent times. The project is therefore compiled with -ffp-contract=off, which matters on every
   --  target with a fused multiply-add.
   --
   --  Those subprograms reach their results through sums, products and quotients by constants only, so an operation
   --  which overflows, or meets a NaN, leaves a result which is not finite. They test their results once in place of
   --  a check on each operation, as Fast_Distance_At_Max_Time does, and raise exactly when one of those checks
   --  would. The operations include those which split a number into halves, which overflow for magnitudes above
   --  about 1.0E300 where the polynomial alone would not.
   ------------------------------------------------------------------------------------------------------------------

   type Ramp_State is record
      Sn   : Snap := 0.0 * mm / s**4;
      Jr   : Jerk := 0.0 * mm / s**3;
      Acc  : Acceleration := 0.0 * mm / s**2;
      Vel  : Velocity := 0.0 * mm / s;
      Dist : Length := 0.0 * mm;
   end record;
   --  A ramp which speeds up from rest, at the start of one of its stages.

   ------------------------------------------------------------------------------------------------------------------
   --  Order
   --
   --  Ghost. Rounding keeps the order of what it rounds, so sums, differences, products with a factor which is not
   --  negative, and quotients by a positive constant keep the order of their operands (section 2 of
   --  docs/spark_trajectory_monotonicity.md). The provers show that for one sum, difference or multiple at a time.
   --  For a product of two unknowns they are given the reals which the factors denote: the exact products are in
   --  order, and rounding keeps that. Everything about order further down is made of these lemmas.
   ------------------------------------------------------------------------------------------------------------------

   P1012 : constant Raw := 2#1.0#E1012
   with Ghost;

   function At_Most (X, Y : Raw) return Boolean
   is (X <= Y)
   with Ghost;
   --  X <= Y for quantities whose dimensions the compiler cannot compare.

   procedure Lemma_Sum_Grows (A, B : Raw)
   with Ghost, Always_Terminates, Pre => Within (A, P1012) and then Within_Nonneg (B, P1012), Post => A + B >= A;

   procedure Lemma_Sum_Grows (A, B : Raw) is null;
   --  Adding what is not negative.

   procedure Lemma_Sum_Order (A, A2, B, B2 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within (A, P1012)
       and then Within (A2, P1012)
       and then Within (B, P1012)
       and then Within (B2, P1012)
       and then A <= A2
       and then B <= B2,
     Post => A + B <= A2 + B2;

   procedure Lemma_Sum_Order (A, A2, B, B2 : Raw) is null;

   procedure Lemma_Difference_Order (A, A2, B, B2 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within (A, P1012)
       and then Within (A2, P1012)
       and then Within (B, P1012)
       and then Within (B2, P1012)
       and then A <= A2
       and then B2 <= B,
     Post => A - B <= A2 - B2;

   procedure Lemma_Difference_Order (A, A2, B, B2 : Raw) is null;

   procedure Lemma_Real_Product_Order (X, Y, Z : Reals.Real)
   with Ghost, Always_Terminates, Pre => X <= Y and then Z >= 0.0, Post => X * Z <= Y * Z;

   procedure Lemma_Real_Product_Order (X, Y, Z : Reals.Real) is null;

   procedure Lemma_Product_Left (A, A2, B : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (A, P600) and then Within_Nonneg (A2, P600) and then Within_Nonneg (B, P340) and then A <= A2,
     Post => A * B <= A2 * B and then A * B >= 0.0
   is
   begin
      Lemma_Order (A, A2);
      Lemma_Nonnegative (A);
      Lemma_Nonnegative (B);
      Lemma_Real_Product_Order (Reals.R (A), Reals.R (A2), Reals.R (B));
      pragma Assert (Reals.R (A) * Reals.R (B) <= Reals.R (A2) * Reals.R (B));
      pragma Assert (Reals.R (A * B) <= Reals.R (A2 * B));
      Lemma_Order (A * B, A2 * B);
   end Lemma_Product_Left;
   --  A product with a factor which is not negative keeps the order of the other factor: the exact products are
   --  in that order, and rounding keeps it.

   procedure Lemma_Product_Right (A, B, B2 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (A, P600) and then Within_Nonneg (B, P340) and then Within_Nonneg (B2, P340) and then B <= B2,
     Post => A * B <= A * B2 and then A * B >= 0.0
   is
   begin
      Lemma_Order (B, B2);
      Lemma_Nonnegative (A);
      Lemma_Nonnegative (B);
      Lemma_Real_Product_Order (Reals.R (B), Reals.R (B2), Reals.R (A));
      pragma Assert (Reals.R (A) * Reals.R (B) <= Reals.R (A) * Reals.R (B2));
      pragma Assert (Reals.R (A * B) <= Reals.R (A * B2));
      Lemma_Order (A * B, A * B2);
   end Lemma_Product_Right;

   procedure Lemma_Travel_Order (V, T, T2 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (V, P800) and then Within_Nonneg (T, P169) and then Within_Nonneg (T2, P169) and then T <= T2,
     Post => V * T <= V * T2
   is
   begin
      Lemma_Order (T, T2);
      Lemma_Nonnegative (V);
      Lemma_Nonnegative (T);
      Lemma_Real_Product_Order (Reals.R (T), Reals.R (T2), Reals.R (V));
      pragma Assert (Reals.R (V) * Reals.R (T) <= Reals.R (V) * Reals.R (T2));
      pragma Assert (Reals.R (V * T) <= Reals.R (V * T2));
      Lemma_Order (V * T, V * T2);
   end Lemma_Travel_Order;
   --  At a speed which is not negative, a longer time is not a shorter way.

   procedure Lemma_Double_Grows (X : Raw)
   with Ghost, Always_Terminates, Pre => Within_Nonneg (X, P1000), Post => X <= 2.0 * X;

   procedure Lemma_Double_Grows (X : Raw) is null;

   generic
      K, K2 : Raw;
   package Scale_Order with Ghost is
      procedure Lemma (X : Raw)
      with Always_Terminates, Pre => Within_Nonneg (X, P340), Post => K * X <= K2 * X and then K * X >= 0.0;
      --  A larger multiple of what is not negative.
   end Scale_Order;

   package body Scale_Order is
      procedure Lemma (X : Raw) is null;
   end Scale_Order;

   package Order_2_3 is new Scale_Order (2.0, 3.0);
   package Order_3_4 is new Scale_Order (3.0, 4.0);
   package Order_3_27 is new Scale_Order (3.0, 27.0);
   package Order_4_5 is new Scale_Order (4.0, 5.0);
   package Order_5_6 is new Scale_Order (5.0, 6.0);
   package Order_6_7 is new Scale_Order (6.0, 7.0);

   procedure Lemma_Sevenfold_Grows (X : Raw)
   with Ghost, Always_Terminates, Pre => Within_Nonneg (X, P340), Post => X <= 7.0 * X;

   procedure Lemma_Sevenfold_Grows (X : Raw) is null;

   procedure Lemma_Quotient_Order (X, Y : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (X, P1000) and then Within_Nonneg (Y, P1000) and then X <= Y,
     Post => X / 24.0 <= Y / 12.0 and then X / 24.0 >= 0.0;

   procedure Lemma_Quotient_Order (X, Y : Raw) is null;
   --  A twenty-fourth of the smaller of two values which are not negative, and a twelfth of the larger.

   function Clamped (X, Lo, Hi : Length) return Length
   is (Length'Min (Length'Max (X, Lo), Hi))
   with Ghost;
   --  X kept between Lo and Hi, as Make_Stage and Stage_Gain keep a distance.

   procedure Lemma_Clamp_Order (X, X2, Lo, Hi : Length)
   with Ghost, Always_Terminates, Pre => X <= X2, Post => Clamped (X, Lo, Hi) <= Clamped (X2, Lo, Hi);

   procedure Lemma_Clamp_Order (X, X2, Lo, Hi : Length) is null;

   procedure Lemma_Clamp_Range (X, Lo, Hi : Length)
   with
     Ghost,
     Always_Terminates,
     Post => Clamped (X, Lo, Hi) <= Hi and then (if Lo <= Hi then Clamped (X, Lo, Hi) >= Lo);

   procedure Lemma_Clamp_Range (X, Lo, Hi : Length) is null;

   ------------------------------------------------------------------------------------------------------------------
   --  Proof support for the distance along a ramp
   --
   --  Ghost, down to Stage_Crackle. The subprograms after it are proved as the other evaluators are, each operation
   --  through its mirror. The bounds of a state at the start of a stage are those which the evaluator had before:
   --  a state from a closed form is at level 0, and each stage it is advanced through takes it one level up.
   ------------------------------------------------------------------------------------------------------------------

   P94   : constant Raw := 2#1.0#E94
   with Ghost;
   P122  : constant Raw := 2#1.0#E122
   with Ghost;
   P123  : constant Raw := 2#1.0#E123
   with Ghost;
   P124  : constant Raw := 2#1.0#E124
   with Ghost;
   P125  : constant Raw := 2#1.0#E125
   with Ghost;
   P197  : constant Raw := 2#1.0#E197
   with Ghost;
   P198  : constant Raw := 2#1.0#E198
   with Ghost;
   P199  : constant Raw := 2#1.0#E199
   with Ghost;
   P200  : constant Raw := 2#1.0#E200
   with Ghost;
   P263  : constant Raw := 2#1.0#E263
   with Ghost;
   P268  : constant Raw := 2#1.0#E268
   with Ghost;
   P292  : constant Raw := 2#1.0#E292
   with Ghost;
   P293  : constant Raw := 2#1.0#E293
   with Ghost;
   P294  : constant Raw := 2#1.0#E294
   with Ghost;
   P295  : constant Raw := 2#1.0#E295
   with Ghost;
   P323  : constant Raw := 2#1.0#E323
   with Ghost;
   P324  : constant Raw := 2#1.0#E324
   with Ghost;
   P325  : constant Raw := 2#1.0#E325
   with Ghost;
   P326  : constant Raw := 2#1.0#E326
   with Ghost;
   P327  : constant Raw := 2#1.0#E327
   with Ghost;
   P328  : constant Raw := 2#1.0#E328
   with Ghost;
   P433  : constant Raw := 2#1.0#E433
   with Ghost;
   P435  : constant Raw := 2#1.0#E435
   with Ghost;
   P436  : constant Raw := 2#1.0#E436
   with Ghost;
   P462  : constant Raw := 2#1.0#E462
   with Ghost;
   P463  : constant Raw := 2#1.0#E463
   with Ghost;
   P464  : constant Raw := 2#1.0#E464
   with Ghost;
   P465  : constant Raw := 2#1.0#E465
   with Ghost;
   P493  : constant Raw := 2#1.0#E493
   with Ghost;
   P494  : constant Raw := 2#1.0#E494
   with Ghost;
   P496  : constant Raw := 2#1.0#E496
   with Ghost;
   P498  : constant Raw := 2#1.0#E498
   with Ghost;
   P499  : constant Raw := 2#1.0#E499
   with Ghost;
   P603  : constant Raw := 2#1.0#E603
   with Ghost;
   P604  : constant Raw := 2#1.0#E604
   with Ghost;
   P605  : constant Raw := 2#1.0#E605
   with Ghost;
   P632  : constant Raw := 2#1.0#E632
   with Ghost;
   P633  : constant Raw := 2#1.0#E633
   with Ghost;
   P634  : constant Raw := 2#1.0#E634
   with Ghost;
   P635  : constant Raw := 2#1.0#E635
   with Ghost;
   P663  : constant Raw := 2#1.0#E663
   with Ghost;
   P664  : constant Raw := 2#1.0#E664
   with Ghost;
   P665  : constant Raw := 2#1.0#E665
   with Ghost;
   P666  : constant Raw := 2#1.0#E666
   with Ghost;
   P667  : constant Raw := 2#1.0#E667
   with Ghost;
   P668  : constant Raw := 2#1.0#E668
   with Ghost;
   P669  : constant Raw := 2#1.0#E669
   with Ghost;
   P773  : constant Raw := 2#1.0#E773
   with Ghost;
   P775  : constant Raw := 2#1.0#E775
   with Ghost;
   P777  : constant Raw := 2#1.0#E777
   with Ghost;
   P778  : constant Raw := 2#1.0#E778
   with Ghost;
   P801  : constant Raw := 2#1.0#E801
   with Ghost;
   P802  : constant Raw := 2#1.0#E802
   with Ghost;
   P803  : constant Raw := 2#1.0#E803
   with Ghost;
   P804  : constant Raw := 2#1.0#E804
   with Ghost;
   P805  : constant Raw := 2#1.0#E805
   with Ghost;
   P833  : constant Raw := 2#1.0#E833
   with Ghost;
   P834  : constant Raw := 2#1.0#E834
   with Ghost;
   P835  : constant Raw := 2#1.0#E835
   with Ghost;
   P836  : constant Raw := 2#1.0#E836
   with Ghost;
   P837  : constant Raw := 2#1.0#E837
   with Ghost;
   P838  : constant Raw := 2#1.0#E838
   with Ghost;
   P839  : constant Raw := 2#1.0#E839
   with Ghost;
   P971  : constant Raw := 2#1.0#E971
   with Ghost;
   P1003 : constant Raw := 2#1.0#E1003
   with Ghost;
   P1004 : constant Raw := 2#1.0#E1004
   with Ghost;
   P1005 : constant Raw := 2#1.0#E1005
   with Ghost;
   P1006 : constant Raw := 2#1.0#E1006
   with Ghost;
   P1007 : constant Raw := 2#1.0#E1007
   with Ghost;
   P1008 : constant Raw := 2#1.0#E1008
   with Ghost;
   P1009 : constant Raw := 2#1.0#E1009
   with Ghost;
   P1010 : constant Raw := 2#1.0#E1010
   with Ghost;
   P1011 : constant Raw := 2#1.0#E1011
   with Ghost;

   --  The bounds of a state at a checkpoint (level 0) and after one to three advances. Each level repeats the bounds
   --  of the later ones, and the contracts below state every level which a caller needs, so that no proof has to
   --  derive one level from another.
   function State_Level_0 (S : Ramp_State) return Boolean
   is (Within (S.Sn, P265)
       and then Within (S.Jr, P431)
       and then Within (S.Acc, P598)
       and then Within (S.Vel, P764x5)
       and then Within (S.Dist, P931x11)
       and then Within (S.Sn, P266)
       and then Within (S.Jr, P429x11)
       and then Within (S.Acc, P597x5)
       and then Within (S.Vel, P763x15)
       and then Within (S.Dist, P937x11)
       and then Within (S.Sn, P265x3)
       and then Within (S.Jr, P430x9)
       and then Within (S.Acc, P600)
       and then Within (S.Vel, P765x5)
       and then Within (S.Dist, P938x11)
       and then Within (S.Sn, P267)
       and then Within (S.Jr, P430x13)
       and then Within (S.Acc, P597x11)
       and then Within (S.Vel, P764x13)
       and then Within (S.Dist, P942))
   with Ghost;

   function State_Level_1 (S : Ramp_State) return Boolean
   is (Within (S.Sn, P266)
       and then Within (S.Jr, P429x11)
       and then Within (S.Acc, P597x5)
       and then Within (S.Vel, P763x15)
       and then Within (S.Dist, P937x11)
       and then Within (S.Sn, P265x3)
       and then Within (S.Jr, P430x9)
       and then Within (S.Acc, P600)
       and then Within (S.Vel, P765x5)
       and then Within (S.Dist, P938x11)
       and then Within (S.Sn, P267)
       and then Within (S.Jr, P430x13)
       and then Within (S.Acc, P597x11)
       and then Within (S.Vel, P764x13)
       and then Within (S.Dist, P942))
   with Ghost;

   function State_Level_2 (S : Ramp_State) return Boolean
   is (Within (S.Sn, P265x3)
       and then Within (S.Jr, P430x9)
       and then Within (S.Acc, P600)
       and then Within (S.Vel, P765x5)
       and then Within (S.Dist, P938x11)
       and then Within (S.Sn, P267)
       and then Within (S.Jr, P430x13)
       and then Within (S.Acc, P597x11)
       and then Within (S.Vel, P764x13)
       and then Within (S.Dist, P942))
   with Ghost;

   function State_Level_3 (S : Ramp_State) return Boolean
   is (Within (S.Sn, P267)
       and then Within (S.Jr, P430x13)
       and then Within (S.Acc, P597x11)
       and then Within (S.Vel, P764x13)
       and then Within (S.Dist, P942))
   with Ghost;

   function Distance_Level_0 (D : Raw) return Boolean
   is (Within (D, P931x11) and then Within (D, P937x11) and then Within (D, P938x11) and then Within (D, P942))
   with Ghost;
   --  The bounds of the distance of a state at level 0.

   function Above (X, Bound : Raw) return Boolean
   is (X >= -Bound)
   with Ghost;
   --  X is not below -Bound, however large it is.

   generic
      L0, L1, L2, L3, L4, U : Raw;
   package Leveled_Gain with Ghost is
      function F (Old, I : Raw) return Raw
      is (Old + I)
      with
        Pre  => Within (Old, L3) and then Within_Nonneg (I, U),
        Post =>
          (if Within (Old, L0) then Within (F'Result, L1))
          and then (if Within (Old, L1) then Within (F'Result, L2))
          and then (if Within (Old, L2) then Within (F'Result, L3))
          and then Within (F'Result, L4)
          and then F'Result >= Old;
      --  Leveled_Sum for an increment which is not negative, and for a value of the last level too.
   end Leveled_Gain;

   generic
      A : Raw;
   package Nonnegative_Part with Ghost is
      function F (X : Raw) return Raw
      is (Raw'Max (0.0, X))
      with Pre => Within (X, A), Post => Within_Nonneg (F'Result, A);
      --  A value, or zero if it is below zero.
   end Nonnegative_Part;

   generic
      A : Raw;
   package Bounded_Clamp with Ghost is
      function F (X, Lo, Hi : Raw) return Raw
      is (Raw'Min (Raw'Max (X, Lo), Hi))
      with Pre => Within (X, A) and then Within (Lo, A) and then Above (Hi, A), Post => Within (F'Result, A);
      --  A value kept between two others, the upper of which may be as large as it likes.
   end Bounded_Clamp;

   generic
      A, B : Raw;
   package Bounded_Limit with Ghost is
      function F (X, Lo, Hi : Raw) return Raw
      is (Raw'Min (Raw'Max (X, Lo), Hi))
      with Pre => Within (X, A) and then Within (Lo, B) and then Within (Hi, B), Post => Within (F'Result, B);
      --  A value kept between two others, whatever its own size.
   end Bounded_Limit;

   generic
      A, C : Raw;
   package Bounded_Unit with Ghost is
      function Over (X : Raw) return Raw
      is (X / 1.0)
      with Pre => Within (X, A), Post => Within (Over'Result, C);

      function Times (X : Raw) return Raw
      is (X * 1.0)
      with Pre => Within (X, A), Post => Within (Times'Result, C);
      --  A value without its unit, and with a unit again.
   end Bounded_Unit;

   package Stage_Acc is new Bounded_Quotient (P597x11, 2.0, P603);
   package Stage_Jr is new Bounded_Quotient (P430x13, 6.0, P433);
   package Stage_Sn is new Bounded_Quotient (P267, 24.0, P263);
   package Stage_Cr is new Bounded_Quotient (P100, 120.0, P94);
   package Stage_Vel is new Wider_Bound (P764x13, P773);
   package Stage_Dist is new Leveled_Gain (P931x11, P937x11, P938x11, P942, P943, P938x5);
   package Stage_Step is new Nonnegative_Part (P938x5);
   package Stage_Clamp is new Bounded_Clamp (P943);
   package Offset_169 is new Bounded_Offset (P169);
   package Gain_Sum is new Bounded_Sum (P942, P1010, P1011);
   package Gain_Limit is new Bounded_Limit (P1011, P943);

   function Stage_OK (Stage : Ramp_Stage) return Boolean
   is (Within_Nonneg (Stage.Start, P169)
       and then Within_Nonneg (Stage.Len, P165)
       and then Within (Stage.Vel, P773)
       and then Within (Stage.Acc_2, P603)
       and then Within (Stage.Jr_6, P433)
       and then Within (Stage.Sn_24, P263)
       and then Within (Stage.C_120, P94)
       and then Within (Stage.Dist, P942)
       and then Within (Stage.Lo, P943)
       and then Within (Stage.Hi, P943))
   with Ghost;
   --  The bounds of a stage of a ramp of the proved domain. Those of the coefficients are powers of two which the
   --  crude bounds of the compensated polynomial start from.

   function Bounds_OK (Bounds : Ramp_Stage_Bounds) return Boolean
   is (for all K in Bounds'Range => Within_Nonneg (Bounds (K), P169))
   with Ghost;

   function Stage_Crackle (Stage : Feedrate_Profile_Stage_Index; Cm : Crackle) return Crackle
   is (case Stage is
         when 1 | 7 | 11 | 13 => Cm,
         when 3 | 5 | 9 | 15  => -Cm,
         when others          => 0.0 * mm / s**5)
   with Pre => Within (Cm, P100), Post => Within (Stage_Crackle'Result, P100);

   function Stage_Length (Stage : Feedrate_Profile_Stage_Index; Profile : Feedrate_Profile_Times) return Time
   is (case Stage is
         when 2 | 6 | 10 | 14 => Profile (2),
         when 4 | 12          => Profile (3),
         when 8               => Profile (4),
         when others          => Profile (1))
   with
     Pre  => Times_In_Range (Profile),
     Post =>
       Within_Nonneg (Stage_Length'Result, P165)
       and then Within (Stage_Length'Result, P165)
       and then Within (Stage_Length'Result, P169);

   procedure Lemma_Stage_End (X, Bound : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (X, Bound) and then Bound <= P169,
     Post => Within_Nonneg (X, P169);

   procedure Lemma_Stage_End (X, Bound : Raw) is null;
   --  The end of a stage is within the duration of a ramp, whichever stage it is.

   function Bounds_Ordered (Bounds : Ramp_Stage_Bounds) return Boolean
   is (for all J in Bounds'Range => (for all K in Bounds'Range => (if J <= K then Bounds (J) <= Bounds (K))))
   with Ghost;
   --  No stage ends before an earlier one does.

   procedure Lemma_Bounds_Order (T1, T2, T3, T4 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Phase_Times_OK (T1, T2, T3, T4),
     Post =>
       T1 <= T1 + T2
       and then T1 + T2 <= 2.0 * T1 + T2
       and then 2.0 * T1 + T2 <= 2.0 * T1 + T2 + T3
       and then 2.0 * T1 + T2 + T3 <= 3.0 * T1 + T2 + T3
       and then 3.0 * T1 + T2 + T3 <= 3.0 * T1 + 2.0 * T2 + T3
       and then 3.0 * T1 + 2.0 * T2 + T3 <= 4.0 * T1 + 2.0 * T2 + T3
       and then 4.0 * T1 + 2.0 * T2 + T3 <= 4.0 * T1 + 2.0 * T2 + T3 + T4
       and then 4.0 * T1 + 2.0 * T2 + T3 + T4 <= 5.0 * T1 + 2.0 * T2 + T3 + T4
       and then 5.0 * T1 + 2.0 * T2 + T3 + T4 <= 5.0 * T1 + 3.0 * T2 + T3 + T4
       and then 5.0 * T1 + 3.0 * T2 + T3 + T4 <= 6.0 * T1 + 3.0 * T2 + T3 + T4
       and then 6.0 * T1 + 3.0 * T2 + T3 + T4 <= 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4
       and then 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 <= 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4
       and then 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 <= 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4
   is
   begin
      Lemma_Stage_Starts (T1, T2, T3, T4);

      --  Each bound differs from the one before it by one more term at the end, or by a larger multiple of one
      --  phase time, which every sum after it then keeps the order of.
      Lemma_Sum_Grows (T1, T2);

      Lemma_Double_Grows (T1);
      Lemma_Sum_Order (T1, 2.0 * T1, T2, T2);

      Lemma_Sum_Grows (2.0 * T1 + T2, T3);

      Order_2_3.Lemma (T1);
      Lemma_Sum_Order (2.0 * T1, 3.0 * T1, T2, T2);
      Lemma_Sum_Order (2.0 * T1 + T2, 3.0 * T1 + T2, T3, T3);

      Lemma_Double_Grows (T2);
      Lemma_Sum_Order (3.0 * T1, 3.0 * T1, T2, 2.0 * T2);
      Lemma_Sum_Order (3.0 * T1 + T2, 3.0 * T1 + 2.0 * T2, T3, T3);

      Order_3_4.Lemma (T1);
      Lemma_Sum_Order (3.0 * T1, 4.0 * T1, 2.0 * T2, 2.0 * T2);
      Lemma_Sum_Order (3.0 * T1 + 2.0 * T2, 4.0 * T1 + 2.0 * T2, T3, T3);

      Lemma_Sum_Grows (4.0 * T1 + 2.0 * T2 + T3, T4);

      Order_4_5.Lemma (T1);
      Lemma_Sum_Order (4.0 * T1, 5.0 * T1, 2.0 * T2, 2.0 * T2);
      Lemma_Sum_Order (4.0 * T1 + 2.0 * T2, 5.0 * T1 + 2.0 * T2, T3, T3);
      Lemma_Sum_Order (4.0 * T1 + 2.0 * T2 + T3, 5.0 * T1 + 2.0 * T2 + T3, T4, T4);

      Order_2_3.Lemma (T2);
      Lemma_Sum_Order (5.0 * T1, 5.0 * T1, 2.0 * T2, 3.0 * T2);
      Lemma_Sum_Order (5.0 * T1 + 2.0 * T2, 5.0 * T1 + 3.0 * T2, T3, T3);
      Lemma_Sum_Order (5.0 * T1 + 2.0 * T2 + T3, 5.0 * T1 + 3.0 * T2 + T3, T4, T4);

      Order_5_6.Lemma (T1);
      Lemma_Sum_Order (5.0 * T1, 6.0 * T1, 3.0 * T2, 3.0 * T2);
      Lemma_Sum_Order (5.0 * T1 + 3.0 * T2, 6.0 * T1 + 3.0 * T2, T3, T3);
      Lemma_Sum_Order (5.0 * T1 + 3.0 * T2 + T3, 6.0 * T1 + 3.0 * T2 + T3, T4, T4);

      Lemma_Double_Grows (T3);
      Lemma_Sum_Order (6.0 * T1 + 3.0 * T2, 6.0 * T1 + 3.0 * T2, T3, 2.0 * T3);
      Lemma_Sum_Order (6.0 * T1 + 3.0 * T2 + T3, 6.0 * T1 + 3.0 * T2 + 2.0 * T3, T4, T4);

      Order_6_7.Lemma (T1);
      Lemma_Sum_Order (6.0 * T1, 7.0 * T1, 3.0 * T2, 3.0 * T2);
      Lemma_Sum_Order (6.0 * T1 + 3.0 * T2, 7.0 * T1 + 3.0 * T2, 2.0 * T3, 2.0 * T3);
      Lemma_Sum_Order (6.0 * T1 + 3.0 * T2 + 2.0 * T3, 7.0 * T1 + 3.0 * T2 + 2.0 * T3, T4, T4);

      Order_3_4.Lemma (T2);
      Lemma_Sum_Order (7.0 * T1, 7.0 * T1, 3.0 * T2, 4.0 * T2);
      Lemma_Sum_Order (7.0 * T1 + 3.0 * T2, 7.0 * T1 + 4.0 * T2, 2.0 * T3, 2.0 * T3);
      Lemma_Sum_Order (7.0 * T1 + 3.0 * T2 + 2.0 * T3, 7.0 * T1 + 4.0 * T2 + 2.0 * T3, T4, T4);
   end Lemma_Bounds_Order;
   --  The sums which Ramp_Bounds forms do not decrease.

   procedure Lemma_Bounds_Ordered (Bounds : Ramp_Stage_Bounds)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Bounds (1) <= Bounds (2)
       and then Bounds (2) <= Bounds (3)
       and then Bounds (3) <= Bounds (4)
       and then Bounds (4) <= Bounds (5)
       and then Bounds (5) <= Bounds (6)
       and then Bounds (6) <= Bounds (7)
       and then Bounds (7) <= Bounds (8)
       and then Bounds (8) <= Bounds (9)
       and then Bounds (9) <= Bounds (10)
       and then Bounds (10) <= Bounds (11)
       and then Bounds (11) <= Bounds (12)
       and then Bounds (12) <= Bounds (13)
       and then Bounds (13) <= Bounds (14),
     Post => Bounds_Ordered (Bounds)
   is
   begin
      for K in Bounds'Range loop
         pragma
           Loop_Invariant
             (for all I in Bounds'First .. K - 1 => (for all J in Bounds'First .. I => Bounds (J) <= Bounds (I)));
         case K is
            when 1  =>
               null;

            when 2  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 3  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 4  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 5  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 6  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 7  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 8  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 9  =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 10 =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 11 =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 12 =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 13 =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));

            when 14 =>
               pragma Assert (Bounds (K - 1) <= Bounds (K));
         end case;
         pragma Assert (K = Bounds'First or else Bounds (K - 1) <= Bounds (K));
         pragma Assert (for all J in Bounds'First .. K => Bounds (J) <= Bounds (K));
      end loop;
   end Lemma_Bounds_Ordered;

   procedure Lemma_Bounds_OK (Bounds : Ramp_Stage_Bounds)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (Bounds (1), P169)
       and then Within_Nonneg (Bounds (2), P169)
       and then Within_Nonneg (Bounds (3), P169)
       and then Within_Nonneg (Bounds (4), P169)
       and then Within_Nonneg (Bounds (5), P169)
       and then Within_Nonneg (Bounds (6), P169)
       and then Within_Nonneg (Bounds (7), P169)
       and then Within_Nonneg (Bounds (8), P169)
       and then Within_Nonneg (Bounds (9), P169)
       and then Within_Nonneg (Bounds (10), P169)
       and then Within_Nonneg (Bounds (11), P169)
       and then Within_Nonneg (Bounds (12), P169)
       and then Within_Nonneg (Bounds (13), P169)
       and then Within_Nonneg (Bounds (14), P169),
     Post => Bounds_OK (Bounds);

   procedure Lemma_Bounds_OK (Bounds : Ramp_Stage_Bounds) is null;

   function Ramp_Bounds (Profile : Feedrate_Profile_Times) return Ramp_Stage_Bounds
   with
     No_Inline,
     Pre  => Times_In_Range (Profile),
     Post => Bounds_OK (Ramp_Bounds'Result) and then Bounds_Ordered (Ramp_Bounds'Result);
   --  When every stage but the last ends: the sums with which the other evaluators compare the time, as they write
   --  them. No bound is below the one before it, because each differs from it by a larger multiple of a phase time
   --  or by one more term.

   function Ramp_Bounds (Profile : Feedrate_Profile_Times) return Ramp_Stage_Bounds is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);

      pragma Assert (Phase_Times_OK (T1, T2, T3, T4));
      Mirror_2  : constant Raw := Pos_Sum_165_165.F (T1, T2)
      with Ghost;
      Mirror_3  : constant Raw := Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2)
      with Ghost;
      Mirror_4  : constant Raw := Pos_Sum_165x3_165.F (Pos_Sum_166_165.F (Pos_Scale_2_165.F (T1), T2), T3)
      with Ghost;
      Mirror_5  : constant Raw := Pos_Sum_167_165.F (Pos_Sum_165x3_165.F (Pos_Scale_3_165.F (T1), T2), T3)
      with Ghost;
      Mirror_6  : constant Raw :=
        Pos_Sum_165x5_165.F (Pos_Sum_165x3_166.F (Pos_Scale_3_165.F (T1), Pos_Scale_2_165.F (T2)), T3)
      with Ghost;
      Mirror_7  : constant Raw :=
        Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3)
      with Ghost;
      Mirror_8  : constant Raw :=
        Pos_Sum_165x7_165.F
          (Pos_Sum_166x3_165.F (Pos_Sum_167_166.F (Pos_Scale_4_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4)
      with Ghost;
      Mirror_9  : constant Raw :=
        Pos_Sum_168_165.F
          (Pos_Sum_165x7_165.F (Pos_Sum_165x5_166.F (Pos_Scale_5_165.F (T1), Pos_Scale_2_165.F (T2)), T3), T4)
      with Ghost;
      Mirror_10 : constant Raw :=
        Pos_Sum_165x9_165.F
          (Pos_Sum_168_165.F (Pos_Sum_165x5_165x3.F (Pos_Scale_5_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4)
      with Ghost;
      Mirror_11 : constant Raw :=
        Pos_Sum_166x5_165.F
          (Pos_Sum_165x9_165.F (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), T3), T4)
      with Ghost;
      Mirror_12 : constant Raw :=
        Pos_Sum_165x11_165.F
          (Pos_Sum_165x9_166.F
             (Pos_Sum_166x3_165x3.F (Pos_Scale_6_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
           T4)
      with Ghost;
      Mirror_13 : constant Raw :=
        Pos_Sum_167x3_165.F
          (Pos_Sum_166x5_166.F
             (Pos_Sum_165x7_165x3.F (Pos_Scale_7_165.F (T1), Pos_Scale_3_165.F (T2)), Pos_Scale_2_165.F (T3)),
           T4)
      with Ghost;
      Mirror_14 : constant Raw :=
        Pos_Sum_165x13_165.F
          (Pos_Sum_165x11_166.F
             (Pos_Sum_165x7_167.F (Pos_Scale_7_165.F (T1), Pos_Scale_4_165.F (T2)), Pos_Scale_2_165.F (T3)),
           T4)
      with Ghost;

      pragma Suppress (Overflow_Check);
      Result : constant Ramp_Stage_Bounds :=
        [1  => T1,
         2  => T1 + T2,
         3  => 2.0 * T1 + T2,
         4  => 2.0 * T1 + T2 + T3,
         5  => 3.0 * T1 + T2 + T3,
         6  => 3.0 * T1 + 2.0 * T2 + T3,
         7  => 4.0 * T1 + 2.0 * T2 + T3,
         8  => 4.0 * T1 + 2.0 * T2 + T3 + T4,
         9  => 5.0 * T1 + 2.0 * T2 + T3 + T4,
         10 => 5.0 * T1 + 3.0 * T2 + T3 + T4,
         11 => 6.0 * T1 + 3.0 * T2 + T3 + T4,
         12 => 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4,
         13 => 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4,
         14 => 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4];
   begin
      Lemma_Stage_End (Result (1), P165);
      Lemma_Stage_End (Mirror_2, P166);
      Lemma_Stage_End (Mirror_3, P165x3);
      Lemma_Stage_End (Mirror_4, P167);
      Lemma_Stage_End (Mirror_5, P165x5);
      Lemma_Stage_End (Mirror_6, P166x3);
      Lemma_Stage_End (Mirror_7, P165x7);
      Lemma_Stage_End (Mirror_8, P168);
      Lemma_Stage_End (Mirror_9, P165x9);
      Lemma_Stage_End (Mirror_10, P166x5);
      Lemma_Stage_End (Mirror_11, P165x11);
      Lemma_Stage_End (Mirror_12, P167x3);
      Lemma_Stage_End (Mirror_13, P165x13);
      Lemma_Stage_End (Mirror_14, P166x7);
      pragma Assert (Within_Nonneg (Result (1), P169));
      pragma Assert (Within_Nonneg (Result (2), P169));
      pragma Assert (Within_Nonneg (Result (3), P169));
      pragma Assert (Within_Nonneg (Result (4), P169));
      pragma Assert (Within_Nonneg (Result (5), P169));
      pragma Assert (Within_Nonneg (Result (6), P169));
      pragma Assert (Within_Nonneg (Result (7), P169));
      pragma Assert (Within_Nonneg (Result (8), P169));
      pragma Assert (Within_Nonneg (Result (9), P169));
      pragma Assert (Within_Nonneg (Result (10), P169));
      pragma Assert (Within_Nonneg (Result (11), P169));
      pragma Assert (Within_Nonneg (Result (12), P169));
      pragma Assert (Within_Nonneg (Result (13), P169));
      pragma Assert (Within_Nonneg (Result (14), P169));
      Lemma_Bounds_OK (Result);
      Lemma_Bounds_Order (T1, T2, T3, T4);
      Lemma_Bounds_Ordered (Result);
      if not (for all Bound of Result => abs Bound <= Time'Last) then
         raise Constraint_Error with "Unrepresentable ramp time.";
      end if;
      return Result;
   end Ramp_Bounds;

   function Stage_At (Bounds : Ramp_Stage_Bounds; T : Time) return Feedrate_Profile_Stage_Index
   with
     Post =>
       (Stage_At'Result = 1 or else T >= Bounds (Stage_At'Result - 1))
       and then (Stage_At'Result = 15 or else T < Bounds (Stage_At'Result));
   --  The stage which contains T: the first which T is before the end of, as the other evaluators select it.

   function Stage_At (Bounds : Ramp_Stage_Bounds; T : Time) return Feedrate_Profile_Stage_Index is
      --  One more than how many bounds T has reached, by bisection.
      Reached : Feedrate_Profile_Stage_Index'Base := (if T >= Bounds (8) then 8 else 0);
   begin
      if T >= Bounds (Reached + 4) then
         Reached := Reached + 4;
      end if;
      if T >= Bounds (Reached + 2) then
         Reached := Reached + 2;
      end if;
      if Reached < 14 and then T >= Bounds (Reached + 1) then
         Reached := Reached + 1;
      end if;
      return Reached + 1;
   end Stage_At;

   procedure Lemma_Stage_Order (Bounds : Ramp_Stage_Bounds; T1, T2 : Time)
   with
     Ghost,
     Always_Terminates,
     Pre  => Bounds_Ordered (Bounds) and then T1 <= T2,
     Post => Stage_At (Bounds, T1) <= Stage_At (Bounds, T2)
   is
   begin
      if Stage_At (Bounds, T2) < Stage_At (Bounds, T1) then
         --  T2 is before the end of its stage, which is no later than the start of the stage of T1.
         pragma Assert (T2 < Bounds (Stage_At (Bounds, T2)));
         pragma Assert (Bounds (Stage_At (Bounds, T2)) <= Bounds (Stage_At (Bounds, T1) - 1));
         pragma Assert (Bounds (Stage_At (Bounds, T1) - 1) <= T1);
         pragma Assert (False);
      end if;
   end Lemma_Stage_Order;
   --  A later time is not in an earlier stage.

   function Stage_End_Distance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Length
   with
     No_Inline,
     Pre  => State_Level_3 (State) and then Within (DT, P169) and then Within (Stage_Crackle, P100),
     Post =>
       (if Within (State.Dist, P931x11) then Within (Stage_End_Distance'Result, P937x11))
       and then (if Within (State.Dist, P937x11) then Within (Stage_End_Distance'Result, P938x11))
       and then (if Within (State.Dist, P938x11) then Within (Stage_End_Distance'Result, P942))
       and then Within (Stage_End_Distance'Result, P943)
       and then Stage_End_Distance'Result >= State.Dist;
   --  The gain at the end of a stage of duration DT and constant crackle Stage_Crackle which starts at State. It is
   --  never below the gain at the start.

   function Stage_End_Distance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Length is
      Local_Mirror : constant Raw :=
        Mul_169_769x5.F
          (DT,
           Sum_764x13_768x9.Add
             (State.Vel,
              Mul_169_599x9.F
                (DT,
                 Sum_596x11_598x15.Add
                   (Div_597x11_By_2.F (State.Acc),
                    Mul_169_429x15.F
                      (DT,
                       Sum_428x9_430x5.Add
                         (Div_430x13_By_6.F (State.Jr),
                          Mul_169_261x5.F
                            (DT,
                             Sum_259x11_259x9.Add
                               (Div_267_By_24.F (State.Sn),
                                Div_269_By_120.F (Mul_169_100.F (DT, Stage_Crackle))))))))))
      with Ghost;
      pragma Assert (Within (Local_Mirror, P938x5));

      pragma Suppress (Overflow_Check);
      Local : constant Length :=
        DT
        * (State.Vel
           + DT * (State.Acc / 2.0 + DT * (State.Jr / 6.0 + DT * (State.Sn / 24.0 + DT * Stage_Crackle / 120.0))));
      --  How far the stage gets in DT.

      pragma Assert (Within (Local, P938x5));
      Result_Mirror : constant Raw := Stage_Dist.F (State.Dist, Stage_Step.F (Local))
      with Ghost;
      pragma Assert (Within (Result_Mirror, P943));

      Result : constant Length := State.Dist + Length'Max (0.0 * mm, Local);
   begin
      if not (abs Local <= Length'Last and then abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Stage_End_Distance;

   function Advanced (Result, State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Boolean
   is (Result.Dist = Stage_End_Distance (State, DT, Stage_Crackle))
   with Ghost, Pre => State_Level_3 (State) and then Within (DT, P169) and then Within (Stage_Crackle, P100);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Advanced);
   --  What Advance returns as the distance. This and the like below say which value a function returns, for the
   --  lemmas about the order of such values. They are hidden from every proof which does not ask for them, so
   --  that the proofs about the size of the values are not burdened with them.

   function Advance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Ramp_State
   with
     No_Inline,
     Pre  =>
       State_Level_2 (State)
       and then Within (DT, P165)
       and then Within (DT, P169)
       and then Within (Stage_Crackle, P100),
     Post =>
       (if State_Level_0 (State) then State_Level_1 (Advance'Result) and then State_Level_2 (Advance'Result))
       and then (if State_Level_1 (State) then State_Level_2 (Advance'Result))
       and then State_Level_3 (Advance'Result)
       and then Advanced (Advance'Result, State, DT, Stage_Crackle);
   --  State after a complete stage of duration DT and constant crackle Stage_Crackle.

   function Advance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Ramp_State is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Advanced);

      pragma Assert (State_Level_3 (State));
      Vel_Mirror : constant Raw :=
        Advance_Vel.F
          (State.Vel,
           Mul_165_598x5.F
             (DT,
              Sum_600_594x11.Add
                (State.Acc,
                 Mul_165_429x11.F
                   (DT,
                    Sum_429x9_426x9.Add
                      (Div_430x9_By_2.F (State.Jr),
                       Mul_165_261x9.F
                         (DT,
                          Sum_264_257x11.Add
                            (Div_265x3_By_6.F (State.Sn), Div_265_By_24.F (Mul_165_100.F (DT, Stage_Crackle)))))))))
      with Ghost;
      Acc_Mirror : constant Raw :=
        Advance_Acc.F
          (State.Acc,
           Mul_165_430x11.F
             (DT,
              Sum_430x9_428x7.Add
                (State.Jr,
                 Mul_165_263x7.F
                   (DT,
                    Sum_264x3_259x11.Add
                      (Div_265x3_By_2.F (State.Sn), Div_265_By_6.F (Mul_165_100.F (DT, Stage_Crackle)))))))
      with Ghost;
      Jr_Mirror  : constant Raw :=
        Advance_Jr.F
          (State.Jr,
           Mul_165_264x7.F (DT, Sum_265x3_264.Add (State.Sn, Div_265_By_2.F (Mul_165_100.F (DT, Stage_Crackle)))))
      with Ghost;
      Sn_Mirror  : constant Raw := Advance_Sn.F (State.Sn, Mul_165_100.F (DT, Stage_Crackle))
      with Ghost;
      pragma
        Assert
          (Within (Vel_Mirror, P764x13)
           and then Within (Acc_Mirror, P597x11)
           and then Within (Jr_Mirror, P430x13)
           and then Within (Sn_Mirror, P267));

      pragma Suppress (Overflow_Check);
      Result : constant Ramp_State :=
        (Dist => Stage_End_Distance (State, DT, Stage_Crackle),
         Vel  =>
           State.Vel + DT * (State.Acc + DT * (State.Jr / 2.0 + DT * (State.Sn / 6.0 + DT * Stage_Crackle / 24.0))),
         Acc  => State.Acc + DT * (State.Jr + DT * (State.Sn / 2.0 + DT * Stage_Crackle / 6.0)),
         Jr   => State.Jr + DT * (State.Sn + DT * Stage_Crackle / 2.0),
         Sn   => State.Sn + DT * Stage_Crackle);
   begin
      if not (abs Result.Vel <= Velocity'Last
              and then abs Result.Acc <= Acceleration'Last
              and then abs Result.Jr <= Jerk'Last
              and then abs Result.Sn <= Snap'Last)
      then
         raise Constraint_Error with "Unrepresentable ramp state.";
      end if;
      return Result;
   end Advance;

   --  The two closed forms which have to be in order, in the pieces their order is shown by: each is a product
   --  of the peak jerk and a sum of phase times, times a polynomial in the phase times, over a constant.

   function Group_Factor_4 (T1, T2, Cm : Raw) return Raw
   is (Mul_431_165x3.F
         (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)), Sum_166_165.Add (Scale_2_165.F (T1), T2)))
   with
     Ghost,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (Cm, P100),
     Post => Within (Group_Factor_4'Result, P596x3);

   function Group_Factor_8 (T1, T2, T3, Cm : Raw) return Raw
   is (Mul_431_167.F
         (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
          Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)))
   with
     Ghost,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165) and then Within (Cm, P100),
     Post => Within (Group_Factor_8'Result, P598);

   function Group_Polynomial_4 (T1, T2 : Raw) return Raw
   is (Sum_331x3_330.Add
         (Sum_330x3_330x3.Add (Scale_3_330.F (Sqr_165.F (T1)), Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
          Sqr_165.F (T2)))
   with Ghost, Pre => Within (T1, P165) and then Within (T2, P165), Post => Within (Group_Polynomial_4'Result, P330x7);

   function Group_Polynomial_8 (T1, T2, T3 : Raw) return Raw
   is (Sum_333x11_331.Add
         (Sum_334x5_330x7.Add
            (Sum_333x9_330x7.Add
               (Sum_333x7_331x7.Add
                  (Sum_332x7_332x7.Add (Scale_27_330.F (Sqr_165.F (T1)), Mul_167x7_165.F (Scale_27_165.F (T1), T2)),
                   Mul_166x7_165.F (Scale_14_165.F (T1), T3)),
                Scale_7_330.F (Sqr_165.F (T2))),
             Mul_165x7_165.F (Scale_7_165.F (T2), T3)),
          Scale_2_330.F (Sqr_165.F (T3))))
   with
     Ghost,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165),
     Post => Within (Group_Polynomial_8'Result, P335x3);

   function Checkpoint_4_Value (T1, T2, Cm : Raw) return Raw
   is (Widen_Dist_922x15.F
         (Div_927x11_By_24.F (Mul_596x3_330x7.F (Group_Factor_4 (T1, T2, Cm), Group_Polynomial_4 (T1, T2)))))
   with Ghost, Pre => Within (T1, P165) and then Within (T2, P165) and then Within (Cm, P100);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Checkpoint_4_Value);
   --  The value of Checkpoint_4_Distance: its mirror. Only what is about the order of the two values looks into
   --  them.

   function Checkpoint_8_Value (T1, T2, T3, Cm : Raw) return Raw
   is (Widen_Dist_931.F
         (Div_933x3_By_12.F (Mul_598_335x3.F (Group_Factor_8 (T1, T2, T3, Cm), Group_Polynomial_8 (T1, T2, T3)))))
   with
     Ghost,
     Pre => Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165) and then Within (Cm, P100);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Checkpoint_8_Value);
   --  The value of Checkpoint_8_Distance: its mirror.

   procedure Lemma_Factor_Order (T1, T2, T3, Cm : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (T1, P165)
       and then Within_Nonneg (T2, P165)
       and then Within_Nonneg (T3, P165)
       and then Within_Nonneg (Cm, P100),
     Post => Group_Factor_4 (T1, T2, Cm) >= 0.0 and then Group_Factor_4 (T1, T2, Cm) <= Group_Factor_8 (T1, T2, T3, Cm)
   is
      Snap  : constant Raw := Mul_100_165.F (Cm, T1);
      Span  : constant Raw := Sum_165_165.Add (T1, T2);
      Jerk  : constant Raw := Mul_265_166.F (Snap, Span);
      Twice : constant Raw := Scale_2_165.F (T1);
      Sum_4 : constant Raw := Sum_166_165.Add (Twice, T2);
      Sum_8 : constant Raw := Sum_165x3_165.Add (Sum_4, T3);
      Acc_4 : constant Raw := Mul_431_165x3.F (Jerk, Sum_4);
      Acc_8 : constant Raw := Mul_431_167.F (Jerk, Sum_8);
   begin
      --  The peak jerk is not negative.
      Lemma_Product_Left (Cm, Cm, T1);
      pragma Assert (Snap >= 0.0);
      Lemma_Sum_Grows (T1, T2);
      pragma Assert (Span >= 0.0);
      Lemma_Product_Left (Snap, Snap, Span);
      pragma Assert (Jerk >= 0.0);

      --  The sum of the later closed form is that of the earlier one and T3 more.
      Order_2_3.Lemma (T1);
      pragma Assert (Twice >= 0.0);
      Lemma_Sum_Grows (Twice, T2);
      pragma Assert (Sum_4 >= 0.0);
      Lemma_Sum_Grows (Sum_4, T3);
      pragma Assert (Sum_4 <= Sum_8);
      Lemma_Product_Right (Jerk, Sum_4, Sum_8);
      pragma Assert (Acc_4 >= 0.0 and then Acc_4 <= Acc_8);
      pragma Assert (Group_Factor_4 (T1, T2, Cm) = Acc_4);
      pragma Assert (Group_Factor_8 (T1, T2, T3, Cm) = Acc_8);
   end Lemma_Factor_Order;

   procedure Lemma_Polynomial_Order (T1, T2, T3 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (T1, P165) and then Within_Nonneg (T2, P165) and then Within_Nonneg (T3, P165),
     Post => Group_Polynomial_4 (T1, T2) >= 0.0 and then Group_Polynomial_4 (T1, T2) <= Group_Polynomial_8 (T1, T2, T3)
   is
      Sq_1 : constant Raw := Sqr_165.F (T1);
      Sq_2 : constant Raw := Sqr_165.F (T2);
      Sq_3 : constant Raw := Sqr_165.F (T3);

      Low_1   : constant Raw := Scale_3_330.F (Sq_1);
      Low_T   : constant Raw := Scale_3_165.F (T1);
      Low_2   : constant Raw := Mul_165x3_165.F (Low_T, T2);
      Low_Sum : constant Raw := Sum_330x3_330x3.Add (Low_1, Low_2);
      Poly_4  : constant Raw := Sum_331x3_330.Add (Low_Sum, Sq_2);

      High_1   : constant Raw := Scale_27_330.F (Sq_1);
      High_T   : constant Raw := Scale_27_165.F (T1);
      High_2   : constant Raw := Mul_167x7_165.F (High_T, T2);
      High_Sum : constant Raw := Sum_332x7_332x7.Add (High_1, High_2);
      Mixed_13 : constant Raw := Mul_166x7_165.F (Scale_14_165.F (T1), T3);
      High_3   : constant Raw := Sum_333x7_331x7.Add (High_Sum, Mixed_13);
      Seven_2  : constant Raw := Scale_7_330.F (Sq_2);
      High_4   : constant Raw := Sum_333x9_330x7.Add (High_3, Seven_2);
      Mixed_23 : constant Raw := Mul_165x7_165.F (Scale_7_165.F (T2), T3);
      High_5   : constant Raw := Sum_334x5_330x7.Add (High_4, Mixed_23);
      Two_3    : constant Raw := Scale_2_330.F (Sq_3);
      Poly_8   : constant Raw := Sum_333x11_331.Add (High_5, Two_3);
   begin
      --  The polynomial of the later closed form exceeds that of the earlier one term for term, and has three
      --  terms more, none of them negative.
      pragma Assert (Sq_1 >= 0.0 and then Sq_2 >= 0.0 and then Sq_3 >= 0.0);
      Order_3_27.Lemma (Sq_1);
      pragma Assert (Low_1 >= 0.0 and then Low_1 <= High_1);
      Order_3_27.Lemma (T1);
      pragma Assert (Low_T >= 0.0 and then Low_T <= High_T);
      Lemma_Product_Left (Low_T, High_T, T2);
      pragma Assert (Low_2 >= 0.0 and then Low_2 <= High_2);
      Lemma_Sum_Grows (Low_1, Low_2);
      Lemma_Sum_Order (Low_1, High_1, Low_2, High_2);
      pragma Assert (Low_Sum >= 0.0 and then Low_Sum <= High_Sum);

      pragma Assert (Scale_14_165.F (T1) >= 0.0);
      Lemma_Product_Left (Scale_14_165.F (T1), Scale_14_165.F (T1), T3);
      pragma Assert (Mixed_13 >= 0.0);
      Lemma_Sum_Grows (High_Sum, Mixed_13);
      pragma Assert (Low_Sum <= High_3);

      Lemma_Sevenfold_Grows (Sq_2);
      pragma Assert (Sq_2 <= Seven_2);
      Lemma_Sum_Grows (Low_Sum, Sq_2);
      pragma Assert (Within (Low_Sum, P1012) and then Within (High_3, P1012));
      pragma Assert (Within (Sq_2, P1012) and then Within (Seven_2, P1012));
      Lemma_Sum_Order (Low_Sum, High_3, Sq_2, Seven_2);
      pragma Assert (Poly_4 >= 0.0 and then Poly_4 <= High_4);

      pragma Assert (Scale_7_165.F (T2) >= 0.0);
      Lemma_Product_Left (Scale_7_165.F (T2), Scale_7_165.F (T2), T3);
      pragma Assert (Mixed_23 >= 0.0);
      Lemma_Sum_Grows (High_4, Mixed_23);
      pragma Assert (Two_3 >= 0.0);
      Lemma_Sum_Grows (High_5, Two_3);
      pragma Assert (Poly_4 <= Poly_8);
      pragma Assert (Group_Polynomial_4 (T1, T2) = Poly_4);
      pragma Assert (Group_Polynomial_8 (T1, T2, T3) = Poly_8);
   end Lemma_Polynomial_Order;

   procedure Lemma_Group_Order (T1, T2, T3, Cm : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (T1, P165)
       and then Within_Nonneg (T2, P165)
       and then Within_Nonneg (T3, P165)
       and then Within_Nonneg (Cm, P100),
     Post =>
       Checkpoint_4_Value (T1, T2, Cm) >= 0.0
       and then Checkpoint_4_Value (T1, T2, Cm) <= Checkpoint_8_Value (T1, T2, T3, Cm)
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Checkpoint_4_Value);
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Checkpoint_8_Value);

      Acc_4  : constant Raw := Group_Factor_4 (T1, T2, Cm);
      Acc_8  : constant Raw := Group_Factor_8 (T1, T2, T3, Cm);
      Poly_4 : constant Raw := Group_Polynomial_4 (T1, T2);
      Poly_8 : constant Raw := Group_Polynomial_8 (T1, T2, T3);
      Top_4  : constant Raw := Mul_596x3_330x7.F (Acc_4, Poly_4);
      Top_8  : constant Raw := Mul_598_335x3.F (Acc_8, Poly_8);
      D4     : constant Raw := Div_927x11_By_24.F (Top_4);
      D8     : constant Raw := Div_933x3_By_12.F (Top_8);
   begin
      Lemma_Factor_Order (T1, T2, T3, Cm);
      Lemma_Polynomial_Order (T1, T2, T3);

      --  The products are in order, and a twenty-fourth of the smaller is at most a twelfth of the larger.
      Lemma_Product_Left (Acc_4, Acc_8, Poly_4);
      Lemma_Product_Right (Acc_8, Poly_4, Poly_8);
      pragma Assert (Top_4 >= 0.0 and then Top_4 <= Top_8);
      pragma Assert (Within_Nonneg (Top_4, P1000) and then Within_Nonneg (Top_8, P1000));
      Lemma_Quotient_Order (Top_4, Top_8);
      pragma Assert (D4 >= 0.0 and then D4 <= D8);
      pragma Assert (Checkpoint_4_Value (T1, T2, Cm) = D4);
      pragma Assert (Checkpoint_8_Value (T1, T2, T3, Cm) = D8);
   end Lemma_Group_Order;
   --  The closed form of the gain at the start of stage 4 is not negative, and that at the start of stage 8 is not
   --  below it, as rounded: the ranges which the first three groups of stages are kept in are in order.

   function Checkpoint_4_Distance (T1, T2 : Time; Cm : Crackle) return Length
   with
     No_Inline,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (Cm, P100),
     Post =>
       Distance_Level_0 (Checkpoint_4_Distance'Result)
       and then Same (Checkpoint_4_Distance'Result, Checkpoint_4_Value (T1, T2, Cm));

   function Checkpoint_8_Distance (T1, T2, T3 : Time; Cm : Crackle) return Length
   with
     No_Inline,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (T3, P165) and then Within (Cm, P100),
     Post =>
       Distance_Level_0 (Checkpoint_8_Distance'Result)
       and then Same (Checkpoint_8_Distance'Result, Checkpoint_8_Value (T1, T2, T3, Cm));

   function Checkpoint_12_Distance (T1, T2, T3, T4 : Time; Cm : Crackle) return Length
   with
     No_Inline,
     Pre  => Ramp_Inputs_OK (T1, T2, T3, T4, Cm),
     Post => Distance_Level_0 (Checkpoint_12_Distance'Result);
   --  The gain at the start of stage 4, 8 or 12, in closed form.

   function Checkpoint_4 (T1, T2 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with
     No_Inline,
     Pre  => Within (T1, P165) and then Within (T2, P165) and then Within (Cm, P100) and then Distance_Level_0 (Dist),
     Post =>
       State_Level_0 (Checkpoint_4'Result)
       and then State_Level_2 (Checkpoint_4'Result)
       and then State_Level_3 (Checkpoint_4'Result);
   --  The state at the start of stage 4, with Dist for Checkpoint_4_Distance. This avoids cancellation from stages
   --  1 .. 3.

   function Checkpoint_8 (T1, T2, T3 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with
     No_Inline,
     Pre  =>
       Within (T1, P165)
       and then Within (T2, P165)
       and then Within (T3, P165)
       and then Within (Cm, P100)
       and then Distance_Level_0 (Dist),
     Post =>
       State_Level_0 (Checkpoint_8'Result)
       and then State_Level_2 (Checkpoint_8'Result)
       and then State_Level_3 (Checkpoint_8'Result);
   --  The state at the start of stage 8. This avoids cancellation from stages 1 .. 7.

   function Checkpoint_12 (T1, T2, T3, T4 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with
     No_Inline,
     Pre  => Ramp_Inputs_OK (T1, T2, T3, T4, Cm) and then Distance_Level_0 (Dist),
     Post =>
       State_Level_0 (Checkpoint_12'Result)
       and then State_Level_2 (Checkpoint_12'Result)
       and then State_Level_3 (Checkpoint_12'Result);
   --  The state at the start of stage 12. This avoids cancellation from stages 8 .. 11.

   function Checkpoint_4_Distance (T1, T2 : Time; Cm : Crackle) return Length is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Checkpoint_4_Value);

      Mirror : constant Raw :=
        Widen_Dist_922x15.F
          (Div_927x11_By_24.F
             (Mul_596x3_330x7.F
                (Mul_431_165x3.F
                   (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                    Sum_166_165.Add (Scale_2_165.F (T1), T2)),
                 Sum_331x3_330.Add
                   (Sum_330x3_330x3.Add (Scale_3_330.F (Sqr_165.F (T1)), Mul_165x3_165.F (Scale_3_165.F (T1), T2)),
                    Sqr_165.F (T2)))))
      with Ghost;
      pragma Assert (Distance_Level_0 (Mirror));

      pragma Suppress (Overflow_Check);
      Result : constant Length := Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) * (3.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 24.0;
   begin
      pragma Assert (Same (Mirror, Checkpoint_4_Value (T1, T2, Cm)));
      pragma Assert (Same (Result, Mirror));
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_4_Distance;

   function Checkpoint_8_Distance (T1, T2, T3 : Time; Cm : Crackle) return Length is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Checkpoint_8_Value);

      Mirror : constant Raw :=
        Widen_Dist_931.F
          (Div_933x3_By_12.F
             (Mul_598_335x3.F
                (Mul_431_167.F
                   (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                    Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)),
                 Sum_333x11_331.Add
                   (Sum_334x5_330x7.Add
                      (Sum_333x9_330x7.Add
                         (Sum_333x7_331x7.Add
                            (Sum_332x7_332x7.Add
                               (Scale_27_330.F (Sqr_165.F (T1)), Mul_167x7_165.F (Scale_27_165.F (T1), T2)),
                             Mul_166x7_165.F (Scale_14_165.F (T1), T3)),
                          Scale_7_330.F (Sqr_165.F (T2))),
                       Mul_165x7_165.F (Scale_7_165.F (T2), T3)),
                    Scale_2_330.F (Sqr_165.F (T3))))))
      with Ghost;
      pragma Assert (Distance_Level_0 (Mirror));

      pragma Suppress (Overflow_Check);
      Result : constant Length :=
        Cm
        * T1
        * (T1 + T2)
        * (2.0 * T1 + T2 + T3)
        * (27.0 * T1**2 + 27.0 * T1 * T2 + 14.0 * T1 * T3 + 7.0 * T2**2 + 7.0 * T2 * T3 + 2.0 * T3**2)
        / 12.0;
   begin
      pragma Assert (Same (Mirror, Checkpoint_8_Value (T1, T2, T3, Cm)));
      pragma Assert (Same (Result, Mirror));
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_8_Distance;

   function Checkpoint_12_Distance (T1, T2, T3, T4 : Time; Cm : Crackle) return Length is
      Lead_Mirror : constant Raw := Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2))
      with Ghost;
      Lead        : constant Raw := Cm * T1 * (T1 + T2)
      with Ghost;
      pragma Assert (Within (Lead_Mirror, P431) and then Within (Lead, P431));

      --  The sum term by term, so that each of its operations is matched with its mirror on its own.
      Mirror_1  : constant Raw := Scale_390_495.F (Cub_165.F (T1))
      with Ghost;
      Part_1    : constant Raw := 390.0 * T1**3
      with Ghost;
      pragma Assert (Within (Mirror_1, P500x13) and then Within (Part_1, P500x13));
      Mirror_2  : constant Raw := Sum_500x13_502x5.Add (Part_1, Mul_337x5_165.F (Scale_585_330.F (Sqr_165.F (T1)), T2))
      with Ghost;
      Part_2    : constant Raw := Part_1 + 585.0 * T1**2 * T2
      with Ghost;
      pragma Assert (Within (Mirror_2, P502x9) and then Within (Part_2, P502x9));
      Mirror_3  : constant Raw := Sum_502x9_501x5.Add (Part_2, Mul_336x5_165.F (Scale_302_330.F (Sqr_165.F (T1)), T3))
      with Ghost;
      Part_3    : constant Raw := Part_2 + 302.0 * T1**2 * T3
      with Ghost;
      pragma Assert (Within (Mirror_3, P504x3) and then Within (Part_3, P504x3));
      Mirror_4  : constant Raw := Sum_504x3_501x3.Add (Part_3, Mul_336x3_165.F (Scale_192_330.F (Sqr_165.F (T1)), T4))
      with Ghost;
      Part_4    : constant Raw := Part_3 + 192.0 * T1**2 * T4
      with Ghost;
      pragma Assert (Within (Mirror_4, P503x7) and then Within (Part_4, P503x7));
      Mirror_5  : constant Raw := Sum_503x7_501x5.Add (Part_4, Mul_171x5_330.F (Scale_293_165.F (T1), Sqr_165.F (T2)))
      with Ghost;
      Part_5    : constant Raw := Part_4 + 293.0 * T1 * T2**2
      with Ghost;
      pragma Assert (Within (Mirror_5, P503x9) and then Within (Part_5, P503x9));
      Mirror_6  : constant Raw :=
        Sum_503x9_501x5.Add (Part_5, Mul_336x5_165.F (Mul_171x5_165.F (Scale_302_165.F (T1), T2), T3))
      with Ghost;
      Part_6    : constant Raw := Part_5 + 302.0 * T1 * T2 * T3
      with Ghost;
      pragma Assert (Within (Mirror_6, P503x11) and then Within (Part_6, P503x11));
      Mirror_7  : constant Raw :=
        Sum_503x11_501x3.Add (Part_6, Mul_336x3_165.F (Mul_171x3_165.F (Scale_192_165.F (T1), T2), T4))
      with Ghost;
      Part_7    : constant Raw := Part_6 + 192.0 * T1 * T2 * T4
      with Ghost;
      pragma Assert (Within (Mirror_7, P505x3) and then Within (Part_7, P505x3));
      Mirror_8  : constant Raw := Sum_505x3_497x15.Add (Part_7, Mul_167x15_330.F (Scale_60_165.F (T1), Sqr_165.F (T3)))
      with Ghost;
      Part_8    : constant Raw := Part_7 + 60.0 * T1 * T3**2
      with Ghost;
      pragma Assert (Within (Mirror_8, P503x13) and then Within (Part_8, P503x13));
      Mirror_9  : constant Raw :=
        Sum_503x13_498x15.Add (Part_8, Mul_333x15_165.F (Mul_168x15_165.F (Scale_120_165.F (T1), T3), T4))
      with Ghost;
      Part_9    : constant Raw := Part_8 + 120.0 * T1 * T3 * T4
      with Ghost;
      pragma Assert (Within (Mirror_9, P504x7) and then Within (Part_9, P504x7));
      Mirror_10 : constant Raw := Sum_504x7_498x3.Add (Part_9, Mul_168x3_330.F (Scale_24_165.F (T1), Sqr_165.F (T4)))
      with Ghost;
      Part_10   : constant Raw := Part_9 + 24.0 * T1 * T4**2
      with Ghost;
      pragma Assert (Within (Mirror_10, P503x15) and then Within (Part_10, P503x15));
      Mirror_11 : constant Raw := Sum_503x15_497x13.Add (Part_10, Scale_49_495.F (Cub_165.F (T2)))
      with Ghost;
      Part_11   : constant Raw := Part_10 + 49.0 * T2**3
      with Ghost;
      pragma Assert (Within (Mirror_11, P507) and then Within (Part_11, P507));
      Mirror_12 : constant Raw := Sum_507_499x5.Add (Part_11, Mul_334x5_165.F (Scale_76_330.F (Sqr_165.F (T2)), T3))
      with Ghost;
      Part_12   : constant Raw := Part_11 + 76.0 * T2**2 * T3
      with Ghost;
      pragma Assert (Within (Mirror_12, P504x9) and then Within (Part_12, P504x9));
      Mirror_13 : constant Raw := Sum_504x9_499x3.Add (Part_12, Mul_334x3_165.F (Scale_48_330.F (Sqr_165.F (T2)), T4))
      with Ghost;
      Part_13   : constant Raw := Part_12 + 48.0 * T2**2 * T4
      with Ghost;
      pragma Assert (Within (Mirror_13, P505x5) and then Within (Part_13, P505x5));
      Mirror_14 : constant Raw :=
        Sum_505x5_496x15.Add (Part_13, Mul_166x15_330.F (Scale_30_165.F (T2), Sqr_165.F (T3)))
      with Ghost;
      Part_14   : constant Raw := Part_13 + 30.0 * T2 * T3**2
      with Ghost;
      pragma Assert (Within (Mirror_14, P504x11) and then Within (Part_14, P504x11));
      Mirror_15 : constant Raw :=
        Sum_504x11_497x15.Add (Part_14, Mul_332x15_165.F (Mul_167x15_165.F (Scale_60_165.F (T2), T3), T4))
      with Ghost;
      Part_15   : constant Raw := Part_14 + 60.0 * T2 * T3 * T4
      with Ghost;
      pragma Assert (Within (Mirror_15, P506x3) and then Within (Part_15, P506x3));
      Mirror_16 : constant Raw := Sum_506x3_497x3.Add (Part_15, Mul_167x3_330.F (Scale_12_165.F (T2), Sqr_165.F (T4)))
      with Ghost;
      Part_16   : constant Raw := Part_15 + 12.0 * T2 * T4**2
      with Ghost;
      pragma Assert (Within (Mirror_16, P504x13) and then Within (Part_16, P504x13));
      Mirror_17 : constant Raw := Sum_504x13_497.Add (Part_16, Scale_4_495.F (Cub_165.F (T3)))
      with Ghost;
      Part_17   : constant Raw := Part_16 + 4.0 * T3**3
      with Ghost;
      pragma Assert (Within (Mirror_17, P505x7) and then Within (Part_17, P505x7));
      Mirror_18 : constant Raw := Sum_505x7_497x3.Add (Part_17, Mul_332x3_165.F (Scale_12_330.F (Sqr_165.F (T3)), T4))
      with Ghost;
      Part_18   : constant Raw := Part_17 + 12.0 * T3**2 * T4
      with Ghost;
      pragma Assert (Within (Mirror_18, P504x15) and then Within (Part_18, P504x15));
      Mirror_19 : constant Raw := Sum_504x15_497x3.Add (Part_18, Mul_167x3_330.F (Scale_12_165.F (T3), Sqr_165.F (T4)))
      with Ghost;
      Part_19   : constant Raw := Part_18 + 12.0 * T3 * T4**2
      with Ghost;
      pragma Assert (Within (Mirror_19, P508) and then Within (Part_19, P508));

      Mirror : constant Raw := Widen_Dist_931x11.F (Div_939_By_24.F (Mul_431_508.F (Lead, Part_19)))
      with Ghost;
      pragma Assert (Distance_Level_0 (Mirror));

      pragma Suppress (Overflow_Check);
      Result : constant Length :=
        Cm
        * T1
        * (T1 + T2)
        * (390.0 * T1**3 + 585.0 * T1**2 * T2 + 302.0 * T1**2 * T3 + 192.0 * T1**2 * T4 + 293.0 * T1 * T2**2
           + 302.0 * T1 * T2 * T3
           + 192.0 * T1 * T2 * T4
           + 60.0 * T1 * T3**2
           + 120.0 * T1 * T3 * T4
           + 24.0 * T1 * T4**2
           + 49.0 * T2**3
           + 76.0 * T2**2 * T3
           + 48.0 * T2**2 * T4
           + 30.0 * T2 * T3**2
           + 60.0 * T2 * T3 * T4
           + 12.0 * T2 * T4**2
           + 4.0 * T3**3
           + 12.0 * T3**2 * T4
           + 12.0 * T3 * T4**2)
        / 24.0;
   begin
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_12_Distance;

   function Checked (State : Ramp_State) return Ramp_State
   with
     Pre  => State_Level_0 (State),
     Post =>
       State_Level_0 (Checked'Result) and then State_Level_2 (Checked'Result) and then State_Level_3 (Checked'Result);
   --  State, which was computed without a check on each operation.

   function Checked (State : Ramp_State) return Ramp_State is
   begin
      if not (abs State.Vel <= Velocity'Last
              and then abs State.Acc <= Acceleration'Last
              and then abs State.Jr <= Jerk'Last
              and then abs State.Sn <= Snap'Last)
      then
         raise Constraint_Error with "Unrepresentable ramp state.";
      end if;
      return State;
   end Checked;

   function Checkpoint_4 (T1, T2 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      Witness
        (Widen_Vel_758x11.F
           (Div_765_By_12.F
              (Mul_431_334.F
                 (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                  Sum_331x7_331.Add
                    (Sum_330x7_330x7.Add (Scale_7_330.F (Sqr_165.F (T1)), Mul_165x7_165.F (Scale_7_165.F (T1), T2)),
                     Scale_2_330.F (Sqr_165.F (T2)))))));
      Witness
        (Widen_Acc_595x3.F
           (Div_596x3_By_2.F
              (Mul_431_165x3.F
                 (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                  Sum_166_165.Add (Scale_2_165.F (T1), T2)))));
      Witness (Widen_Jr_431.F (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2))));
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
            Jr   => Cm * T1 * (T1 + T2),
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0,
            Vel  => Cm * T1 * (T1 + T2) * (7.0 * T1**2 + 7.0 * T1 * T2 + 2.0 * T2**2) / 12.0,
            Dist => Dist));
   end Checkpoint_4;

   function Checkpoint_8 (T1, T2, T3 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      Witness
        (Widen_Vel_762x7.F
           (Div_763x7_By_2.F
              (Mul_598_165x7.F
                 (Mul_431_167.F
                    (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                     Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3)),
                  Sum_166x3_165.Add (Sum_167_166.Add (Scale_4_165.F (T1), Scale_2_165.F (T2)), T3)))));
      Witness
        (Widen_Acc_598.F
           (Mul_431_167.F
              (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
               Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3))));
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
            Jr   => 0.0 * mm / s**3,
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3),
            Vel  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3) / 2.0,
            Dist => Dist));
   end Checkpoint_8;

   function Checkpoint_12 (T1, T2, T3, T4 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      Witness
        (Widen_Vel_764x5.F
           (Div_767x7_By_12.F
              (Mul_431_336x7.F
                 (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                  Sum_335x13_332x3.Add
                    (Sum_337x3_331x3.Add
                       (Sum_335x11_332x3.Add
                          (Sum_336x5_331x15.Add
                             (Sum_335x9_331x11.Add
                                (Sum_338_333x3.Add
                                   (Sum_336x3_332x15.Add
                                      (Sum_335x3_335x3.Add
                                         (Scale_89_330.F (Sqr_165.F (T1)), Mul_170x3_165.F (Scale_89_165.F (T1), T2)),
                                       Mul_167x15_165.F (Scale_60_165.F (T1), T3)),
                                    Mul_168x3_165.F (Scale_24_165.F (T1), T4)),
                                 Scale_22_330.F (Sqr_165.F (T2))),
                              Mul_166x15_165.F (Scale_30_165.F (T2), T3)),
                           Mul_167x3_165.F (Scale_12_165.F (T2), T4)),
                        Scale_6_330.F (Sqr_165.F (T3))),
                     Mul_167x3_165.F (Scale_12_165.F (T3), T4))))));
      Witness
        (Widen_Acc_595x5.F
           (Div_596x5_By_2.F
              (Mul_431_165x5.F
                 (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)),
                  Sum_165x3_166.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), Scale_2_165.F (T3))))));
      Witness (Widen_Jr_431.F (Neg_431.F (Mul_265_166.F (Mul_100_165.F (Cm, T1), Sum_165_165.Add (T1, T2)))));
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
            Jr   => -Cm * T1 * (T1 + T2),
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0,
            Vel  =>
              Cm
              * T1
              * (T1 + T2)
              * (89.0 * T1**2 + 89.0 * T1 * T2 + 60.0 * T1 * T3 + 24.0 * T1 * T4 + 22.0 * T2**2 + 30.0 * T2 * T3
                 + 12.0 * T2 * T4
                 + 6.0 * T3**2
                 + 12.0 * T3 * T4)
              / 12.0,
            Dist => Dist));
   end Checkpoint_12;

   function Stage_Inputs_OK
     (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length) return Boolean
   is (State_Level_3 (State)
       and then Within_Nonneg (Start, P169)
       and then Within_Nonneg (Len, P165)
       and then Within (Len, P169)
       and then Within (Stage_Crackle, P100)
       and then Within (Lo, P942)
       and then Within (Lo, P943)
       and then Above (Hi, P943))
   with Ghost;
   --  The precondition of Make_Stage.

   function Stage_Made
     (Result : Ramp_Stage; State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length)
      return Boolean
   is (Result.Start = Start
       and then Result.Len = Len
       and then Result.Lo = Clamped (State.Dist, Lo, Hi)
       and then Result.Hi = Clamped (Stage_End_Distance (State, Len, Stage_Crackle), Lo, Hi))
   with Ghost, Pre => Stage_Inputs_OK (State, Start, Len, Stage_Crackle, Lo, Hi);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Stage_Made);
   --  What Make_Stage returns as the start, the length and the limits of a stage.

   function Make_Stage
     (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length) return Ramp_Stage
   with
     No_Inline,
     Pre  =>
       State_Level_3 (State)
       and then Within_Nonneg (Start, P169)
       and then Within_Nonneg (Len, P165)
       and then Within (Len, P169)
       and then Within (Stage_Crackle, P100)
       and then Within (Lo, P942)
       and then Within (Lo, P943)
       and then Above (Hi, P943),
     Post =>
       Stage_OK (Make_Stage'Result) and then Stage_Made (Make_Stage'Result, State, Start, Len, Stage_Crackle, Lo, Hi);
   --  The stage which starts at State at time Start and nominally lasts Len, in a group of stages whose gain is to
   --  stay between Lo and Hi.

   function Make_Stage
     (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length) return Ramp_Stage
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Stage_Made);

      pragma Suppress (Overflow_Check);
      --  A finite value divided by a constant above one is finite.
   begin
      Witness (Stage_Vel.F (State.Vel));
      Witness (Stage_Acc.F (State.Acc));
      Witness (Stage_Jr.F (State.Jr));
      Witness (Stage_Sn.F (State.Sn));
      Witness (Stage_Cr.F (Stage_Crackle));
      Witness (Stage_Clamp.F (State.Dist, Lo, Hi));
      Witness (Stage_Clamp.F (Stage_End_Distance (State, Len, Stage_Crackle), Lo, Hi));
      return
        (Start => Start,
         Len   => Len,
         Vel   => State.Vel,
         Acc_2 => State.Acc / 2.0,
         Jr_6  => State.Jr / 6.0,
         Sn_24 => State.Sn / 24.0,
         C_120 => Stage_Crackle / 120.0,
         Dist  => State.Dist,
         Lo    => Length'Min (Length'Max (State.Dist, Lo), Hi),
         Hi    => Length'Min (Length'Max (Stage_End_Distance (State, Len, Stage_Crackle), Lo), Hi));
   end Make_Stage;

   procedure Lemma_Stage_Limits (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length)
   with
     Ghost,
     Always_Terminates,
     Pre  => Stage_Inputs_OK (State, Start, Len, Stage_Crackle, Lo, Hi),
     Post =>
       Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Lo
       <= Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Hi
       and then Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Hi <= Hi
       and then (if Lo <= Hi then Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Lo >= Lo)
       and then Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Start = Start
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Stage_Made);
   begin
      Lemma_Clamp_Order (State.Dist, Stage_End_Distance (State, Len, Stage_Crackle), Lo, Hi);
      Lemma_Clamp_Range (State.Dist, Lo, Hi);
      Lemma_Clamp_Range (Stage_End_Distance (State, Len, Stage_Crackle), Lo, Hi);
   end Lemma_Stage_Limits;
   --  The limits of a stage are in order, because the stage does not end below where it starts, and they are
   --  within the range of its group if that range is in order.

   procedure Lemma_Stages_Join
     (State                : Ramp_State;
      Start, Len           : Time;
      Stage_Crackle        : Crackle;
      Lo, Hi               : Length;
      Next_Start, Next_Len : Time;
      Next_Crackle         : Crackle)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       State_Level_2 (State)
       and then Within (Len, P165)
       and then Stage_Inputs_OK (State, Start, Len, Stage_Crackle, Lo, Hi)
       and then Stage_Inputs_OK (Advance (State, Len, Stage_Crackle), Next_Start, Next_Len, Next_Crackle, Lo, Hi),
     Post =>
       Make_Stage (State, Start, Len, Stage_Crackle, Lo, Hi).Hi
       <= Make_Stage (Advance (State, Len, Stage_Crackle), Next_Start, Next_Len, Next_Crackle, Lo, Hi).Lo
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Stage_Made);
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Advanced);
   begin
      Lemma_Clamp_Order
        (Stage_End_Distance (State, Len, Stage_Crackle), Advance (State, Len, Stage_Crackle).Dist, Lo, Hi);
   end Lemma_Stages_Join;
   --  Within a group a stage starts where the one before it ends: the distance of Advance is the end of the stage.

   function Stage_Span_OK (Start, Len : Time; Stage_Crackle : Crackle) return Boolean
   is (Within_Nonneg (Start, P169)
       and then Within_Nonneg (Len, P165)
       and then Within (Len, P165)
       and then Within (Len, P169)
       and then Within (Stage_Crackle, P100))
   with Ghost;
   --  The start, the length and the crackle of a stage of a ramp of the proved domain.

   function Group_Is_Ordered
     (Stage_1, Stage_2, Stage_3, Stage_4 : Ramp_Stage; Start_1, Start_2, Start_3, Start_4 : Time; Lo, Hi : Length)
      return Boolean
   is (Stage_1.Start = Start_1
       and then Stage_2.Start = Start_2
       and then Stage_3.Start = Start_3
       and then Stage_4.Start = Start_4
       and then Stage_1.Lo <= Stage_1.Hi
       and then Stage_1.Hi <= Stage_2.Lo
       and then Stage_2.Lo <= Stage_2.Hi
       and then Stage_2.Hi <= Stage_3.Lo
       and then Stage_3.Lo <= Stage_3.Hi
       and then Stage_3.Hi <= Stage_4.Lo
       and then Stage_4.Lo <= Stage_4.Hi
       and then Stage_3.Hi <= Hi
       and then Stage_4.Hi <= Hi
       and then (if Lo <= Hi then Stage_1.Lo >= Lo))
   with Ghost;
   --  Four successive stages of one group: each starts where it is said to, their limits are in order, and they
   --  are within the range of the group if that range is in order.

   procedure Lemma_Group_Limits
     (State          : Ramp_State;
      Start_1, Len_1 : Time;
      Crackle_1      : Crackle;
      Start_2, Len_2 : Time;
      Crackle_2      : Crackle;
      Start_3, Len_3 : Time;
      Crackle_3      : Crackle;
      Start_4, Len_4 : Time;
      Crackle_4      : Crackle;
      Lo, Hi         : Length)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       State_Level_0 (State)
       and then State_Level_2 (State)
       and then State_Level_3 (State)
       and then Stage_Span_OK (Start_1, Len_1, Crackle_1)
       and then Stage_Span_OK (Start_2, Len_2, Crackle_2)
       and then Stage_Span_OK (Start_3, Len_3, Crackle_3)
       and then Stage_Span_OK (Start_4, Len_4, Crackle_4)
       and then Within (Lo, P942)
       and then Within (Lo, P943)
       and then Above (Hi, P943),
     Post =>
       Group_Is_Ordered
         (Make_Stage (State, Start_1, Len_1, Crackle_1, Lo, Hi),
          Make_Stage (Advance (State, Len_1, Crackle_1), Start_2, Len_2, Crackle_2, Lo, Hi),
          Make_Stage
            (Advance (Advance (State, Len_1, Crackle_1), Len_2, Crackle_2), Start_3, Len_3, Crackle_3, Lo, Hi),
          Make_Stage
            (Advance (Advance (Advance (State, Len_1, Crackle_1), Len_2, Crackle_2), Len_3, Crackle_3),
             Start_4,
             Len_4,
             Crackle_4,
             Lo,
             Hi),
          Start_1,
          Start_2,
          Start_3,
          Start_4,
          Lo,
          Hi)
   is
      State_2 : constant Ramp_State := Advance (State, Len_1, Crackle_1);
      State_3 : constant Ramp_State := Advance (State_2, Len_2, Crackle_2);
      State_4 : constant Ramp_State := Advance (State_3, Len_3, Crackle_3);
   begin
      pragma Assert (State_Level_2 (State_2) and then State_Level_3 (State_2));
      pragma Assert (State_Level_2 (State_3) and then State_Level_3 (State_3));
      pragma Assert (State_Level_3 (State_4));
      pragma Assert (Stage_Inputs_OK (State, Start_1, Len_1, Crackle_1, Lo, Hi));
      pragma Assert (Stage_Inputs_OK (State_2, Start_2, Len_2, Crackle_2, Lo, Hi));
      pragma Assert (Stage_Inputs_OK (State_3, Start_3, Len_3, Crackle_3, Lo, Hi));
      pragma Assert (Stage_Inputs_OK (State_4, Start_4, Len_4, Crackle_4, Lo, Hi));
      Lemma_Stage_Limits (State, Start_1, Len_1, Crackle_1, Lo, Hi);
      Lemma_Stages_Join (State, Start_1, Len_1, Crackle_1, Lo, Hi, Start_2, Len_2, Crackle_2);
      Lemma_Stage_Limits (State_2, Start_2, Len_2, Crackle_2, Lo, Hi);
      Lemma_Stages_Join (State_2, Start_2, Len_2, Crackle_2, Lo, Hi, Start_3, Len_3, Crackle_3);
      Lemma_Stage_Limits (State_3, Start_3, Len_3, Crackle_3, Lo, Hi);
      Lemma_Stages_Join (State_3, Start_3, Len_3, Crackle_3, Lo, Hi, Start_4, Len_4, Crackle_4);
      Lemma_Stage_Limits (State_4, Start_4, Len_4, Crackle_4, Lo, Hi);
   end Lemma_Group_Limits;
   --  A group of stages, the first from a closed form and each of the others from the end of the one before.

   function Rising_Stage
     (Profile : Feedrate_Profile_Times; Cm : Crackle; Bounds : Ramp_Stage_Bounds; Stage : Feedrate_Profile_Stage_Index)
      return Ramp_Stage;
   --  The stage Stage of the ramp which speeds up from rest with crackle Cm, started as described above.
   --
   --  This has no contract, and must not be called in an assertion: GNATprove then analyses it within
   --  Distance_At_Time, which is where the stage is shown to be the one which Prepare makes.

   function Rising_Stage
     (Profile : Feedrate_Profile_Times; Cm : Crackle; Bounds : Ramp_Stage_Bounds; Stage : Feedrate_Profile_Stage_Index)
      return Ramp_Stage
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);

      Zero : constant Crackle := 0.0 * mm / s**5;

      State  : Ramp_State;
      Lo, Hi : Length;
   begin
      pragma Assert (Ramp_Inputs_OK (T1, T2, T3, T4, Cm));
      pragma Assert (Within (-Cm, P100));
      pragma Assert (Within (Zero, P100));
      pragma Assert (State_Level_0 (State) and then State_Level_2 (State) and then State_Level_3 (State));

      case Stage is
         when 1 .. 3   =>
            Lo := 0.0 * mm;
            Hi := Checkpoint_4_Distance (T1, T2, Cm);
            if Stage >= 2 then
               State := Advance (State, T1, Cm);
            end if;
            pragma
              Assert
                ((State_Level_0 (State) or else State_Level_1 (State))
                 and then State_Level_2 (State)
                 and then State_Level_3 (State));
            if Stage >= 3 then
               State := Advance (State, T2, Zero);
            end if;
            pragma Assert (State_Level_3 (State));

         when 4 .. 7   =>
            Lo := Checkpoint_4_Distance (T1, T2, Cm);
            Hi := Checkpoint_8_Distance (T1, T2, T3, Cm);
            State := Checkpoint_4 (T1, T2, Cm, Lo);
            if Stage >= 5 then
               State := Advance (State, T3, Zero);
            end if;
            pragma
              Assert
                ((State_Level_0 (State) or else State_Level_1 (State))
                 and then State_Level_2 (State)
                 and then State_Level_3 (State));
            if Stage >= 6 then
               State := Advance (State, T1, -Cm);
            end if;
            pragma Assert (State_Level_2 (State) and then State_Level_3 (State));
            if Stage >= 7 then
               State := Advance (State, T2, Zero);
            end if;
            pragma Assert (State_Level_3 (State));

         when 8 .. 11  =>
            Lo := Checkpoint_8_Distance (T1, T2, T3, Cm);
            Hi := Length'Max (Checkpoint_12_Distance (T1, T2, T3, T4, Cm), Lo);
            State := Checkpoint_8 (T1, T2, T3, Cm, Lo);
            if Stage >= 9 then
               State := Advance (State, T4, Zero);
            end if;
            pragma
              Assert
                ((State_Level_0 (State) or else State_Level_1 (State))
                 and then State_Level_2 (State)
                 and then State_Level_3 (State));
            if Stage >= 10 then
               State := Advance (State, T1, -Cm);
            end if;
            pragma Assert (State_Level_2 (State) and then State_Level_3 (State));
            if Stage >= 11 then
               State := Advance (State, T2, Zero);
            end if;
            pragma Assert (State_Level_3 (State));

         when 12 .. 15 =>
            Hi := Length'Last;
            Lo := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
            State := Checkpoint_12 (T1, T2, T3, T4, Cm, Lo);
            Lo := Length'Max (Lo, Checkpoint_8_Distance (T1, T2, T3, Cm));
            if Stage >= 13 then
               State := Advance (State, T3, Zero);
            end if;
            pragma
              Assert
                ((State_Level_0 (State) or else State_Level_1 (State))
                 and then State_Level_2 (State)
                 and then State_Level_3 (State));
            if Stage >= 14 then
               State := Advance (State, T1, Cm);
            end if;
            pragma Assert (State_Level_2 (State) and then State_Level_3 (State));
            if Stage = 15 then
               State := Advance (State, T2, Zero);
            end if;
            pragma Assert (State_Level_3 (State));
      end case;

      pragma Assert (State_Level_3 (State));
      pragma Assert (Within (Lo, P942) and then Within (Lo, P943) and then Above (Hi, P943));
      pragma Assert (Stage = 1 or else Within_Nonneg (Bounds (Stage - 1), P169));

      return
        Make_Stage
          (State,
           (if Stage = 1 then 0.0 * s else Bounds (Stage - 1)),
           Stage_Length (Stage, Profile),
           Stage_Crackle (Stage, Cm),
           Lo,
           Hi);
   end Rising_Stage;

   Splitter : constant Dimensionless := 134_217_729.0;
   --  2**27 + 1. A number times this, less what that product exceeds the number by, is the number rounded to its
   --  upper 26 bits, and the rest of the number then also has at most 26 bits. Products of such halves are exact.

   ------------------------------------------------------------------------------------------------------------------
   --  Bounds of the compensated polynomial
   --
   --  Ghost, down to High_Half. Nothing is proved here about what the scheme computes, only that no operation of it
   --  overflows, and for that crude bounds are enough: the upper half of a number within A is within 2**30 A, the
   --  error of a product within P is within 2**64 P, and the error of a sum within S is within 2**4 S. That makes
   --  the result at most 2**67 times the plain polynomial's bound of 2**943, where the domain leaves 2**80.
   --
   --  The three operations are used at every level of the Horner scheme, each time with larger operands. Their
   --  mirrors are generic in the bounds, and their postconditions state the bound for each level they are used at.
   ------------------------------------------------------------------------------------------------------------------

   generic
      A, W, D, H : Raw;
   package High_Half_Bound with Ghost is
      package Wide is new Bounded_Scale (Splitter, A, W);
      package Rest is new Bounded_Sum (W, A, D);
      package High is new Bounded_Sum (W, D, H);

      function F (V : Raw) return Raw
      is (High.Sub (Wide.F (V), Rest.Sub (Wide.F (V), V)))
      with Pre => Within (V, A), Post => Within (F'Result, H);
      --  The mirror of High_Half for a number within A.
   end High_Half_Bound;

   generic
      A, AH, AL, P, E1, E2, E3, E4, E : Raw;
   package Product_Error_Bound with Ghost is
      package A_Rest is new Bounded_Sum (A, AH, AL);
      package X_Rest is new Bounded_Sum (P169, P199, P200);
      package Low_Low is new Bounded_Product (AL, P200, E3);
      package High_High is new Bounded_Product (AH, P199, E1);
      package Low_High is new Bounded_Product (AL, P199, E2);
      package High_Low is new Bounded_Product (AH, P200, E2);
      package First is new Bounded_Sum (P, E1, E2);
      package Second is new Bounded_Sum (E2, E2, E3);
      package Third is new Bounded_Sum (E3, E2, E4);
      package Last is new Bounded_Sum (E3, E4, E);

      function F (V, V_High, X, X_High, Product : Raw) return Raw
      is (Last.Sub
            (Low_Low.F (A_Rest.Sub (V, V_High), X_Rest.Sub (X, X_High)),
             Third.Sub
               (Second.Sub
                  (First.Sub (Product, High_High.F (V_High, X_High)), Low_High.F (A_Rest.Sub (V, V_High), X_High)),
                High_Low.F (V_High, X_Rest.Sub (X, X_High)))))
      with
        Pre  =>
          Within (V, A)
          and then Within (V_High, AH)
          and then Within (X, P169)
          and then Within (X_High, P199)
          and then Within (Product, P),
        Post => Within (F'Result, E);
      --  The mirror of Product_Error for a factor within A, its upper half within AH and their product with a time
      --  within P.
   end Product_Error_Bound;

   generic
      A, C, O, B, L, E : Raw;
   package Sum_Error_Bound with Ghost is
      package Other is new Bounded_Sum (C, A, O);
      package Back is new Bounded_Sum (C, O, B);
      package Left is new Bounded_Sum (A, B, L);
      package Right is new Bounded_Sum (A, O, B);
      package Both is new Bounded_Sum (L, B, E);

      function F (V, W, Sum : Raw) return Raw
      is (Both.Add (Left.Sub (V, Back.Sub (Sum, Other.Sub (Sum, V))), Right.Sub (W, Other.Sub (Sum, V))))
      with Pre => Within (V, A) and then Within (W, A) and then Within (Sum, C), Post => Within (F'Result, E);
      --  The mirror of Sum_Error for terms within A and their sum within C.
   end Sum_Error_Bound;

   package High_Half_94 is new High_Half_Bound (P94, P122, P123, P124);
   package High_Half_169 is new High_Half_Bound (P169, P197, P198, P199);
   package High_Half_264 is new High_Half_Bound (P264, P292, P293, P294);
   package High_Half_434 is new High_Half_Bound (P434, P462, P463, P464);
   package High_Half_604 is new High_Half_Bound (P604, P632, P633, P634);
   package High_Half_774 is new High_Half_Bound (P774, P802, P803, P804);

   package Product_Error_94 is new Product_Error_Bound (P94, P124, P125, P263, P323, P324, P325, P326, P327);
   package Product_Error_264 is new Product_Error_Bound (P264, P294, P295, P433, P493, P494, P495, P496, P497);
   package Product_Error_434 is new Product_Error_Bound (P434, P464, P465, P603, P663, P664, P665, P666, P667);
   package Product_Error_604 is new Product_Error_Bound (P604, P634, P635, P773, P833, P834, P835, P836, P837);
   package Product_Error_774 is new Product_Error_Bound (P774, P804, P805, P943, P1003, P1004, P1005, P1006, P1007);

   package Sum_Error_263 is new Sum_Error_Bound (P263, P264, P265, P266, P267, P268);
   package Sum_Error_433 is new Sum_Error_Bound (P433, P434, P435, P436, P437, P438);
   package Sum_Error_603 is new Sum_Error_Bound (P603, P604, P605, P606, P607, P608);
   package Sum_Error_773 is new Sum_Error_Bound (P773, P774, P775, P776, P777, P778);

   package Unit_94 is new Bounded_Unit (P94, P94);
   package Unit_169 is new Bounded_Unit (P169, P169);
   package Unit_263 is new Bounded_Unit (P263, P263);
   package Unit_433 is new Bounded_Unit (P433, P433);
   package Unit_603 is new Bounded_Unit (P603, P603);
   package Unit_773 is new Bounded_Unit (P773, P773);
   package Unit_1010 is new Bounded_Unit (P1010, P1010);

   package Mul_94_169 is new Bounded_Product (P94, P169, P263);
   package Mul_264_169 is new Bounded_Product (P264, P169, P433);
   package Mul_328_169 is new Bounded_Product (P328, P169, P497);
   package Mul_434_169 is new Bounded_Product (P434, P169, P603);
   package Mul_499_169 is new Bounded_Product (P499, P169, P668);
   package Mul_604_169 is new Bounded_Product (P604, P169, P773);
   package Mul_669_169 is new Bounded_Product (P669, P169, P838);
   package Mul_774_169 is new Bounded_Product (P774, P169, P943);
   package Mul_839_169 is new Bounded_Product (P839, P169, P1008);
   package Sum_263_263 is new Bounded_Sum (P263, P263, P264);
   package Sum_327_268 is new Bounded_Sum (P327, P268, P328);
   package Sum_433_433 is new Bounded_Sum (P433, P433, P434);
   package Sum_497_438 is new Bounded_Sum (P497, P438, P498);
   package Sum_497_498 is new Bounded_Sum (P497, P498, P499);
   package Sum_603_603 is new Bounded_Sum (P603, P603, P604);
   package Sum_667_608 is new Bounded_Sum (P667, P608, P668);
   package Sum_668_668 is new Bounded_Sum (P668, P668, P669);
   package Sum_773_773 is new Bounded_Sum (P773, P773, P774);
   package Sum_837_778 is new Bounded_Sum (P837, P778, P838);
   package Sum_838_838 is new Bounded_Sum (P838, P838, P839);
   package Sum_943_1009 is new Bounded_Sum (P943, P1009, P1010);
   package Sum_1008_1007 is new Bounded_Sum (P1008, P1007, P1009);

   function High_Half (A : Dimensionless) return Dimensionless
   with
     Inline_Always,
     Pre  => Within (A, P774),
     Post =>
       Within (High_Half'Result, P804)
       and then (if Within (A, P94) then Within (High_Half'Result, P124))
       and then (if Within (A, P169) then Within (High_Half'Result, P199))
       and then (if Within (A, P264) then Within (High_Half'Result, P294))
       and then (if Within (A, P434) then Within (High_Half'Result, P464))
       and then (if Within (A, P604) then Within (High_Half'Result, P634));
   --  A rounded to its upper 26 bits.

   function Product_Error (A, A_High, X, X_High, Product : Dimensionless) return Dimensionless
   with
     Inline_Always,
     Pre  =>
       Within (A, P774)
       and then Within (A_High, P804)
       and then Within (X, P169)
       and then Within (X_High, P199)
       and then Within (Product, P943),
     Post =>
       Within (Product_Error'Result, P1007)
       and then
         (if Within (A, P94) and then Within (A_High, P124) and then Within (Product, P263)
          then Within (Product_Error'Result, P327))
       and then
         (if Within (A, P264) and then Within (A_High, P294) and then Within (Product, P433)
          then Within (Product_Error'Result, P497))
       and then
         (if Within (A, P434) and then Within (A_High, P464) and then Within (Product, P603)
          then Within (Product_Error'Result, P667))
       and then
         (if Within (A, P604) and then Within (A_High, P634) and then Within (Product, P773)
          then Within (Product_Error'Result, P837));
   --  What the rounding of A * X to Product lost, exactly, from the halves of A and X.

   function Sum_Error (A, B, Sum : Dimensionless) return Dimensionless
   with
     Inline_Always,
     Pre  => Within (A, P773) and then Within (B, P773) and then Within (Sum, P774),
     Post =>
       Within (Sum_Error'Result, P778)
       and then
         (if Within (A, P263) and then Within (B, P263) and then Within (Sum, P264)
          then Within (Sum_Error'Result, P268))
       and then
         (if Within (A, P433) and then Within (B, P433) and then Within (Sum, P434)
          then Within (Sum_Error'Result, P438))
       and then
         (if Within (A, P603) and then Within (B, P603) and then Within (Sum, P604)
          then Within (Sum_Error'Result, P608));
   --  What the rounding of A + B to Sum lost, exactly.

   function Compensated_Local (Stage : Ramp_Stage; X : Time) return Length
   with Pre => Stage_OK (Stage) and then Within (X, P169), Post => Within (Compensated_Local'Result, P1010);
   --  X * (Vel + X * (Acc_2 + X * (Jr_6 + X * (Sn_24 + X * C_120)))) by the Horner scheme, with the rounding error
   --  of every product and sum carried along in a second Horner scheme and added at the end. The error of a product
   --  is exact from the halves of its factors, so no target needs a fused multiply-add, and every target computes
   --  the same value provided that it fuses nothing here itself (see -ffp-contract=off above).

   function High_Half (A : Dimensionless) return Dimensionless is
      Mirror     : constant Raw := High_Half_774.F (A)
      with Ghost;
      Mirror_94  : constant Raw := (if Within (A, P94) then High_Half_94.F (A) else 0.0)
      with Ghost;
      Mirror_169 : constant Raw := (if Within (A, P169) then High_Half_169.F (A) else 0.0)
      with Ghost;
      Mirror_264 : constant Raw := (if Within (A, P264) then High_Half_264.F (A) else 0.0)
      with Ghost;
      Mirror_434 : constant Raw := (if Within (A, P434) then High_Half_434.F (A) else 0.0)
      with Ghost;
      Mirror_604 : constant Raw := (if Within (A, P604) then High_Half_604.F (A) else 0.0)
      with Ghost;
      pragma Assert (Within (Mirror, P804));
      pragma Assert (if Within (A, P94) then Within (Mirror_94, P124));
      pragma Assert (if Within (A, P169) then Within (Mirror_169, P199));
      pragma Assert (if Within (A, P264) then Within (Mirror_264, P294));
      pragma Assert (if Within (A, P434) then Within (Mirror_434, P464));
      pragma Assert (if Within (A, P604) then Within (Mirror_604, P634));

      pragma Suppress (Overflow_Check);
      Wide : constant Dimensionless := Splitter * A;
   begin
      return Wide - (Wide - A);
   end High_Half;

   function Product_Error (A, A_High, X, X_High, Product : Dimensionless) return Dimensionless is
      Mirror     : constant Raw := Product_Error_774.F (A, A_High, X, X_High, Product)
      with Ghost;
      Mirror_94  : constant Raw :=
        (if Within (A, P94) and then Within (A_High, P124) and then Within (Product, P263)
         then Product_Error_94.F (A, A_High, X, X_High, Product)
         else 0.0)
      with Ghost;
      Mirror_264 : constant Raw :=
        (if Within (A, P264) and then Within (A_High, P294) and then Within (Product, P433)
         then Product_Error_264.F (A, A_High, X, X_High, Product)
         else 0.0)
      with Ghost;
      Mirror_434 : constant Raw :=
        (if Within (A, P434) and then Within (A_High, P464) and then Within (Product, P603)
         then Product_Error_434.F (A, A_High, X, X_High, Product)
         else 0.0)
      with Ghost;
      Mirror_604 : constant Raw :=
        (if Within (A, P604) and then Within (A_High, P634) and then Within (Product, P773)
         then Product_Error_604.F (A, A_High, X, X_High, Product)
         else 0.0)
      with Ghost;
      pragma Assert (Within (Mirror, P1007));
      pragma
        Assert
          (if Within (A, P94) and then Within (A_High, P124) and then Within (Product, P263)
           then Within (Mirror_94, P327));
      pragma
        Assert
          (if Within (A, P264) and then Within (A_High, P294) and then Within (Product, P433)
           then Within (Mirror_264, P497));
      pragma
        Assert
          (if Within (A, P434) and then Within (A_High, P464) and then Within (Product, P603)
           then Within (Mirror_434, P667));
      pragma
        Assert
          (if Within (A, P604) and then Within (A_High, P634) and then Within (Product, P773)
           then Within (Mirror_604, P837));

      pragma Suppress (Overflow_Check);
      A_Low : constant Dimensionless := A - A_High;
      X_Low : constant Dimensionless := X - X_High;
   begin
      return A_Low * X_Low - (((Product - A_High * X_High) - A_Low * X_High) - A_High * X_Low);
   end Product_Error;

   function Sum_Error (A, B, Sum : Dimensionless) return Dimensionless is
      Mirror     : constant Raw := Sum_Error_773.F (A, B, Sum)
      with Ghost;
      Mirror_263 : constant Raw :=
        (if Within (A, P263) and then Within (B, P263) and then Within (Sum, P264)
         then Sum_Error_263.F (A, B, Sum)
         else 0.0)
      with Ghost;
      Mirror_433 : constant Raw :=
        (if Within (A, P433) and then Within (B, P433) and then Within (Sum, P434)
         then Sum_Error_433.F (A, B, Sum)
         else 0.0)
      with Ghost;
      Mirror_603 : constant Raw :=
        (if Within (A, P603) and then Within (B, P603) and then Within (Sum, P604)
         then Sum_Error_603.F (A, B, Sum)
         else 0.0)
      with Ghost;
      pragma Assert (Within (Mirror, P778));
      pragma
        Assert
          (if Within (A, P263) and then Within (B, P263) and then Within (Sum, P264) then Within (Mirror_263, P268));
      pragma
        Assert
          (if Within (A, P433) and then Within (B, P433) and then Within (Sum, P434) then Within (Mirror_433, P438));
      pragma
        Assert
          (if Within (A, P603) and then Within (B, P603) and then Within (Sum, P604) then Within (Mirror_603, P608));

      pragma Suppress (Overflow_Check);
      Other : constant Dimensionless := Sum - A;
   begin
      return (A - (Sum - Other)) + (B - Other);
   end Sum_Error;

   function Compensated_Local (Stage : Ramp_Stage; X : Time) return Length is
      pragma Suppress (Overflow_Check);
      --  An operation below which overflows, or meets a NaN, leaves a result which is not finite: every later
      --  operation is a sum or a product. Testing the result once therefore raises wherever a check on each
      --  operation would, those of the plain scheme among them.

      Mirror_X : constant Raw := Unit_169.Over (X)
      with Ghost;
      Mirror_5 : constant Raw := Unit_94.Over (Stage.C_120)
      with Ghost;
      Mirror_4 : constant Raw := Unit_263.Over (Stage.Sn_24)
      with Ghost;
      Mirror_3 : constant Raw := Unit_433.Over (Stage.Jr_6)
      with Ghost;
      Mirror_2 : constant Raw := Unit_603.Over (Stage.Acc_2)
      with Ghost;
      Mirror_1 : constant Raw := Unit_773.Over (Stage.Vel)
      with Ghost;
      pragma
        Assert
          (Within (Mirror_X, P169)
           and then Within (Mirror_5, P94)
           and then Within (Mirror_4, P263)
           and then Within (Mirror_3, P433)
           and then Within (Mirror_2, P603)
           and then Within (Mirror_1, P773));

      Raw_X  : constant Dimensionless := X / s;
      X_High : constant Dimensionless := High_Half (Raw_X);

      C_5 : constant Dimensionless := Stage.C_120 / (mm / s**5);
      C_4 : constant Dimensionless := Stage.Sn_24 / (mm / s**4);
      C_3 : constant Dimensionless := Stage.Jr_6 / (mm / s**3);
      C_2 : constant Dimensionless := Stage.Acc_2 / (mm / s**2);
      C_1 : constant Dimensionless := Stage.Vel / (mm / s);

      pragma Assert (Within (Raw_X, P169) and then Within (X_High, P199));
      pragma
        Assert
          (Within (C_5, P94)
           and then Within (C_4, P263)
           and then Within (C_3, P433)
           and then Within (C_2, P603)
           and then Within (C_1, P773));

      --  P is a product by X and S the sum of it and the next coefficient, both rounded. R is what the scheme has
      --  lost so far, itself carried by the Horner scheme.
      Mirror_P_4 : constant Raw := Mul_94_169.F (C_5, Raw_X)
      with Ghost;
      P_4        : constant Dimensionless := C_5 * Raw_X;
      pragma Assert (Within (Mirror_P_4, P263) and then Within (P_4, P263));
      Mirror_S_4 : constant Raw := Sum_263_263.Add (P_4, C_4)
      with Ghost;
      S_4        : constant Dimensionless := P_4 + C_4;
      pragma Assert (Within (Mirror_S_4, P264) and then Within (S_4, P264));
      High_5     : constant Raw := High_Half (C_5)
      with Ghost;
      pragma Assert (Within (High_5, P124));
      Lost_P_4   : constant Raw := Product_Error (C_5, High_5, Raw_X, X_High, P_4)
      with Ghost;
      Lost_S_4   : constant Raw := Sum_Error (P_4, C_4, S_4)
      with Ghost;
      pragma Assert (Within (Lost_P_4, P327) and then Within (Lost_S_4, P268));
      Mirror_R_4 : constant Raw := Sum_327_268.Add (Lost_P_4, Lost_S_4)
      with Ghost;
      R_4        : constant Dimensionless :=
        Product_Error (C_5, High_Half (C_5), Raw_X, X_High, P_4) + Sum_Error (P_4, C_4, S_4);
      pragma Assert (Within (Mirror_R_4, P328) and then Within (R_4, P328));

      Mirror_P_3 : constant Raw := Mul_264_169.F (S_4, Raw_X)
      with Ghost;
      P_3        : constant Dimensionless := S_4 * Raw_X;
      pragma Assert (Within (Mirror_P_3, P433) and then Within (P_3, P433));
      Mirror_S_3 : constant Raw := Sum_433_433.Add (P_3, C_3)
      with Ghost;
      S_3        : constant Dimensionless := P_3 + C_3;
      pragma Assert (Within (Mirror_S_3, P434) and then Within (S_3, P434));
      High_4     : constant Raw := High_Half (S_4)
      with Ghost;
      pragma Assert (Within (High_4, P294));
      Lost_P_3   : constant Raw := Product_Error (S_4, High_4, Raw_X, X_High, P_3)
      with Ghost;
      Lost_S_3   : constant Raw := Sum_Error (P_3, C_3, S_3)
      with Ghost;
      pragma Assert (Within (Lost_P_3, P497) and then Within (Lost_S_3, P438));
      Mirror_R_3 : constant Raw := Sum_497_498.Add (Mul_328_169.F (R_4, Raw_X), Sum_497_438.Add (Lost_P_3, Lost_S_3))
      with Ghost;
      R_3        : constant Dimensionless :=
        R_4 * Raw_X + (Product_Error (S_4, High_Half (S_4), Raw_X, X_High, P_3) + Sum_Error (P_3, C_3, S_3));
      pragma Assert (Within (Mirror_R_3, P499) and then Within (R_3, P499));

      Mirror_P_2 : constant Raw := Mul_434_169.F (S_3, Raw_X)
      with Ghost;
      P_2        : constant Dimensionless := S_3 * Raw_X;
      pragma Assert (Within (Mirror_P_2, P603) and then Within (P_2, P603));
      Mirror_S_2 : constant Raw := Sum_603_603.Add (P_2, C_2)
      with Ghost;
      S_2        : constant Dimensionless := P_2 + C_2;
      pragma Assert (Within (Mirror_S_2, P604) and then Within (S_2, P604));
      High_3     : constant Raw := High_Half (S_3)
      with Ghost;
      pragma Assert (Within (High_3, P464));
      Lost_P_2   : constant Raw := Product_Error (S_3, High_3, Raw_X, X_High, P_2)
      with Ghost;
      Lost_S_2   : constant Raw := Sum_Error (P_2, C_2, S_2)
      with Ghost;
      pragma Assert (Within (Lost_P_2, P667) and then Within (Lost_S_2, P608));
      Mirror_R_2 : constant Raw := Sum_668_668.Add (Mul_499_169.F (R_3, Raw_X), Sum_667_608.Add (Lost_P_2, Lost_S_2))
      with Ghost;
      R_2        : constant Dimensionless :=
        R_3 * Raw_X + (Product_Error (S_3, High_Half (S_3), Raw_X, X_High, P_2) + Sum_Error (P_2, C_2, S_2));
      pragma Assert (Within (Mirror_R_2, P669) and then Within (R_2, P669));

      Mirror_P_1 : constant Raw := Mul_604_169.F (S_2, Raw_X)
      with Ghost;
      P_1        : constant Dimensionless := S_2 * Raw_X;
      pragma Assert (Within (Mirror_P_1, P773) and then Within (P_1, P773));
      Mirror_S_1 : constant Raw := Sum_773_773.Add (P_1, C_1)
      with Ghost;
      S_1        : constant Dimensionless := P_1 + C_1;
      pragma Assert (Within (Mirror_S_1, P774) and then Within (S_1, P774));
      High_2     : constant Raw := High_Half (S_2)
      with Ghost;
      pragma Assert (Within (High_2, P634));
      Lost_P_1   : constant Raw := Product_Error (S_2, High_2, Raw_X, X_High, P_1)
      with Ghost;
      Lost_S_1   : constant Raw := Sum_Error (P_1, C_1, S_1)
      with Ghost;
      pragma Assert (Within (Lost_P_1, P837) and then Within (Lost_S_1, P778));
      Mirror_R_1 : constant Raw := Sum_838_838.Add (Mul_669_169.F (R_2, Raw_X), Sum_837_778.Add (Lost_P_1, Lost_S_1))
      with Ghost;
      R_1        : constant Dimensionless :=
        R_2 * Raw_X + (Product_Error (S_2, High_Half (S_2), Raw_X, X_High, P_1) + Sum_Error (P_1, C_1, S_1));
      pragma Assert (Within (Mirror_R_1, P839) and then Within (R_1, P839));

      Mirror_P_0 : constant Raw := Mul_774_169.F (S_1, Raw_X)
      with Ghost;
      P_0        : constant Dimensionless := S_1 * Raw_X;
      pragma Assert (Within (Mirror_P_0, P943) and then Within (P_0, P943));
      High_1     : constant Raw := High_Half (S_1)
      with Ghost;
      pragma Assert (Within (High_1, P804));
      Lost_P_0   : constant Raw := Product_Error (S_1, High_1, Raw_X, X_High, P_0)
      with Ghost;
      pragma Assert (Within (Lost_P_0, P1007));
      Mirror     : constant Raw := Sum_943_1009.Add (P_0, Sum_1008_1007.Add (Mul_839_169.F (R_1, Raw_X), Lost_P_0))
      with Ghost;
      Result     : constant Dimensionless :=
        P_0 + (R_1 * Raw_X + Product_Error (S_1, High_Half (S_1), Raw_X, X_High, P_0));
      pragma Assert (Within (Mirror, P1010));
   begin
      pragma Assert (Within (Result, P1010));
      Witness (Unit_1010.Times (Result));
      if not (abs Result <= Dimensionless'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result * mm;
   end Compensated_Local;

   function Stage_Offset (Stage : Ramp_Stage; T : Time) return Time
   is (Time'Min (T - Stage.Start, Stage.Len))
   with Ghost, Pre => Stage_OK (Stage) and then Within_Nonneg (T, P169), Post => Within (Stage_Offset'Result, P169);
   --  The time into Stage at which its polynomial is evaluated for the time T.

   function Gain_Made (Result : Length; Stage : Ramp_Stage; T : Time) return Boolean
   is (Result = Clamped (Stage.Dist + Compensated_Local (Stage, Stage_Offset (Stage, T)), Stage.Lo, Stage.Hi))
   with Ghost, Pre => Stage_OK (Stage) and then Within_Nonneg (T, P169);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Gain_Made);
   --  What Stage_Gain returns.

   function Stage_Gain (Stage : Ramp_Stage; T : Time) return Length
   with
     No_Inline,
     Pre  => Stage_OK (Stage) and then Within_Nonneg (T, P169),
     Post => Within (Stage_Gain'Result, P943) and then Gain_Made (Stage_Gain'Result, Stage, T);
   --  The gain at T, which Stage contains.

   function Stage_Gain (Stage : Ramp_Stage; T : Time) return Length is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Gain_Made);
   begin
      Witness (Offset_169.F (T, Stage.Start));
      pragma Assert (Within (Time'Min (T - Stage.Start, Stage.Len), P169));
      Witness (Gain_Sum.Add (Stage.Dist, Compensated_Local (Stage, Time'Min (T - Stage.Start, Stage.Len))));
      Witness
        (Gain_Limit.F
           (Stage.Dist + Compensated_Local (Stage, Time'Min (T - Stage.Start, Stage.Len)), Stage.Lo, Stage.Hi));
      return
        Length'Min
          (Length'Max (Stage.Dist + Compensated_Local (Stage, Time'Min (T - Stage.Start, Stage.Len)), Stage.Lo),
           Stage.Hi);
   end Stage_Gain;

   Rest_Tolerance : constant Dimensionless := 2.0**(-40);
   --  A ramp which was solved to end at rest has a final speed within a few units in the last place of its starting
   --  speed of zero, on either side, which is hundreds of times less than this fraction of it. A final speed this
   --  little below zero is taken for rest, which lengthens the distance at a time by at most that speed times the
   --  time. A ramp which ends further below zero is taken to turn back.

   ------------------------------------------------------------------------------------------------------------------
   --  Bounds of the shape of a ramp
   --
   --  Ghost, down to Shape_Of. A ramp which slows down is evaluated from its final speed, which is at most its
   --  starting speed, and from the distance of the whole ramp. The postcondition of Distance_At_Time bounds the
   --  distance relative to the starting speed, so the distance of the whole ramp has to be bounded relative to the
   --  final speed: it is at most travel at that speed for the longest ramp, and a little more.
   ------------------------------------------------------------------------------------------------------------------

   Part_128 : constant Raw := 2#1.0#E-7
   with Ghost;

   package Sum_772_765 is new Bounded_Sum (P772, P765, P773);
   package Mul_773_165x15 is new Bounded_Product (P773, P165x15, P942);
   package Sym_943 is new Symmetric_Bound (P943);
   package Slowing_Travel is new Nonnegative_Product (P800, P165x15, P969);

   function Nonpositive (X : Raw) return Boolean
   is (X <= 0.0)
   with Ghost;

   function Whole_OK (Whole, Vel : Raw) return Boolean
   is (Within (Whole, P970) and then Whole <= Ramp_Reach (Vel) + P942)
   with Ghost, Pre => Within (Vel, P800);
   --  The distance of a whole ramp which starts at the speed Vel.

   function Shape_OK (Shape : Ramp_Shape) return Boolean
   is (Within_Nonneg (Shape.Total, P165x15)
       and then Within (Shape.Vel, P800)
       and then
         (if Shape.Kind = Slows_Down then Within_Nonneg (Shape.Vel, P800) and then Whole_OK (Shape.Whole, Shape.Vel)))
   with Ghost;
   --  The bounds of the shape of a ramp of the proved domain.

   generic
      Limit, U, Final : Raw;
   package Relative_End with Ghost is
      function Add (X, Z, I : Raw) return Raw
      is (Z + I)
      with
        Pre  => Within_Nonneg (X, Limit) and then Symmetric (Z, X) and then Symmetric (I, U),
        Post => abs Add'Result <= X + Final;

      function Sub (X, Z, I : Raw) return Raw
      is (Z - I)
      with
        Pre  => Within_Nonneg (X, Limit) and then Symmetric (Z, X) and then Symmetric (I, U),
        Post => abs Sub'Result <= X + Final;
      --  A value within X, and an increment within U more or less.
   end Relative_End;

   generic
      A, U, C : Raw;
   package Lower_Sum with Ghost is
      function F (X, Y : Raw) return Raw
      is (X + Y)
      with Pre => Within_Nonneg (X, A) and then Within (Y, U), Post => Within (F'Result, C) and then F'Result >= -U;
      --  A sum which is not far below zero, because its first term is not below it.
   end Lower_Sum;

   generic
      Limit, BW, A, U, C, Final : Raw;
   package Upper_Difference with Ghost is
      function F (X, W, B : Raw) return Raw
      is (W - B)
      with
        Pre  =>
          Within_Nonneg (X, Limit) and then W <= X + BW and then Within (W, A) and then Within (B, A) and then B >= -U,
        Post => Within (F'Result, C) and then F'Result <= X + Final;
      --  What is left of a value at most BW above X when something not far below zero is taken from it.
   end Upper_Difference;

   package Travel_End is new Relative_End (P969, P943, P950);
   package Slowing_Reach is new Lower_Sum (P969, P943, P970);
   package Slowing_Rest is new Upper_Difference (P969, P942, P970, P943, P971, P950);

   procedure Lemma_Nonpositive_Product (X, Y : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => X <= 0.0 and then Y >= 0.0 and then Within (X, P598) and then Within (Y, P168),
     Post => X * Y <= 0.0;

   procedure Lemma_Nonpositive_Product (X, Y : Raw) is null;

   procedure Lemma_Slower (V, G : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (V, P800) and then G <= 0.0 and then Within (G, P766),
     Post => V + G <= V;

   procedure Lemma_Slower (V, G : Raw) is null;

   procedure Lemma_Slowing_End (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Formula_In_Range (T1, T2, T3, T4, Max_Crackle, Start_Vel) and then Nonpositive (Max_Crackle),
     Post => Velocity_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel) <= Start_Vel
   is
      Snap_Peak : constant Raw := Mul_100_165.F (Max_Crackle, T1);
      Jerk_Peak : constant Raw := Mul_265_166.F (Snap_Peak, Sum_165_165.Add (T1, T2));
      Acc_Peak  : constant Raw :=
        Mul_431_167.F (Jerk_Peak, Sum_165x3_165.Add (Sum_166_165.Add (Scale_2_165.F (T1), T2), T3));
      Gain      : constant Raw := Mirrored_Gain (T1, T2, T3, T4, Max_Crackle);
   begin
      Lemma_Stage_Starts (T1, T2, T3, T4);
      Lemma_Nonpositive_Product (Max_Crackle, T1);
      Lemma_Nonpositive_Product (Snap_Peak, T1 + T2);
      Lemma_Nonpositive_Product (Jerk_Peak, 2.0 * T1 + T2 + T3);
      Lemma_Nonpositive_Product (Acc_Peak, 4.0 * T1 + 2.0 * T2 + T3 + T4);
      pragma Assert (Nonpositive (Gain));
      Lemma_Slower (Start_Vel, Gain);
      Lemma_Unit (Start_766.F (Start_Vel, Sym_766.F (Gain)));
   end Lemma_Slowing_End;
   --  A ramp with crackle which is not positive ends no faster than it starts: the change of speed is a product of
   --  the crackle and of sums of phase times, and each partial product keeps its sign.

   procedure Lemma_Duration_Sign (T1, T2, T3, T4 : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Phase_Times_OK (T1, T2, T3, T4),
     Post => Within_Nonneg (Mirrored_Duration (T1, T2, T3, T4), P165x15)
   is
   begin
      pragma Assert (Scale_8_165.F (T1) >= 0.0);
      pragma Assert (Scale_4_165.F (T2) >= 0.0);
      pragma Assert (Sum_168_167.Add (Scale_8_165.F (T1), Scale_4_165.F (T2)) >= 0.0);
      pragma Assert (Scale_2_165.F (T3) >= 0.0);
      pragma
        Assert
          (Sum_167x3_166.Add (Sum_168_167.Add (Scale_8_165.F (T1), Scale_4_165.F (T2)), Scale_2_165.F (T3)) >= 0.0);
   end Lemma_Duration_Sign;

   procedure Lemma_Mean_Speed (V, H : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => V >= P772 and then V <= P800 and then Within (H, P765),
     Post => V + H >= 0.0 and then V + H <= V + V * Part_128;

   procedure Lemma_Mean_Speed (V, H : Raw) is null;
   --  Half the change of speed is little beside a starting speed above P772.

   procedure Lemma_Travelled (M, Q, D : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => M >= 0.0 and then M <= Q and then Q <= P801 and then Within_Nonneg (D, P165x15),
     Post => M * D <= Q * P165x15;

   procedure Lemma_Travelled (M, Q, D : Raw) is null;

   procedure Lemma_Scaled_Travel (V : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => V >= P772 and then V <= P800,
     Post => (V + V * Part_128) * P165x15 <= V * P169;

   procedure Lemma_Scaled_Travel (V : Raw) is null;
   --  A ramp lasts at most fifteen phase times, and P169 is sixteen.

   procedure Lemma_Reach_Slack (W, R : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (R, P969) and then (W <= P942 or else W <= R),
     Post => W <= R + P942;

   procedure Lemma_Reach_Slack (W, R : Raw) is null;

   procedure Lemma_Whole (V, H, D : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (V, P800) and then Within (H, P765) and then Within_Nonneg (D, P165x15),
     Post => Mul_797x9_165x15.F (Sum_800_765.Add (V, H), D) <= Ramp_Reach (V) + P942
   is
      Whole : constant Raw := Mul_797x9_165x15.F (Sum_800_765.Add (V, H), D);
   begin
      pragma Assert (Whole = (V + H) * D);
      if V <= P772 then
         Witness (Mul_773_165x15.F (Sum_772_765.Add (V, H), D));
         pragma Assert (Within ((V + H) * D, P942));
         pragma Assert (Whole <= P942);
      else
         Lemma_Mean_Speed (V, H);
         Lemma_Travelled (V + H, V + V * Part_128, D);
         Lemma_Scaled_Travel (V);
         pragma Assert ((V + H) * D <= V * P169);
         pragma Assert (V * P169 = Ramp_Reach (V));
         pragma Assert (Whole <= Ramp_Reach (V));
      end if;
      Lemma_Reach_Slack (Whole, Ramp_Reach (V));
   end Lemma_Whole;
   --  The mean speed V + H over the duration D covers at most travel at V for the longest ramp and P942 more: a
   --  small V leaves the product small, and a large one is hardly changed by H.

   procedure Lemma_Slowing_Whole (T1, T2, T3, T4 : Time; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Formula_In_Range (T1, T2, T3, T4, Max_Crackle, Start_Vel) and then Within_Nonneg (Start_Vel, P800),
     Post => Whole_OK (Distance_Formula (T1, T2, T3, T4, Max_Crackle, Start_Vel), Start_Vel)
   is
   begin
      Lemma_Duration_Sign (T1, T2, T3, T4);
      Lemma_Whole
        (Start_Vel, Div_766_By_2.F (Mirrored_Gain (T1, T2, T3, T4, Max_Crackle)), Mirrored_Duration (T1, T2, T3, T4));
      Lemma_Unit
        (Widen_966x9_970.F
           (Mul_797x9_165x15.F
              (Sum_800_765.Add (Start_Vel, Div_766_By_2.F (Mirrored_Gain (T1, T2, T3, T4, Max_Crackle))),
               Mirrored_Duration (T1, T2, T3, T4))));
   end Lemma_Slowing_Whole;

   procedure Lemma_Travel_Reach (V, Start : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Start, P800) and then abs V <= abs Start,
     Post => Travel_Reach (V) <= Travel_Reach (Start);

   procedure Lemma_Travel_Reach (V, Start : Raw) is null;
   --  The bound of the distance of a ramp which is evaluated from a speed no larger than the one it starts at.

   function Kind_Made
     (Result : Ramp_Shape; Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
      return Boolean
   is (if Max_Crackle >= 0.0 * mm / s**5
       then Result.Kind = Speeds_Up and then Result.Vel = Start_Vel
       elsif Fast_Velocity_At_Max_Time (Profile, Max_Crackle, Start_Vel) >= 0.0 * mm / s
       then Result.Kind = Slows_Down)
   with Ghost, Pre => Ramp_In_Range (Profile, Max_Crackle, Start_Vel);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Kind_Made);
   --  Which kind of ramp Shape_Of finds: one which speeds up, and one which slows down to a speed which is not
   --  negative, are what they are.

   function Shape_Of (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Shape
   with
     No_Inline,
     Pre  => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post =>
       Shape_OK (Shape_Of'Result)
       and then Shape_Of'Result.Total = Total_Time (Profile)
       and then abs Shape_Of'Result.Vel <= abs Start_Vel
       and then Kind_Made (Shape_Of'Result, Profile, Max_Crackle, Start_Vel);

   function Shape_Of (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Shape
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Kind_Made);
   begin
      pragma Assert (Within_Nonneg (Total_Time (Profile), P165x15));
      pragma Assert (Within (Start_Vel, P800));
      if Max_Crackle >= 0.0 * mm / s**5 then
         return (Kind => Speeds_Up, Vel => Start_Vel, Total => Total_Time (Profile), Whole => 0.0 * mm);
      end if;
      declare
         End_Vel : constant Velocity := Fast_Velocity_At_Max_Time (Profile, Max_Crackle, Start_Vel);
      begin
         Lemma_Slowing_End (Profile (1), Profile (2), Profile (3), Profile (4), Max_Crackle, Start_Vel);
         pragma Assert (End_Vel <= Start_Vel);
         if End_Vel < -Rest_Tolerance * abs Start_Vel then
            --  The ramp turns back, so its distance is not monotone and nothing is gained by evaluating it
            --  backwards.
            return (Kind => Turns_Back, Vel => Start_Vel, Total => Total_Time (Profile), Whole => 0.0 * mm);
         end if;
         declare
            Final : constant Velocity := Velocity'Max (0.0 * mm / s, End_Vel);
         begin
            pragma Assert (Within_Nonneg (Final, P800) and then abs Final <= abs Start_Vel);
            Lemma_Slowing_Whole (Profile (1), Profile (2), Profile (3), Profile (4), -Max_Crackle, Final);
            pragma Assert (Whole_OK (Fast_Distance_At_Max_Time (Profile, -Max_Crackle, Final), Final));
            return
              (Kind  => Slows_Down,
               Vel   => Final,
               Total => Total_Time (Profile),
               Whole => Fast_Distance_At_Max_Time (Profile, -Max_Crackle, Final));
         end;
      end;
   end Shape_Of;

   function Rising_Time (Shape : Ramp_Shape; T : Time) return Time
   is (if Shape.Kind = Slows_Down then Time'Max (0.0 * s, Shape.Total - T) else T)
   with
     Pre  => Within_Nonneg (Shape.Total, P165x15) and then Within_Nonneg (T, P165x15),
     Post => Within_Nonneg (Rising_Time'Result, P165x15) and then Within_Nonneg (Rising_Time'Result, P169);
   --  The time at which the ramp which speeds up from rest is evaluated for the distance at T.

   procedure Lemma_Reach (V : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (V, P800),
     Post => Travel_Reach (V) = Ramp_Reach (V) + P950 and then Travel_Reach (V) >= 0.0;

   procedure Lemma_Reach (V : Raw) is null;

   procedure Lemma_Rest (Rest, Reach : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within_Nonneg (Reach, P969) and then Within (Rest, P971) and then Rest <= Reach + P950,
     Post => Within (Raw'Max (0.0, Rest), Reach + P950);

   procedure Lemma_Rest (Rest, Reach : Raw) is null;
   --  What is left of a ramp which slows down, kept from falling below zero.

   package Sum_970_970 is new Bounded_Sum (P970, P970, P971);

   function Distance_Made (Result : Length; Shape : Ramp_Shape; T, Rising_T : Time; Gain : Length) return Boolean
   is (case Shape.Kind is
         when Speeds_Up  =>
           Same (Result, Travel_End.Add (Ramp_Reach (Shape.Vel), Ramp_Travel (Shape.Vel, T), Sym_943.F (Gain))),
         when Turns_Back => True,
         when Slows_Down =>
           At_Most (0.0, Result)
           and then
             (if T <= 0.0 * s
              then Same (Result, 0.0)
              else
                Same
                  (Result,
                   Raw'Max
                     (0.0,
                      Sum_970_970.Sub (Shape.Whole, Slowing_Reach.F (Slowing_Travel.F (Shape.Vel, Rising_T), Gain))))))
   with
     Ghost,
     Pre =>
       Shape_OK (Shape)
       and then Within_Nonneg (T, P165x15)
       and then Within_Nonneg (Rising_T, P165x15)
       and then Within (Gain, P943);
   pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Distance_Made);
   --  What Ramp_Distance returns for a ramp which does not turn back, written with the mirrors of its operations.

   function Ramp_Distance (Shape : Ramp_Shape; T, Rising_T : Time; Gain : Length) return Length
   with
     No_Inline,
     Pre  =>
       Shape_OK (Shape)
       and then Within_Nonneg (T, P165x15)
       and then Within_Nonneg (Rising_T, P165x15)
       and then Within (Gain, P943),
     Post =>
       Within (Ramp_Distance'Result, Travel_Reach (Shape.Vel))
       and then Distance_Made (Ramp_Distance'Result, Shape, T, Rising_T, Gain);
   --  The distance at T, from the gain at Rising_T of the ramp which speeds up from rest.

   function Ramp_Distance (Shape : Ramp_Shape; T, Rising_T : Time; Gain : Length) return Length is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Distance_Made);
   begin
      Lemma_Reach (Shape.Vel);
      case Shape.Kind is
         when Speeds_Up  =>
            Witness (Travel_End.Add (Ramp_Reach (Shape.Vel), Ramp_Travel (Shape.Vel, T), Sym_943.F (Gain)));
            return Shape.Vel * T + Gain;

         when Turns_Back =>
            Witness (Travel_End.Sub (Ramp_Reach (Shape.Vel), Ramp_Travel (Shape.Vel, T), Sym_943.F (Gain)));
            return Shape.Vel * T - Gain;

         when Slows_Down =>
            if T <= 0.0 * s then
               return 0.0 * mm;
            end if;
            Witness
              (Slowing_Rest.F
                 (Ramp_Reach (Shape.Vel),
                  Shape.Whole,
                  Slowing_Reach.F (Slowing_Travel.F (Shape.Vel, Rising_T), Gain)));
            Lemma_Rest (Shape.Whole - (Shape.Vel * Rising_T + Gain), Ramp_Reach (Shape.Vel));
            --  Whole is not the backwards ramp's own distance at its end, so the difference can be just below zero.
            return Length'Max (0.0 * mm, Shape.Whole - (Shape.Vel * Rising_T + Gain));
      end case;
   end Ramp_Distance;

   ------------------------------------------------------------------------------------------------------------------
   --  The two evaluations agree
   --
   --  Distance_At_Time of a ramp and of a Prepared_Ramp make the same calls on the same values. A proof can use
   --  that only where it sees the values themselves. The result of a function is known to its callers through its
   --  postcondition, and an equality of records or of floating-point numbers in a postcondition is not the
   --  identity which makes two calls the same call: a zero has two signs. So nothing is stated about the stages.
   --  The proof is made where both evaluations can be seen through. Rising_Stage has no contract, so that it is
   --  analysed within Distance_At_Time, and Prepare is an expression function, whose definition is known wherever
   --  it is called. The refined postcondition of Distance_At_Time is that it returns what the prepared form
   --  returns, which is shown for the stage which contains the time, one stage at a time.
   ------------------------------------------------------------------------------------------------------------------

   function Prepared_Distance (Ramp : Prepared_Ramp; T : Time; Stage : Feedrate_Profile_Stage_Index) return Length
   is (Ramp_Distance
         (Ramp.Shape, T, Rising_Time (Ramp.Shape, T), Stage_Gain (Ramp.Stages (Stage), Rising_Time (Ramp.Shape, T))))
   with Ghost, Pre => Is_Prepared (Ramp) and then T >= No_Time and then T <= Ramp_Time (Ramp);
   --  The distance at T of a prepared ramp, if T is in its stage Stage.

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   with
     Refined_Post =>
       abs Distance_At_Time'Result <= abs Start_Vel * Ramp_Time_Max + Evaluated_Travel_Max
       and then Distance_At_Time'Result = Distance_At_Time (Prepare (Profile, Max_Crackle, Start_Vel), T)
   is
      Shape    : constant Ramp_Shape := Shape_Of (Profile, Max_Crackle, Start_Vel);
      Rising_T : constant Time := Rising_Time (Shape, T);
      Bounds   : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);
      Lemma_Travel_Reach (Shape.Vel, Start_Vel);

      declare
         Stage : constant Ramp_Stage := Rising_Stage (Profile, abs Max_Crackle, Bounds, Stage_At (Bounds, Rising_T));

         Selected : constant Feedrate_Profile_Stage_Index := Stage_At (Bounds, Rising_T)
         with Ghost;
         Direct   : constant Length := Ramp_Distance (Shape, T, Rising_T, Stage_Gain (Stage, Rising_T))
         with Ghost;
         Prepared : constant Prepared_Ramp := Prepare (Profile, Max_Crackle, Start_Vel)
         with Ghost;
      begin
         pragma Assert (Stage_OK (Stage));
         pragma Assert (Is_Prepared (Prepared));

         --  Whichever stage contains the time, Rising_Stage has made it as Prepare makes it.
         pragma Assert (if Selected = 1 then Direct = Prepared_Distance (Prepared, T, 1));
         pragma Assert (if Selected = 2 then Direct = Prepared_Distance (Prepared, T, 2));
         pragma Assert (if Selected = 3 then Direct = Prepared_Distance (Prepared, T, 3));
         pragma Assert (if Selected = 4 then Direct = Prepared_Distance (Prepared, T, 4));
         pragma Assert (if Selected = 5 then Direct = Prepared_Distance (Prepared, T, 5));
         pragma Assert (if Selected = 6 then Direct = Prepared_Distance (Prepared, T, 6));
         pragma Assert (if Selected = 7 then Direct = Prepared_Distance (Prepared, T, 7));
         pragma Assert (if Selected = 8 then Direct = Prepared_Distance (Prepared, T, 8));
         pragma Assert (if Selected = 9 then Direct = Prepared_Distance (Prepared, T, 9));
         pragma Assert (if Selected = 10 then Direct = Prepared_Distance (Prepared, T, 10));
         pragma Assert (if Selected = 11 then Direct = Prepared_Distance (Prepared, T, 11));
         pragma Assert (if Selected = 12 then Direct = Prepared_Distance (Prepared, T, 12));
         pragma Assert (if Selected = 13 then Direct = Prepared_Distance (Prepared, T, 13));
         pragma Assert (if Selected = 14 then Direct = Prepared_Distance (Prepared, T, 14));
         pragma Assert (if Selected = 15 then Direct = Prepared_Distance (Prepared, T, 15));
         pragma Assert (Direct = Prepared_Distance (Prepared, T, Selected));
         pragma Assert (Distance_At_Time (Prepared, T) = Prepared_Distance (Prepared, T, Selected));
         return Ramp_Distance (Shape, T, Rising_T, Stage_Gain (Stage, Rising_T));
      end;
   end Distance_At_Time;

   function Is_Prepared (Ramp : Prepared_Ramp) return Boolean
   is (Shape_OK (Ramp.Shape)
       and then Bounds_OK (Ramp.Bounds)
       and then (for all K in Feedrate_Profile_Stage_Index => Stage_OK (Ramp.Stages (K))));

   function Prepare
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Prepared_Ramp
   is (declare
         T1 : constant Time := Profile (1);
         T2 : constant Time := Profile (2);
         T3 : constant Time := Profile (3);
         T4 : constant Time := Profile (4);
         Cm : constant Crackle := abs Max_Crackle;

         Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
         D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
         D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
         D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
         Late   : constant Length := Length'Max (D12, D8);

         --  Every stage as Rising_Stage makes it, with each state taken from the stage before in place of a new
         --  start from the closed form of its group.
         State_1  : constant Ramp_State := (others => <>);
         Stage_1  : constant Ramp_Stage :=
           Make_Stage (State_1, 0.0 * s, Stage_Length (1, Profile), Stage_Crackle (1, Cm), 0.0 * mm, D4);
         State_2  : constant Ramp_State := Advance (State_1, Stage_Length (1, Profile), Stage_Crackle (1, Cm));
         Stage_2  : constant Ramp_Stage :=
           Make_Stage (State_2, Bounds (1), Stage_Length (2, Profile), Stage_Crackle (2, Cm), 0.0 * mm, D4);
         State_3  : constant Ramp_State := Advance (State_2, Stage_Length (2, Profile), Stage_Crackle (2, Cm));
         Stage_3  : constant Ramp_Stage :=
           Make_Stage (State_3, Bounds (2), Stage_Length (3, Profile), Stage_Crackle (3, Cm), 0.0 * mm, D4);
         State_4  : constant Ramp_State := Checkpoint_4 (T1, T2, Cm, D4);
         Stage_4  : constant Ramp_Stage :=
           Make_Stage (State_4, Bounds (3), Stage_Length (4, Profile), Stage_Crackle (4, Cm), D4, D8);
         State_5  : constant Ramp_State := Advance (State_4, Stage_Length (4, Profile), Stage_Crackle (4, Cm));
         Stage_5  : constant Ramp_Stage :=
           Make_Stage (State_5, Bounds (4), Stage_Length (5, Profile), Stage_Crackle (5, Cm), D4, D8);
         State_6  : constant Ramp_State := Advance (State_5, Stage_Length (5, Profile), Stage_Crackle (5, Cm));
         Stage_6  : constant Ramp_Stage :=
           Make_Stage (State_6, Bounds (5), Stage_Length (6, Profile), Stage_Crackle (6, Cm), D4, D8);
         State_7  : constant Ramp_State := Advance (State_6, Stage_Length (6, Profile), Stage_Crackle (6, Cm));
         Stage_7  : constant Ramp_Stage :=
           Make_Stage (State_7, Bounds (6), Stage_Length (7, Profile), Stage_Crackle (7, Cm), D4, D8);
         State_8  : constant Ramp_State := Checkpoint_8 (T1, T2, T3, Cm, D8);
         Stage_8  : constant Ramp_Stage :=
           Make_Stage (State_8, Bounds (7), Stage_Length (8, Profile), Stage_Crackle (8, Cm), D8, Late);
         State_9  : constant Ramp_State := Advance (State_8, Stage_Length (8, Profile), Stage_Crackle (8, Cm));
         Stage_9  : constant Ramp_Stage :=
           Make_Stage (State_9, Bounds (8), Stage_Length (9, Profile), Stage_Crackle (9, Cm), D8, Late);
         State_10 : constant Ramp_State := Advance (State_9, Stage_Length (9, Profile), Stage_Crackle (9, Cm));
         Stage_10 : constant Ramp_Stage :=
           Make_Stage (State_10, Bounds (9), Stage_Length (10, Profile), Stage_Crackle (10, Cm), D8, Late);
         State_11 : constant Ramp_State := Advance (State_10, Stage_Length (10, Profile), Stage_Crackle (10, Cm));
         Stage_11 : constant Ramp_Stage :=
           Make_Stage (State_11, Bounds (10), Stage_Length (11, Profile), Stage_Crackle (11, Cm), D8, Late);
         State_12 : constant Ramp_State := Checkpoint_12 (T1, T2, T3, T4, Cm, D12);
         Stage_12 : constant Ramp_Stage :=
           Make_Stage (State_12, Bounds (11), Stage_Length (12, Profile), Stage_Crackle (12, Cm), Late, Length'Last);
         State_13 : constant Ramp_State := Advance (State_12, Stage_Length (12, Profile), Stage_Crackle (12, Cm));
         Stage_13 : constant Ramp_Stage :=
           Make_Stage (State_13, Bounds (12), Stage_Length (13, Profile), Stage_Crackle (13, Cm), Late, Length'Last);
         State_14 : constant Ramp_State := Advance (State_13, Stage_Length (13, Profile), Stage_Crackle (13, Cm));
         Stage_14 : constant Ramp_Stage :=
           Make_Stage (State_14, Bounds (13), Stage_Length (14, Profile), Stage_Crackle (14, Cm), Late, Length'Last);
         State_15 : constant Ramp_State := Advance (State_14, Stage_Length (14, Profile), Stage_Crackle (14, Cm));
         Stage_15 : constant Ramp_Stage :=
           Make_Stage (State_15, Bounds (14), Stage_Length (15, Profile), Stage_Crackle (15, Cm), Late, Length'Last);
       begin
         (Shape  => Shape_Of (Profile, Max_Crackle, Start_Vel),
          Bounds => Bounds,
          Stages =>
            [Stage_1,
             Stage_2,
             Stage_3,
             Stage_4,
             Stage_5,
             Stage_6,
             Stage_7,
             Stage_8,
             Stage_9,
             Stage_10,
             Stage_11,
             Stage_12,
             Stage_13,
             Stage_14,
             Stage_15]));

   function Stage_Is_Ordered (Ramp : Prepared_Ramp; Stage : Feedrate_Profile_Stage_Index) return Boolean
   is (Ramp.Stages (Stage).Start = (if Stage = 1 then 0.0 * s else Ramp.Bounds (Stage - 1))
       and then Ramp.Stages (Stage).Lo <= Ramp.Stages (Stage).Hi
       and then (if Stage < 15 then Ramp.Stages (Stage).Hi <= Ramp.Stages (Stage + 1).Lo))
   with Ghost;
   --  The stage starts where the one before it ends, its limits are in order, and its upper limit is at most the
   --  lower limit of the next stage.

   function Is_Ordered (Ramp : Prepared_Ramp) return Boolean
   is (Bounds_Ordered (Ramp.Bounds)
       and then (for all Stage in Feedrate_Profile_Stage_Index => Stage_Is_Ordered (Ramp, Stage)));

   procedure Lemma_Is_Ordered (Ramp : Prepared_Ramp)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Bounds_Ordered (Ramp.Bounds)
       and then Stage_Is_Ordered (Ramp, 1)
       and then Stage_Is_Ordered (Ramp, 2)
       and then Stage_Is_Ordered (Ramp, 3)
       and then Stage_Is_Ordered (Ramp, 4)
       and then Stage_Is_Ordered (Ramp, 5)
       and then Stage_Is_Ordered (Ramp, 6)
       and then Stage_Is_Ordered (Ramp, 7)
       and then Stage_Is_Ordered (Ramp, 8)
       and then Stage_Is_Ordered (Ramp, 9)
       and then Stage_Is_Ordered (Ramp, 10)
       and then Stage_Is_Ordered (Ramp, 11)
       and then Stage_Is_Ordered (Ramp, 12)
       and then Stage_Is_Ordered (Ramp, 13)
       and then Stage_Is_Ordered (Ramp, 14)
       and then Stage_Is_Ordered (Ramp, 15),
     Post => Is_Ordered (Ramp);

   procedure Lemma_Is_Ordered (Ramp : Prepared_Ramp) is null;

   procedure Lemma_Later (X, Y : Length)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (X, P942) and then Within (Y, P942),
     Post =>
       Within (Length'Max (X, Y), P942) and then Within (Length'Max (X, Y), P943) and then Y <= Length'Max (X, Y);

   procedure Lemma_Later (X, Y : Length) is null;
   --  The later of two closed forms, which the last group of stages starts from.

   procedure Lemma_Prepared_Group_1 (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post =>
       Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 1)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 2)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 3)
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := abs Max_Crackle;

      Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
      D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
      D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
      D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
      Late   : constant Length := Length'Max (D12, D8);

      State : constant Ramp_State := (others => <>);
      Next  : constant Ramp_State := Checkpoint_4 (T1, T2, Cm, D4);

      Ramp : constant Prepared_Ramp := Prepare (Profile, Max_Crackle, Start_Vel);
   begin
      Lemma_Group_Order (T1, T2, T3, Cm);
      Lemma_Later (D12, D8);
      pragma Assert (At_Most (0.0, D4) and then D4 <= D8 and then D8 <= Late);
      Lemma_Group_Limits
        (State,
         0.0 * s,
         Stage_Length (1, Profile),
         Stage_Crackle (1, Cm),
         Bounds (1),
         Stage_Length (2, Profile),
         Stage_Crackle (2, Cm),
         Bounds (2),
         Stage_Length (3, Profile),
         Stage_Crackle (3, Cm),
         Bounds (3),
         Stage_Length (4, Profile),
         Stage_Crackle (4, Cm),
         0.0 * mm,
         D4);
      Lemma_Stage_Limits (Next, Bounds (3), Stage_Length (4, Profile), Stage_Crackle (4, Cm), D4, D8);
      pragma Assert (Stage_Is_Ordered (Ramp, 1));
      pragma Assert (Stage_Is_Ordered (Ramp, 2));
      pragma Assert (Stage_Is_Ordered (Ramp, 3));
   end Lemma_Prepared_Group_1;

   procedure Lemma_Prepared_Group_2 (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post =>
       Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 4)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 5)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 6)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 7)
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := abs Max_Crackle;

      Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
      D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
      D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
      D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
      Late   : constant Length := Length'Max (D12, D8);

      State : constant Ramp_State := Checkpoint_4 (T1, T2, Cm, D4);
      Next  : constant Ramp_State := Checkpoint_8 (T1, T2, T3, Cm, D8);

      Ramp : constant Prepared_Ramp := Prepare (Profile, Max_Crackle, Start_Vel);
   begin
      Lemma_Group_Order (T1, T2, T3, Cm);
      Lemma_Later (D12, D8);
      pragma Assert (At_Most (0.0, D4) and then D4 <= D8 and then D8 <= Late);
      Lemma_Group_Limits
        (State,
         Bounds (3),
         Stage_Length (4, Profile),
         Stage_Crackle (4, Cm),
         Bounds (4),
         Stage_Length (5, Profile),
         Stage_Crackle (5, Cm),
         Bounds (5),
         Stage_Length (6, Profile),
         Stage_Crackle (6, Cm),
         Bounds (6),
         Stage_Length (7, Profile),
         Stage_Crackle (7, Cm),
         D4,
         D8);
      Lemma_Stage_Limits (Next, Bounds (7), Stage_Length (8, Profile), Stage_Crackle (8, Cm), D8, Late);
      pragma Assert (Stage_Is_Ordered (Ramp, 4));
      pragma Assert (Stage_Is_Ordered (Ramp, 5));
      pragma Assert (Stage_Is_Ordered (Ramp, 6));
      pragma Assert (Stage_Is_Ordered (Ramp, 7));
   end Lemma_Prepared_Group_2;

   procedure Lemma_Prepared_Group_3 (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post =>
       Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 8)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 9)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 10)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 11)
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := abs Max_Crackle;

      Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
      D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
      D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
      D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
      Late   : constant Length := Length'Max (D12, D8);

      State : constant Ramp_State := Checkpoint_8 (T1, T2, T3, Cm, D8);
      Next  : constant Ramp_State := Checkpoint_12 (T1, T2, T3, T4, Cm, D12);

      Ramp : constant Prepared_Ramp := Prepare (Profile, Max_Crackle, Start_Vel);
   begin
      Lemma_Group_Order (T1, T2, T3, Cm);
      Lemma_Later (D12, D8);
      pragma Assert (At_Most (0.0, D4) and then D4 <= D8 and then D8 <= Late);
      Lemma_Group_Limits
        (State,
         Bounds (7),
         Stage_Length (8, Profile),
         Stage_Crackle (8, Cm),
         Bounds (8),
         Stage_Length (9, Profile),
         Stage_Crackle (9, Cm),
         Bounds (9),
         Stage_Length (10, Profile),
         Stage_Crackle (10, Cm),
         Bounds (10),
         Stage_Length (11, Profile),
         Stage_Crackle (11, Cm),
         D8,
         Late);
      Lemma_Stage_Limits (Next, Bounds (11), Stage_Length (12, Profile), Stage_Crackle (12, Cm), Late, Length'Last);
      pragma Assert (Stage_Is_Ordered (Ramp, 8));
      pragma Assert (Stage_Is_Ordered (Ramp, 9));
      pragma Assert (Stage_Is_Ordered (Ramp, 10));
      pragma Assert (Stage_Is_Ordered (Ramp, 11));
   end Lemma_Prepared_Group_3;

   procedure Lemma_Prepared_Group_4 (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   with
     Ghost,
     Always_Terminates,
     Pre  => Ramp_In_Range (Profile, Max_Crackle, Start_Vel),
     Post =>
       Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 12)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 13)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 14)
       and then Stage_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel), 15)
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := abs Max_Crackle;

      Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
      D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
      D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
      D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
      Late   : constant Length := Length'Max (D12, D8);

      State : constant Ramp_State := Checkpoint_12 (T1, T2, T3, T4, Cm, D12);

      Ramp : constant Prepared_Ramp := Prepare (Profile, Max_Crackle, Start_Vel);
   begin
      Lemma_Group_Order (T1, T2, T3, Cm);
      Lemma_Later (D12, D8);
      pragma Assert (At_Most (0.0, D4) and then D4 <= D8 and then D8 <= Late);
      Lemma_Group_Limits
        (State,
         Bounds (11),
         Stage_Length (12, Profile),
         Stage_Crackle (12, Cm),
         Bounds (12),
         Stage_Length (13, Profile),
         Stage_Crackle (13, Cm),
         Bounds (13),
         Stage_Length (14, Profile),
         Stage_Crackle (14, Cm),
         Bounds (14),
         Stage_Length (15, Profile),
         Stage_Crackle (15, Cm),
         Late,
         Length'Last);
      pragma Assert (Stage_Is_Ordered (Ramp, 12));
      pragma Assert (Stage_Is_Ordered (Ramp, 13));
      pragma Assert (Stage_Is_Ordered (Ramp, 14));
      pragma Assert (Stage_Is_Ordered (Ramp, 15));
   end Lemma_Prepared_Group_4;

   procedure Lemma_Prepared_Is_Ordered (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   is
   begin
      Lemma_Prepared_Group_1 (Profile, Max_Crackle, Start_Vel);
      Lemma_Prepared_Group_2 (Profile, Max_Crackle, Start_Vel);
      Lemma_Prepared_Group_3 (Profile, Max_Crackle, Start_Vel);
      Lemma_Prepared_Group_4 (Profile, Max_Crackle, Start_Vel);
      pragma Assert (Bounds_Ordered (Prepare (Profile, Max_Crackle, Start_Vel).Bounds));
      Lemma_Is_Ordered (Prepare (Profile, Max_Crackle, Start_Vel));
   end Lemma_Prepared_Is_Ordered;

   ------------------------------------------------------------------------------------------------------------------
   --  The distance of a prepared ramp does not decrease
   --
   --  Ghost. What is shown is sections 3.5 and 3.6 of docs/spark_trajectory_monotonicity.md: if the compensated
   --  polynomial of a stage does not decrease between two times into the stage, the distance does not decrease
   --  between the two times of the ramp. That the polynomial does not decrease is a hypothesis, Stages_Are_Monotone,
   --  which nothing here proves or assumes: it is about the bits of the compensated scheme. A quantified expression
   --  cannot range over floating-point numbers, so the hypothesis is stated for the two times which the conclusion
   --  is about, which is less than monotonicity over the whole stage.
   --
   --  Between two times in one stage the gain is a sum with the polynomial, kept between the same limits. Between
   --  two times in different stages the limits alone order the gains, because the table is in order.
   ------------------------------------------------------------------------------------------------------------------

   function Stage_Is_Monotone (Stage : Ramp_Stage; X1, X2 : Time) return Boolean
   is (if At_Most (0.0, X1) and then X1 <= X2 and then X2 <= Stage.Len
       then Compensated_Local (Stage, X1) <= Compensated_Local (Stage, X2))
   with Ghost, Pre => Stage_OK (Stage) and then Within (X1, P169) and then Within (X2, P169);
   --  The compensated polynomial of Stage does not decrease from X1 to X2, if these are times into the stage in
   --  that order.

   function Rising_Gain (Ramp : Prepared_Ramp; T : Time) return Length
   is (Stage_Gain (Ramp.Stages (Stage_At (Ramp.Bounds, T)), T))
   with Ghost, Pre => Is_Prepared (Ramp) and then Within_Nonneg (T, P169);
   --  The gain at T of the ramp which speeds up from rest.

   function Gain_Is_Monotone (Ramp : Prepared_Ramp; T1, T2 : Time) return Boolean
   is (if Stage_At (Ramp.Bounds, T1) = Stage_At (Ramp.Bounds, T2)
       then
         Stage_Is_Monotone
           (Ramp.Stages (Stage_At (Ramp.Bounds, T1)),
            Stage_Offset (Ramp.Stages (Stage_At (Ramp.Bounds, T1)), T1),
            Stage_Offset (Ramp.Stages (Stage_At (Ramp.Bounds, T1)), T2)))
   with Ghost, Pre => Is_Prepared (Ramp) and then Within_Nonneg (T1, P169) and then Within_Nonneg (T2, P169);
   --  The hypothesis for two times of the ramp which speeds up from rest: it is about the polynomial only if both
   --  are in one stage.

   function Moves_Forward (Ramp : Prepared_Ramp) return Boolean
   is (Ramp.Shape.Kind = Slows_Down
       or else (Ramp.Shape.Kind = Speeds_Up and then Within_Nonneg (Ramp.Shape.Vel, P800)));

   function Stages_Are_Monotone (Ramp : Prepared_Ramp; T1, T2 : Time) return Boolean
   is (if Ramp.Shape.Kind = Slows_Down
       then Gain_Is_Monotone (Ramp, Rising_Time (Ramp.Shape, T2), Rising_Time (Ramp.Shape, T1))
       else Gain_Is_Monotone (Ramp, T1, T2));

   procedure Lemma_Table_Chain (Ramp : Prepared_Ramp; First, Last : Feedrate_Profile_Stage_Index)
   with
     Ghost,
     Always_Terminates,
     Pre  => Is_Ordered (Ramp) and then First < Last,
     Post => Ramp.Stages (First).Hi <= Ramp.Stages (Last).Lo
   is
   begin
      for Stage in First + 1 .. Last loop
         pragma
           Loop_Invariant
             (for all Earlier in First + 1 .. Stage - 1 => Ramp.Stages (First).Hi <= Ramp.Stages (Earlier).Lo);
         pragma Assert (Stage_Is_Ordered (Ramp, Stage - 1));
         pragma Assert (Ramp.Stages (Stage - 1).Hi <= Ramp.Stages (Stage).Lo);
         pragma Assert (Stage = First + 1 or else Ramp.Stages (First).Hi <= Ramp.Stages (Stage - 1).Lo);
         pragma Assert (Ramp.Stages (First).Hi <= Ramp.Stages (Stage).Lo);
      end loop;
   end Lemma_Table_Chain;
   --  The upper limit of a stage is at most the lower limit of every later stage.

   procedure Lemma_Offset_Order (Stage : Ramp_Stage; T1, T2 : Time)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Stage_OK (Stage)
       and then Within_Nonneg (T1, P169)
       and then Within_Nonneg (T2, P169)
       and then Stage.Start <= T1
       and then T1 <= T2,
     Post =>
       At_Most (0.0, Stage_Offset (Stage, T1))
       and then Stage_Offset (Stage, T1) <= Stage_Offset (Stage, T2)
       and then Stage_Offset (Stage, T2) <= Stage.Len
   is
   begin
      Lemma_Difference_Order (T1, T2, Stage.Start, Stage.Start);
      pragma Assert (At_Most (0.0, T1 - Stage.Start));
      pragma Assert (T1 - Stage.Start <= T2 - Stage.Start);
   end Lemma_Offset_Order;
   --  The times into a stage of two times at or after its start are in their order, and within the stage.

   procedure Lemma_Stage_Gain_Order (Stage : Ramp_Stage; T1, T2 : Time)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Stage_OK (Stage)
       and then Within_Nonneg (T1, P169)
       and then Within_Nonneg (T2, P169)
       and then
         Compensated_Local (Stage, Stage_Offset (Stage, T1)) <= Compensated_Local (Stage, Stage_Offset (Stage, T2)),
     Post => Stage_Gain (Stage, T1) <= Stage_Gain (Stage, T2)
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Gain_Made);
   begin
      Lemma_Sum_Order
        (Stage.Dist,
         Stage.Dist,
         Compensated_Local (Stage, Stage_Offset (Stage, T1)),
         Compensated_Local (Stage, Stage_Offset (Stage, T2)));
      Lemma_Clamp_Order
        (Stage.Dist + Compensated_Local (Stage, Stage_Offset (Stage, T1)),
         Stage.Dist + Compensated_Local (Stage, Stage_Offset (Stage, T2)),
         Stage.Lo,
         Stage.Hi);
   end Lemma_Stage_Gain_Order;
   --  Within a stage the gain is in the order of the polynomial.

   procedure Lemma_Gain_Order (Ramp : Prepared_Ramp; T1, T2 : Time)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Is_Prepared (Ramp)
       and then Is_Ordered (Ramp)
       and then Within_Nonneg (T1, P169)
       and then Within_Nonneg (T2, P169)
       and then T1 <= T2
       and then Gain_Is_Monotone (Ramp, T1, T2),
     Post => Rising_Gain (Ramp, T1) <= Rising_Gain (Ramp, T2)
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Gain_Made);

      First : constant Feedrate_Profile_Stage_Index := Stage_At (Ramp.Bounds, T1);
      Last  : constant Feedrate_Profile_Stage_Index := Stage_At (Ramp.Bounds, T2);
   begin
      Lemma_Stage_Order (Ramp.Bounds, T1, T2);
      pragma Assert (Stage_OK (Ramp.Stages (First)) and then Stage_OK (Ramp.Stages (Last)));
      pragma Assert (Stage_Is_Ordered (Ramp, First) and then Stage_Is_Ordered (Ramp, Last));
      if First = Last then
         pragma Assert (Ramp.Stages (First).Start <= T1);
         Lemma_Offset_Order (Ramp.Stages (First), T1, T2);
         pragma
           Assert
             (Stage_Is_Monotone
                (Ramp.Stages (First), Stage_Offset (Ramp.Stages (First), T1), Stage_Offset (Ramp.Stages (First), T2)));
         Lemma_Stage_Gain_Order (Ramp.Stages (First), T1, T2);
      else
         Lemma_Table_Chain (Ramp, First, Last);
         Lemma_Clamp_Range
           (Ramp.Stages (First).Dist + Compensated_Local (Ramp.Stages (First), Stage_Offset (Ramp.Stages (First), T1)),
            Ramp.Stages (First).Lo,
            Ramp.Stages (First).Hi);
         Lemma_Clamp_Range
           (Ramp.Stages (Last).Dist + Compensated_Local (Ramp.Stages (Last), Stage_Offset (Ramp.Stages (Last), T2)),
            Ramp.Stages (Last).Lo,
            Ramp.Stages (Last).Hi);
         pragma Assert (Stage_Gain (Ramp.Stages (First), T1) <= Ramp.Stages (First).Hi);
         pragma Assert (Ramp.Stages (Last).Lo <= Stage_Gain (Ramp.Stages (Last), T2));
      end if;
   end Lemma_Gain_Order;
   --  The gain of the ramp which speeds up from rest does not decrease (3.5).

   function Distance_At_Time (Ramp : Prepared_Ramp; T : Time) return Length
   with
     Refined_Post =>
       abs Distance_At_Time'Result <= Ramp_Distance_Max
       and then
         Distance_At_Time'Result = Prepared_Distance (Ramp, T, Stage_At (Ramp.Bounds, Rising_Time (Ramp.Shape, T)))
   is
      Rising_T : constant Time := Rising_Time (Ramp.Shape, T);
   begin
      pragma Assert (T <= Ramp.Shape.Total);
      pragma Assert (T >= 0.0 * s);

      return
        Ramp_Distance (Ramp.Shape, T, Rising_T, Stage_Gain (Ramp.Stages (Stage_At (Ramp.Bounds, Rising_T)), Rising_T));
   end Distance_At_Time;

   procedure Lemma_Prepared_Distance
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity)
   is null;

   procedure Lemma_Prepared_Moves_Forward
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity)
   is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Kind_Made);
   begin
      pragma Assert (Kind_Made (Shape_Of (Profile, Max_Crackle, Start_Vel), Profile, Max_Crackle, Start_Vel));
      pragma Assert (Prepare (Profile, Max_Crackle, Start_Vel).Shape = Shape_Of (Profile, Max_Crackle, Start_Vel));
   end Lemma_Prepared_Moves_Forward;

   procedure Lemma_Distance_Is_Monotone (Ramp : Prepared_Ramp; T1, T2 : Time) is
      pragma Annotate (GNATprove, Unhide_Info, "Expression_Function_Body", Distance_Made);

      Shape    : constant Ramp_Shape := Ramp.Shape;
      Rising_1 : constant Time := Rising_Time (Shape, T1);
      Rising_2 : constant Time := Rising_Time (Shape, T2);
      Gain_1   : constant Length := Rising_Gain (Ramp, Rising_1);
      Gain_2   : constant Length := Rising_Gain (Ramp, Rising_2);
      Result_1 : constant Length := Ramp_Distance (Shape, T1, Rising_1, Gain_1);
      Result_2 : constant Length := Ramp_Distance (Shape, T2, Rising_2, Gain_2);
   begin
      pragma Assert (Distance_At_Time (Ramp, T1) = Result_1);
      pragma Assert (Distance_At_Time (Ramp, T2) = Result_2);
      pragma Assert (Within_Nonneg (Shape.Vel, P800));

      if Shape.Kind = Speeds_Up then
         --  Travel at the starting speed and the gain both grow.
         Lemma_Gain_Order (Ramp, T1, T2);
         Lemma_Travel_Order (Shape.Vel, T1, T2);
         Witness (Slowing_Travel.F (Shape.Vel, T1));
         Witness (Slowing_Travel.F (Shape.Vel, T2));
         Lemma_Sum_Order (Shape.Vel * T1, Shape.Vel * T2, Gain_1, Gain_2);
         pragma Assert (Result_1 = Shape.Vel * T1 + Gain_1);
         pragma Assert (Result_2 = Shape.Vel * T2 + Gain_2);
      elsif T1 > 0.0 * s then
         --  The time which is left shrinks, so what the mirrored ramp has left to cover shrinks, and what is taken
         --  from the whole distance with it.
         Lemma_Difference_Order (Shape.Total, Shape.Total, T2, T1);
         pragma Assert (Rising_2 <= Rising_1);
         Lemma_Gain_Order (Ramp, Rising_2, Rising_1);
         Lemma_Travel_Order (Shape.Vel, Rising_2, Rising_1);
         declare
            Rest_1 : constant Raw := Slowing_Reach.F (Slowing_Travel.F (Shape.Vel, Rising_1), Gain_1);
            Rest_2 : constant Raw := Slowing_Reach.F (Slowing_Travel.F (Shape.Vel, Rising_2), Gain_2);
            Left_1 : constant Raw := Sum_970_970.Sub (Shape.Whole, Rest_1);
            Left_2 : constant Raw := Sum_970_970.Sub (Shape.Whole, Rest_2);
         begin
            Lemma_Sum_Order (Shape.Vel * Rising_2, Shape.Vel * Rising_1, Gain_2, Gain_1);
            pragma Assert (Rest_2 <= Rest_1);
            Lemma_Difference_Order (Shape.Whole, Shape.Whole, Rest_1, Rest_2);
            pragma Assert (Left_1 <= Left_2);
            pragma Assert (Same (Result_1, Raw'Max (0.0, Left_1)));
            pragma Assert (Same (Result_2, Raw'Max (0.0, Left_2)));
         end;
      else
         --  The distance at the start of a ramp which slows down is zero, and no distance of it is below zero.
         pragma Assert (Same (Result_1, 0.0));
         pragma Assert (At_Most (0.0, Result_2));
      end if;
   end Lemma_Distance_Is_Monotone;

   ------------------------------------------------------------------------------------------------------------------
   --  Bounds of the evaluation of a complete profile
   --
   --  Ghost. A complete profile is evaluated through the evaluators of its two ramps, whose postconditions bound
   --  their results relative to the starting speed of the ramp. The lemmas below turn such a bound into one of the
   --  predicates of the bounded operations, and the mirrors in the bodies then bound the sums and products which
   --  join the two ramps and the coast.
   ------------------------------------------------------------------------------------------------------------------

   P171 : constant Raw := 2#1.0#E171
   with Ghost;
   P783 : constant Raw := 2#1.0#E783
   with Ghost;
   P784 : constant Raw := 2#1.0#E784
   with Ghost;
   P948 : constant Raw := 2#1.0#E948
   with Ghost;
   P949 : constant Raw := 2#1.0#E949
   with Ghost;
   P951 : constant Raw := 2#1.0#E951
   with Ghost;
   P952 : constant Raw := 2#1.0#E952
   with Ghost;
   P953 : constant Raw := 2#1.0#E953
   with Ghost;
   P954 : constant Raw := 2#1.0#E954
   with Ghost;

   package Mul_783_166 is new Bounded_Product (P783, P166, P949);
   package Mul_783_165 is new Bounded_Product (P783, P165, P948);
   package Sum_951_949 is new Bounded_Sum (P951, P949, P952);
   package Sum_951_948 is new Bounded_Sum (P951, P948, P952);
   package Sum_952_953 is new Bounded_Sum (P952, P953, P954);
   package Widen_952_954 is new Wider_Bound (P952, P954);
   package Widen_951_954 is new Wider_Bound (P951, P954);

   procedure Lemma_Ramp_Speed (Start, V : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Start, P100) and then abs V <= abs Start + P782,
     Post => Within (V, P783) and then abs V <= abs Start + P784;
   --  The speed at a time of the acceleration ramp, or at its end.

   procedure Lemma_Ramp_Speed (Start, V : Raw) is null;

   procedure Lemma_Second_Ramp_Speed (Start, Mid, V : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Start, P100) and then Within (Mid, P783) and then abs V <= abs Mid + P782,
     Post => abs V <= abs Start + P784
   is
   begin
      pragma Assert (Within (V, P784));
   end Lemma_Second_Ramp_Speed;
   --  The speed at a time of the deceleration ramp, which starts at the speed Mid.

   procedure Lemma_Ramp_Travel (Start, D : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Start, P100) and then abs D <= abs Start * P169 + P950,
     Post => Within (D, P951)
   is
   begin
      pragma Assert (abs Start * P169 <= P269);
   end Lemma_Ramp_Travel;
   --  The distance at a time of the acceleration ramp, or at its end.

   procedure Lemma_Second_Ramp_Travel (Mid, D : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Mid, P783) and then abs D <= abs Mid * P169 + P950,
     Post => Within (D, P953)
   is
   begin
      pragma Assert (abs Mid * P169 <= P952);
   end Lemma_Second_Ramp_Travel;
   --  The distance at a time of the deceleration ramp, which starts at the speed Mid.

   procedure Lemma_Profile_Travel (Start, D : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Start, P100) and then Within (D, P954),
     Post => abs D <= abs Start * P171 + P954
   is
   begin
      pragma Assert (abs Start * P171 >= 0.0 and then abs Start * P171 <= P271);
   end Lemma_Profile_Travel;
   --  A distance within Evaluated_Profile_Travel_Max, as the postcondition of Distance_At_Time states it.

   procedure Lemma_Coast_Offset (T, Ramp_Time, Coast : Raw)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within_Nonneg (Ramp_Time, P169)
       and then Within_Nonneg (Coast, P165)
       and then T > Ramp_Time
       and then T < Ramp_Time + Coast,
     Post => Within (T - Ramp_Time, P166);
   --  A time within the coast is less than the coast time past the end of the acceleration ramp, but for the
   --  rounding of the sum which it was compared with.

   procedure Lemma_Coast_Offset (T, Ramp_Time, Coast : Raw) is null;

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Crackle_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**5;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Crackle_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap is
   begin
      pragma Assert (T <= Total_Time (Profile.Accel) + Profile.Coast + Total_Time (Profile.Decel));
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Snap_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**4;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Snap_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Jerk_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**3;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Jerk_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Acceleration_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**2;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Acceleration_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         Lemma_Ramp_Speed (Start_Vel, Velocity_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel));
         return Velocity_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel);
      end if;

      declare
         --  Only the later phases start from the end of acceleration.
         Mid_Vel : constant Velocity :=
           Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      begin
         Lemma_Ramp_Speed (Start_Vel, Mid_Vel);
         if T < Total_Time (Profile.Accel) + Profile.Coast then
            return Mid_Vel;
         else
            declare
               Decel_T : constant Time :=
                 Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
            begin
               Lemma_Second_Ramp_Speed
                 (Start_Vel, Mid_Vel, Velocity_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel));
               return Velocity_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel);
            end;
         end if;
      end;
   end Velocity_At_Time;

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         Lemma_Ramp_Travel (Start_Vel, Distance_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel));
         Lemma_Profile_Travel
           (Start_Vel, Widen_951_954.F (Distance_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel)));
         return Distance_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel);
      end if;

      declare
         --  Only the later phases start from the end of acceleration.
         Mid_Vel    : constant Velocity :=
           Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
         Accel_Dist : constant Length :=
           Distance_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      begin
         Lemma_Ramp_Speed (Start_Vel, Mid_Vel);
         Lemma_Ramp_Travel (Start_Vel, Accel_Dist);
         if T < Total_Time (Profile.Accel) + Profile.Coast then
            Lemma_Coast_Offset (T, Total_Time (Profile.Accel), Profile.Coast);
            Lemma_Profile_Travel
              (Start_Vel,
               Widen_952_954.F
                 (Sum_951_949.Add (Accel_Dist, Mul_783_166.F (Mid_Vel, T - Total_Time (Profile.Accel)))));
            return Accel_Dist + Mid_Vel * (T - Total_Time (Profile.Accel));
         else
            declare
               Mid_Dist : constant Length := Mid_Vel * Profile.Coast;
               Decel_T  : constant Time :=
                 Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
            begin
               Lemma_Second_Ramp_Travel (Mid_Vel, Distance_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel));
               Lemma_Profile_Travel
                 (Start_Vel,
                  Sum_952_953.Add
                    (Sum_951_948.Add (Accel_Dist, Mul_783_165.F (Mid_Vel, Profile.Coast)),
                     Distance_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel)));
               return Accel_Dist + Mid_Dist + Distance_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel);
            end;
         end if;
      end;
   end Distance_At_Time;

end Prunt.Motion_Planner.Trajectory.Profiles;
