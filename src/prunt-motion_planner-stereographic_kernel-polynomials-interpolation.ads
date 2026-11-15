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

--  Lagrange interpolation: a polynomial with no more coefficients than there are nodes is the combination of the
--  Lagrange basis polynomials of the nodes weighted by its values there. Its magnitude anywhere is therefore at
--  most its largest magnitude at a node times the sum of the magnitudes of the basis polynomials, which is the
--  Lebesgue function of the nodes. Everything here is ghost code.

package Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   type Node_Array is array (Positive range <>) of Real;

   function Is_Increasing (Nodes : Node_Array) return Boolean
   is (Nodes'First = 1
       and then Nodes'Last >= 1
       and then Nodes'Last < Maximum_Length
       and then (for all I in 1 .. Nodes'Last - 1 => Nodes (I) < Nodes (I + 1)));
   --  Whether these are nodes: at least one, in strictly increasing order, so that no two coincide.

   function Numerator (Nodes : Node_Array; I : Positive; X : Real; Count : Natural) return Real
   is (if Count = 0
       then 1.0
       elsif Count = I
       then Numerator (Nodes, I, X, Count - 1)
       else Numerator (Nodes, I, X, Count - 1) * (X - Nodes (Count)))
   with Pre => Nodes'First = 1 and then Count <= Nodes'Last, Subprogram_Variant => (Decreases => Count);
   --  The product of X - Nodes (J) over J in 1 .. Count other than I.

   function Weight_Denominator (Nodes : Node_Array; I : Positive) return Real
   is (Numerator (Nodes, I, Nodes (I), Nodes'Last))
   with Pre => Nodes'First = 1 and then I <= Nodes'Last;
   --  The product of Nodes (I) - Nodes (J) over every other node J.

   procedure Lemma_Weight_Denominator_Is_Nonzero (Nodes : Node_Array; I : Positive)
   with Pre => Is_Increasing (Nodes) and then I <= Nodes'Last, Post => Weight_Denominator (Nodes, I) /= 0.0;

   function Basis (Nodes : Node_Array; I : Positive; X : Real) return Real
   is (if Weight_Denominator (Nodes, I) = 0.0
       then 0.0
       else Numerator (Nodes, I, X, Nodes'Last) / Weight_Denominator (Nodes, I))
   with Pre => Nodes'First = 1 and then I <= Nodes'Last;
   --  The Lagrange basis polynomial of node I at X: one at that node and zero at every other. The denominator is
   --  never zero for increasing nodes.

   function Lebesgue_Partial (Nodes : Node_Array; X : Real; Count : Natural) return Real
   is (if Count = 0 then 0.0 else Lebesgue_Partial (Nodes, X, Count - 1) + abs Basis (Nodes, Count, X))
   with Pre => Nodes'First = 1 and then Count <= Nodes'Last, Subprogram_Variant => (Decreases => Count);

   function Lebesgue_Sum (Nodes : Node_Array; X : Real) return Real
   is (if Nodes'Last >= 1 then Lebesgue_Partial (Nodes, X, Nodes'Last) else 0.0)
   with Pre => Nodes'First = 1;
   --  The Lebesgue function of the nodes at X: the sum of the magnitudes of their basis polynomials.

   procedure Lemma_Interpolation_Bound (P : Polynomial; Nodes : Node_Array; X, Largest, Lebesgue : Real)
   with
     Pre  =>
       Is_Polynomial (P)
       and then Is_Increasing (Nodes)
       and then P'Last < Nodes'Last
       and then (for all I in Nodes'Range => abs Eval (P, Nodes (I)) <= Largest)
       and then Lebesgue_Sum (Nodes, X) <= Lebesgue,
     Post => abs Eval (P, X) <= Lebesgue * Largest;
   --  A polynomial with at most as many coefficients as there are nodes is bounded at X by the Lebesgue function
   --  there times its largest magnitude at a node.

end Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation;
