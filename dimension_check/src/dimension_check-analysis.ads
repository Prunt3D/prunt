--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  dimension_check implements the dimensionality checking of GNAT and is derived from it, in particular from
--  gcc/ada/sem_dim.adb, Copyright (C) 2011-2025, Free Software Foundation, Inc.
--
--  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
--  later version.
--
--  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
--  details.
--
--  You should have received a copy of the GNU General Public License along with this program, in the file COPYING. If
--  not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Dimension_Check.Dimensions; use Dimension_Check.Dimensions;
with Dimension_Check.Rationals;  use Dimension_Check.Rationals;
with Dimension_Check.Reports;
with Libadalang.Analysis;        use Libadalang.Analysis;

package Dimension_Check.Analysis is

   type Extra_Check is
     (Expression_Function_Results,
      --  The expression of an expression function has the dimensions of its result subtype.

      Default_Expressions,
      --  A default expression of a parameter or discriminant has the dimensions of its subtype. GNAT only warns
      --  about numeric literals there.

      Qualified_Expressions,
      --  The operand of a qualified expression has the dimensions of the qualifying subtype. GNAT gives a qualified
      --  expression the dimensions of its subtype without looking at the operand.

      Attribute_Arguments,
      --  The value arguments of attributes such as Length'Max (A, B) have the dimensions of the prefix subtype.

      Membership_Tests,
      --  The tested value and the bounds or values of a membership test have the same dimensions.

      Delta_Aggregates,
      --  The components of a delta aggregate have the dimensions of their subtypes. GNAT does not check them.

      Conditional_Initial_Values,
      --  An if or case expression that is the initial value of an object, or the actual of a generic formal object,
      --  has the dimensions of the object. GNAT gives such an expression the subtype of the object, and so accepts
      --  it whatever its dimensions.

      Explicit_Dimensionless);
   --  Checks that GNAT's own dimensionality checking does not make. Explicit_Dimensionless separates a subtype that is
   --  annotated as dimensionless from the root type of the system, which has no dimensions of its own: an actual
   --  for a dimensionless formal must be dimensionless, a dimensionless constant cannot take the dimensions of its
   --  initial value, and the other extra checks apply to dimensionless subtypes as well. GNAT treats both as
   --  having no dimensions, so a formal of either subtype accepts any actual.

   type Extra_Check_Set is array (Extra_Check) of Boolean;

   type Checker is limited private;
   --  The state of one job: the dimension systems found so far and caches of computed dimensions. A checker is used
   --  by one task at a time, and only with units of one analysis context.

   procedure Check_Unit (Self : in out Checker; Unit : Analysis_Unit; Extras : Extra_Check_Set);
   --  Check every expression and declaration of Unit and record the findings with Dimension_Check.Reports.

private

   use Dimension_Check.Reports;

   type System_Id is new Natural;
   No_System : constant System_Id := 0;
   --  The system of a value whose dimensions do not depend on one, such as a numeric literal.

   package Name_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type System_Info is record
      Names : Name_Vectors.Vector;
   end record;
   --  A dimension system: the names of its base dimensions, in the order of Vector positions.

   package System_Vectors is new Ada.Containers.Vectors (Positive, System_Info);

   type Dim_Kind is (Not_Dimensioned, Dimensioned, Unknown);
   --  Not_Dimensioned values are not of a dimension system type. Unknown values could not be analysed, or follow
   --  from an error already reported; they match any dimensions so that one error does not cause others.

   type Literal_Kind is (Not_Literal, Zero_Literal, Nonzero_Literal, Context_Typed);
   --  Whether an expression is a numeric literal, possibly in parentheses, and whether its value is zero. GNAT
   --  gives a literal the dimensions of its context in some places, and a zero literal any dimensions there.
   --  Context_Typed is a named number, or a conditional expression of literals and named numbers: GNAT gives such an
   --  initial value of an object the subtype of the object, and so its dimensions.

   type Dim_Info is record
      Kind    : Dim_Kind := Not_Dimensioned;
      Dims    : Vector := Dimensionless;
      System  : System_Id := No_System;
      Literal : Literal_Kind := Not_Literal;
   end record;
   --  The dimensions of an expression.

   Unknown_Dims : constant Dim_Info := (Kind => Unknown, others => <>);

   type Subtype_Kind is (Not_In_System, Free, Declared);
   --  Free is the root type of a system and any subtype of it without a dimension annotation: its values have no
   --  dimensions, but formals of it accept any dimensions, and constants of it take the dimensions of their value.

   type Subtype_Info is record
      Kind    : Subtype_Kind := Not_In_System;
      Dims    : Vector := Dimensionless;
      System  : System_Id := No_System;
      Is_Root : Boolean := False;
   end record;
   --  The dimensions of a subtype. Is_Root is True for the first subtype of the root type of a system.

   type Special_Kind is
     (Ordinary, Elementary_Sqrt, Elementary_Power, Elementary_Other, Annotated_Power, Annotated_Root);
   --  How a call of a subprogram treats dimensions. Elementary kinds are functions of an instance of
   --  Ada.Numerics.Generic_Elementary_Functions; annotated kinds are functions with a Power or Root annotation.

   type Special_Info is record
      Kind   : Special_Kind := Ordinary;
      Degree : Rational := One;
   end record;
   --  Degree is the root taken by an Elementary_Sqrt or Annotated_Root function.

   function Node_Hash (Node : Ada_Node) return Ada.Containers.Hash_Type;
   --  Hash a node for use as a map key.

   package Dim_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Ada_Node,
        Element_Type    => Dim_Info,
        Hash            => Node_Hash,
        Equivalent_Keys => "=");

   package Subtype_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Ada_Node,
        Element_Type    => Subtype_Info,
        Hash            => Node_Hash,
        Equivalent_Keys => "=");

   package Special_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Ada_Node,
        Element_Type    => Special_Info,
        Hash            => Node_Hash,
        Equivalent_Keys => "=");

   package System_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Ada_Node,
        Element_Type    => System_Id,
        Hash            => Node_Hash,
        Equivalent_Keys => "=");

   type Warning_Switch is record
      Line : Positive;
      Off  : Boolean;
   end record;
   --  A pragma Warnings that turns the literal warnings of this checker off or on from Line onward.

   package Switch_Vectors is new Ada.Containers.Vectors (Positive, Warning_Switch);

   type Checker is limited record
      Extras    : Extra_Check_Set := [others => True];
      Systems   : System_Vectors.Vector;
      Roots     : System_Maps.Map;
      Types     : Subtype_Maps.Map;
      Exprs     : Dim_Maps.Map;
      Constants : Dim_Maps.Map;
      Specials  : Special_Maps.Map;
      Unit      : Analysis_Unit;
      Switches  : Switch_Vectors.Vector;
   end record;

   type Optional_Rational (Present : Boolean := False) is record
      case Present is
         when True =>
            Value : Rational;

         when False =>
            null;
      end case;
   end record;
   --  The value of a static expression, if it could be evaluated.

   type Actual_List is array (Positive range <>) of Expr;
   --  A list of expressions, such as the arguments of an annotation or the branches of a conditional expression.

   type Parameter is record
      Formal : Defining_Name;
      Actual : Expr;
   end record;
   --  A formal parameter of a call and its explicit actual, which is a null node when the default is used.

   type Parameter_List is array (Positive range <>) of Parameter;

   procedure Report (Self : in out Checker; Node : Ada_Node'Class; Kind : Severity; Message : String);
   --  Record a finding located at the start of Node. Literal warnings are dropped where a pragma Warnings turns them
   --  off.

   procedure Note_Unresolved (Self : in out Checker; Node : Ada_Node'Class; What : String);
   --  Record a note that Node could not be analysed because Libadalang failed to resolve it.

   function Image (Self : Checker; Info : Dim_Info) return String;
   --  Return the dimensions of Info as "[]" or for example "[Millimeter * Second**(-1)]".

   function Image (Self : Checker; Info : Subtype_Info) return String;
   --  Return the dimensions of the subtype described by Info, as for a Dim_Info.

   function To_Dims (Info : Subtype_Info) return Dim_Info;
   --  Return the dimensions of a value of the subtype described by Info.

   function Same (Left, Right : Dim_Info) return Boolean;
   --  Return False only if both are dimensioned values with different dimensions.

   function Annotation_Arguments (Decl : Basic_Decl'Class; Kind : String) return Actual_List;
   --  Return the first non-inherited annotation (Dimension_Check, Kind, ...) of Decl as the list of its arguments
   --  from Dimension_Check onward, or an empty list if there is none. Kind is compared in lower case.

   function System_Of_Root (Self : in out Checker; Decl : Base_Type_Decl'Class) return System_Id;
   --  Return the system declared by a Dimension_System annotation of Decl, registering it on first use, or No_System.

   function Parse_Dimensions
     (Self : in out Checker; Decl : Basic_Decl'Class; System : System_Id; Result : out Vector) return Boolean;
   --  Parse the Dimension annotation of Decl into Result. Return False if Decl has no such annotation. Malformed
   --  arguments are reported and ignored.

   function Type_Info (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info;
   --  Return the dimensions of the subtype declared by Decl.

   function Compute_Type_Info (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info;
   --  Uncached implementation of Type_Info.

   function Type_Expr_Info (Self : in out Checker; Type_Expression : Type_Expr'Class) return Subtype_Info;
   --  Return the dimensions of the subtype denoted by a type expression of a declaration.

   function Is_Universal (Node : Ada_Node'Class; Decl : Base_Type_Decl'Class) return Boolean;
   --  Return whether Decl is universal_integer or universal_real.

   function Static_Value (E : Expr'Class; Integer_Mode : Boolean) return Optional_Rational;
   --  Evaluate the static numeric expression E exactly. In Integer_Mode "/" truncates as Ada integer division does.
   --  Literals, named numbers, constants with static values, parentheses, unary + and -, and the binary operators
   --  + - * / and ** with an integer exponent are evaluated.

   function Is_Integer_Division (E : Expr'Class) return Boolean;
   --  Return whether E is a division of integers, possibly negated or in parentheses, such as (1 / 2). GNAT read
   --  such an exponent as a fraction for dimensioned types, where Ada reads it as an integer division.

   function Dims_Of (Self : in out Checker; E : Expr'Class) return Dim_Info;
   --  Return the dimensions of the expression E. Never reports a finding except notes for unresolved names.

   function Compute_Dims (Self : in out Checker; E : Expr'Class) return Dim_Info;
   --  Uncached implementation of Dims_Of.

   function Leaf_Dims (Self : in out Checker; E : Expr'Class) return Dim_Info;
   --  Return the dimensions of the subtype that Libadalang gives as the type of E.

   function Name_Dims (Self : in out Checker; E : Name'Class) return Dim_Info;
   --  Return the dimensions of an identifier or a dotted name, following constants whose subtype does not fix their
   --  dimensions to their initial value.

   function Constant_Dims (Self : in out Checker; Decl : Object_Decl'Class) return Dim_Info;
   --  Return the dimensions of the initial value of the constant declared by Decl, or Unknown_Dims.

   function Operator_Decl (Operator : Op'Class) return Basic_Decl;
   --  Return the user-defined subprogram that an operator calls, or a null node for a predefined operator.

   function Called_Decl (Call : Call_Expr'Class) return Basic_Decl;
   --  Return the subprogram declaration called by Call, or a null node when it calls an access value.

   function Special_Of (Self : in out Checker; Callee : Basic_Decl'Class) return Special_Info;
   --  Return how calls of Callee treat dimensions, following renamings.

   function Parameters_Of (Call : Ada_Node'Class; Spec : Base_Subp_Spec'Class) return Parameter_List;
   --  Return the formals of the subprogram specification Spec, in order, with the explicit actuals of Call (a call
   --  expression or an operator).

   function Delta_Base_Type (E : Expr'Class) return Base_Type_Decl;
   --  Return the type of the base expression of E if E is a delta aggregate, possibly in parentheses, or a null node.
   --  Ada gives a delta aggregate the type of its base expression.

   function Misresolved (Call : Ada_Node'Class; Spec : Base_Subp_Spec'Class) return Boolean;
   --  Return whether Libadalang resolved Call to a Callee whose formal does not have the type of a delta aggregate
   --  actual. Libadalang can pick the wrong overload of an operator for such an actual.

   function Callee_Name (Callee : Basic_Decl'Class) return String;
   --  Return the name of Callee as written in its declaration, for messages.

   function Call_Dims (Self : in out Checker; Call : Expr'Class; Callee : Basic_Decl'Class) return Dim_Info;
   --  Return the dimensions of the result of calling Callee, where Call is a call expression or an operator.

   function Power_Dims (Base : Dim_Info; Exponent : Expr'Class; Integer_Mode : Boolean) return Dim_Info;
   --  Return the dimensions of Base raised to the static exponent Exponent.

   function Combine (Left, Right : Dim_Info; Dims : Vector) return Dim_Info;
   --  Return the result of a predefined arithmetic operator with operands Left and Right and result dimensions Dims,
   --  or Unknown_Dims or Not_Dimensioned when the operands do not allow a result.

   function Conversion_Dims (Self : in out Checker; Conversion : Call_Expr'Class) return Dim_Info;
   --  Return the dimensions of a type conversion. A conversion to the root type keeps the dimensions of its operand.

   function Conversion_Operand (Conversion : Call_Expr'Class) return Expr;
   --  Return the operand of a type conversion.

   function Attribute_Dims
     (Self : in out Checker; Attribute : Attribute_Ref'Class; Whole : Expr'Class) return Dim_Info;
   --  Return the dimensions of an attribute reference. Whole is the attribute reference itself or, for an attribute
   --  function such as Length'Max (A, B), the call of it, which Libadalang represents as a call expression whose name
   --  is the attribute reference.

   function Branch_Dims (Self : in out Checker; Branches : Actual_List) return Dim_Info;
   --  Return the common dimensions of the branches of a conditional expression, or Unknown_Dims if they differ.

   function Branches_Of (E : Expr'Class) return Actual_List;
   --  Return the dependent expressions of an if expression or a case expression.

   function Enclosing_Function_Result (Node : Ada_Node'Class) return Type_Expr;
   --  Return the result subtype of the subprogram body that encloses Node, or a null node.

   procedure Check_Value
     (Self              : in out Checker;
      Value             : Expr'Class;
      Target            : Subtype_Info;
      Context           : String;
      Lenient_Literals  : Boolean;
      Allow_Propagation : Boolean := False;
      Report_Errors     : Boolean := True;
      Context_Typed_OK  : Boolean := False);
   --  Check that Value has the dimensions of Target. With Lenient_Literals, a numeric literal takes the dimensions of
   --  Target with a warning unless it is zero. With Allow_Propagation, any dimensions are accepted. With
   --  Context_Typed_OK, a Context_Typed value is accepted.

   procedure Check_Range (Self : in out Checker; Type_Expression : Type_Expr'Class; Target : Subtype_Info);
   --  Check the bounds of a range constraint of Type_Expression, if any, against Target.

   procedure Check_Bounds (Self : in out Checker; Range_Expression : Expr'Class; Target : Subtype_Info);
   --  Check the bounds of an explicit range A .. B against Target.

   procedure Check_Compared (Self : in out Checker; Left, Right : Expr'Class; Is_Error : out Boolean);
   --  Apply GNAT's rule for the operands of a comparison: when their dimensions differ and one is a numeric literal,
   --  the literal takes the dimensions of the other, with a warning unless it is zero. Is_Error is set when the
   --  dimensions differ and neither is a literal; the caller reports the error.

   procedure Check_Call
     (Self : in out Checker; Call : Ada_Node'Class; Callee : Basic_Decl'Class; Spec : Base_Subp_Spec'Class);
   --  Check the actual parameters of a call of the subprogram with specification Spec. Callee is its declaration, or
   --  a null node for a call through an access value.

   procedure Check_Predefined_Operator (Self : in out Checker; Operation : Bin_Op'Class);
   --  Check a predefined binary operator.

   procedure Check_Conversion (Self : in out Checker; Conversion : Call_Expr'Class);
   --  Check that a type conversion does not change dimensions, except to the root type of a system.

   procedure Check_Branches (Self : in out Checker; E : Expr'Class);
   --  Check that all dependent expressions of a conditional expression have the same dimensions.

   procedure Check_Aggregate (Self : in out Checker; Aggregate : Base_Aggregate'Class);
   --  Check the components of an array or record aggregate against their subtypes.

   procedure Check_Array_Components (Self : in out Checker; Assocs : Assoc_List'Class; Component : Subtype_Info);
   --  Check the component expressions of an array aggregate, descending into subaggregates.

   function Array_Component (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info;
   --  Return the dimensions of the component subtype of the array type Decl.

   procedure Check_Attribute_Arguments
     (Self : in out Checker; Attribute : Attribute_Ref'Class; Args : Assoc_List'Class);
   --  Check the value arguments Args of a scalar attribute against the prefix subtype.

   function Access_Spec (Call : Call_Expr'Class) return Base_Subp_Spec;
   --  Return the profile of the access-to-subprogram value that Call calls, or a null node.

   function Called_Attribute (Call : Call_Expr'Class) return Attribute_Ref;
   --  Return the attribute reference that Call calls, as in Length'Max (A, B), or a null node.

   procedure Check_Membership (Self : in out Checker; Membership : Membership_Expr'Class);
   --  Check that the values and bounds of a membership test have the dimensions of the tested value.

   procedure Check_Annotations (Self : in out Checker; Decl : Basic_Decl'Class);
   --  Check the dimension annotations of a declaration: parse them, and report annotations on declarations that
   --  cannot carry them.

   function Unchecked_Initial_Value (Self : Checker; Value : Expr'Class) return Boolean;
   --  Return whether GNAT accepts Value as the initial value of an object whatever its dimensions, and the
   --  Conditional_Initial_Values check is off.

   function Is_Inside (Inner, Outer : Ada_Node'Class) return Boolean;
   --  Return whether Inner is Outer or one of its descendants.

   procedure Check_Node (Self : in out Checker; Node : Ada_Node'Class);
   --  Make every check that applies to Node itself.

   procedure Collect_Warning_Switches (Self : in out Checker);
   --  Record the pragmas Warnings of the current unit that turn literal warnings off or on.

   function Warnings_Off (Self : Checker; Line : Positive) return Boolean;
   --  Return whether literal warnings are turned off at Line of the current unit.

   function Display_Filename (Filename : String) return String;
   --  Return Filename relative to the current directory if it is below it.

end Dimension_Check.Analysis;
