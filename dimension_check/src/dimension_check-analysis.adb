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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded;
with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common;       use Libadalang.Common;

package body Dimension_Check.Analysis is

   function UTF8 (Text : Langkit_Support.Text.Text_Type) return String renames Langkit_Support.Text.To_UTF8;

   function Text_Of (Node : Ada_Node'Class) return String
   is (UTF8 (Node.Text));

   function Lower_Text (Node : Ada_Node'Class) return String
   is (To_Lower (Text_Of (Node)));

   function Is_Identifier (E : Expr'Class; Name : String) return Boolean
   is (not E.Is_Null and then E.Kind = Ada_Identifier and then Lower_Text (E) = Name);

   function Without_Parens (E : Expr'Class) return Expr is
      Result : Expr := E.As_Expr;
   begin
      while not Result.Is_Null and then Result.Kind = Ada_Paren_Expr loop
         Result := Result.As_Paren_Expr.F_Expr;
      end loop;
      return Result;
   end Without_Parens;

   function Same_Node (Left, Right : Ada_Node'Class) return Boolean
   is (not Left.Is_Null
       and then not Right.Is_Null
       and then Left.Unit = Right.Unit
       and then Left.Sloc_Range = Right.Sloc_Range
       and then Left.Kind = Right.Kind);

   function Without_Literal (Info : Dim_Info) return Dim_Info
   is ((Info with delta Literal => Not_Literal));

   No_Actuals : constant Actual_List (1 .. 0) := [others => No_Expr];

   Wildcard_Zero : constant Dim_Info :=
     (Kind => Dimensioned, Dims => Dimensionless, System => No_System, Literal => Not_Literal);
   --  The dimensions of a universal value such as a named number: dimensionless in no particular system.

   ---------------
   -- Node_Hash --
   ---------------

   function Node_Hash (Node : Ada_Node) return Ada.Containers.Hash_Type
   is (Libadalang.Analysis.Hash (Node));

   ----------------------
   -- Display_Filename --
   ----------------------

   function Display_Filename (Filename : String) return String is
      Current_Directory : constant String := Ada.Directories.Current_Directory;
   begin
      if Filename'Length > Current_Directory'Length
        and then Filename (Filename'First .. Filename'First + Current_Directory'Length - 1) = Current_Directory
        and then Filename (Filename'First + Current_Directory'Length) in '/' | '\'
      then
         return Filename (Filename'First + Current_Directory'Length + 1 .. Filename'Last);
      else
         return Filename;
      end if;
   end Display_Filename;

   ------------
   -- Report --
   ------------

   procedure Report (Self : in out Checker; Node : Ada_Node'Class; Kind : Severity; Message : String) is
      Sloc : constant Source_Location_Range := Node.Sloc_Range;
      File : constant String := Display_Filename (Node.Unit.Get_Filename);
   begin
      if Kind = Warning and then Node.Unit = Self.Unit and then Warnings_Off (Self, Positive (Sloc.Start_Line)) then
         return;
      end if;

      Reports.Add
        ((File_Length    => File'Length,
          Message_Length => Message'Length,
          Kind           => Kind,
          Line           => Positive (Sloc.Start_Line),
          Column         => Positive (Sloc.Start_Column),
          File           => File,
          Message        => Message));
   end Report;

   ---------------------
   -- Note_Unresolved --
   ---------------------

   procedure Note_Unresolved (Self : in out Checker; Node : Ada_Node'Class; What : String) is
   begin
      Report (Self, Node, Note, "not checked, Libadalang could not resolve " & What);
   end Note_Unresolved;

   -----------
   -- Image --
   -----------

   function Image (Self : Checker; Info : Dim_Info) return String is
      Result : Unbounded_String;
   begin
      case Info.Kind is
         when Not_Dimensioned =>
            return "(no dimension system)";

         when Unknown         =>
            return "(unknown)";

         when Dimensioned     =>
            null;
      end case;

      for I in Dimension_Index loop
         if Info.Dims (I) /= Zero then
            if Length (Result) > 0 then
               Append (Result, " * ");
            end if;

            if Info.System /= No_System and then I <= Self.Systems (Positive (Info.System)).Names.Last_Index then
               Append (Result, Self.Systems (Positive (Info.System)).Names (I));
            else
               Append (Result, "Dimension_" & Trim (I'Image, Ada.Strings.Both));
            end if;

            if Info.Dims (I) = One then
               null;
            elsif Is_Integer (Info.Dims (I)) and then Numerator (Info.Dims (I)) > 0 then
               Append (Result, "**" & Rationals.Image (Info.Dims (I)));
            else
               Append (Result, "**(" & Rationals.Image (Info.Dims (I)) & ")");
            end if;
         end if;
      end loop;

      return "[" & To_String (Result) & "]";
   end Image;

   function Image (Self : Checker; Info : Subtype_Info) return String
   is (Image (Self, To_Dims (Info)));

   -------------
   -- To_Dims --
   -------------

   function To_Dims (Info : Subtype_Info) return Dim_Info is
   begin
      case Info.Kind is
         when Not_In_System =>
            return (Kind => Not_Dimensioned, others => <>);

         when Free          =>
            return (Kind => Dimensioned, Dims => Dimensionless, System => Info.System, Literal => Not_Literal);

         when Declared      =>
            return (Kind => Dimensioned, Dims => Info.Dims, System => Info.System, Literal => Not_Literal);
      end case;
   end To_Dims;

   ----------
   -- Same --
   ----------

   function Same (Left, Right : Dim_Info) return Boolean
   is (Left.Kind /= Dimensioned or else Right.Kind /= Dimensioned or else Left.Dims = Right.Dims);

   --------------------------
   -- Annotation_Arguments --
   --------------------------

   function Annotation_Arguments (Decl : Basic_Decl'Class; Kind : String) return Actual_List is

      function Pragma_Items (Prag : Pragma_Node'Class) return Actual_List is
         Args  : constant Base_Assoc_List := Prag.F_Args;
         Count : Natural := 0;
      begin
         for Arg of Args loop
            if Arg.Kind = Ada_Pragma_Argument_Assoc and then Arg.As_Pragma_Argument_Assoc.F_Name.Is_Null then
               Count := Count + 1;
            end if;
         end loop;

         return Result : Actual_List (1 .. Count) do
            Count := 0;
            for Arg of Args loop
               if Arg.Kind = Ada_Pragma_Argument_Assoc and then Arg.As_Pragma_Argument_Assoc.F_Name.Is_Null then
                  Count := Count + 1;
                  Result (Count) := Arg.As_Pragma_Argument_Assoc.F_Expr;
               end if;
            end loop;
         end return;
      end Pragma_Items;

      function Aspect_Items (Value : Expr'Class) return Actual_List is
         E : constant Expr := Without_Parens (Value);
      begin
         if E.Is_Null then
            return No_Actuals;
         elsif E.Kind = Ada_Aggregate then
            declare
               Assocs : constant Assoc_List := E.As_Aggregate.F_Assocs;
               Count  : Natural := 0;
            begin
               return Result : Actual_List (1 .. Assocs.Children_Count) do
                  for Assoc of Assocs loop
                     Count := Count + 1;
                     if Assoc.Kind = Ada_Aggregate_Assoc then
                        Result (Count) := Assoc.As_Aggregate_Assoc.F_R_Expr;
                     else
                        Result (Count) := No_Expr;
                     end if;
                  end loop;
               end return;
            end;
         else
            return [1 => E];
         end if;
      end Aspect_Items;

      function Items_Of (A : Aspect) return Actual_List is
         N : constant Ada_Node := Node (A).As_Ada_Node;
      begin
         if not N.Is_Null and then N.Kind = Ada_Pragma_Node then
            return Pragma_Items (N.As_Pragma_Node);
         elsif not N.Is_Null and then N.Kind = Ada_Aspect_Assoc then
            return Aspect_Items (N.As_Aspect_Assoc.F_Expr);
         else
            return Aspect_Items (Value (A));
         end if;
      end Items_Of;

   begin
      for A of Decl.P_Get_Annotations loop
         if Exists (A) and then not Inherited (A) then
            declare
               Items : constant Actual_List := Items_Of (A);
            begin
               if Items'Length >= 2
                 and then Is_Identifier (Items (Items'First), "dimension_check")
                 and then Is_Identifier (Items (Items'First + 1), Kind)
               then
                  return Items (Items'First) & Items (Items'First + 2 .. Items'Last);
               end if;
            end;
         end if;
      end loop;

      return No_Actuals;
   end Annotation_Arguments;

   --------------------
   -- System_Of_Root --
   --------------------

   function System_Of_Root (Self : in out Checker; Decl : Base_Type_Decl'Class) return System_Id is
      Key : constant Ada_Node := Decl.As_Ada_Node;
   begin
      if Self.Roots.Contains (Key) then
         return Self.Roots (Key);
      end if;

      declare
         Args   : constant Actual_List := Annotation_Arguments (Decl, "dimension_system");
         System : System_Info;
         Id     : System_Id := No_System;
      begin
         if Args'Length > 0 then
            for I in Args'First + 1 .. Args'Last loop
               if Args (I).Is_Null or else Args (I).Kind /= Ada_String_Literal then
                  Report (Self, Args (Args'First), Error, "the name of a dimension must be a string literal");
               else
                  declare
                     Name : constant String := UTF8 (Args (I).As_String_Literal.P_Denoted_Value);
                  begin
                     if Name'Length = 0 then
                        Report (Self, Args (I), Error, "the name of a dimension must not be empty");
                     elsif (for some Other of System.Names => To_Lower (Other) = To_Lower (Name)) then
                        Report (Self, Args (I), Error, "dimension """ & Name & """ is declared twice");
                     elsif Natural (System.Names.Length) = Max_Dimensions then
                        Report
                          (Self,
                           Args (I),
                           Error,
                           "a dimension system has at most" & Integer'Image (Max_Dimensions) & " dimensions");
                     else
                        System.Names.Append (Name);
                     end if;
                  end;
               end if;
            end loop;

            if Args'Length = 1 then
               Report (Self, Args (Args'First), Error, "a dimension system needs at least one dimension");
            end if;

            Self.Systems.Append (System);
            Id := System_Id (Self.Systems.Last_Index);
         end if;

         Self.Roots.Insert (Key, Id);
         return Id;
      end;
   end System_Of_Root;

   ----------------------
   -- Parse_Dimensions --
   ----------------------

   function Parse_Dimensions
     (Self : in out Checker; Decl : Basic_Decl'Class; System : System_Id; Result : out Vector) return Boolean
   is
      Args : constant Actual_List := Annotation_Arguments (Decl, "dimension");
      Seen : array (Dimension_Index) of Boolean := [others => False];
      I    : Positive;

      function Exponent_Of (E : Expr'Class; Value : out Rational) return Boolean is
      begin
         if not E.Is_Null and then E.Kind = Ada_String_Literal then
            declare
               Text  : constant String := Trim (UTF8 (E.As_String_Literal.P_Denoted_Value), Ada.Strings.Both);
               Slash : constant Natural := Index (Text, "/");
            begin
               if Slash = 0 then
                  Value := To_Rational (Long_Long_Integer'Value (Text));
               else
                  Value :=
                    To_Rational
                      (Long_Long_Integer'Value (Text (Text'First .. Slash - 1)),
                       Long_Long_Integer'Value (Text (Slash + 1 .. Text'Last)));
               end if;
               return True;
            exception
               when Constraint_Error =>
                  return False;
            end;
         elsif Is_Integer_Division (E) then
            return False;
         else
            declare
               Static : constant Optional_Rational := Static_Value (E, Integer_Mode => True);
            begin
               if Static.Present then
                  Value := Static.Value;
                  return True;
               else
                  return False;
               end if;
            end;
         end if;
      end Exponent_Of;

   begin
      Result := Dimensionless;

      if Args'Length = 0 then
         return False;
      elsif System = No_System then
         return True;
      end if;

      I := Args'First + 1;
      while I <= Args'Last loop
         declare
            Name_Expr : constant Expr := Args (I);
            Index     : Natural := 0;
            Value     : Rational;
         begin
            if Name_Expr.Is_Null or else Name_Expr.Kind /= Ada_String_Literal then
               Report (Self, Args (Args'First), Error, "expected the name of a dimension as a string literal");
               return True;
            end if;

            declare
               Name  : constant String := UTF8 (Name_Expr.As_String_Literal.P_Denoted_Value);
               Names : Name_Vectors.Vector renames Self.Systems (Positive (System)).Names;
               Known : Unbounded_String;
            begin
               for J in 1 .. Names.Last_Index loop
                  if To_Lower (Names (J)) = To_Lower (Name) then
                     Index := J;
                  end if;
                  Append (Known, (if J = 1 then "" else ", ") & Names (J));
               end loop;

               if Index = 0 then
                  Report
                    (Self,
                     Name_Expr,
                     Error,
                     "unknown dimension """ & Name & """, expected one of " & To_String (Known));
               elsif Seen (Index) then
                  Report (Self, Name_Expr, Error, "dimension """ & Name & """ is given twice");
                  Index := 0;
               end if;
            end;

            if I = Args'Last then
               Report (Self, Name_Expr, Error, "missing exponent after the name of a dimension");
               return True;
            end if;

            if not Exponent_Of (Args (I + 1), Value) then
               Report
                 (Self,
                  (if Args (I + 1).Is_Null then Name_Expr.As_Ada_Node else Args (I + 1).As_Ada_Node),
                  Error,
                  "the exponent of a dimension must be a static integer or a string such as ""1/2""");
            elsif Index /= 0 then
               Seen (Index) := True;
               Result (Index) := Value;
            end if;
         end;
         I := I + 2;
      end loop;

      return True;
   end Parse_Dimensions;

   ---------------
   -- Type_Info --
   ---------------

   function Type_Info (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info is
   begin
      if Decl.Is_Null then
         return (others => <>);
      end if;

      declare
         Key : constant Ada_Node := Decl.As_Ada_Node;
      begin
         if Self.Types.Contains (Key) then
            return Self.Types (Key);
         end if;

         --  Guard against a subtype chain that leads back to itself in illegal code.
         Self.Types.Insert (Key, (others => <>));

         declare
            Result : Subtype_Info;
         begin
            begin
               Result := Compute_Type_Info (Self, Decl);
            exception
               when E : Property_Error =>
                  Note_Unresolved (Self, Decl, "the subtype: " & Exception_Message (E));
                  Result := (others => <>);
            end;
            Self.Types.Replace (Key, Result);
            return Result;
         end;
      end;
   end Type_Info;

   -----------------------
   -- Compute_Type_Info --
   -----------------------

   function Compute_Type_Info (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info is
   begin
      if Decl.Kind = Ada_Subtype_Decl then
         declare
            Parent_Decl : constant Base_Type_Decl := Decl.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl;
            Parent      : constant Subtype_Info := Type_Info (Self, Parent_Decl);
            Own         : Vector;
         begin
            if Parent.Kind = Not_In_System then
               if Parse_Dimensions (Self, Decl, No_System, Own) then
                  Report
                    (Self,
                     Decl,
                     Error,
                     "Dimension annotation on a subtype of a type that has no Dimension_System annotation");
               end if;
               return Parent;
            elsif Parse_Dimensions (Self, Decl, Parent.System, Own) then
               if Parent.Kind = Declared and then not Is_Dimensionless (Parent.Dims) and then Parent.Dims /= Own then
                  Report
                    (Self,
                     Decl,
                     Error,
                     "subtype already has dimension "
                     & Image (Self, Parent)
                     & " from its parent subtype, it cannot be annotated with "
                     & Image (Self, Subtype_Info'(Declared, Own, Parent.System, False)));
               end if;
               return (Kind => Declared, Dims => Own, System => Parent.System, Is_Root => False);
            else
               return (Parent with delta Is_Root => False);
            end if;
         end;
      else
         declare
            System : constant System_Id := System_Of_Root (Self, Decl);
         begin
            if System /= No_System then
               return (Kind => Free, Dims => Dimensionless, System => System, Is_Root => True);
            else
               return (others => <>);
            end if;
         end;
      end if;
   end Compute_Type_Info;

   --------------------
   -- Type_Expr_Info --
   --------------------

   function Type_Expr_Info (Self : in out Checker; Type_Expression : Type_Expr'Class) return Subtype_Info is
   begin
      if Type_Expression.Is_Null then
         return (others => <>);
      else
         return Type_Info (Self, Type_Expression.P_Designated_Type_Decl);
      end if;
   end Type_Expr_Info;

   ------------------
   -- Is_Universal --
   ------------------

   function Is_Universal (Node : Ada_Node'Class; Decl : Base_Type_Decl'Class) return Boolean
   is (not Decl.Is_Null
       and then
         (Decl.As_Ada_Node = Node.P_Universal_Real_Type.As_Ada_Node
          or else Decl.As_Ada_Node = Node.P_Universal_Int_Type.As_Ada_Node));

   ------------------
   -- Static_Value --
   ------------------

   function Static_Value (E : Expr'Class; Integer_Mode : Boolean) return Optional_Rational is

      Unknown : constant Optional_Rational := (Present => False);

      function Eval (E : Expr'Class; Integer_Mode : Boolean; Depth : Natural) return Optional_Rational is
      begin
         if E.Is_Null or else Depth > 32 then
            return Unknown;
         end if;

         case E.Kind is
            when Ada_Paren_Expr                     =>
               return Eval (E.As_Paren_Expr.F_Expr, Integer_Mode, Depth + 1);

            when Ada_Int_Literal | Ada_Real_Literal =>
               return (Present => True, Value => Parse_Numeric_Literal (Text_Of (E)));

            when Ada_Un_Op                          =>
               declare
                  Operand : constant Optional_Rational := Eval (E.As_Un_Op.F_Expr, Integer_Mode, Depth + 1);
               begin
                  if not Operand.Present then
                     return Unknown;
                  end if;

                  case E.As_Un_Op.F_Op.Kind is
                     when Ada_Op_Plus  =>
                        return Operand;

                     when Ada_Op_Minus =>
                        return (Present => True, Value => -Operand.Value);

                     when others       =>
                        return Unknown;
                  end case;
               end;

            when Ada_Bin_Op                         =>
               declare
                  B     : constant Bin_Op := E.As_Bin_Op;
                  Left  : constant Optional_Rational := Eval (B.F_Left, Integer_Mode, Depth + 1);
                  Right : Optional_Rational;
               begin
                  if not Left.Present then
                     return Unknown;
                  end if;

                  --  The exponent of ** is an integer whatever the mode of its base.
                  Right := Eval (B.F_Right, Integer_Mode or else B.F_Op.Kind = Ada_Op_Pow, Depth + 1);
                  if not Right.Present then
                     return Unknown;
                  end if;

                  case B.F_Op.Kind is
                     when Ada_Op_Plus  =>
                        return (Present => True, Value => Left.Value + Right.Value);

                     when Ada_Op_Minus =>
                        return (Present => True, Value => Left.Value - Right.Value);

                     when Ada_Op_Mult  =>
                        return (Present => True, Value => Left.Value * Right.Value);

                     when Ada_Op_Div   =>
                        if Integer_Mode then
                           return (Present => True, Value => Integer_Divide (Left.Value, Right.Value));
                        else
                           return (Present => True, Value => Left.Value / Right.Value);
                        end if;

                     when Ada_Op_Pow   =>
                        if not Is_Integer (Right.Value) or else abs Numerator (Right.Value) > 64 then
                           return Unknown;
                        end if;

                        declare
                           Result : Rational := One;
                        begin
                           for I in 1 .. abs Numerator (Right.Value) loop
                              Result := Result * Left.Value;
                           end loop;
                           if Numerator (Right.Value) < 0 then
                              Result := One / Result;
                           end if;
                           return (Present => True, Value => Result);
                        end;

                     when others       =>
                        return Unknown;
                  end case;
               end;

            when Ada_Identifier | Ada_Dotted_Name   =>
               declare
                  Decl : constant Basic_Decl := E.As_Name.P_Referenced_Decl;
               begin
                  if Decl.Is_Null then
                     return Unknown;
                  elsif Decl.Kind = Ada_Number_Decl then
                     declare
                        Value_Expr : constant Expr := Decl.As_Number_Decl.F_Expr;
                     begin
                        return
                          Eval
                            (Value_Expr,
                             Is_Universal (Value_Expr, Value_Expr.P_Expression_Type)
                             and then
                               Value_Expr.P_Expression_Type.As_Ada_Node = Value_Expr.P_Universal_Int_Type.As_Ada_Node,
                             Depth + 1);
                     end;
                  elsif Decl.Kind = Ada_Object_Decl
                    and then Decl.As_Object_Decl.F_Has_Constant
                    and then not Decl.As_Object_Decl.F_Default_Expr.Is_Null
                  then
                     return
                       Eval
                         (Decl.As_Object_Decl.F_Default_Expr,
                          Decl.As_Object_Decl.F_Type_Expr.P_Designated_Type_Decl.P_Is_Int_Type,
                          Depth + 1);
                  else
                     return Unknown;
                  end if;
               end;

            when others                             =>
               return Unknown;
         end case;
      exception
         when Constraint_Error | Property_Error =>
            return Unknown;
      end Eval;

   begin
      return Eval (E, Integer_Mode, 0);
   end Static_Value;

   -------------------------
   -- Is_Integer_Division --
   -------------------------

   function Is_Integer_Division (E : Expr'Class) return Boolean is
      Inner : Expr := Without_Parens (E);
   begin
      if not Inner.Is_Null
        and then Inner.Kind = Ada_Un_Op
        and then Inner.As_Un_Op.F_Op.Kind in Ada_Op_Minus | Ada_Op_Plus
      then
         Inner := Without_Parens (Inner.As_Un_Op.F_Expr);
      end if;

      return
        not Inner.Is_Null
        and then Inner.Kind = Ada_Bin_Op
        and then Inner.As_Bin_Op.F_Op.Kind = Ada_Op_Div
        and then Without_Parens (Inner.As_Bin_Op.F_Left).Kind = Ada_Int_Literal
        and then Without_Parens (Inner.As_Bin_Op.F_Right).Kind = Ada_Int_Literal;
   end Is_Integer_Division;

   -------------
   -- Dims_Of --
   -------------

   function Dims_Of (Self : in out Checker; E : Expr'Class) return Dim_Info is
   begin
      if E.Is_Null then
         return Unknown_Dims;
      end if;

      declare
         Key    : constant Ada_Node := E.As_Ada_Node;
         Result : Dim_Info;
      begin
         if Self.Exprs.Contains (Key) then
            return Self.Exprs (Key);
         end if;

         begin
            Result := Compute_Dims (Self, E);
         exception
            when X : Property_Error =>
               Note_Unresolved (Self, E, "the expression: " & Exception_Message (X));
               Result := Unknown_Dims;
         end;

         Self.Exprs.Include (Key, Result);
         return Result;
      end;
   end Dims_Of;

   ------------------
   -- Compute_Dims --
   ------------------

   function Compute_Dims (Self : in out Checker; E : Expr'Class) return Dim_Info is
   begin
      case E.Kind is
         when Ada_Paren_Expr                     =>
            return Dims_Of (Self, E.As_Paren_Expr.F_Expr);

         when Ada_Int_Literal | Ada_Real_Literal =>
            declare
               Is_Zero_Literal : Boolean := False;
            begin
               begin
                  Is_Zero_Literal := Parse_Numeric_Literal (Text_Of (E)) = Zero;
               exception
                  when Constraint_Error =>
                     null;
               end;

               return
                 (Kind    => Dimensioned,
                  Dims    => Dimensionless,
                  System  => No_System,
                  Literal => (if Is_Zero_Literal then Zero_Literal else Nonzero_Literal));
            end;

         when Ada_Un_Op                          =>
            declare
               U      : constant Un_Op := E.As_Un_Op;
               Callee : constant Basic_Decl := Operator_Decl (U.F_Op);
            begin
               if not Callee.Is_Null then
                  return Call_Dims (Self, U, Callee);
               end if;

               case U.F_Op.Kind is
                  when Ada_Op_Minus | Ada_Op_Plus | Ada_Op_Abs =>
                     return Without_Literal (Dims_Of (Self, U.F_Expr));

                  when others                                  =>
                     return (Kind => Not_Dimensioned, others => <>);
               end case;
            end;

         when Ada_Bin_Op | Ada_Relation_Op       =>
            declare
               B      : constant Bin_Op := E.As_Bin_Op;
               Callee : constant Basic_Decl := Operator_Decl (B.F_Op);
            begin
               if not Callee.Is_Null then
                  return Call_Dims (Self, B, Callee);
               end if;

               case B.F_Op.Kind is
                  when Ada_Op_Pow                                           =>
                     return Power_Dims (Dims_Of (Self, B.F_Left), B.F_Right, Integer_Mode => True);

                  when Ada_Op_Plus | Ada_Op_Minus | Ada_Op_Mod | Ada_Op_Rem =>
                     declare
                        Left  : constant Dim_Info := Dims_Of (Self, B.F_Left);
                        Right : constant Dim_Info := Dims_Of (Self, B.F_Right);
                     begin
                        if Left.Kind = Dimensioned and then Right.Kind = Dimensioned and then Left.Dims /= Right.Dims
                        then
                           return Unknown_Dims;
                        else
                           return Combine (Left, Right, Left.Dims);
                        end if;
                     end;

                  when Ada_Op_Mult                                          =>
                     declare
                        Left  : constant Dim_Info := Dims_Of (Self, B.F_Left);
                        Right : constant Dim_Info := Dims_Of (Self, B.F_Right);
                     begin
                        return Combine (Left, Right, Left.Dims + Right.Dims);
                     end;

                  when Ada_Op_Div                                           =>
                     declare
                        Left  : constant Dim_Info := Dims_Of (Self, B.F_Left);
                        Right : constant Dim_Info := Dims_Of (Self, B.F_Right);
                     begin
                        return Combine (Left, Right, Left.Dims - Right.Dims);
                     end;

                  when others                                               =>
                     return (Kind => Not_Dimensioned, others => <>);
               end case;
            end;

         when Ada_Call_Expr                      =>
            declare
               C : constant Call_Expr := E.As_Call_Expr;
            begin
               if not Called_Attribute (C).Is_Null then
                  return Attribute_Dims (Self, Called_Attribute (C), C);
               end if;

               case C.P_Kind is
                  when Type_Conversion            =>
                     return Conversion_Dims (Self, C);

                  when Array_Index                =>
                     return Leaf_Dims (Self, C);

                  when Array_Slice | Family_Index =>
                     return (Kind => Not_Dimensioned, others => <>);

                  when Call                       =>
                     declare
                        Callee : constant Basic_Decl := Called_Decl (C);
                     begin
                        if Callee.Is_Null then
                           return Leaf_Dims (Self, C);
                        else
                           return Call_Dims (Self, C, Callee);
                        end if;
                     end;
               end case;
            end;

         when Ada_Qual_Expr                      =>
            declare
               Target : constant Subtype_Info := Type_Info (Self, E.As_Qual_Expr.F_Prefix.P_Name_Designated_Type);
            begin
               --  As in GNAT, a qualified expression has the dimensions of the qualifying subtype, whatever those of
               --  its operand (checked as an extra check).
               return To_Dims (Target);
            end;

         when Ada_Attribute_Ref                  =>
            return Attribute_Dims (Self, E.As_Attribute_Ref, E);

         when Ada_If_Expr | Ada_Case_Expr        =>
            return Branch_Dims (Self, Branches_Of (E));

         when Ada_Decl_Expr                      =>
            return Dims_Of (Self, E.As_Decl_Expr.F_Expr);

         when Ada_Raise_Expr                     =>
            return Unknown_Dims;

         when Ada_Target_Name                    =>
            declare
               P : Ada_Node := E.Parent;
            begin
               while not P.Is_Null and then P.Kind /= Ada_Assign_Stmt loop
                  P := P.Parent;
               end loop;

               if P.Is_Null then
                  return Unknown_Dims;
               else
                  return Without_Literal (Dims_Of (Self, P.As_Assign_Stmt.F_Dest));
               end if;
            end;

         when Ada_Identifier | Ada_Dotted_Name   =>
            return Name_Dims (Self, E.As_Name);

         when Ada_Membership_Expr
            | Ada_Quantified_Expr
            | Ada_String_Literal
            | Ada_Char_Literal
            | Ada_Null_Literal
            | Ada_Box_Expr
            | Ada_Base_Aggregate
            | Ada_Concat_Op                      =>
            return (Kind => Not_Dimensioned, others => <>);

         when others                             =>
            return Leaf_Dims (Self, E);
      end case;
   end Compute_Dims;

   ---------------
   -- Leaf_Dims --
   ---------------

   function Leaf_Dims (Self : in out Checker; E : Expr'Class) return Dim_Info is
      T : constant Base_Type_Decl := E.P_Expression_Type;
   begin
      if T.Is_Null then
         return Unknown_Dims;
      elsif Is_Universal (E, T) then
         return Wildcard_Zero;
      else
         return To_Dims (Type_Info (Self, T));
      end if;
   end Leaf_Dims;

   ---------------
   -- Name_Dims --
   ---------------

   function Name_Dims (Self : in out Checker; E : Name'Class) return Dim_Info is
      Decl : constant Basic_Decl := E.P_Referenced_Decl;
   begin
      if Decl.Is_Null then
         return Leaf_Dims (Self, E);
      end if;

      case Decl.Kind is
         when Ada_Number_Decl =>
            declare
               Value : constant Dim_Info := Dims_Of (Self, Decl.As_Number_Decl.F_Expr);
            begin
               if Value.Kind = Dimensioned then
                  return (Value with delta Literal => Context_Typed);
               else
                  return (Wildcard_Zero with delta Literal => Context_Typed);
               end if;
            end;

         when Ada_Object_Decl =>
            declare
               O : constant Object_Decl := Decl.As_Object_Decl;
            begin
               if O.F_Has_Constant and then O.F_Renaming_Clause.Is_Null then
                  declare
                     Info : constant Subtype_Info := Type_Expr_Info (Self, O.F_Type_Expr);
                  begin
                     if Info.Kind = Free
                       or else
                         (Info.Kind = Declared
                          and then Is_Dimensionless (Info.Dims)
                          and then not Self.Extras (Explicit_Dimensionless))
                     then
                        declare
                           Value : constant Dim_Info := Constant_Dims (Self, O);
                        begin
                           if Value.Kind = Dimensioned and then not Is_Dimensionless (Value.Dims) then
                              return Value;
                           end if;
                        end;
                     end if;
                  end;
               end if;

               return Leaf_Dims (Self, E);
            end;

         when others          =>
            return Leaf_Dims (Self, E);
      end case;
   end Name_Dims;

   -------------------
   -- Constant_Dims --
   -------------------

   function Constant_Dims (Self : in out Checker; Decl : Object_Decl'Class) return Dim_Info is
      Key : constant Ada_Node := Decl.As_Ada_Node;
   begin
      if Self.Constants.Contains (Key) then
         return Self.Constants (Key);
      end if;

      --  A constant whose value refers to itself is illegal, but must not loop here.
      Self.Constants.Insert (Key, Unknown_Dims);

      declare
         Value  : Expr := Decl.F_Default_Expr;
         Result : Dim_Info := Unknown_Dims;
      begin
         if Value.Is_Null then
            --  A deferred constant gets its value in its full declaration.
            declare
               Full : constant Basic_Decl := Decl.P_Next_Part_For_Decl;
            begin
               if not Full.Is_Null and then Full.Kind = Ada_Object_Decl then
                  Value := Full.As_Object_Decl.F_Default_Expr;
               end if;
            end;
         end if;

         if not Value.Is_Null then
            Result := Without_Literal (Dims_Of (Self, Value));
         end if;

         Self.Constants.Replace (Key, Result);
         return Result;
      end;
   end Constant_Dims;

   -------------------
   -- Operator_Decl --
   -------------------

   function Operator_Decl (Operator : Op'Class) return Basic_Decl is
      Decl : constant Basic_Decl := Operator.P_Referenced_Decl;
   begin
      if Decl.Is_Null or else Decl.Kind = Ada_Synthetic_Subp_Decl or else Decl.P_Is_Predefined_Operator then
         return No_Basic_Decl;
      else
         return Decl;
      end if;
   end Operator_Decl;

   -----------------
   -- Called_Decl --
   -----------------

   function Called_Decl (Call : Call_Expr'Class) return Basic_Decl is
      Decl : constant Basic_Decl := Call.F_Name.P_Referenced_Decl;
   begin
      if Decl.Is_Null or else Decl.Kind = Ada_Synthetic_Subp_Decl or else Decl.P_Subp_Spec_Or_Null.Is_Null then
         return No_Basic_Decl;
      else
         return Decl;
      end if;
   end Called_Decl;

   -----------------
   -- Access_Spec --
   -----------------

   function Access_Spec (Call : Call_Expr'Class) return Base_Subp_Spec is
      Called : constant Base_Formal_Param_Holder := Call.F_Name.P_Called_Subp_Spec;
   begin
      if not Called.Is_Null and then Called.Kind = Ada_Subp_Spec then
         return Called.As_Base_Subp_Spec;
      end if;

      --  Libadalang gives no profile for a call such as F (X) of an object F of an access-to-subprogram type.
      declare
         Object      : constant Basic_Decl := Call.F_Name.P_Referenced_Decl;
         Object_Type : Type_Expr := No_Type_Expr;
         Decl        : Base_Type_Decl;
      begin
         if Object.Is_Null then
            return No_Base_Subp_Spec;
         end if;

         case Object.Kind is
            when Ada_Object_Decl    =>
               Object_Type := Object.As_Object_Decl.F_Type_Expr;

            when Ada_Param_Spec     =>
               Object_Type := Object.As_Param_Spec.F_Type_Expr;

            when Ada_Component_Decl =>
               Object_Type := Object.As_Component_Decl.F_Component_Def.F_Type_Expr;

            when others             =>
               return No_Base_Subp_Spec;
         end case;

         if Object_Type.Is_Null then
            return No_Base_Subp_Spec;
         end if;

         Decl := Object_Type.P_Designated_Type_Decl;
         if not Decl.Is_Null
           and then Decl.Kind in Ada_Type_Decl
           and then Decl.As_Type_Decl.F_Type_Def.Kind = Ada_Access_To_Subp_Def
         then
            return Decl.As_Type_Decl.F_Type_Def.As_Access_To_Subp_Def.F_Subp_Spec.As_Base_Subp_Spec;
         else
            return No_Base_Subp_Spec;
         end if;
      end;
   end Access_Spec;

   ----------------------
   -- Called_Attribute --
   ----------------------

   function Called_Attribute (Call : Call_Expr'Class) return Attribute_Ref is
      Callee : constant Name := Call.F_Name;
   begin
      if not Callee.Is_Null and then Callee.Kind = Ada_Attribute_Ref then
         return Callee.As_Attribute_Ref;
      else
         return No_Attribute_Ref;
      end if;
   end Called_Attribute;

   ----------------
   -- Special_Of --
   ----------------

   function Special_Of (Self : in out Checker; Callee : Basic_Decl'Class) return Special_Info is
      Key : constant Ada_Node := Callee.As_Ada_Node;

      function In_Elementary_Functions (Decl : Basic_Decl'Class) return Boolean is
         Unit_Name : Unbounded_String;
      begin
         for Part of Decl.P_Enclosing_Compilation_Unit.P_Syntactic_Fully_Qualified_Name loop
            Append
              (Unit_Name,
               (if Length (Unit_Name) = 0 then "" else ".")
               & To_Lower (UTF8 (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Part))));
         end loop;
         return To_String (Unit_Name) = "ada.numerics.generic_elementary_functions";
      end In_Elementary_Functions;

      function Compute return Special_Info is
         Decl : Basic_Decl := Callee.As_Basic_Decl;
      begin
         for Step in 1 .. 16 loop
            if Annotation_Arguments (Decl, "power")'Length > 0 then
               return (Kind => Annotated_Power, Degree => One);
            end if;

            declare
               Root_Args : constant Actual_List := Annotation_Arguments (Decl, "root");
            begin
               if Root_Args'Length > 0 then
                  if Root_Args'Length = 2 then
                     declare
                        Degree : constant Optional_Rational :=
                          Static_Value (Root_Args (Root_Args'Last), Integer_Mode => True);
                     begin
                        if Degree.Present and then Numerator (Degree.Value) > 0 then
                           return (Kind => Annotated_Root, Degree => Degree.Value);
                        end if;
                     end;
                  end if;

                  Report
                    (Self,
                     Root_Args (Root_Args'First),
                     Error,
                     "a Root annotation takes one positive static integer, such as (Dimension_Check, Root, 3)");
                  return (Kind => Ordinary, Degree => One);
               end if;
            end;

            if In_Elementary_Functions (Decl) then
               declare
                  Name : constant String := To_Lower (UTF8 (Decl.P_Defining_Name.Text));
               begin
                  if Name = "sqrt" then
                     return (Kind => Elementary_Sqrt, Degree => To_Rational (2));
                  elsif Name = """**""" then
                     return (Kind => Elementary_Power, Degree => One);
                  else
                     return (Kind => Elementary_Other, Degree => One);
                  end if;
               end;
            end if;

            exit when Decl.Kind /= Ada_Subp_Renaming_Decl;

            declare
               Renamed : constant Basic_Decl :=
                 Decl.As_Subp_Renaming_Decl.F_Renames.F_Renamed_Object.P_Referenced_Decl;
            begin
               exit when Renamed.Is_Null;
               Decl := Renamed;
            end;
         end loop;

         return (Kind => Ordinary, Degree => One);
      end Compute;

   begin
      if Callee.Is_Null then
         return (Kind => Ordinary, Degree => One);
      elsif Self.Specials.Contains (Key) then
         return Self.Specials (Key);
      end if;

      declare
         Result : Special_Info;
      begin
         begin
            Result := Compute;
         exception
            when Property_Error =>
               Result := (Kind => Ordinary, Degree => One);
         end;
         Self.Specials.Include (Key, Result);
         return Result;
      end;
   end Special_Of;

   -------------------
   -- Parameters_Of --
   -------------------

   function Parameters_Of (Call : Ada_Node'Class; Spec : Base_Subp_Spec'Class) return Parameter_List is
      Count : Natural := 0;
   begin
      if Spec.Is_Null then
         return [1 .. 0 => <>];
      end if;

      for P of Spec.P_Params loop
         Count := Count + P.F_Ids.Children_Count;
      end loop;

      return Result : Parameter_List (1 .. Count) do
         Count := 0;
         for P of Spec.P_Params loop
            for Id of P.F_Ids loop
               Count := Count + 1;
               Result (Count) := (Formal => Id.As_Defining_Name, Actual => No_Expr);
            end loop;
         end loop;

         case Call.Kind is
            when Ada_Bin_Op | Ada_Relation_Op =>
               if Result'Length >= 1 then
                  Result (1).Actual := Call.As_Bin_Op.F_Left;
               end if;
               if Result'Length >= 2 then
                  Result (2).Actual := Call.As_Bin_Op.F_Right;
               end if;

            when Ada_Un_Op                    =>
               if Result'Length >= 1 then
                  Result (1).Actual := Call.As_Un_Op.F_Expr;
               end if;

            when Ada_Call_Expr                =>
               declare
                  Suffix : constant Ada_Node := Call.As_Call_Expr.F_Suffix;
               begin
                  if not Suffix.Is_Null and then Suffix.Kind = Ada_Assoc_List then
                     declare
                        Matched : Boolean := False;
                        Next    : Positive := 1;
                     begin
                        for Pair of Suffix.As_Assoc_List.P_Zip_With_Params loop
                           for I in Result'Range loop
                              if Same_Node (Result (I).Formal, Param (Pair)) then
                                 Result (I).Actual := Actual (Pair).As_Expr;
                                 Matched := True;
                              end if;
                           end loop;
                        end loop;

                        --  Libadalang does not match the actuals of some calls, such as F (X) for an access to
                        --  subprogram F. Match them by position and name here.
                        if not Matched then
                           if Call.As_Call_Expr.F_Name.P_Is_Dot_Call then
                              Next := 2;
                           end if;

                           for Assoc of Suffix.As_Assoc_List loop
                              if Assoc.Kind = Ada_Param_Assoc then
                                 if Assoc.As_Param_Assoc.F_Designator.Is_Null then
                                    if Next <= Result'Last then
                                       Result (Next).Actual := Assoc.As_Param_Assoc.F_R_Expr;
                                    end if;
                                    Next := Next + 1;
                                 else
                                    for I in Result'Range loop
                                       if Lower_Text (Result (I).Formal)
                                         = Lower_Text (Assoc.As_Param_Assoc.F_Designator)
                                       then
                                          Result (I).Actual := Assoc.As_Param_Assoc.F_R_Expr;
                                       end if;
                                    end loop;
                                 end if;
                              end if;
                           end loop;
                        end if;
                     end;
                  end if;
               end;

            when others                       =>
               null;
         end case;
      end return;
   end Parameters_Of;

   ---------------------
   -- Delta_Base_Type --
   ---------------------

   function Delta_Base_Type (E : Expr'Class) return Base_Type_Decl is
      Inner : constant Expr := Without_Parens (E);
   begin
      if not Inner.Is_Null
        and then Inner.Kind in Ada_Delta_Aggregate | Ada_Bracket_Delta_Aggregate
        and then not Inner.As_Base_Aggregate.F_Ancestor_Expr.Is_Null
      then
         return Inner.As_Base_Aggregate.F_Ancestor_Expr.P_Expression_Type;
      else
         return No_Base_Type_Decl;
      end if;
   end Delta_Base_Type;

   -----------------
   -- Misresolved --
   -----------------

   function Misresolved (Call : Ada_Node'Class; Spec : Base_Subp_Spec'Class) return Boolean is
   begin
      for P of Parameters_Of (Call, Spec) loop
         if not P.Actual.Is_Null then
            declare
               Actual_Type : constant Base_Type_Decl := Delta_Base_Type (P.Actual);
               Formal_Decl : constant Basic_Decl := P.Formal.P_Basic_Decl;
            begin
               if not Actual_Type.Is_Null
                 and then not Formal_Decl.Is_Null
                 and then Formal_Decl.Kind = Ada_Param_Spec
                 and then
                   Actual_Type.P_Base_Subtype.As_Ada_Node
                   /= Formal_Decl.As_Param_Spec.F_Type_Expr.P_Designated_Type_Decl.P_Base_Subtype.As_Ada_Node
               then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Misresolved;

   -----------------
   -- Callee_Name --
   -----------------

   function Callee_Name (Callee : Basic_Decl'Class) return String
   is (UTF8 (Callee.P_Defining_Name.Text));

   ---------------
   -- Call_Dims --
   ---------------

   function Call_Dims (Self : in out Checker; Call : Expr'Class; Callee : Basic_Decl'Class) return Dim_Info is
      Special : constant Special_Info := Special_Of (Self, Callee);
   begin
      if Misresolved (Call, Callee.P_Subp_Spec_Or_Null) then
         return Unknown_Dims;
      end if;

      case Special.Kind is
         when Ordinary | Elementary_Other        =>
            return Leaf_Dims (Self, Call);

         when Elementary_Sqrt | Annotated_Root   =>
            declare
               Parameters : constant Parameter_List := Parameters_Of (Call, Callee.P_Subp_Spec_Or_Null);
            begin
               if Parameters'Length < 1 or else Parameters (1).Actual.Is_Null then
                  return Unknown_Dims;
               end if;

               declare
                  Argument : constant Dim_Info := Dims_Of (Self, Parameters (1).Actual);
               begin
                  if Argument.Kind /= Dimensioned then
                     return Argument;
                  else
                     return (Argument with delta Dims => Argument.Dims / Special.Degree, Literal => Not_Literal);
                  end if;
               end;
            end;

         when Elementary_Power | Annotated_Power =>
            declare
               Parameters : constant Parameter_List := Parameters_Of (Call, Callee.P_Subp_Spec_Or_Null);
            begin
               if Parameters'Length < 2 or else Parameters (1).Actual.Is_Null or else Parameters (2).Actual.Is_Null
               then
                  return Unknown_Dims;
               end if;

               return Power_Dims (Dims_Of (Self, Parameters (1).Actual), Parameters (2).Actual, Integer_Mode => False);
            end;
      end case;
   end Call_Dims;

   ----------------
   -- Power_Dims --
   ----------------

   function Power_Dims (Base : Dim_Info; Exponent : Expr'Class; Integer_Mode : Boolean) return Dim_Info is
   begin
      if Base.Kind /= Dimensioned then
         return Base;
      elsif Integer_Mode and then Is_Integer_Division (Exponent) then
         return Unknown_Dims;
      elsif Is_Dimensionless (Base.Dims) then
         return Without_Literal (Base);
      end if;

      declare
         Value : constant Optional_Rational := Static_Value (Exponent, Integer_Mode);
      begin
         if not Value.Present then
            return Unknown_Dims;
         end if;

         return (Base with delta Dims => Base.Dims * Value.Value, Literal => Not_Literal);
      exception
         when Constraint_Error =>
            return Unknown_Dims;
      end;
   end Power_Dims;

   -------------
   -- Combine --
   -------------

   function Combine (Left, Right : Dim_Info; Dims : Vector) return Dim_Info is
   begin
      if Left.Kind = Unknown or else Right.Kind = Unknown then
         return Unknown_Dims;
      elsif Left.Kind = Not_Dimensioned and then Right.Kind = Not_Dimensioned then
         return (Kind => Not_Dimensioned, others => <>);
      elsif Left.Kind = Not_Dimensioned or else Right.Kind = Not_Dimensioned then
         --  Operands of one predefined operator have one type, so this is a value that Libadalang typed differently
         --  from its context (such as a universal attribute). Do not guess.
         return Unknown_Dims;
      else
         return
           (Kind    => Dimensioned,
            Dims    => Dims,
            System  => (if Left.System /= No_System then Left.System else Right.System),
            Literal => Not_Literal);
      end if;
   end Combine;

   ------------------------
   -- Conversion_Operand --
   ------------------------

   function Conversion_Operand (Conversion : Call_Expr'Class) return Expr is
      Suffix : constant Ada_Node := Conversion.F_Suffix;
   begin
      if not Suffix.Is_Null and then Suffix.Kind = Ada_Assoc_List then
         for Assoc of Suffix.As_Assoc_List loop
            if Assoc.Kind = Ada_Param_Assoc then
               return Assoc.As_Param_Assoc.F_R_Expr;
            end if;
         end loop;
      end if;
      return No_Expr;
   end Conversion_Operand;

   ---------------------
   -- Conversion_Dims --
   ---------------------

   function Conversion_Dims (Self : in out Checker; Conversion : Call_Expr'Class) return Dim_Info is
      Target  : constant Subtype_Info := Type_Info (Self, Conversion.F_Name.P_Name_Designated_Type);
      Operand : constant Expr := Conversion_Operand (Conversion);
   begin
      if Target.Kind = Not_In_System then
         return (Kind => Not_Dimensioned, others => <>);
      elsif Operand.Is_Null then
         return To_Dims (Target);
      end if;

      declare
         Operand_Type : constant Base_Type_Decl := Operand.P_Expression_Type;
      begin
         if Operand_Type.Is_Null then
            return Unknown_Dims;
         elsif Is_Universal (Operand, Operand_Type) or else Type_Info (Self, Operand_Type).Kind = Not_In_System then
            --  A conversion from a type outside the system gives the dimensions of the target.
            return To_Dims (Target);
         end if;
      end;

      declare
         Value : constant Dim_Info := Dims_Of (Self, Operand);
      begin
         if Value.Kind = Unknown then
            return Unknown_Dims;
         elsif Value.Kind = Dimensioned and then not Is_Dimensionless (Value.Dims) then
            return Without_Literal (Value);
         else
            return To_Dims (Target);
         end if;
      end;
   end Conversion_Dims;

   --------------------
   -- Attribute_Dims --
   --------------------

   function Attribute_Dims (Self : in out Checker; Attribute : Attribute_Ref'Class; Whole : Expr'Class) return Dim_Info
   is
      Attr   : constant String := Lower_Text (Attribute.F_Attribute);
      Prefix : constant Name := Attribute.F_Prefix;
   begin
      if Attr in "old" | "loop_entry" then
         return Without_Literal (Dims_Of (Self, Prefix));
      elsif Attr = "result" then
         return Leaf_Dims (Self, Whole);
      end if;

      declare
         Result_Type : constant Base_Type_Decl := Whole.P_Expression_Type;
         Result_Info : Subtype_Info;
      begin
         if Result_Type.Is_Null then
            return Unknown_Dims;
         elsif Result_Type.As_Ada_Node = Whole.P_Universal_Int_Type.As_Ada_Node then
            return (Kind => Not_Dimensioned, others => <>);
         elsif Result_Type.As_Ada_Node = Whole.P_Universal_Real_Type.As_Ada_Node then
            --  GNAT gives a universal real attribute such as Length'Small or Length'Model_Epsilon the type of its
            --  context, and so the dimensions of its prefix below.
            Result_Info := (Kind => Free, others => <>);
         else
            Result_Info := Type_Info (Self, Result_Type);
         end if;

         if Result_Info.Kind = Not_In_System then
            return (Kind => Not_Dimensioned, others => <>);
         elsif Result_Info.Kind = Declared and then not Is_Dimensionless (Result_Info.Dims) then
            return To_Dims (Result_Info);
         end if;

         --  As in GNAT, an attribute of a dimension system type that has no dimensions of its own takes those of
         --  its prefix, such as Length'Max (A, B) or X'Old.
         declare
            Prefix_Decl : constant Basic_Decl := Prefix.P_Referenced_Decl;
         begin
            if not Prefix_Decl.Is_Null and then Prefix_Decl.Kind in Ada_Base_Type_Decl then
               return To_Dims ((Type_Info (Self, Prefix_Decl.As_Base_Type_Decl) with delta Is_Root => False));
            else
               return Without_Literal (Dims_Of (Self, Prefix));
            end if;
         end;
      end;
   end Attribute_Dims;

   -----------------
   -- Branches_Of --
   -----------------

   function Branches_Of (E : Expr'Class) return Actual_List is
   begin
      if E.Kind = Ada_If_Expr then
         declare
            I     : constant If_Expr := E.As_If_Expr;
            Count : Positive := 1 + I.F_Alternatives.Children_Count + (if I.F_Else_Expr.Is_Null then 0 else 1);
         begin
            return Result : Actual_List (1 .. Count) do
               Result (1) := I.F_Then_Expr;
               Count := 1;
               for Alternative of I.F_Alternatives loop
                  Count := Count + 1;
                  Result (Count) := Alternative.As_Elsif_Expr_Part.F_Then_Expr;
               end loop;
               if not I.F_Else_Expr.Is_Null then
                  Result (Result'Last) := I.F_Else_Expr;
               end if;
            end return;
         end;
      elsif E.Kind = Ada_Case_Expr then
         declare
            Cases : constant Case_Expr_Alternative_List := E.As_Case_Expr.F_Cases;
            Count : Natural := 0;
         begin
            return Result : Actual_List (1 .. Cases.Children_Count) do
               for Alternative of Cases loop
                  Count := Count + 1;
                  Result (Count) := Alternative.As_Case_Expr_Alternative.F_Expr;
               end loop;
            end return;
         end;
      else
         return No_Actuals;
      end if;
   end Branches_Of;

   -----------------
   -- Branch_Dims --
   -----------------

   function Branch_Dims (Self : in out Checker; Branches : Actual_List) return Dim_Info is
      Result : Dim_Info;
   begin
      if Branches'Length = 0 then
         return Unknown_Dims;
      end if;

      Result := Dims_Of (Self, Branches (Branches'First));

      --  An if expression without an else part is Boolean.
      if Branches'Length = 1
        and then Branches (Branches'First).Parent.Kind = Ada_If_Expr
        and then Branches (Branches'First).Parent.As_If_Expr.F_Else_Expr.Is_Null
      then
         return (Kind => Not_Dimensioned, others => <>);
      end if;

      for I in Branches'First + 1 .. Branches'Last loop
         declare
            Other : constant Dim_Info := Dims_Of (Self, Branches (I));
         begin
            if Other.Kind = Unknown then
               null;
            elsif Result.Kind = Unknown then
               Result := Without_Literal (Other);
            elsif Other.Kind /= Result.Kind or else Other.Dims /= Result.Dims then
               return Unknown_Dims;
            elsif Other.Literal = Not_Literal then
               Result.Literal := Not_Literal;
            end if;
         end;
      end loop;

      return (Result with delta Literal => (if Result.Literal = Not_Literal then Not_Literal else Context_Typed));
   end Branch_Dims;

   -------------------------------
   -- Enclosing_Function_Result --
   -------------------------------

   function Enclosing_Function_Result (Node : Ada_Node'Class) return Type_Expr is
      P : Ada_Node := Node.Parent;
   begin
      while not P.Is_Null loop
         case P.Kind is
            when Ada_Subp_Body | Ada_Expr_Function                                                                =>
               return P.As_Base_Subp_Body.F_Subp_Spec.F_Subp_Returns;

            when Ada_Entry_Body | Ada_Accept_Stmt | Ada_Accept_Stmt_With_Stmts | Ada_Task_Body | Ada_Package_Body =>
               return No_Type_Expr;

            when others                                                                                           =>
               P := P.Parent;
         end case;
      end loop;
      return No_Type_Expr;
   end Enclosing_Function_Result;

   -----------------
   -- Check_Value --
   -----------------

   procedure Check_Value
     (Self              : in out Checker;
      Value             : Expr'Class;
      Target            : Subtype_Info;
      Context           : String;
      Lenient_Literals  : Boolean;
      Allow_Propagation : Boolean := False;
      Report_Errors     : Boolean := True;
      Context_Typed_OK  : Boolean := False)
   is
      Expected : constant Dim_Info := To_Dims (Target);
      Actual   : Dim_Info;
   begin
      if Target.Kind = Not_In_System or else Value.Is_Null then
         return;
      end if;

      Actual := Dims_Of (Self, Value);
      if Same (Actual, Expected) then
         return;
      elsif Lenient_Literals and then Actual.Literal in Zero_Literal | Nonzero_Literal then
         if Actual.Literal = Nonzero_Literal then
            Report
              (Self,
               Value,
               Warning,
               "numeric literal "
               & Text_Of (Without_Parens (Value))
               & " is assumed to have dimension "
               & Image (Self, Expected));
         end if;
      elsif Context_Typed_OK and then Actual.Literal = Context_Typed then
         null;
      elsif Allow_Propagation or else not Report_Errors then
         null;
      else
         Report
           (Self,
            Value,
            Error,
            Context & ": expected dimension " & Image (Self, Expected) & ", found " & Image (Self, Actual));
      end if;
   end Check_Value;

   ------------------
   -- Check_Bounds --
   ------------------

   procedure Check_Bounds (Self : in out Checker; Range_Expression : Expr'Class; Target : Subtype_Info) is
      R : constant Expr := Without_Parens (Range_Expression);
   begin
      if not R.Is_Null and then R.Kind = Ada_Bin_Op and then R.As_Bin_Op.F_Op.Kind = Ada_Op_Double_Dot then
         Check_Value (Self, R.As_Bin_Op.F_Left, Target, "dimensions mismatch in range", Lenient_Literals => True);
         Check_Value (Self, R.As_Bin_Op.F_Right, Target, "dimensions mismatch in range", Lenient_Literals => True);
      end if;
   end Check_Bounds;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (Self : in out Checker; Type_Expression : Type_Expr'Class; Target : Subtype_Info) is
   begin
      if not Type_Expression.Is_Null
        and then Type_Expression.Kind in Ada_Subtype_Indication_Range
        and then not Type_Expression.As_Subtype_Indication.F_Constraint.Is_Null
        and then Type_Expression.As_Subtype_Indication.F_Constraint.Kind = Ada_Range_Constraint
      then
         Check_Bounds
           (Self, Type_Expression.As_Subtype_Indication.F_Constraint.As_Range_Constraint.F_Range.F_Range, Target);
      end if;
   end Check_Range;

   --------------------
   -- Check_Compared --
   --------------------

   procedure Check_Compared (Self : in out Checker; Left, Right : Expr'Class; Is_Error : out Boolean) is
      L : constant Dim_Info := Dims_Of (Self, Left);
      R : constant Dim_Info := Dims_Of (Self, Right);
   begin
      Is_Error := False;

      if Same (L, R) then
         return;
      elsif L.Literal in Zero_Literal | Nonzero_Literal then
         if L.Literal = Nonzero_Literal then
            Report
              (Self,
               Left,
               Warning,
               "numeric literal "
               & Text_Of (Without_Parens (Left))
               & " is assumed to have dimension "
               & Image (Self, R));
         end if;
      elsif R.Literal in Zero_Literal | Nonzero_Literal then
         if R.Literal = Nonzero_Literal then
            Report
              (Self,
               Right,
               Warning,
               "numeric literal "
               & Text_Of (Without_Parens (Right))
               & " is assumed to have dimension "
               & Image (Self, L));
         end if;
      else
         Is_Error := True;
      end if;
   end Check_Compared;

   ----------------
   -- Check_Call --
   ----------------

   procedure Check_Call
     (Self : in out Checker; Call : Ada_Node'Class; Callee : Basic_Decl'Class; Spec : Base_Subp_Spec'Class)
   is
      Special    : constant Special_Info := Special_Of (Self, Callee);
      Parameters : constant Parameter_List := Parameters_Of (Call, Spec);
      Name_Text  : constant String :=
        (if not Callee.Is_Null
         then Callee_Name (Callee)
         elsif Call.Kind = Ada_Call_Expr
         then Text_Of (Call.As_Call_Expr.F_Name)
         else Text_Of (Call));
   begin
      if Misresolved (Call, Spec) then
         Note_Unresolved (Self, Call, "the call: the formal of " & Name_Text & " has another type");
         return;
      end if;

      case Special.Kind is
         when Elementary_Other                   =>
            for P of Parameters loop
               if not P.Actual.Is_Null then
                  declare
                     Value : constant Dim_Info := Dims_Of (Self, P.Actual);
                  begin
                     if Value.Kind = Dimensioned and then not Is_Dimensionless (Value.Dims) then
                        Report
                          (Self,
                           P.Actual,
                           Error,
                           "dimensions mismatch in call of "
                           & Name_Text
                           & ": expected dimension [], found "
                           & Image (Self, Value));
                     end if;
                  end;
               end if;
            end loop;

         when Elementary_Power | Annotated_Power =>
            if Parameters'Length >= 2
              and then not Parameters (1).Actual.Is_Null
              and then not Parameters (2).Actual.Is_Null
            then
               declare
                  Base     : constant Dim_Info := Dims_Of (Self, Parameters (1).Actual);
                  Exponent : constant Dim_Info := Dims_Of (Self, Parameters (2).Actual);
               begin
                  if Exponent.Kind = Dimensioned and then not Is_Dimensionless (Exponent.Dims) then
                     Report
                       (Self,
                        Parameters (2).Actual,
                        Error,
                        "the exponent must be dimensionless, found " & Image (Self, Exponent));
                  elsif Base.Kind = Dimensioned
                    and then not Is_Dimensionless (Base.Dims)
                    and then not Static_Value (Parameters (2).Actual, Integer_Mode => False).Present
                  then
                     Report
                       (Self,
                        Parameters (2).Actual,
                        Error,
                        "the exponent of an operand with dimension "
                        & Image (Self, Base)
                        & " must be a static expression of literals and named numbers");
                  end if;
               end;
            end if;

         when Elementary_Sqrt | Annotated_Root   =>
            null;

         when Ordinary                           =>
            for P of Parameters loop
               if not P.Actual.Is_Null then
                  declare
                     Formal_Decl : constant Basic_Decl := P.Formal.P_Basic_Decl;
                     Formal      : Subtype_Info;
                  begin
                     if not Formal_Decl.Is_Null and then Formal_Decl.Kind = Ada_Param_Spec then
                        Formal := Type_Expr_Info (Self, Formal_Decl.As_Param_Spec.F_Type_Expr);
                        if Formal.Kind = Declared
                          and then (not Is_Dimensionless (Formal.Dims) or else Self.Extras (Explicit_Dimensionless))
                        then
                           declare
                              Value : constant Dim_Info := Dims_Of (Self, P.Actual);
                           begin
                              if not Same (Value, To_Dims (Formal)) then
                                 Report
                                   (Self,
                                    P.Actual,
                                    Error,
                                    "dimensions mismatch in call of "
                                    & Name_Text
                                    & ": expected dimension "
                                    & Image (Self, Formal)
                                    & ", found "
                                    & Image (Self, Value));
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
      end case;
   end Check_Call;

   -------------------------------
   -- Check_Predefined_Operator --
   -------------------------------

   procedure Check_Predefined_Operator (Self : in out Checker; Operation : Bin_Op'Class) is
      Operator : constant Op := Operation.F_Op;

      procedure Mismatch is
      begin
         Report
           (Self,
            Operator,
            Error,
            "both operands for operation """
            & Text_Of (Operator)
            & """ must have same dimensions: left operand has dimension "
            & Image (Self, Dims_Of (Self, Operation.F_Left))
            & ", right operand has dimension "
            & Image (Self, Dims_Of (Self, Operation.F_Right)));
      end Mismatch;

   begin
      case Operator.Kind is
         when Ada_Op_Plus | Ada_Op_Minus | Ada_Op_Mod | Ada_Op_Rem                     =>
            if not Same (Dims_Of (Self, Operation.F_Left), Dims_Of (Self, Operation.F_Right)) then
               Mismatch;
            end if;

         when Ada_Op_Eq | Ada_Op_Neq | Ada_Op_Lt | Ada_Op_Lte | Ada_Op_Gt | Ada_Op_Gte =>
            declare
               Is_Error : Boolean;
            begin
               Check_Compared (Self, Operation.F_Left, Operation.F_Right, Is_Error);
               if Is_Error then
                  Mismatch;
               end if;
            end;

         when Ada_Op_Pow                                                               =>
            declare
               Base : constant Dim_Info := Dims_Of (Self, Operation.F_Left);
            begin
               if Base.Kind /= Dimensioned then
                  null;
               elsif Is_Integer_Division (Operation.F_Right) then
                  declare
                     Value : constant Optional_Rational := Static_Value (Operation.F_Right, Integer_Mode => True);
                  begin
                     Report
                       (Self,
                        Operation.F_Right,
                        Error,
                        "the exponent "
                        & Text_Of (Operation.F_Right)
                        & " is an integer division"
                        & (if Value.Present then " with value " & Rationals.Image (Value.Value) else "")
                        & "; write a real exponent such as 0.5 or (1.0 / 3.0)");
                  end;
               elsif not Is_Dimensionless (Base.Dims)
                 and then not Static_Value (Operation.F_Right, Integer_Mode => True).Present
               then
                  Report
                    (Self,
                     Operation.F_Right,
                     Error,
                     "the exponent of an operand with dimension "
                     & Image (Self, Base)
                     & " must be a static expression of literals and named numbers");
               end if;
            end;

         when others                                                                   =>
            null;
      end case;
   end Check_Predefined_Operator;

   ----------------------
   -- Check_Conversion --
   ----------------------

   procedure Check_Conversion (Self : in out Checker; Conversion : Call_Expr'Class) is
      Target  : constant Subtype_Info := Type_Info (Self, Conversion.F_Name.P_Name_Designated_Type);
      Operand : constant Expr := Conversion_Operand (Conversion);
   begin
      if Target.Kind = Not_In_System or else Target.Is_Root or else Operand.Is_Null then
         return;
      end if;

      declare
         Operand_Type : constant Base_Type_Decl := Operand.P_Expression_Type;
      begin
         if Operand_Type.Is_Null
           or else Is_Universal (Operand, Operand_Type)
           or else Type_Info (Self, Operand_Type).Kind = Not_In_System
         then
            return;
         end if;
      end;

      declare
         Value : constant Dim_Info := Dims_Of (Self, Operand);
      begin
         if not Same (Value, To_Dims (Target)) then
            Report
              (Self,
               Conversion,
               Error,
               "dimensions mismatch in conversion: expression has dimension "
               & Image (Self, Value)
               & ", target subtype has dimension "
               & Image (Self, Target));
         end if;
      end;
   end Check_Conversion;

   --------------------
   -- Check_Branches --
   --------------------

   procedure Check_Branches (Self : in out Checker; E : Expr'Class) is
      Branches : constant Actual_List := Branches_Of (E);
      First    : Dim_Info;
   begin
      if Branches'Length < 2 then
         return;
      end if;

      First := Dims_Of (Self, Branches (Branches'First));
      for I in Branches'First + 1 .. Branches'Last loop
         declare
            Other : constant Dim_Info := Dims_Of (Self, Branches (I));
         begin
            if First.Kind = Unknown then
               First := Other;
            elsif not Same (First, Other) then
               Report
                 (Self,
                  Branches (I),
                  Error,
                  "dimensions mismatch in conditional expression: expected dimension "
                  & Image (Self, First)
                  & ", found "
                  & Image (Self, Other));
               return;
            end if;
         end;
      end loop;
   end Check_Branches;

   ---------------------
   -- Array_Component --
   ---------------------

   function Array_Component (Self : in out Checker; Decl : Base_Type_Decl'Class) return Subtype_Info is
      D : Base_Type_Decl := Decl.As_Base_Type_Decl;
   begin
      for Step in 1 .. 16 loop
         exit when D.Is_Null;

         if D.Kind = Ada_Subtype_Decl then
            D := D.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl;
         elsif D.Kind in Ada_Type_Decl then
            declare
               Def : constant Type_Def := D.As_Type_Decl.F_Type_Def;
            begin
               if Def.Kind = Ada_Array_Type_Def then
                  return Type_Expr_Info (Self, Def.As_Array_Type_Def.F_Component_Type.F_Type_Expr);
               elsif Def.Kind = Ada_Derived_Type_Def then
                  D := Def.As_Derived_Type_Def.F_Subtype_Indication.P_Designated_Type_Decl;
               else
                  exit;
               end if;
            end;
         else
            exit;
         end if;
      end loop;

      return (others => <>);
   end Array_Component;

   ----------------------------
   -- Check_Array_Components --
   ----------------------------

   procedure Check_Array_Components (Self : in out Checker; Assocs : Assoc_List'Class; Component : Subtype_Info) is
   begin
      for Assoc of Assocs loop
         declare
            Value : Expr := No_Expr;
         begin
            if Assoc.Kind = Ada_Aggregate_Assoc then
               Value := Assoc.As_Aggregate_Assoc.F_R_Expr;
            elsif Assoc.Kind = Ada_Iterated_Assoc then
               Value := Assoc.As_Iterated_Assoc.F_R_Expr;
            end if;

            if Value.Is_Null or else Value.Kind = Ada_Box_Expr then
               null;
            elsif Value.Kind in Ada_Aggregate | Ada_Bracket_Aggregate then
               --  A subaggregate of a multidimensional array.
               Check_Array_Components (Self, Value.As_Base_Aggregate.F_Assocs, Component);
            else
               Check_Value
                 (Self, Value, Component, "dimensions mismatch in array aggregate", Lenient_Literals => False);
            end if;
         end;
      end loop;
   end Check_Array_Components;

   ---------------------
   -- Check_Aggregate --
   ---------------------

   procedure Check_Aggregate (Self : in out Checker; Aggregate : Base_Aggregate'Class) is
      Is_Delta : constant Boolean := not Delta_Base_Type (Aggregate).Is_Null;
      T        : constant Base_Type_Decl :=
        (if Is_Delta then Delta_Base_Type (Aggregate) else Aggregate.P_Expression_Type);
   begin
      if T.Is_Null
        or else Aggregate.Kind = Ada_Null_Record_Aggregate
        or else (Is_Delta and then not Self.Extras (Delta_Aggregates))
      then
         return;
      end if;

      if T.P_Is_Array_Type then
         declare
            Component : constant Subtype_Info := Array_Component (Self, T);
         begin
            if Component.Kind /= Not_In_System then
               Check_Array_Components (Self, Aggregate.F_Assocs, Component);
            end if;
         end;
      elsif T.P_Is_Record_Type and then Aggregate.Kind /= Ada_Bracket_Aggregate then
         for Pair of Aggregate.P_Aggregate_Params loop
            declare
               Value : constant Expr := Actual (Pair).As_Expr;
               Decl  : constant Basic_Decl := Param (Pair).P_Basic_Decl;
               Info  : Subtype_Info;
            begin
               if Value.Is_Null or else Value.Kind = Ada_Box_Expr or else Decl.Is_Null then
                  null;
               else
                  if Decl.Kind = Ada_Component_Decl then
                     Info := Type_Expr_Info (Self, Decl.As_Component_Decl.F_Component_Def.F_Type_Expr);
                  elsif Decl.Kind = Ada_Discriminant_Spec then
                     Info := Type_Expr_Info (Self, Decl.As_Discriminant_Spec.F_Type_Expr);
                  end if;
                  Check_Value
                    (Self, Value, Info, "dimensions mismatch in record aggregate", Lenient_Literals => False);
               end if;
            end;
         end loop;
      end if;
   end Check_Aggregate;

   -------------------------------
   -- Check_Attribute_Arguments --
   -------------------------------

   procedure Check_Attribute_Arguments
     (Self : in out Checker; Attribute : Attribute_Ref'Class; Args : Assoc_List'Class)
   is
      Name        : constant String := Lower_Text (Attribute.F_Attribute);
      All_Values  : constant Boolean := Name in "min" | "max" | "adjacent" | "remainder";
      First_Value : constant Boolean :=
        Name
        in "ceiling"
         | "floor"
         | "rounding"
         | "unbiased_rounding"
         | "machine_rounding"
         | "truncation"
         | "machine"
         | "model"
         | "pred"
         | "succ"
         | "fraction"
         | "scaling"
         | "leading_part"
         | "compose"
         | "copy_sign";
   begin
      if (not All_Values and then not First_Value) or else Args.Is_Null then
         return;
      end if;

      declare
         Prefix_Decl : constant Basic_Decl := Attribute.F_Prefix.P_Referenced_Decl;
         Target      : Subtype_Info;
         Position    : Natural := 0;
      begin
         if Prefix_Decl.Is_Null or else Prefix_Decl.Kind not in Ada_Base_Type_Decl then
            return;
         end if;

         Target := Type_Info (Self, Prefix_Decl.As_Base_Type_Decl);
         if Target.Kind /= Declared
           or else (Is_Dimensionless (Target.Dims) and then not Self.Extras (Explicit_Dimensionless))
         then
            return;
         end if;

         for Assoc of Args loop
            Position := Position + 1;
            if Assoc.Kind = Ada_Param_Assoc and then (All_Values or else Position = 1) then
               Check_Value
                 (Self,
                  Assoc.As_Param_Assoc.F_R_Expr,
                  Target,
                  "dimensions mismatch in attribute " & Text_Of (Attribute.F_Attribute),
                  Lenient_Literals => True);
            end if;
         end loop;
      end;
   end Check_Attribute_Arguments;

   ----------------------
   -- Check_Membership --
   ----------------------

   procedure Check_Membership (Self : in out Checker; Membership : Membership_Expr'Class) is
      Tested : constant Expr := Membership.F_Expr;

      procedure Check_One (Value : Expr'Class) is
         Is_Error : Boolean;
      begin
         Check_Compared (Self, Tested, Value, Is_Error);
         if Is_Error then
            Report
              (Self,
               Value,
               Error,
               "dimensions mismatch in membership test: tested value has dimension "
               & Image (Self, Dims_Of (Self, Tested))
               & ", found "
               & Image (Self, Dims_Of (Self, Value)));
         end if;
      end Check_One;

   begin
      for Alternative of Membership.F_Membership_Exprs loop
         declare
            A : constant Expr := Without_Parens (Alternative.As_Expr);
         begin
            if A.Is_Null then
               null;
            elsif A.Kind = Ada_Bin_Op and then A.As_Bin_Op.F_Op.Kind = Ada_Op_Double_Dot then
               Check_One (A.As_Bin_Op.F_Left);
               Check_One (A.As_Bin_Op.F_Right);
            elsif A.Kind in Ada_Identifier | Ada_Dotted_Name
              and then not A.As_Name.P_Referenced_Decl.Is_Null
              and then A.As_Name.P_Referenced_Decl.Kind in Ada_Base_Type_Decl
            then
               null;
            elsif A.Kind = Ada_Attribute_Ref then
               null;
            else
               Check_One (A);
            end if;
         end;
      end loop;
   end Check_Membership;

   -----------------------
   -- Check_Annotations --
   -----------------------

   procedure Check_Annotations (Self : in out Checker; Decl : Basic_Decl'Class) is
      type Annotation_Kind is (Dimension_System, Dimension, Power, Root);
      Count : array (Annotation_Kind) of Natural := [others => 0];
   begin
      for A of Decl.P_Get_Annotations loop
         if Exists (A) and then not Inherited (A) then
            declare
               N     : constant Ada_Node := Node (A).As_Ada_Node;
               Items : Actual_List (1 .. 2) := [others => No_Expr];
            begin
               if not N.Is_Null and then N.Kind = Ada_Aspect_Assoc then
                  declare
                     E : constant Expr := Without_Parens (N.As_Aspect_Assoc.F_Expr);
                     I : Natural := 0;
                  begin
                     if not E.Is_Null and then E.Kind = Ada_Aggregate then
                        for Assoc of E.As_Aggregate.F_Assocs loop
                           I := I + 1;
                           exit when I > 2;
                           if Assoc.Kind = Ada_Aggregate_Assoc then
                              Items (I) := Assoc.As_Aggregate_Assoc.F_R_Expr;
                           end if;
                        end loop;
                     elsif not E.Is_Null then
                        Items (1) := E;
                     end if;
                  end;
               elsif not N.Is_Null and then N.Kind = Ada_Pragma_Node then
                  declare
                     I : Natural := 0;
                  begin
                     for Arg of N.As_Pragma_Node.F_Args loop
                        I := I + 1;
                        exit when I > 2;
                        if Arg.Kind = Ada_Pragma_Argument_Assoc then
                           Items (I) := Arg.As_Pragma_Argument_Assoc.F_Expr;
                        end if;
                     end loop;
                  end;
               end if;

               if Is_Identifier (Items (1), "dimension_check") then
                  declare
                     Kind  : constant String := (if Items (2).Is_Null then "" else Lower_Text (Items (2)));
                     Found : Boolean := False;
                  begin
                     for K in Annotation_Kind loop
                        if To_Lower (K'Image) = Kind then
                           Found := True;
                           Count (K) := Count (K) + 1;
                           if Count (K) = 2 then
                              Report (Self, Items (1), Error, "more than one " & Text_Of (Items (2)) & " annotation");
                           end if;
                        end if;
                     end loop;

                     if not Found then
                        Report
                          (Self,
                           Items (1),
                           Error,
                           "unknown Dimension_Check annotation, expected Dimension_System, Dimension, Power or Root");
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      if Count (Dimension_System) > 0 then
         if Decl.Kind not in Ada_Type_Decl then
            Report (Self, Decl, Error, "a Dimension_System annotation must be on a type declaration");
         else
            declare
               Unused : constant System_Id := System_Of_Root (Self, Decl.As_Base_Type_Decl);
            begin
               null;
            end;
         end if;
      end if;

      if Count (Dimension) > 0 then
         if Decl.Kind /= Ada_Subtype_Decl then
            Report (Self, Decl, Error, "a Dimension annotation must be on a subtype declaration");
         else
            declare
               Unused : constant Subtype_Info := Type_Info (Self, Decl.As_Base_Type_Decl);
            begin
               null;
            end;
         end if;
      end if;

      if Count (Power) > 0 or else Count (Root) > 0 then
         declare
            Spec   : constant Base_Subp_Spec := Decl.P_Subp_Spec_Or_Null;
            Params : Natural := 0;
         begin
            if Spec.Is_Null or else Spec.Kind /= Ada_Subp_Spec or else Spec.As_Subp_Spec.F_Subp_Returns.Is_Null then
               Report (Self, Decl, Error, "a Power or Root annotation must be on a function");
            else
               for P of Spec.P_Params loop
                  Params := Params + P.F_Ids.Children_Count;
               end loop;

               if Count (Power) > 0 and then Params /= 2 then
                  Report (Self, Decl, Error, "a function with a Power annotation must have two parameters");
               elsif Count (Root) > 0 and then Params /= 1 then
                  Report (Self, Decl, Error, "a function with a Root annotation must have one parameter");
               else
                  declare
                     Unused : constant Special_Info := Special_Of (Self, Decl);
                  begin
                     null;
                  end;
               end if;
            end if;
         end;
      end if;
   end Check_Annotations;

   -----------------------------
   -- Unchecked_Initial_Value --
   -----------------------------

   function Unchecked_Initial_Value (Self : Checker; Value : Expr'Class) return Boolean
   is (not Self.Extras (Conditional_Initial_Values)
       and then not Without_Parens (Value).Is_Null
       and then Without_Parens (Value).Kind in Ada_If_Expr | Ada_Case_Expr);

   ---------------
   -- Is_Inside --
   ---------------

   function Is_Inside (Inner, Outer : Ada_Node'Class) return Boolean is
      P : Ada_Node := Inner.As_Ada_Node;
   begin
      while not P.Is_Null loop
         if P = Outer.As_Ada_Node then
            return True;
         end if;
         P := P.Parent;
      end loop;
      return False;
   end Is_Inside;

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node (Self : in out Checker; Node : Ada_Node'Class) is
   begin
      case Node.Kind is
         when Ada_Assign_Stmt                                                    =>
            declare
               Target : constant Dim_Info := Dims_Of (Self, Node.As_Assign_Stmt.F_Dest);
               Value  : constant Dim_Info := Dims_Of (Self, Node.As_Assign_Stmt.F_Expr);
            begin
               if not Same (Target, Value) then
                  Report
                    (Self,
                     Node,
                     Error,
                     "dimensions mismatch in assignment: left-hand side has dimension "
                     & Image (Self, Target)
                     & ", right-hand side has dimension "
                     & Image (Self, Value));
               end if;
            end;

         when Ada_Object_Decl | Ada_Extended_Return_Stmt_Object_Decl             =>
            declare
               O      : constant Object_Decl := Node.As_Object_Decl;
               Target : constant Subtype_Info := Type_Expr_Info (Self, O.F_Type_Expr);
            begin
               Check_Range (Self, O.F_Type_Expr, Target);

               if not O.F_Renaming_Clause.Is_Null then
                  Check_Value
                    (Self,
                     O.F_Renaming_Clause.F_Renamed_Object,
                     Target,
                     "dimensions mismatch in object renaming declaration",
                     Lenient_Literals => False);
               elsif not O.F_Default_Expr.Is_Null and then not Unchecked_Initial_Value (Self, O.F_Default_Expr) then
                  Check_Value
                    (Self,
                     O.F_Default_Expr,
                     Target,
                     "dimensions mismatch in object declaration",
                     Lenient_Literals  => True,
                     Context_Typed_OK  => True,
                     Allow_Propagation =>
                       O.F_Has_Constant
                       and then
                         (Target.Kind = Free
                          or else (Is_Dimensionless (Target.Dims) and then not Self.Extras (Explicit_Dimensionless))));
               end if;
            end;

         when Ada_Component_Decl                                                 =>
            declare
               C      : constant Component_Decl := Node.As_Component_Decl;
               Target : constant Subtype_Info := Type_Expr_Info (Self, C.F_Component_Def.F_Type_Expr);
            begin
               Check_Range (Self, C.F_Component_Def.F_Type_Expr, Target);
               Check_Value
                 (Self,
                  C.F_Default_Expr,
                  Target,
                  "dimensions mismatch in component declaration",
                  Lenient_Literals => True);
            end;

         when Ada_Param_Spec                                                     =>
            if not Node.As_Param_Spec.F_Default_Expr.Is_Null then
               Check_Value
                 (Self,
                  Node.As_Param_Spec.F_Default_Expr,
                  Type_Expr_Info (Self, Node.As_Param_Spec.F_Type_Expr),
                  "dimensions mismatch in default expression",
                  Lenient_Literals => True,
                  Report_Errors    => Self.Extras (Default_Expressions));
            end if;

         when Ada_Discriminant_Spec                                              =>
            if not Node.As_Discriminant_Spec.F_Default_Expr.Is_Null then
               Check_Value
                 (Self,
                  Node.As_Discriminant_Spec.F_Default_Expr,
                  Type_Expr_Info (Self, Node.As_Discriminant_Spec.F_Type_Expr),
                  "dimensions mismatch in default expression",
                  Lenient_Literals => True,
                  Report_Errors    => Self.Extras (Default_Expressions));
            end if;

         when Ada_Subtype_Decl                                                   =>
            Check_Annotations (Self, Node.As_Basic_Decl);
            Check_Range (Self, Node.As_Subtype_Decl.F_Subtype, Type_Info (Self, Node.As_Base_Type_Decl));

         when Ada_Type_Decl                                                      =>
            Check_Annotations (Self, Node.As_Basic_Decl);
            if Node.As_Basic_Decl.P_Has_Aspect (Langkit_Support.Text.To_Unbounded_Text ("Dimension_System")) then
               Report
                 (Self,
                  Node,
                  Warning,
                  "dimension_check does not read GNAT's Dimension_System aspect; write the annotation"
                  & " Annotate => (Dimension_Check, Dimension_System, ...), see tools/migrate_dimensions.py");
            end if;

         when Ada_Basic_Subp_Decl | Ada_Subp_Body | Ada_Generic_Subp_Decl        =>
            Check_Annotations (Self, Node.As_Basic_Decl);

         when Ada_Expr_Function                                                  =>
            Check_Annotations (Self, Node.As_Basic_Decl);
            if Self.Extras (Expression_Function_Results) then
               Check_Value
                 (Self,
                  Node.As_Expr_Function.F_Expr,
                  Type_Expr_Info (Self, Node.As_Expr_Function.F_Subp_Spec.F_Subp_Returns),
                  "dimensions mismatch in expression function result",
                  Lenient_Literals => False);
            end if;

         when Ada_Return_Stmt                                                    =>
            if not Node.As_Return_Stmt.F_Return_Expr.Is_Null then
               Check_Value
                 (Self,
                  Node.As_Return_Stmt.F_Return_Expr,
                  Type_Expr_Info (Self, Enclosing_Function_Result (Node)),
                  "dimensions mismatch in return statement",
                  Lenient_Literals => False);
            end if;

         when Ada_Extended_Return_Stmt                                           =>
            declare
               Result : constant Subtype_Info := Type_Expr_Info (Self, Enclosing_Function_Result (Node));
               Object : constant Subtype_Info :=
                 Type_Expr_Info (Self, Node.As_Extended_Return_Stmt.F_Decl.F_Type_Expr);
            begin
               if Result.Kind /= Not_In_System
                 and then Object.Kind /= Not_In_System
                 and then not Same (To_Dims (Result), To_Dims (Object))
               then
                  Report
                    (Self,
                     Node.As_Extended_Return_Stmt.F_Decl,
                     Error,
                     "dimensions mismatch in extended return statement: expected dimension "
                     & Image (Self, Result)
                     & ", found "
                     & Image (Self, Object));
               end if;
            end;

         when Ada_Call_Expr                                                      =>
            declare
               C : constant Call_Expr := Node.As_Call_Expr;
            begin
               if not Called_Attribute (C).Is_Null then
                  if Self.Extras (Attribute_Arguments)
                    and then not C.F_Suffix.Is_Null
                    and then C.F_Suffix.Kind = Ada_Assoc_List
                  then
                     Check_Attribute_Arguments (Self, Called_Attribute (C), C.F_Suffix.As_Assoc_List);
                  end if;
                  return;
               end if;

               case C.P_Kind is
                  when Type_Conversion =>
                     Check_Conversion (Self, C);

                  when Call            =>
                     declare
                        Callee : constant Basic_Decl := Called_Decl (C);
                     begin
                        if not Callee.Is_Null then
                           Check_Call (Self, C, Callee, Callee.P_Subp_Spec_Or_Null);
                        elsif not Access_Spec (C).Is_Null then
                           --  A call through an access-to-subprogram value.
                           Check_Call (Self, C, No_Basic_Decl, Access_Spec (C));
                        end if;
                     end;

                  when others          =>
                     null;
               end case;
            end;

         when Ada_Bin_Op | Ada_Relation_Op                                       =>
            declare
               Callee : constant Basic_Decl := Operator_Decl (Node.As_Bin_Op.F_Op);
            begin
               if Callee.Is_Null then
                  Check_Predefined_Operator (Self, Node.As_Bin_Op);
               else
                  Check_Call (Self, Node, Callee, Callee.P_Subp_Spec_Or_Null);
               end if;
            end;

         when Ada_Un_Op                                                          =>
            declare
               Callee : constant Basic_Decl := Operator_Decl (Node.As_Un_Op.F_Op);
            begin
               if not Callee.Is_Null then
                  Check_Call (Self, Node, Callee, Callee.P_Subp_Spec_Or_Null);
               end if;
            end;

         when Ada_Generic_Package_Instantiation | Ada_Generic_Subp_Instantiation =>
            --  GNAT checks the actual of a generic formal object as the initial value of an object.
            for Pair of Node.As_Generic_Instantiation.P_Inst_Params loop
               declare
                  Formal : Basic_Decl := Param (Pair).P_Basic_Decl;
                  Value  : constant Expr := Actual (Pair).As_Expr;
               begin
                  if not Formal.Is_Null and then Formal.Kind = Ada_Generic_Formal_Obj_Decl then
                     Formal := Formal.As_Generic_Formal_Obj_Decl.F_Decl;
                  end if;

                  if not Formal.Is_Null
                    and then Formal.Kind = Ada_Object_Decl
                    and then Is_Inside (Value, Node)
                    and then not Unchecked_Initial_Value (Self, Value)
                  then
                     Check_Value
                       (Self,
                        Value,
                        Type_Expr_Info (Self, Formal.As_Object_Decl.F_Type_Expr),
                        "dimensions mismatch in generic actual",
                        Lenient_Literals => True,
                        Context_Typed_OK => True);
                  end if;
               end;
            end loop;

         when Ada_If_Expr | Ada_Case_Expr                                        =>
            Check_Branches (Self, Node.As_Expr);

         when Ada_Base_Aggregate                                                 =>
            Check_Aggregate (Self, Node.As_Base_Aggregate);

         when Ada_Qual_Expr                                                      =>
            if Self.Extras (Qualified_Expressions) then
               declare
                  Target : constant Subtype_Info :=
                    Type_Info (Self, Node.As_Qual_Expr.F_Prefix.P_Name_Designated_Type);
               begin
                  if Target.Kind = Declared
                    and then (not Is_Dimensionless (Target.Dims) or else Self.Extras (Explicit_Dimensionless))
                  then
                     Check_Value
                       (Self,
                        Node.As_Qual_Expr.F_Suffix,
                        Target,
                        "dimensions mismatch in qualified expression",
                        Lenient_Literals => True);
                  end if;
               end;
            end if;

         when Ada_Attribute_Ref                                                  =>
            if Self.Extras (Attribute_Arguments) and then not Node.As_Attribute_Ref.F_Args.Is_Null then
               Check_Attribute_Arguments (Self, Node.As_Attribute_Ref, Node.As_Attribute_Ref.F_Args);
            end if;

         when Ada_Membership_Expr                                                =>
            if Self.Extras (Membership_Tests) then
               Check_Membership (Self, Node.As_Membership_Expr);
            end if;

         when others                                                             =>
            null;
      end case;
   end Check_Node;

   ------------------------------
   -- Collect_Warning_Switches --
   ------------------------------

   procedure Collect_Warning_Switches (Self : in out Checker) is

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Ada_Pragma_Node and then Lower_Text (Node.As_Pragma_Node.F_Id) = "warnings" then
            declare
               Args   : Actual_List (1 .. 3) := [others => No_Expr];
               Count  : Natural := 0;
               First  : Positive := 1;
               Is_Off : Boolean;
            begin
               for Arg of Node.As_Pragma_Node.F_Args loop
                  Count := Count + 1;
                  exit when Count > 3;
                  if Arg.Kind = Ada_Pragma_Argument_Assoc then
                     Args (Count) := Arg.As_Pragma_Argument_Assoc.F_Expr;
                  end if;
               end loop;

               if Is_Identifier (Args (1), "gnatprove") then
                  return Over;
               elsif Is_Identifier (Args (1), "gnat") then
                  First := 2;
               end if;

               if Is_Identifier (Args (First), "off") then
                  Is_Off := True;
               elsif Is_Identifier (Args (First), "on") then
                  Is_Off := False;
               else
                  return Over;
               end if;

               if First + 1 > Args'Last
                 or else Args (First + 1).Is_Null
                 or else
                   (Args (First + 1).Kind = Ada_String_Literal
                    and then
                      (Index (To_Lower (UTF8 (Args (First + 1).As_String_Literal.P_Denoted_Value)), "assumed to") > 0
                       or else UTF8 (Args (First + 1).As_String_Literal.P_Denoted_Value) = "*"))
               then
                  Self.Switches.Append (Warning_Switch'(Line => Positive (Node.Sloc_Range.Start_Line), Off => Is_Off));
               end if;
            end;
            return Over;
         end if;
         return Into;
      end Visit;

   begin
      Self.Switches.Clear;
      Self.Unit.Root.Traverse (Visit'Access);
   end Collect_Warning_Switches;

   ------------------
   -- Warnings_Off --
   ------------------

   function Warnings_Off (Self : Checker; Line : Positive) return Boolean is
      Result : Boolean := False;
   begin
      for Switch of Self.Switches loop
         exit when Switch.Line > Line;
         Result := Switch.Off;
      end loop;
      return Result;
   end Warnings_Off;

   ----------------
   -- Check_Unit --
   ----------------

   procedure Check_Unit (Self : in out Checker; Unit : Analysis_Unit; Extras : Extra_Check_Set) is

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         begin
            Check_Node (Self, Node);
         exception
            when E : Property_Error =>
               Note_Unresolved (Self, Node, "the construct: " & Exception_Message (E));
            when E : others =>
               Report
                 (Self,
                  Node,
                  Error,
                  "internal error in dimension_check: " & Exception_Name (E) & ": " & Exception_Message (E));
         end;
         return Into;
      end Visit;

   begin
      Self.Extras := Extras;
      Self.Unit := Unit;

      if Unit.Root.Is_Null then
         return;
      end if;

      for D of Unit.Diagnostics loop
         Report (Self, Unit.Root, Error, "cannot parse " & Langkit_Support.Diagnostics.To_Pretty_String (D));
      end loop;

      Collect_Warning_Switches (Self);
      Unit.Root.Traverse (Visit'Access);
   end Check_Unit;

end Dimension_Check.Analysis;
