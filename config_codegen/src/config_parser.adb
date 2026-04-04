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

with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Directories;             use Ada.Directories;
with Langkit_Support.Text;        use Langkit_Support.Text;
with VSS.Strings.Conversions;
with Ada.Text_IO;

package body Config_Parser is

   pragma Extensions_Allowed (On);

   function Strip (Str : Text_Type) return Virtual_String is
   begin
      if Str (Str'First) = '"' then
         return To_Virtual_String (Str (Str'First + 1 .. Str'Last - 1));
      else
         return To_Virtual_String (Str);
      end if;
   end Strip;

   function Argument (Assocs : Assoc_List; Index : Positive) return Text_Type is
   begin
      return Assocs.Child (Assocs.First_Child_Index + Index - 1).As_Aggregate_Assoc.F_R_Expr.Text;
   end Argument;

   function Has_Argument (Assocs : Assoc_List; Index : Positive) return Boolean is
     (Assocs.First_Child_Index + Index - 1 <= Assocs.Last_Child_Index);

   function Parse_Unit (Assocs : Assoc_List) return Unit_Data is
      Conversion : constant Virtual_String := Strip (Argument (Assocs, 3));
   begin
      if Has_Argument (Assocs, 4) then
         return (Conversion => Conversion, Display => Strip (Argument (Assocs, 4)));
      else
         return (Conversion => Conversion, Display => Conversion);
      end if;
   end Parse_Unit;

   procedure Raise_Error (Node : Ada_Node'Class; Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
      --  Message may be too long for an exception.
      if Node.Is_Null then
         raise Constraint_Error with "Error at unknown location: " & Message;
      else
         raise Constraint_Error with "Error at " & Node.Image & ": " & Message;
      end if;
   end Raise_Error;

   function Is_Controller_Generic_Discrete_Type (Decl : Base_Type_Decl) return Boolean is
      Name : constant Text_Type := Decl.F_Name.Text;
   begin
      if Name /= "Motor_Name"
        and then Name /= "Heater_Name"
        and then Name /= "Thermistor_Name"
        and then Name /= "Board_Temperature_Probe_Name"
        and then Name /= "Fan_Name"
        and then Name /= "Input_Switch_Name"
      then
         return False;
      end if;

      return
        Decl.P_Fully_Qualified_Name = "Prunt.Controller_Generic_Types." & Name
        or else Simple_Name (Decl.Unit.Get_Filename) = "prunt-controller_generic_types.ads";
   end Is_Controller_Generic_Discrete_Type;

   function Has_Implicit_Config_Type (Decl : Base_Type_Decl; Value : out Config_Type) return Boolean is
   begin
      if Decl.P_Fully_Qualified_Name = "Standard.Boolean" then
         Value := (Boolean_Kind, (null record));
      elsif Decl.P_Fully_Qualified_Name = "VSS.Strings.Virtual_String" then
         Value := (String_Kind, (null record));
      elsif Decl.P_Fully_Qualified_Name = "Prunt.Dimensionless_Ratio" then
         Value := (Ratio_Kind, (Unit => (Conversion => "", Display => "")));
      elsif Is_Controller_Generic_Discrete_Type (Decl) then
         Value := (Enum_Kind, (Present_When => []));
      else
         return False;
      end if;

      return True;
   end Has_Implicit_Config_Type;

   procedure Register_Implicit_Config_Type (Decl : Base_Type_Decl; Config : in out Config_Maps.Map) is
      Value : Config_Type;
      Name  : constant Virtual_String := To_Virtual_String (Decl.P_Fully_Qualified_Name);
   begin
      if Has_Implicit_Config_Type (Decl, Value) and then not Config.Contains (Name) then
         Config.Insert (Name, Value);
      end if;
   end Register_Implicit_Config_Type;

   function Has_Prunt_Config_Aspect (Decl : Base_Type_Decl; Recursive : Boolean := True) return Boolean is
      Value : Config_Type;
   begin
      if Has_Implicit_Config_Type (Decl, Value) then
         return True;
      elsif not Decl.F_Aspects.Is_Null then
         for Assoc of Decl.F_Aspects.F_Aspect_Assocs loop
            if Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate then
               declare
                  Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
               begin
                  if Assocs.Child (Assocs.First_Child_Index).As_Aggregate_Assoc.F_R_Expr.Text = "Prunt_Config" then
                     if not Recursive or else Decl.Kind in Ada_Type_Decl then
                        return True;
                     elsif Decl.Kind in Ada_Subtype_Decl then
                        --  TODO: Need an error check on As_Base_Type_Decl.
                        return
                          Has_Prunt_Config_Aspect
                            (Decl.As_Subtype_Decl.F_Subtype.F_Name.P_Referenced_Decl.As_Base_Type_Decl, True);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end if;

      return False;
   end Has_Prunt_Config_Aspect;

   function Get_Comments_Starting_After (Start_Token : Token_Reference) return Virtual_String is
      Token       : Token_Reference := Start_Token;
      Result      : Virtual_String := "";
      Last_Was_LF : Boolean := False;
   begin
      Token := Next (Token);

      while Token not in No_Token and then Kind (Data (Token)) in Ada_Whitespace loop
         Token := Next (Token);
      end loop;

      while Token not in No_Token and then Kind (Data (Token)) in Ada_Comment loop
         declare
            Raw_Text : constant Wide_Wide_String := Text (Token);
            Trimmed  : constant Wide_Wide_String := Trim (Raw_Text (Raw_Text'First + 2 .. Raw_Text'Last), Both);
         begin
            if Trimmed = "" then
               Result.Append (""" & VSS.Characters.Latin.Line_Feed & """);
               Last_Was_LF := True;
            elsif Result /= "" and not Last_Was_LF then
               Result.Append (" ");
            else
               Last_Was_LF := False;
            end if;

            for C of Trimmed loop
               if C in '"' | '\' then
                  Result.Append (To_Virtual_String ("" & C));
               end if;
               Result.Append (To_Virtual_String ("" & C));
            end loop;

         end;
         Token := Next (Token);

         while Token not in No_Token and then Kind (Data (Token)) in Ada_Whitespace loop
            Token := Next (Token);
         end loop;
      end loop;

      return Result;
   end Get_Comments_Starting_After;

   function Get_Range (Decl : Base_Type_Decl) return Range_Spec is
      function Recurse (Inner_Decl : Base_Type_Decl) return Range_Spec is
      begin
         if Inner_Decl.Kind in Ada_Type_Decl then
            if Inner_Decl.As_Type_Decl.F_Type_Def.Kind in Ada_Signed_Int_Type_Def then
               return Inner_Decl.As_Type_Decl.F_Type_Def.As_Signed_Int_Type_Def.F_Range;
            elsif Inner_Decl.As_Type_Decl.F_Type_Def.Kind in Ada_Floating_Point_Def then
               return Inner_Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def.F_Range;
            else
               return No_Range_Spec;
            end if;
         elsif Inner_Decl.Kind in Ada_Subtype_Decl then
            declare
               Indication : constant Subtype_Indication := Inner_Decl.As_Subtype_Decl.F_Subtype;
               Constr     : constant Constraint := Indication.F_Constraint;
            begin
               if not Constr.Is_Null and then Constr.Kind in Ada_Range_Constraint then
                  return Constr.As_Range_Constraint.F_Range;
               end if;

               declare
                  Parent_Basic : constant Basic_Decl := Indication.F_Name.P_Referenced_Decl;
               begin
                  if Parent_Basic.Is_Null then
                     Raise_Error
                       (Inner_Decl, "Parent of type could not be resolved. Resolution triggered from " & Decl.Image);
                  elsif Parent_Basic.Kind not in Ada_Base_Type_Decl then
                     Raise_Error
                       (Inner_Decl,
                        "Type does not resolve to a plain type declaration. Resolution triggered from " & Decl.Image);
                  else
                     return Get_Range (Parent_Basic.As_Base_Type_Decl);
                  end if;
               end;
            end;
         else
            Raise_Error
              (Inner_Decl,
               "Type does not resolve to a plain type declaration. Resolution triggered from " & Decl.Image);
         end if;

         raise Constraint_Error with "Should be unreachable.";
      end Recurse;
   begin
      return Recurse (Decl);
   end Get_Range;

   function Get_Base_Def (Decl : Base_Type_Decl) return Type_Def is
      function Recurse (Inner_Decl : Base_Type_Decl) return Type_Def is
      begin
         if Inner_Decl.Kind in Ada_Type_Decl then
            declare
               Def : constant Type_Def := Inner_Decl.As_Type_Decl.F_Type_Def;
            begin
               if Def.Kind in Ada_Derived_Type_Def then
                  declare
                     Ref : constant Basic_Decl :=
                       Def.As_Derived_Type_Def.F_Subtype_Indication.F_Name.P_Referenced_Decl;
                  begin
                     if Ref.Is_Null then
                        Raise_Error
                          (Inner_Decl,
                           "Parent of type could not be resolved. Resolution triggered from " & Decl.Image);
                     elsif Ref.Kind in Ada_Base_Type_Decl then
                        return Recurse (Ref.As_Base_Type_Decl);
                     else
                        Raise_Error
                          (Inner_Decl,
                           "Type does not resolve to a plain type declaration. Resolution triggered from "
                           & Decl.Image);
                     end if;
                  end;
               else
                  return Def;
               end if;
            end;
         elsif Inner_Decl.Kind in Ada_Subtype_Decl then
            declare
               Ref : constant Basic_Decl := Inner_Decl.As_Subtype_Decl.F_Subtype.F_Name.P_Referenced_Decl;
            begin
               if Ref.Is_Null then
                  Raise_Error
                    (Inner_Decl, "Parent of type could not be resolved. Resolution triggered from " & Decl.Image);
               elsif Ref.Kind in Ada_Base_Type_Decl then
                  return Recurse (Ref.As_Base_Type_Decl);
               else
                  Raise_Error
                    (Inner_Decl,
                     "Type does not resolve to a plain type declaration. Resolution triggered from " & Decl.Image);
               end if;
            end;
         else
            Raise_Error
              (Inner_Decl,
               "Type does not resolve to a plain type declaration. Resolution triggered from " & Decl.Image);
         end if;

         raise Constraint_Error with "Should be unreachable.";
      end Recurse;
   begin
      return Recurse (Decl);
   end Get_Base_Def;

   function Is_Numeric_Base (Decl : Base_Type_Decl) return Boolean is
   begin
      return Get_Base_Def (Decl).Kind in Ada_Signed_Int_Type_Def | Ada_Floating_Point_Def;
   end Is_Numeric_Base;

   procedure Extract_Bounds
     (Ada_Range : Range_Spec;
      Min, Max  : out Virtual_String;
      Error_For : Ada_Node'Class;
      Context   : String)
   is
   begin
      if Ada_Range.Is_Null then
         Min := "";
         Max := "";
      elsif Ada_Range.F_Range.Kind in Ada_Bin_Op then
         Min := To_Virtual_String (Ada_Range.F_Range.As_Bin_Op.F_Left.Text);
         Max := To_Virtual_String (Ada_Range.F_Range.As_Bin_Op.F_Right.Text);
      else
         Raise_Error (Error_For, "Only basic range constraints are supported. " & Context);
      end if;
   end Extract_Bounds;

   function Effective_Min
     (Type_Name        : Virtual_String;
      Range_Min        : Virtual_String;
      Annotation_Min   : Virtual_String) return Virtual_String
   is
   begin
      if Range_Min.Is_Empty then
         return Annotation_Min;
      elsif Annotation_Min.Is_Empty then
         return Range_Min;
      else
         return Type_Name & "'Max (" & Range_Min & ", " & Annotation_Min & ")";
      end if;
   end Effective_Min;

   function Effective_Max
     (Type_Name        : Virtual_String;
      Range_Max        : Virtual_String;
      Annotation_Max   : Virtual_String) return Virtual_String
   is
   begin
      if Range_Max.Is_Empty then
         return Annotation_Max;
      elsif Annotation_Max.Is_Empty then
         return Range_Max;
      else
         return Type_Name & "'Min (" & Range_Max & ", " & Annotation_Max & ")";
      end if;
   end Effective_Max;

   function Parse_Record (Decl : Base_Type_Decl; Implicit_Config : in out Config_Maps.Map) return Config_Type is
      function Parse_Component_Items (Items : Ada_Node_List) return Component_Data_Maps.Map is
         Components : Component_Data_Maps.Map;
      begin
         for Item of Items when Item.Kind in Ada_Component_Decl loop
            --  Skip null component declarations, aspects, and pragmas.
            declare
               Comp_Decl : constant Component_Decl := Item.As_Component_Decl;
               T_Expr    : constant Type_Expr := Comp_Decl.F_Component_Def.F_Type_Expr;
            begin
               if T_Expr.Kind not in Ada_Subtype_Indication then
                  Raise_Error (T_Expr, "Anonymous types are not supported.");
               end if;

               declare
                  Component : Component_Data;
                  Desig     : constant Base_Type_Decl := T_Expr.P_Designated_Type_Decl;
               begin
                  if Desig.Is_Null then
                     Raise_Error (T_Expr, "Could not resolve type.");
                  end if;

                  Register_Implicit_Config_Type (Desig, Implicit_Config);

                  Component :=
                    (Type_Name           => To_Virtual_String (Desig.P_Fully_Qualified_Name),
                     Default             =>
                       (if Comp_Decl.F_Default_Expr.Is_Null
                        then ""
                        else To_Virtual_String (Comp_Decl.F_Default_Expr.Text)),
                     Description         => Get_Comments_Starting_After (Item.Token_End),
                     Min                 => "",
                     Max                 => "",
                     Fixed_Kind          => "",
                     Options_Expr        => "",
                     Present_When        => "",
                     Schema_Default_Expr => "",
                     Unit                => (Conversion => "", Display => ""));
                  begin
                     if not Comp_Decl.F_Aspects.Is_Null then
                        for Assoc of
                          Comp_Decl.F_Aspects.F_Aspect_Assocs
                          when Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate
                        loop
                           declare
                              Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
                           begin

                              if Argument (Assocs, 1) = "Prunt_Config" then
                                 if Argument (Assocs, 2) = "Unit" then
                                    Component.Unit := Parse_Unit (Assocs);
                                 elsif Argument (Assocs, 2) = "Fixed_Kind" then
                                    Component.Fixed_Kind := Strip (Argument (Assocs, 3));
                                 elsif Argument (Assocs, 2) = "Options_Expr" then
                                    Component.Options_Expr := Strip (Argument (Assocs, 3));
                                 elsif Argument (Assocs, 2) = "Present_When" then
                                    Component.Present_When := Strip (Argument (Assocs, 3));
                                 elsif Argument (Assocs, 2) = "Schema_Default_Expr" then
                                    Component.Schema_Default_Expr := Strip (Argument (Assocs, 3));
                                 elsif Argument (Assocs, 2) = "Min" then
                                    Component.Min := Strip (Argument (Assocs, 3));
                                 elsif Argument (Assocs, 2) = "Max" then
                                    Component.Max := Strip (Argument (Assocs, 3));
                                 else
                                    Raise_Error
                                      (Assoc,
                                       "Unhandled Prunt_Config key (" & Argument (Assocs, 2)'Image & ").");
                                 end if;
                              end if;
                           end;
                        end loop;
                     end if;

                     if not Has_Prunt_Config_Aspect (Desig) then
                        Raise_Error
                          (Item,
                           "Type must have Prunt_Config annotation aspect to be used in config record. Declared at "
                           & Desig.Image);
                     end if;

                     if not Is_Numeric_Base (Desig)
                       and then
                         ((Component.Min.Is_Empty and then not Component.Max.Is_Empty)
                          or else (Component.Max.Is_Empty and then not Component.Min.Is_Empty))
                     then
                        Raise_Error (Item, "Both min and max aspect must be specified if either is specified.");
                     end if;

                     if Is_Numeric_Base (Desig) then
                        declare
                           Range_Min, Range_Max : Virtual_String;
                        begin
                           if Comp_Decl.F_Component_Def.F_Type_Expr.Kind in Ada_Subtype_Indication then
                              declare
                                 Constr : constant Constraint :=
                                   Comp_Decl.F_Component_Def.F_Type_Expr.As_Subtype_Indication.F_Constraint;
                              begin
                                 if not Constr.Is_Null then
                                    if Constr.Kind in Ada_Range_Constraint then
                                       Extract_Bounds
                                         (Constr.As_Range_Constraint.F_Range,
                                          Range_Min,
                                          Range_Max,
                                          Constr,
                                          Constr.Text'Image);
                                    else
                                       Raise_Error
                                         (Constr, "Only range constraints are supported. " & Constr.Text'Image);
                                    end if;
                                 end if;
                              end;
                           end if;

                           if Range_Min.Is_Empty then
                              if Component.Min.Is_Empty and then Get_Range (Desig).Is_Null then
                                 Raise_Error (Item, "Component or underlying type requires a range constraint.");
                              end if;

                              if not Get_Range (Desig).Is_Null then
                                 Extract_Bounds
                                   (Get_Range (Desig),
                                    Range_Min,
                                    Range_Max,
                                    Get_Range (Desig),
                                    Get_Range (Desig).Text'Image);
                              end if;
                           end if;

                           Component.Min := Effective_Min (Component.Type_Name, Range_Min, Component.Min);
                           Component.Max := Effective_Max (Component.Type_Name, Range_Max, Component.Max);

                           if Component.Min.Is_Empty or else Component.Max.Is_Empty then
                              Raise_Error
                                (Item,
                                 "Both min and max must be provided by annotations or range constraints.");
                           end if;
                        end;
                     end if;

                     for Id of Comp_Decl.F_Ids loop
                        Components.Insert (To_Virtual_String (Id.Text), Component);
                     end loop;
                  end;
               end;
            end;
         end loop;

         return Components;
      end Parse_Component_Items;

      Def : constant Record_Type_Def := Decl.As_Type_Decl.F_Type_Def.As_Record_Type_Def;

      Data : Record_Data :=
        (Has_Variant => False,
         Components  =>
           (if Def.F_Record_Def.F_Components.Is_Null or else Def.F_Record_Def.F_Components.F_Components.Is_Null
            then []
            else Parse_Component_Items (Def.F_Record_Def.F_Components.F_Components)),
         Description => Get_Comments_Starting_After (Def.F_Record_Def.Token_Start),
         Tabbed      => False);
      --  Comment is first comment inside `record ... end record`.
      --  TODO: Add better support for `null record` here. Currently this will look for a comment after `null`.
   begin
      if not Decl.F_Aspects.Is_Null then
         for Assoc of
           Decl.F_Aspects.F_Aspect_Assocs
           when Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate
         loop
            declare
               function Argument (Index : Positive) return Text_Type is
                  Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
               begin
                  return Assocs.Child (Assocs.First_Child_Index + Index - 1).As_Aggregate_Assoc.F_R_Expr.Text;
               end Argument;
            begin
               if Argument (1) = "Prunt_Config" then
                  if Argument (2) = "Tabbed" then
                     Data.Tabbed := True;
                  elsif Argument (2) = "User_Config" or Argument (2) = "Root_User_Config" then
                     null;
                  else
                     Raise_Error (Assoc, "Unhandled Prunt_Config key. " & Argument (2)'Image);
                  end if;
               end if;
            end;
         end loop;
      end if;

      if not Def.F_Record_Def.F_Components.F_Variant_Part.Is_Null then
         Data :=
           (Has_Variant          => True,
            Components           => @.Components,
            Description          => @.Description,
            Tabbed               => @.Tabbed,
            Discriminant_Type    => "",
            Discriminant         => To_Virtual_String (Def.F_Record_Def.F_Components.F_Variant_Part.F_Discr_Name.Text),
            Discriminant_Default => "",
            Variants             => []);

         for Spec of Decl.As_Type_Decl.F_Discriminants.As_Known_Discriminant_Part.F_Discr_Specs loop
            for D_Id of Spec.F_Ids loop
               if D_Id.Text = Def.F_Record_Def.F_Components.F_Variant_Part.F_Discr_Name.Text then
                  Register_Implicit_Config_Type (Spec.F_Type_Expr.P_Designated_Type_Decl, Implicit_Config);
                  Data.Discriminant_Type :=
                    To_Virtual_String (Spec.F_Type_Expr.P_Designated_Type_Decl.P_Fully_Qualified_Name);
                  Data.Discriminant_Default := To_Virtual_String (Spec.F_Default_Expr.Text);
               else
                  Raise_Error (Spec, "Multiple discriminants not supported.");
               end if;
            end loop;
         end loop;

         for Variant of Def.F_Record_Def.F_Components.F_Variant_Part.F_Variant loop
            if Variant.F_Choices.First_Child_Index /= Variant.F_Choices.Last_Child_Index
              or else Variant.F_Choices.First_Child.Kind not in Ada_Identifier
            then
               Raise_Error (Variant.F_Choices, "Variant choices must be singular plain identifiers.");
            end if;

            declare
               Tok : Token_Reference := Variant.F_Choices.Token_End;
            begin
               while Tok not in No_Token and then Kind (Libadalang.Common.Data (Tok)) not in Ada_Arrow loop
                  Tok := Next (Tok);
               end loop;

               Data.Variants.Insert
                 (To_Virtual_String (Variant.F_Choices.First_Child.As_Identifier.Text),
                  (Components  =>
                     (if Variant.F_Components.Is_Null or else Variant.F_Components.F_Components.Is_Null
                      then []
                      else Parse_Component_Items (Variant.F_Components.F_Components)),
                   Description => Get_Comments_Starting_After (Tok)));
            end;
         end loop;
      end if;

      return (Record_Kind, Data);
   end Parse_Record;

   function Parse_Array (Decl : Base_Type_Decl; Implicit_Config : in out Config_Maps.Map) return Config_Type is
      Def          : constant Array_Type_Def := Decl.As_Type_Decl.F_Type_Def.As_Array_Type_Def;
      Is_Tabbed    : Boolean := False;
      Present_When : Virtual_String := "";
      Min, Max     : Virtual_String := "";
   begin
      --  TODO: We need to handle index types with range constraints here.
      --  TODO: We need to handle ratio element types with range constraints here.
      if not Decl.F_Aspects.Is_Null then
         for Assoc of
           Decl.F_Aspects.F_Aspect_Assocs
           when Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate
         loop
            declare
               function Argument (Index : Positive) return Text_Type is
                  Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
               begin
                  return Assocs.Child (Assocs.First_Child_Index + Index - 1).As_Aggregate_Assoc.F_R_Expr.Text;
               end Argument;
            begin
               if Argument (1) = "Prunt_Config" then
                  if Argument (2) = "Tabbed" then
                     Is_Tabbed := True;
                  elsif Argument (2) = "Present_When" then
                     Present_When := Strip (Argument (3));
                  elsif Argument (2) = "User_Config" then
                     null;
                  else
                     Raise_Error (Assoc, "Unhandled Prunt_Config key.");
                  end if;
               end if;
            end;
         end loop;
      end if;

      declare
         Index_Decl : constant Basic_Decl :=
           Def.F_Indices.Child (1).As_Constraint_List.Child (1).As_Subtype_Indication.F_Name.P_Referenced_Decl;
         Elem_Decl  : constant Base_Type_Decl := Def.F_Component_Type.F_Type_Expr.P_Designated_Type_Decl;
      begin
         if Index_Decl.Is_Null then
            Raise_Error (Def, "Could not resolve index type.");
         end if;

         if Index_Decl.Kind not in Ada_Base_Type_Decl then
            Raise_Error (Def, "Index must resolve to Base_Type_Decl.");
         end if;

         if Elem_Decl.Is_Null then
            Raise_Error (Def, "Could not resolve element type.");
         end if;

         Register_Implicit_Config_Type (Index_Decl.As_Base_Type_Decl, Implicit_Config);
         Register_Implicit_Config_Type (Elem_Decl, Implicit_Config);

         if not Has_Prunt_Config_Aspect (Index_Decl.As_Base_Type_Decl) then
            Raise_Error
              (Decl,
               "Index type must have Prunt_Config annotation aspect to be used in config record. Declared at "
               & Index_Decl.Image);
         end if;

         if not Has_Prunt_Config_Aspect (Elem_Decl) then
            Raise_Error
              (Decl,
               "Element type must have Prunt_Config annotation aspect to be used in config record. Declared at "
               & Elem_Decl.Image);
         end if;

         if Is_Numeric_Base (Elem_Decl) then
            declare
               Range_Min, Range_Max : Virtual_String;
            begin
               if Def.F_Component_Type.F_Type_Expr.Kind in Ada_Subtype_Indication then
                  declare
                     Constr : constant Constraint :=
                       Def.F_Component_Type.F_Type_Expr.As_Subtype_Indication.F_Constraint;
                  begin
                     if not Constr.Is_Null then
                        if Constr.Kind in Ada_Range_Constraint then
                           Extract_Bounds
                             (Constr.As_Range_Constraint.F_Range,
                              Range_Min,
                              Range_Max,
                              Constr,
                              Constr.Text'Image);
                        else
                           Raise_Error (Constr, "Only range constraints are supported. " & Constr.Text'Image);
                        end if;
                     end if;
                  end;
               end if;

               if Range_Min.Is_Empty then
                  if Min.Is_Empty and then Get_Range (Elem_Decl).Is_Null then
                     Raise_Error (Decl, "Array element or underlying type requires a range constraint.");
                  end if;

                  if not Get_Range (Elem_Decl).Is_Null then
                     Extract_Bounds
                       (Get_Range (Elem_Decl),
                        Range_Min,
                        Range_Max,
                        Get_Range (Elem_Decl),
                        Get_Range (Elem_Decl).Text'Image);
                  end if;
               end if;

               Min := Effective_Min (To_Virtual_String (Elem_Decl.P_Fully_Qualified_Name), Range_Min, Min);
               Max := Effective_Max (To_Virtual_String (Elem_Decl.P_Fully_Qualified_Name), Range_Max, Max);
            end;
         end if;

         return
           (Array_Kind,
            (Index_Type   => To_Virtual_String (Index_Decl.P_Fully_Qualified_Name),
             Element_Type => To_Virtual_String (Elem_Decl.P_Fully_Qualified_Name),
             Tabbed       => Is_Tabbed,
             Present_When => Present_When,
             Min          => Min,
             Max          => Max));
      end;
   end Parse_Array;

   function Parse_Enum (Decl : Base_Type_Decl) return Config_Type is
      Data : Enum_Data := (Present_When => []);
   begin
      if not Decl.F_Aspects.Is_Null then
         for Assoc of
           Decl.F_Aspects.F_Aspect_Assocs
           when Assoc.F_Id.Text = "Annotate"
           and then Assoc.F_Expr.Kind in Ada_Aggregate
           and then not Assoc.F_Expr.As_Aggregate.F_Assocs.Is_Null
         loop
           declare
               Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
            begin
               if Argument (Assocs, 1) = "Prunt_Config" then
                  if Argument (Assocs, 2) = "Present_When" then
                     Data.Present_When.Insert (Strip (Argument (Assocs, 3)), Strip (Argument (Assocs, 4)));
                  elsif Argument (Assocs, 2) = "User_Config" then
                     null;
                  else
                     Raise_Error (Assoc, "Unhandled Prunt_Config key.");
                  end if;
               end if;
            end;
         end loop;
      end if;

      return (Enum_Kind, Data);
   end Parse_Enum;

   function Parse_Integer (Decl : Base_Type_Decl) return Config_Type is
   begin
      if not Decl.F_Aspects.Is_Null then
         for Assoc of
           Decl.F_Aspects.F_Aspect_Assocs
           when Assoc.F_Id.Text = "Annotate"
           and then Assoc.F_Expr.Kind in Ada_Aggregate
           and then not Assoc.F_Expr.As_Aggregate.F_Assocs.Is_Null
         loop
           declare
               Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
            begin
               if Argument (Assocs, 1) = "Prunt_Config" and then Argument (Assocs, 2) = "Unit" then
                  return (Integer_Kind, (Unit => Parse_Unit (Assocs)));
               end if;
            end;
         end loop;
      end if;

      return (Integer_Kind, (Unit => (Conversion => "", Display => "")));
   end Parse_Integer;

   function Parse_Float (Decl : Base_Type_Decl) return Config_Type is
   begin
      if not Decl.F_Aspects.Is_Null then
         for Assoc of
           Decl.F_Aspects.F_Aspect_Assocs
           when Assoc.F_Id.Text = "Annotate"
           and then Assoc.F_Expr.Kind in Ada_Aggregate
           and then not Assoc.F_Expr.As_Aggregate.F_Assocs.Is_Null
         loop
           declare
               Assocs : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
            begin
               if Argument (Assocs, 1) = "Prunt_Config" and then Argument (Assocs, 2) = "Unit" then
                  return (Float_Kind, (Unit => Parse_Unit (Assocs)));
               end if;
            end;
         end loop;
      end if;

      return (Float_Kind, (Unit => (Conversion => "", Display => "")));
   end Parse_Float;

   function Parse_Gcode_Command (Decl : Subp_Decl) return Gcode_Command_Data is
      function Parse_Argument_Kinds
        (Arg_Type_Name : Virtual_String; Default_Value : Virtual_String) return Gcode_Argument_Kind_Set
      is
         Res  : Gcode_Argument_Kind_Set := (others => False);
         Name : constant String := Conversions.To_UTF_8_String (Arg_Type_Name);
      begin
         if Index (Name, "Integer") > 0 then
            Res (Integer_Kind) := True;
         end if;
         if Index (Name, "Float") > 0 or else Index (Name, "Dimensionless") > 0 then
            Res (Integer_Kind) := True;
            Res (Float_Kind) := True;
         end if;
         if Index (Name, "String") > 0 then
            Res (String_Kind) := True;
         end if;
         if Index (Name, "No_Value") > 0 then
            Res (No_Value_Kind) := True;
         end if;
         if Index (Name, "Optional") > 0 or else Default_Value /= "" then
            Res (Not_Present_Kind) := True;
         end if;
         return Res;
      end Parse_Argument_Kinds;

      Command : Gcode_Command_Data :=
        (Name         => To_Virtual_String (Decl.F_Subp_Spec.F_Subp_Name.Text),
         Arguments    => [],
         Has_This     => False,
         Has_Self_Ref => False,
         Has_Planner  => False,
         Description  => Get_Comments_Starting_After (Decl.Token_End));
   begin

      for Param_Spec of Decl.F_Subp_Spec.F_Subp_Params.F_Params loop
         for Id of Param_Spec.F_Ids loop
            if Id.Text = "This" then
               Command.Has_This := True;
            elsif Id.Text = "Self_Ref" then
               Command.Has_Self_Ref := True;
            elsif Id.Text = "Planner" then
               Command.Has_Planner := True;
            else
               declare
                  Type_Name : constant Virtual_String := To_Virtual_String (Param_Spec.F_Type_Expr.Text);
                  Default   : constant Virtual_String :=
                    (if Param_Spec.F_Default_Expr.Is_Null
                     then ""
                     else To_Virtual_String (Param_Spec.F_Default_Expr.Text));

                  Arg_Data : constant Gcode_Argument_Data :=
                    (Type_Name   => Type_Name,
                     Default     => Default,
                     Description =>
                       Get_Comments_Starting_After
                         ((if Kind (Data (Next (Param_Spec.Token_End))) = Ada_Semicolon
                           then Next (Next (Param_Spec.Token_End))
                           else Param_Spec.Token_End)),
                     Arg_Kinds   => Parse_Argument_Kinds (Type_Name, Default));
               begin
                  Command.Arguments.Insert (To_Virtual_String (Id.Text), Arg_Data);
               end;
            end if;
         end loop;
      end loop;

      return Command;
   end Parse_Gcode_Command;

   function Format_Gcode_Arguments (Arguments : Gcode_Argument_Maps.Map) return String is
      Result : Virtual_String := "(";
      First  : Boolean        := True;
   begin
      for C in Arguments.Iterate loop
         if not First then
            Result.Append (", ");
         end if;

         Result.Append (Gcode_Argument_Maps.Key (C));
         Result.Append (" => ");
         Result.Append (Conversions.To_Virtual_String (Gcode_Argument_Maps.Element (C)'Image));
         First := False;
      end loop;

      Result.Append (")");
      return Conversions.To_UTF_8_String (Result);
   end Format_Gcode_Arguments;

   function Parse (Context : Libadalang.Analysis.Analysis_Context; Filename : String) return Module_Data is
      Result : Module_Data := (Name => "", Filename => "", Root_Type => "", others => <>);

      function Visit (N : Ada_Node'Class) return Visit_Status is
      begin
         if N.Kind in Ada_Base_Type_Decl and then not N.As_Base_Type_Decl.F_Aspects.Is_Null then
            declare
               Decl : constant Base_Type_Decl := N.As_Base_Type_Decl;
               Name : constant Virtual_String := To_Virtual_String (Decl.P_Fully_Qualified_Name);
            begin
               for Assoc of Decl.F_Aspects.F_Aspect_Assocs loop
                  if Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate then
                     declare
                        Args : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
                     begin
                        if Args.Child (Args.First_Child_Index + 0).As_Aggregate_Assoc.F_R_Expr.Text = "Prunt_Config"
                        then
                           if Args.Child (Args.First_Child_Index + 1).As_Aggregate_Assoc.F_R_Expr.Text
                             = "Root_User_Config"
                           then
                              if Result.Root_Type /= "" then
                                 Raise_Error (Decl, "Multiple Root_User_Config types found in module.");
                              end if;
                              Result.Root_Type := Name;
                           end if;

                           if Decl.Kind in Ada_Type_Decl then
                              declare
                                 Def : constant Type_Def := Decl.As_Type_Decl.F_Type_Def;
                              begin
                                 if Def.Kind = Ada_Record_Type_Def then
                                    Result.Config.Insert (Name, Parse_Record (Decl, Result.Config));
                                 elsif Def.Kind = Ada_Array_Type_Def then
                                    Result.Config.Insert (Name, Parse_Array (Decl, Result.Config));
                                 elsif Def.Kind in Ada_Signed_Int_Type_Def then
                                    Result.Config.Insert (Name, Parse_Integer (Decl));
                                 elsif Def.Kind in Ada_Floating_Point_Def then
                                    Result.Config.Insert (Name, Parse_Float (Decl));
                                 elsif Def.Kind in Ada_Enum_Type_Def | Ada_Formal_Discrete_Type_Def then
                                    Result.Config.Insert (Name, Parse_Enum (Decl));
                                 elsif Def.Kind in Ada_Derived_Type_Def then
                                    if Get_Base_Def (Decl).Kind in Ada_Floating_Point_Def then
                                       Result.Config.Insert (Name, Parse_Float (Decl));
                                    elsif Get_Base_Def (Decl).Kind in Ada_Signed_Int_Type_Def then
                                       Result.Config.Insert (Name, Parse_Integer (Decl));
                                    else
                                       raise Constraint_Error; --  TODO
                                    end if;
                                 else
                                    Decl.Print;
                                    raise Constraint_Error; --  TODO
                                 end if;
                              end;
                           elsif Decl.Kind in Ada_Subtype_Decl then
                              if Get_Base_Def (Decl).Kind in Ada_Floating_Point_Def then
                                 Result.Config.Insert (Name, Parse_Float (Decl));
                              elsif Get_Base_Def (Decl).Kind in Ada_Signed_Int_Type_Def then
                                 Result.Config.Insert (Name, Parse_Integer (Decl));
                              else
                                 raise Constraint_Error; --  TODO
                              end if;
                           else
                              raise Constraint_Error; --  TODO
                           end if;

                           return Over;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         elsif N.Kind in Ada_Subp_Decl and then not N.As_Subp_Decl.F_Aspects.Is_Null then
            declare
               Decl : constant Subp_Decl := N.As_Subp_Decl;
            begin
               for Assoc of Decl.F_Aspects.F_Aspect_Assocs loop
                  if Assoc.F_Id.Text = "Annotate" and then Assoc.F_Expr.Kind in Ada_Aggregate then
                     declare
                        Args : constant Assoc_List := Assoc.F_Expr.As_Aggregate.F_Assocs;
                     begin
                        if Args.Child (Args.First_Child_Index + 0).As_Aggregate_Assoc.F_R_Expr.Text = "Prunt_Config"
                          and then Args.Child (Args.First_Child_Index + 1).As_Aggregate_Assoc.F_R_Expr.Text
                                   = "Gcode_Command"
                        then
                           declare
                              Raw_Id : constant Wide_Wide_String :=
                                Args.Child (Args.First_Child_Index + 2).As_Aggregate_Assoc.F_R_Expr.Text;
                              Key    : Gcode_Key;
                           begin
                              if Raw_Id (Raw_Id'First + 1) = 'M' or Raw_Id (Raw_Id'First + 1) = 'G' then
                                 Key.Letter := To_Virtual_String (Raw_Id (Raw_Id'First + 1 .. Raw_Id'First + 1));
                                 Key.Number :=
                                   Integer'Value
                                     (VSS.Strings.Conversions.To_UTF_8_String
                                        (To_Virtual_String (Raw_Id (Raw_Id'First + 2 .. Raw_Id'Last - 1))));
                              else
                                 Raise_Error (Assoc, "Invalid Gcode command identifier.");
                              end if;

                              declare
                                 New_Cmd : constant Gcode_Command_Data := Parse_Gcode_Command (Decl);
                                 use Gcode_Command_Maps;
                                 use Gcode_Command_Vectors;

                                 function Check_Overlap (C1, C2 : Gcode_Command_Data) return Boolean is
                                    function Are_Kinds_Disjoint (K1, K2 : Gcode_Argument_Kind_Set) return Boolean is
                                    begin
                                       for K in Gcode_Argument_Kind loop
                                          if K1 (K) and K2 (K) then
                                             return False;
                                          end if;
                                       end loop;
                                       return True;
                                    end Are_Kinds_Disjoint;
                                 begin
                                    for C in C1.Arguments.Iterate loop
                                       if C2.Arguments.Contains (Gcode_Argument_Maps.Key (C)) then
                                          if Are_Kinds_Disjoint
                                               (Gcode_Argument_Maps.Element (C).Arg_Kinds,
                                                C2.Arguments.Element (Gcode_Argument_Maps.Key (C)).Arg_Kinds)
                                          then
                                             return False;
                                          end if;
                                       elsif not Gcode_Argument_Maps.Element (C).Arg_Kinds (Not_Present_Kind) then
                                          return False;
                                       end if;
                                    end loop;

                                    for C in C2.Arguments.Iterate loop
                                       if C1.Arguments.Contains (Gcode_Argument_Maps.Key (C)) then
                                          if Are_Kinds_Disjoint
                                               (Gcode_Argument_Maps.Element (C).Arg_Kinds,
                                                C1.Arguments.Element (Gcode_Argument_Maps.Key (C)).Arg_Kinds)
                                          then
                                             return False;
                                          end if;
                                       elsif not Gcode_Argument_Maps.Element (C).Arg_Kinds (Not_Present_Kind) then
                                          return False;
                                       end if;
                                    end loop;

                                    return True;
                                 end Check_Overlap;
                              begin
                                 if Result.Gcode_Commands.Contains (Key) then
                                    declare
                                       Variants : Vector := Result.Gcode_Commands.Element (Key);
                                    begin
                                       for V of Variants loop
                                          if Check_Overlap (V, New_Cmd) then
                                             Raise_Error
                                               (Decl,
                                                "Gcode command overlap detected with "
                                                & VSS.Strings.Conversions.To_UTF_8_String (V.Name)
                                                & " ("
                                                & Format_Gcode_Arguments (New_Cmd.Arguments)
                                                & ", "
                                                & Format_Gcode_Arguments (V.Arguments)
                                                & ").");
                                          end if;
                                       end loop;
                                       Variants.Append (New_Cmd);
                                       Result.Gcode_Commands.Replace (Key, Variants);
                                    end;
                                 else
                                    declare
                                       Variants : Vector;
                                    begin
                                       Variants.Append (New_Cmd);
                                       Result.Gcode_Commands.Insert (Key, Variants);
                                    end;
                                 end if;
                              end;

                              return Over;
                           end;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end if;
         return Into;
      end Visit;

      Root : constant Ada_Node := Context.Get_From_File (Filename).Root;
   begin
      if Root.As_Compilation_Unit.F_Body.As_Library_Item.F_Item.Kind = Ada_Package_Decl then
         Result.Name :=
           To_Virtual_String
             (Root.As_Compilation_Unit.F_Body.As_Library_Item.F_Item.As_Package_Decl.F_Package_Name.F_Name.Text);
         Root.Traverse (Visit'Access);
      elsif Root.As_Compilation_Unit.F_Body.As_Library_Item.F_Item.Kind = Ada_Generic_Package_Decl then
         Result.Name :=
           To_Virtual_String
             (Root
                .As_Compilation_Unit
                .F_Body
                .As_Library_Item
                .F_Item
                .As_Generic_Package_Decl
                .F_Package_Decl
                .F_Package_Name
                .Text);
         Root.Traverse (Visit'Access);
      end if;

      Result.Filename := Conversions.To_Virtual_String (Simple_Name (Filename));
      return Result;
   end Parse;

end Config_Parser;
