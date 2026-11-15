with Units; use Units;

package Annotations is

   subtype Bad_Name is Dimensioned_Float
   with
     Annotate =>
       (Dimension_Check,
        Dimension,
        "Meter",
        1);  --! error: unknown dimension "Meter", expected one of Millimeter, Second
   subtype Missing is Dimensioned_Float
   with Annotate => (Dimension_Check, Dimension, "Millimeter");  --! error: missing exponent
   subtype Twice is Dimensioned_Float
   with
     Annotate => (Dimension_Check, Dimension, "Second", 1, "Second", 2);  --! error: dimension "Second" is given twice
   subtype Fraction is Dimensioned_Float
   with
     Annotate =>
       (Dimension_Check,
        Dimension,
        "Millimeter",
        1 / 2);  --! error: the exponent of a dimension must be a static integer
   subtype Conflict is Length
   with Annotate => (Dimension_Check, Dimension, "Second", 1);  --! error: subtype already has dimension [Millimeter]
   subtype Same_Again is Length with Annotate => (Dimension_Check, Dimension, "Millimeter", 1);
   type Not_Subtype is new Long_Float
   with Annotate => (Dimension_Check, Dimension, "Millimeter", 1);  --! error: must be on a subtype declaration
   subtype Outside is Long_Float
   with Annotate => (Dimension_Check, Dimension, "Millimeter", 1);  --! error: has no Dimension_System annotation
   subtype Misspelled is Dimensioned_Float
   with Annotate => (Dimension_Check, Dimensions);  --! error: unknown Dimension_Check annotation
   subtype Doubled is Dimensioned_Float
   with
     Annotate => (Dimension_Check, Dimension),
     Annotate => (Dimension_Check, Dimension);  --! error: more than one Dimension annotation
   type Empty_System is new Long_Float
   with Annotate => (Dimension_Check, Dimension_System);  --! error: needs at least one dimension
   type Doubled_System is new Long_Float
   with Annotate => (Dimension_Check, Dimension_System, "A", "a");  --! error: dimension "a" is declared twice
   type Bad_System_Name is new Long_Float
   with Annotate => (Dimension_Check, Dimension_System, Length);  --! error: must be a string literal
   function Bad_Power (X : Dimensioned_Float) return Dimensioned_Float
   with Annotate => (Dimension_Check, Power);  --! error: must have two parameters
   procedure Not_Function (X : Dimensioned_Float)
   with Annotate => (Dimension_Check, Root, 2);  --! error: must be on a function
   function Bad_Root (X : Dimensioned_Float) return Dimensioned_Float
   with Annotate => (Dimension_Check, Root, 0);  --! error: takes one positive static integer
   subtype Other_Tool is Dimensioned_Float with Annotate => (GNATprove, Dimension, "Meter", 1);

   subtype By_Pragma is Dimensioned_Float;
   pragma Annotate (Dimension_Check, Dimension, "Second", 1, Entity => By_Pragma);
   P1 : By_Pragma := 1.0 * s;
   P2 : By_Pragma := 1.0 * mm;                              --! error: dimensions mismatch in object declaration

   pragma Warnings (Off, "assumed to be");
   Off_1 : constant Length := 5.0;
   pragma Warnings (On, "assumed to be");
   On_1  : constant Length := 5.0;                           --! warning: numeric literal 5.0 is assumed
   pragma Warnings (Off);
   Off_2 : constant Length := 5.0;
   pragma Warnings (On);
   pragma Warnings (Off, "some other message");
   On_2  : constant Length := 5.0;                           --! warning: numeric literal 5.0 is assumed
   pragma Warnings (On, "some other message");

end Annotations;
