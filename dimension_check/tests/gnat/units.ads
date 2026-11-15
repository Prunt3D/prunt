--  GNAT's own dimension aspects. run_tests.py compiles the tests in this directory with GNAT and compares its
--  findings with those of dimension_check on copies rewritten by tools/migrate_dimensions.py.

with Ada.Numerics.Generic_Elementary_Functions;

package Units is

   type Dimensioned_Float is new Long_Float
   with
     Dimension_System =>
       ((Unit_Name => Millimeter, Unit_Symbol => "mm", Dim_Symbol => "Length"),
        (Unit_Name => Second, Unit_Symbol => "s", Dim_Symbol => "Time"));

   subtype Length is Dimensioned_Float with Dimension => (Symbol => "mm", Millimeter => 1, others => 0);
   subtype Time is Dimensioned_Float with Dimension => (Symbol => "s", Second => 1, others => 0);
   subtype Velocity is Dimensioned_Float with Dimension => (Symbol => "mm/s", Millimeter => 1, Second => -1);
   subtype Area is Dimensioned_Float with Dimension => (Symbol => "mm2", Millimeter => 2, others => 0);
   subtype Root_Length is Dimensioned_Float with Dimension => (Symbol => "rmm", Millimeter => 1 / 2, others => 0);
   subtype Dimensionless is Dimensioned_Float with Dimension => (Symbol => "x", others => 0);
   subtype Unannotated is Dimensioned_Float;
   subtype Positive_Length is Length range 0.0 .. Length'Last;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);

   function "**" (Left, Right : Dimensioned_Float'Base) return Dimensioned_Float'Base renames Math."**";

   pragma Warnings (Off, "assumed to be");
   mm : constant Length := 1.0;
   s  : constant Time := 1.0;
   pragma Warnings (On, "assumed to be");

   Half : constant := 0.5;
   Two  : constant := 2;

   type Position is array (1 .. 3) of Length;
   type Offset is new Position;
   type Scale is array (1 .. 3) of Dimensionless;
   type Grid is array (1 .. 2, 1 .. 2) of Length;

   function "*" (Left : Scale; Right : Length) return Offset;

   type Move is record
      Distance : Length := 0.0 * mm;
      Duration : Time := 0.0 * s;
   end record;

   function Speed (Distance : Length; Duration : Time) return Velocity;
   function Twice (X : Dimensionless) return Dimensionless;
   function Anything (X : Dimensioned_Float) return Dimensioned_Float;
   procedure Advance (Distance : in out Length; By : Length);

   type Length_Function is access function (X : Length) return Length;

   generic
      Step : Length;
   package Stepper is
      function Next (X : Length) return Length
      is (X + Step);
   end Stepper;

end Units;
