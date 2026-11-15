with Units; use Units;

package Declarations is

   L1  : Length := 1.0 * s;
   L2  : Length := 0.0;
   L3  : Length := 3.0;
   L4  : Length := -3.0;
   L5  : Length := Half;
   L8  : Length := (Half);
   L9  : Length := -Half;
   L10 : Length := (if L2 > 0.0 * mm then 1.0 else Half);
   L11 : Length := 2.0 * Half;
   L6  : Length range 0.0 .. 5.0 * s := 1.0 * mm;
   L7  : Length range 0.0 .. 5.0 := 1.0 * mm;

   C1 : constant Dimensioned_Float := 2.0 * mm;
   C2 : constant Length := C1;
   C3 : constant Time := C1;
   C4 : constant Dimensionless := 2.0 * mm;
   C5 : constant Length := C4;
   C6 : constant Unannotated := 2.0 * s;
   C7 : constant Time := C6;
   C8 : constant Length := C6;
   I1 : constant Length := (if L2 > 0.0 * mm then L1 else 0.0 * mm);
   I2 : constant Length := (if L2 > 0.0 * mm then C3 else 0.0 * s);
   I3 : constant Length :=
     (declare
        X : constant Time := 1.0 * s;
      begin
        X);
   V1 : Dimensioned_Float := 2.0 * mm;

   subtype Short is Length range 0.0 * mm .. 1.0 * s;
   subtype Shorter is Length range 0.0 .. 2.0;

   R1 : Length renames L1;
   R2 : Time renames L1;

   type Rec is record
      A : Length := 1.0 * s;
      B : Length := 2.0;
      C : Length range 0.0 .. 1.0 * s;
      D : Length;
      E : Length := Half;
   end record;

   function F1 (X : Length := 1.0 * s) return Length;
   function F2 (X : Length := 2.0) return Length;
   function F3 (X : Length) return Time
   is (X);
   function F4 (X : Length) return Length
   is (X * 2.0);
   function F5 (X : Length) return Area
   is (X * X);

end Declarations;
