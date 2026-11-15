with Units; use Units;

package Declarations is

   L1  : Length := 1.0 * s;                                  --! error: dimensions mismatch in object declaration
   L2  : Length := 0.0;
   L3  : Length := 3.0;                                      --! warning: numeric literal 3.0 is assumed
   L4  : Length := -3.0;                                     --! error: dimensions mismatch in object declaration
   L5  : Length := Half;
   L8  : Length := (Half);
   L9  : Length := -Half;                                    --! error: dimensions mismatch in object declaration
   L10 : Length := (if L2 > 0.0 * mm then 1.0 else Half);
   L11 : Length := 2.0 * Half;                              --! error: dimensions mismatch in object declaration
   L6  : Length range 0.0 .. 5.0 * s := 1.0 * mm;            --! error: dimensions mismatch in range
   L7  : Length range 0.0 .. 5.0 := 1.0 * mm;                --! warning: numeric literal 5.0 is assumed

   C1 : constant Dimensioned_Float := 2.0 * mm;
   C2 : constant Length := C1;
   C3 : constant Time := C1;                                --! error: dimensions mismatch in object declaration
   C4 : constant Dimensionless := 2.0 * mm;                 --! extra error: dimensions mismatch in object declaration
   C5 : constant Length := C4;                              --! extra error: dimensions mismatch in object declaration
   C6 : constant Unannotated := 2.0 * s;
   C7 : constant Time := C6;
   C8 : constant Length := C6;                              --! error: dimensions mismatch in object declaration
   I1 : constant Length := (if L2 > 0.0 * mm then L1 else 0.0 * mm);
   I2 : constant Length := (if L2 > 0.0 * mm then C3 else 0.0 * s);  --! extra error: dimensions mismatch in object
   I3 : constant Length :=  --! error: dimensions mismatch in object declaration
     (declare
        X : constant Time := 1.0 * s;
      begin
        X);
   V1 : Dimensioned_Float := 2.0 * mm;                      --! error: dimensions mismatch in object declaration

   subtype Short is Length range 0.0 * mm .. 1.0 * s;       --! error: dimensions mismatch in range
   subtype Shorter is Length range 0.0 .. 2.0;              --! warning: numeric literal 2.0 is assumed

   R1 : Length renames L1;
   R2 : Time renames L1;                                    --! error: dimensions mismatch in object renaming

   type Rec is record
      A : Length := 1.0 * s;                                --! error: dimensions mismatch in component declaration
      B : Length := 2.0;                                    --! warning: numeric literal 2.0 is assumed
      C : Length range 0.0 .. 1.0 * s;                      --! error: dimensions mismatch in range
      D : Length;
      E : Length := Half;                                   --! error: dimensions mismatch in component declaration
   end record;

   function F1 (X : Length := 1.0 * s) return Length;       --! extra error: dimensions mismatch in default expression
   function F2 (X : Length := 2.0) return Length;           --! warning: numeric literal 2.0 is assumed
   function F3 (X : Length) return Time
   is (X);             --! extra error: dimensions mismatch in expression function
   function F4 (X : Length) return Length
   is (X * 2.0);
   function F5 (X : Length) return Area
   is (X * X);

   Deferred : constant Dimensioned_Float;
   D2       : constant Length := Deferred;
   D3       : constant Time := Deferred;                          --! error: dimensions mismatch in object declaration

private

   Deferred : constant Dimensioned_Float := 1.0 * mm;

end Declarations;
