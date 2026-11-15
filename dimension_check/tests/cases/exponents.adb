with Units; use Units;

procedure Exponents is
   L : Length := 1.0 * mm;
   T : Time := 1.0 * s;
   A : Area := L * L;
   D : Dimensionless := 1.0;
   R : Root_Length;
   N : Natural := 2;
   X : Dimensioned_Float := 0.5;
begin
   A := L**2;
   A := L**Two;
   A := L**2#10#;
   L := A**0.5;
   L := A**(1.0 / 2.0);
   L := A**2#0.1#;
   L := A**5.0E-1;
   L := A**Half;
   L := (L**3)**(1.0 / 3.0);
   L := (A**3)**(1.0 / 6.0);
   L := (A**2)**0.25;
   L := A**(-0.5) * A;
   A := L**(4.0 / 3.0) * L**(2.0 / 3.0);
   R := L**0.5;
   L := R**2;
   D := L**0;
   D := D**N;
   D := D**X;
   L := L**Two;                         --! error: dimensions mismatch in assignment
   T := A**0.5;                         --! error: dimensions mismatch in assignment
   A := L**N;                           --! error: must be a static expression
   L := A**X;                           --! error: must be a static expression
   L := A**(1 / 2);                     --! error: the exponent (1 / 2) is an integer division with value 0
   L := A**(-1 / 2);                    --! error: is an integer division
   D := D**(1 / 2);                     --! error: is an integer division
   L := A**L;                           --! error: the exponent must be dimensionless
end Exponents;
