with Units; use Units;

procedure Operators is
   L : Length := 2.0 * mm;
   T : Time := 1.0 * s;
   V : Velocity := L / T;
   A : Area := L**2;
   D : Dimensionless := 1.0;
   R : Root_Length;
   N : Natural := 2;
begin
   L := V * T;
   L := A**(1 / 2);
   L := (L**3)**(1 / 3);
   L := (A**3)**(1 / 6);
   L := (A**2)**(1 / 4);
   A := L**(4 / 3) * L**(2 / 3);
   L := A**(-1 / 2) * A;
   R := L**(1 / 2);
   L := R**2;
   L := Math.Sqrt (A);
   R := Math.Sqrt (L);
   A := L**Two;
   D := L**0;
   D := D**N;
   T := L;
   V := L * T;
   if L > T then
      null;
   end if;
   L := L + T;
   L := L + 1.0;
   L := L - T;
   L := Length (D);
   D := Dimensionless (L);
   L := Length (Long_Float (T));
   L := Length (5.0);
   L := Dimensioned_Float (L);
   L := Unannotated (L);
   L := (if L > 0.0 * mm then L else T);
   L := (if L > 0.0 * mm then L elsif L > 1.0 * mm then T else L);
   L := 0.0;
   if L > 5.0 then
      null;
   end if;
   if 5.0 > L then
      null;
   end if;
   if L > 0.0 and then L /= 0.0 then
      null;
   end if;
   if L > -5.0 then
      null;
   end if;
   L := abs L - (-L);
   A := L**2 + Area (L * L);
   L := L * D + L / D;
   L := L * Half;
   L := L + Half;
   if L > Half then
      null;
   end if;
   L := L**Two;
   T := A**(1 / 2);
   L := Length'Small;
   L := L * Dimensionless'Model_Epsilon;
   D := Length'Model_Epsilon;
   L := Length'Last;
   D := Math.Sin (L);
   D := Math.Sin (L / mm);
   D := Math.Arctan (L, L);
   L := @ + L;
   L := @ + T;
end Operators;
