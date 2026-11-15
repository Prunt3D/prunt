with Units; use Units;

procedure Basic is
   L : Length := 2.0 * mm;
   T : Time := 1.0 * s;
   V : Velocity := L / T;
   A : Area := L**2;
   D : Dimensionless := 1.0;
   P : Position := [others => 0.0 * mm];
   M : Move := (Distance => L, Duration => T);
begin
   L := V * T;
   L := A**0.5;
   L := Math.Sqrt (A);
   L := (L**3)**(1.0 / 3.0);
   T := L;                                    --! error: dimensions mismatch in assignment
   V := L * T;                                --! error: dimensions mismatch in assignment
   if L > T then
      --! error: operation ">" must have same dimensions
      null;
   end if;
   L := L + T;                                --! error: operation "+" must have same dimensions
   L := L + 1.0;                              --! error: operation "+" must have same dimensions
   P (1) := T;                                --! error: dimensions mismatch in assignment
   M.Distance := T;                           --! error: dimensions mismatch in assignment
   L := Length (D);                           --! error: dimensions mismatch in conversion
   D := Dimensionless (L);                    --! error: dimensions mismatch in conversion --! error: assignment
   L := Length (Long_Float (T));
   L := Length (5.0);
   L := Dimensioned_Float (L);
   L := Unannotated (L);                      --! error: dimensions mismatch in conversion
   L := (if L > 0.0 * mm then L else T);      --! error: dimensions mismatch in conditional expression
   L := (if L > 0.0 * mm then L elsif L > 1.0 * mm then T else L);  --! error: conditional expression
   L := 0.0;                                  --! error: dimensions mismatch in assignment
   if L > 5.0 then
      --! warning: numeric literal 5.0 is assumed to have dimension [Millimeter]
      null;
   end if;
   if 5.0 > L then
      --! warning: numeric literal 5.0 is assumed to have dimension [Millimeter]
      null;
   end if;
   if L > 0.0 and then L /= 0.0 then
      null;
   end if;
   if L > -5.0 then
      --! error: operation ">" must have same dimensions
      null;
   end if;
   L := abs L - (-L);
   A := L**2 + Area (L * L);
   L := L * D + L / D;
   L := L * Half;
   L := L + Half;                             --! error: operation "+" must have same dimensions
   if L > Half then
      --! error: operation ">" must have same dimensions
      null;
   end if;
end Basic;
