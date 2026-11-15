with Units; use Units;

procedure Expressions is
   type Kind is (First, Second);
   L : Length := 1.0 * mm;
   T : Time := 1.0 * s;
   D : Dimensionless := 1.0;
   P : Position := [others => 0.0 * mm];
   G : Grid := [others => [others => 0.0 * mm]];
   M : Move;
   K : Kind := First;

   function Check (X : Length) return Length
   with Post => Check'Result > X'Old and then Check'Result /= 0.0 * s;  --! error: operation "/=" must have same

   function Check (X : Length) return Length
   is (X);
begin
   L := Length'Max (L, T);                --! extra error: dimensions mismatch in attribute Max
   L := Length'Max (L, 0.0);
   L := Length'Min (L, 5.0);              --! extra warning: numeric literal 5.0 is assumed
   D := Dimensionless'Max (D, L);         --! extra error: dimensions mismatch in attribute Max
   D := Dimensioned_Float'Max (L, T);
   L := Length'Floor (T);                 --! extra error: dimensions mismatch in attribute Floor
   L := Length'Adjacent (L, L);
   T := Length'Max (L, L);                --! error: dimensions mismatch in assignment
   L := Length'(T);                       --! extra error: dimensions mismatch in qualified expression
   L := Length'(5.0);                     --! extra warning: numeric literal 5.0 is assumed
   L := Length'(L);
   L := Length'Small;
   L := L * Dimensionless'Model_Epsilon;
   D := Length'Model_Epsilon;             --! error: dimensions mismatch in assignment
   L := Length'Last;
   if L in 0.0 * mm .. 1.0 * s then
      --! extra error: dimensions mismatch in membership test
      null;
   end if;
   if L in 0.0 .. 1.0 * mm | 2.0 * mm then
      null;
   end if;
   if L in Positive_Length then
      null;
   end if;
   L :=
     (case K is
        when First  => L,
        when Second => T);  --! error: dimensions mismatch in conditional expression
   L :=
     (case K is
        when First  => L,
        when Second => 2.0 * L);
   L :=
     (declare
        X : constant Length := L * 2.0;
      begin
        X);
   L :=  --! error: dimensions mismatch in assignment
     (declare
        X : constant Length := L * 2.0;
      begin
        X * X);
   L := (if L > 0.0 * mm then L else raise Program_Error);
   L := @ + L;
   L := @ + T;                            --! error: operation "+" must have same dimensions
   G := [[1.0 * mm, 2.0 * mm], [3.0 * mm, 4.0 * s]];  --! error: dimensions mismatch in array aggregate
   P := [for I in 1 .. 3 => L];
   P := [for I in 1 .. 3 => T];           --! error: dimensions mismatch in array aggregate
   P := [P with delta 1 => T];            --! extra error: dimensions mismatch in array aggregate
   P := (1 => L, 2 => L, others => 0.0);  --! error: dimensions mismatch in array aggregate
   M := (M with delta Distance => T);     --! extra error: dimensions mismatch in record aggregate
   M := (L, T);
   M := (T, L);                           --! error: record aggregate --! error: record aggregate
   M := (Distance => L, others => <>);
   L := P (1) + T;                        --! error: operation "+" must have same dimensions
   L := G (1, 2);
   L := M.Distance;
   L := M.Duration;                       --! error: dimensions mismatch in assignment
   for E of P loop
      L := E;
      T := E;                             --! error: dimensions mismatch in assignment
   end loop;
   pragma Assert (L > T);                 --! error: operation ">" must have same dimensions
end Expressions;
