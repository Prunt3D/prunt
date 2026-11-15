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
   with Post => Check'Result > X'Old and then Check'Result /= 0.0 * s;

   function Check (X : Length) return Length
   is (X);
begin
   L := Length'Max (L, T);
   L := Length'Max (L, 0.0);
   D := Dimensionless'Max (D, L);
   D := Dimensioned_Float'Max (L, T);
   L := Length'Floor (T);
   T := Length'Max (L, L);
   L := Length'(T);
   L := Length'(L);
   if L in 0.0 * mm .. 1.0 * s then
      null;
   end if;
   if L in Positive_Length then
      null;
   end if;
   L :=
     (case K is
        when First  => L,
        when Second => T);
   L :=
     (case K is
        when First  => L,
        when Second => 2.0 * L);
   L :=  --! GNAT false positive: a declare expression has no dimensions
     (declare
        X : constant Length := L * 2.0;
      begin
        X);
   L :=
     (declare
        X : constant Length := L * 2.0;
      begin
        X * X);
   G := [[1.0 * mm, 2.0 * mm], [3.0 * mm, 4.0 * s]];
   P := [for I in 1 .. 3 => L];
   P := [for I in 1 .. 3 => T];
   P := [P with delta 1 => T];
   M := (M with delta Distance => T);
   P := (1 => L, 2 => L, others => 0.0);
   M := (L, T);
   M := (T, L);
   M := (Distance => L, others => <>);
   L := P (1) + T;
   L := G (1, 2);
   L := M.Distance;
   L := M.Duration;
   for E of P loop
      L := E;
      T := E;
   end loop;
   pragma Assert (L > T);
end Expressions;
