with Units; use Units;

procedure Nonstatic is
   L : Length := 2.0 * mm;
   A : Area;
   N : Natural := 2;
begin
   A := L**N;  --! GNAT abandons the compilation after this error
end Nonstatic;
