with Units; use Units;

procedure Calls is
   L : Length := 1.0 * mm;
   T : Time := 1.0 * s;
   V : Velocity;
   D : Dimensionless := 1.0;
   O : Offset := [others => 0.0 * mm];
   K : Scale := [others => 0.0];
   package Steps is new Stepper (Step => 1.0 * mm);
begin
   V := Speed (L, T);
   V := Speed (T, T);
   V := Speed (Duration => T, Distance => L);
   V := Speed (Duration => L, Distance => L);
   D := Twice (D);
   D := Twice (L);
   L := Anything (L);
   D := Anything (L);
   Advance (L, T);
   Advance (T, L);
   T := Math.Sqrt (L * L);
   L := Steps.Next (L);
   L := Steps.Next (T);
   O := K * L;
   O := K * T;
end Calls;
