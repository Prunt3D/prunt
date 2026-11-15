with Units; use Units;

procedure Calls is
   L : Length := 1.0 * mm;
   T : Time := 1.0 * s;
   V : Velocity;
   D : Dimensionless := 1.0;
   O : Offset := [others => 0.0 * mm];
   K : Scale := [others => 0.0];
   F : Length_Function;
   R : Root_Length;
   package Steps is new Stepper (Step => 1.0 * mm);
begin
   V := Speed (L, T);
   V :=
     Speed (T, T);                     --! error: dimensions mismatch in call of Speed: expected dimension [Millimeter]
   V := Speed (Duration => T, Distance => L);
   V :=
     Speed
       (Duration => L, Distance => L);  --! error: dimensions mismatch in call of Speed: expected dimension [Second]
   D := Twice (D);
   D := Twice (L);                        --! extra error: dimensions mismatch in call of Twice
   L := Anything (L);                     --! error: dimensions mismatch in assignment
   D := Anything (L);
   Advance (L, T);                        --! error: dimensions mismatch in call of Advance
   Advance (T, L);                        --! error: dimensions mismatch in call of Advance
   L := Math.Sqrt (L * L);
   T := Math.Sqrt (L * L);                --! error: dimensions mismatch in assignment
   R := Math.Sqrt (L);
   L := R * R;
   L := R**2;
   D := Math.Sin (L);                     --! error: dimensions mismatch in call of Sin
   D := Math.Sin (L / mm);
   D := Math.Arctan (L, L);               --! error: call of Arctan --! error: call of Arctan
   L := Power (L * L, 0.5);
   L := Power (L * L * L, 1.0 / 3.0);
   L := Power (L, D);                     --! error: must be a static expression
   L := Power (L, L);                     --! error: the exponent must be dimensionless
   D := Power (D, D);
   L := Cube_Root (L * L * L);
   L := Cube_Root (L);                    --! error: dimensions mismatch in assignment
   L := F (L);
   L := F (T);                            --! error: dimensions mismatch in call of F
   L := F.all (T);                        --! error: dimensions mismatch in call of F.all
   L := Steps.Next (L);
   L := Steps.Next (T);                   --! error: dimensions mismatch in call of Next
   O := K * L;
   O := K * T;                            --! error: dimensions mismatch in call of "*"
   L := abs O;
   D := abs K;
   L := abs (O with delta 1 => 0.0 * mm); --! note: the formal of "abs" has another type
end Calls;
