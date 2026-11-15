with Units; use Units;

procedure Generics is
   C : Boolean := True;
   package Steps_1 is new Stepper (Step => 1.0 * s);       --! error: dimensions mismatch in generic actual
   package Steps_2 is new Stepper (Step => 2.0);           --! warning: numeric literal 2.0 is assumed
   package Steps_3 is new Stepper (Step => 0.0);
   package Steps_4 is new Stepper (Step => 1.0 * mm);
   package Steps_5 is new Stepper (Half);
   package Steps_7 is new Stepper ((if C then 1.0 * s else 0.0 * s));  --! extra error: generic actual
   package Steps_6 is new Stepper (1.0 * s);               --! error: dimensions mismatch in generic actual
begin
   null;
end Generics;
