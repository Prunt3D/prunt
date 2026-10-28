--  Focused compiler benchmark for the runtime operations used by the planner.
--  Each kernel consumes changing positive normal inputs, matching the common planner path.
--  The Bits_* kernels preserve the former planner helpers for performance comparisons only.

with Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Prunt; use Prunt;

procedure Runtime_Math_Time is
   use type Ada.Real_Time.Time;
   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   type Kernel_Kind is
     (Power_Half,
      Square_Root,
      Power_Quarter,
      Square_Root_Twice,
      Power_Third,
      Power_Fifth,
      Attribute_Exponent,
      Bits_Exponent,
      Attribute_Fraction,
      Bits_Fraction,
      Attribute_Scaling,
      Attribute_Adjacent);
   Selected   : constant Kernel_Kind := Kernel_Kind'Value (Ada.Command_Line.Argument (1));
   Iterations : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 2 then Positive'Value (Ada.Command_Line.Argument (2)) else 10_000_000);

   type Float_Bits is mod 2**64;
   function To_Bits is new Ada.Unchecked_Conversion (Dimensionless, Float_Bits);
   function From_Bits is new Ada.Unchecked_Conversion (Float_Bits, Dimensionless);
   Exponent_Mask : constant Float_Bits := 16#7FF0_0000_0000_0000#;

   function Half (X : Dimensionless) return Dimensionless
   is (X**(1 / 2));
   function Root (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math.Sqrt (X));
   function Quarter (X : Dimensionless) return Dimensionless
   is (X**(1 / 4));
   function Root_Twice (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (X)));
   function Third (X : Dimensionless) return Dimensionless
   is (X**(1 / 3));
   function Fifth (X : Dimensionless) return Dimensionless
   is (X**(1 / 5));
   function Exponent (X : Dimensionless) return Dimensionless
   is (Dimensionless (Dimensionless'Exponent (X)));
   function Fraction (X : Dimensionless) return Dimensionless
   is (Dimensionless'Fraction (X));
   function Scaling (X : Dimensionless) return Dimensionless
   is (Dimensionless'Scaling (X, -3));
   function Adjacent (X : Dimensionless) return Dimensionless
   is (Dimensionless'Adjacent (X, Dimensionless'Last));

   function Fast_Exponent (X : Dimensionless) return Dimensionless;
   function Fast_Fraction (X : Dimensionless) return Dimensionless;

   function Fast_Exponent (X : Dimensionless) return Dimensionless is
      Biased : constant Float_Bits := (To_Bits (X) and Exponent_Mask) / 2**52;
   begin
      if Biased = 0 or else Biased = 16#7FF# then
         return Dimensionless (Dimensionless'Exponent (X));
      end if;
      return Dimensionless (Integer (Biased) - 1022);
   end Fast_Exponent;

   function Fast_Fraction (X : Dimensionless) return Dimensionless is
      Bits   : constant Float_Bits := To_Bits (X);
      Biased : constant Float_Bits := (Bits and Exponent_Mask) / 2**52;
   begin
      if Biased = 0 or else Biased = 16#7FF# then
         return Dimensionless'Fraction (X);
      end if;
      return From_Bits ((Bits and not Exponent_Mask) or 1022 * 2**52);
   end Fast_Fraction;

   generic
      with function Kernel (X : Dimensionless) return Dimensionless;
   procedure Measure;

   procedure Measure is
      Sum     : Dimensionless := 0.0;
      Started : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      for I in 1 .. Iterations loop
         Sum := Sum + Kernel (0.5 + Dimensionless (I mod 8_192) / 1_024.0);
      end loop;
      Ada.Text_IO.Put_Line
        ("wall_seconds=" & Duration'Image (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Started)));
      Ada.Text_IO.Put_Line ("sum=" & Dimensionless'Image (Sum));
   end Measure;

   procedure Measure_Half is new Measure (Half);
   procedure Measure_Root is new Measure (Root);
   procedure Measure_Quarter is new Measure (Quarter);
   procedure Measure_Root_Twice is new Measure (Root_Twice);
   procedure Measure_Third is new Measure (Third);
   procedure Measure_Fifth is new Measure (Fifth);
   procedure Measure_Exponent is new Measure (Exponent);
   procedure Measure_Fast_Exponent is new Measure (Fast_Exponent);
   procedure Measure_Fraction is new Measure (Fraction);
   procedure Measure_Fast_Fraction is new Measure (Fast_Fraction);
   procedure Measure_Scaling is new Measure (Scaling);
   procedure Measure_Adjacent is new Measure (Adjacent);
begin
   Ada.Text_IO.Put_Line ("kernel=" & Selected'Image & " iterations=" & Iterations'Image);
   case Selected is
      when Power_Half         =>
         Measure_Half;

      when Square_Root        =>
         Measure_Root;

      when Power_Quarter      =>
         Measure_Quarter;

      when Square_Root_Twice  =>
         Measure_Root_Twice;

      when Power_Third        =>
         Measure_Third;

      when Power_Fifth        =>
         Measure_Fifth;

      when Attribute_Exponent =>
         Measure_Exponent;

      when Bits_Exponent      =>
         Measure_Fast_Exponent;

      when Attribute_Fraction =>
         Measure_Fraction;

      when Bits_Fraction      =>
         Measure_Fast_Fraction;

      when Attribute_Scaling  =>
         Measure_Scaling;

      when Attribute_Adjacent =>
         Measure_Adjacent;
   end case;
end Runtime_Math_Time;
