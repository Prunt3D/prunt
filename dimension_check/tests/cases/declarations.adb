package body Declarations is

   function F1 (X : Length := 1.0 * s) return Length is
      --! extra error: dimensions mismatch in default expression
   begin
      if X > 0.0 * mm then
         return X;
      elsif X > 1.0 * mm then
         return X * X;                                      --! error: dimensions mismatch in return statement
      else
         return 0.0;                                        --! error: dimensions mismatch in return statement
      end if;
   end F1;

   function F2 (X : Length := 2.0) return Length is
      --! warning: numeric literal 2.0 is assumed
   begin
      return Result : Length := X do
         null;
      end return;
   end F2;

   function Bad_Extended (X : Length) return Time is
   begin
      return Result : Length := X do
         --! error: dimensions mismatch in extended return
         null;
      end return;
   end Bad_Extended;

   procedure Nested is
      function Inner return Time
      is (1.0 * s);
      T : Time := Inner;
   begin
      null;
   end Nested;

end Declarations;
