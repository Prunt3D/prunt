package body Declarations is

   function F1 (X : Length := 1.0 * s) return Length is
   begin
      if X > 0.0 * mm then
         return X;
      elsif X > 1.0 * mm then
         return X * X;
      else
         return 0.0;
      end if;
   end F1;

   function F2 (X : Length := 2.0) return Length is
   begin
      return Result : Length := X do
         null;
      end return;
   end F2;

   function Bad_Extended (X : Length) return Time is
   begin
      return Result : Length := X do
         null;
      end return;
   end Bad_Extended;

end Declarations;
