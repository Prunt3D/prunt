--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

package body Prunt.Cleanup_Exceptions is

   function Empty_Occurrence return Ada.Exceptions.Exception_Occurrence is
   begin
      return Result : Ada.Exceptions.Exception_Occurrence do
         Ada.Exceptions.Save_Occurrence (Result, Ada.Exceptions.Null_Occurrence);
      end return;
   end Empty_Occurrence;

   procedure Reraise (This : Holder) is
   begin
      Ada.Exceptions.Reraise_Occurrence (This.Error);
   end Reraise;

   procedure Save (This : in out Holder; Occurrence : Ada.Exceptions.Exception_Occurrence) is
      use type Ada.Exceptions.Exception_Id;
   begin
      if Ada.Exceptions.Exception_Identity (This.Error) = Ada.Exceptions.Null_Id then
         Ada.Exceptions.Save_Occurrence (This.Error, Occurrence);
      end if;
   end Save;

   procedure Transfer (From : in out Holder; To : in out Holder) is
   begin
      To.Save (From.Error);
      Ada.Exceptions.Save_Occurrence (From.Error, Ada.Exceptions.Null_Occurrence);
   end Transfer;

end Prunt.Cleanup_Exceptions;
