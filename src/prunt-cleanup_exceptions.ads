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

with Ada.Exceptions;

package Prunt.Cleanup_Exceptions is

   pragma Elaborate_Body;

   type Holder is tagged limited private;
   --  Collect cleanup failures while allowing the remaining cleanup operations to run.

   procedure Save (This : in out Holder; Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Preserve the first exception; subsequent failures do not replace it.

   procedure Reraise (This : Holder);
   --  Propagate the saved exception, or do nothing if cleanup succeeded.

   procedure Transfer (From : in out Holder; To : in out Holder);
   --  Save From's exception into To as Save would, then leave From empty.

private

   function Empty_Occurrence return Ada.Exceptions.Exception_Occurrence;

   type Holder is tagged limited record
      Error : Ada.Exceptions.Exception_Occurrence := Empty_Occurrence;
   end record;

end Prunt.Cleanup_Exceptions;
