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

package body Prunt.Generic_Lock is

   pragma Extensions_Allowed (On);

   function Lock return Lock_Holder is
   begin
      Lock_Manager.Lock;
      return (Ada.Finalization.Limited_Controlled with Already_Finalized => False);
   end Lock;

   protected body Lock_Manager is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock (Holder : in out Lock_Holder) is
      begin
         if Holder.Already_Finalized then
            return;
         end if;

         pragma Annotate (Xcov, Exempt_On, "Should be unreachable.");
         if not Locked then
            raise Program_Error with "Attempted unlock when not locked.";
         end if;
         pragma Annotate (Xcov, Exempt_Off);

         Holder.Already_Finalized := True;
         Locked := False;
      end Unlock;
   end Lock_Manager;

   overriding
   procedure Finalize (Object : in out Lock_Holder) is
   begin
      Lock_Manager.Unlock (Object);
   end Finalize;

end Prunt.Generic_Lock;
