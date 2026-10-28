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

pragma Extensions_Allowed (On);

with Prunt.Controller_Generic_Types;
with Prunt.Logger;
with Prunt.Module_Types;

pragma Warnings (Off, "unit ""Prunt.Status_Manager"" is not referenced");
--  Used by child units.
with Prunt.Status_Manager;
pragma Warnings (On, "unit ""Prunt.Status_Manager"" is not referenced");

generic
   with package My_Controller_Generic_Types is new Prunt.Controller_Generic_Types (<>);
   with package My_Logger is new Prunt.Logger;
package Prunt.Default_Modules is
   pragma Warnings (Off, "use clause for package ""My_Controller_Generic_Types"" has no effect");
   --  Used by child units.
   use My_Controller_Generic_Types;
   pragma Warnings (On, "use clause for package ""My_Controller_Generic_Types"" has no effect");

   package My_Modules renames My_Controller_Generic_Types.My_Modules;

   subtype Planner_Interface is Module_Types.Planner_Interface;
end Prunt.Default_Modules;
