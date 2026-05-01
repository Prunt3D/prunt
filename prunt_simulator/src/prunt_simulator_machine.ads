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
with Prunt; use Prunt;
with Prunt_Simulator_Types;

package Prunt_Simulator_Machine is

   subtype Queued_Command is Prunt_Simulator_Types.Generic_Types.Queued_Command;
   subtype Motor_Position is Prunt_Simulator_Types.Generic_Types.Motor_Position;

   type Last_Command_Reporter is access procedure (Index : Command_Index);
   type Loop_Cycles_Reporter is access procedure (Index : Command_Index; Cycles : Dimensionless);
   type Error_Reporter is access procedure
     (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean);

   procedure Set_Reporters
     (Last_Command : Last_Command_Reporter; Loop_Cycles : Loop_Cycles_Reporter; Error : Error_Reporter);

   procedure Enqueue_Command (Command : Queued_Command);
   procedure Reset_Position (Pos : Motor_Position);
   procedure Wait_Until_Idle (Last_Command_Index : Command_Index);
   procedure Reset_Hardware;

end Prunt_Simulator_Machine;
