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

with Prunt_Simulator_Samples;

package body Prunt_Simulator_Machine is

   Queue_Capacity : constant Positive := 65_536;

   type Command_Array is array (Positive range <>) of Queued_Command;

   protected Command_Queue is
      entry Enqueue (Command : Queued_Command);
      entry Dequeue (Command : out Queued_Command);
      procedure Clear;
   private
      Commands : Command_Array (1 .. Queue_Capacity);
      Count    : Natural := 0;
      Head     : Positive := 1;
      Tail     : Positive := 1;
   end Command_Queue;

   protected Execution_State is
      procedure Reset;
      procedure Report (Index : Command_Index);
      function Last return Command_Index;
   private
      Last_Executed : Command_Index := 0;
   end Execution_State;

   Last_Command_Callback : Last_Command_Reporter := null;
   Loop_Cycles_Callback  : Loop_Cycles_Reporter := null;
   Error_Callback        : Error_Reporter := null;

   function Sample_Period_Delay return Duration;
   function To_Axis_Position (Pos : Motor_Position) return Prunt_Simulator_Samples.Axis_Position;
   task Executor;

   protected body Command_Queue is
      entry Enqueue (Command : Queued_Command) when Count < Queue_Capacity is
      begin
         Commands (Tail) := Command;
         Tail := (if Tail = Queue_Capacity then 1 else Tail + 1);
         Count := Count + 1;
      end Enqueue;

      entry Dequeue (Command : out Queued_Command) when Count > 0 is
      begin
         Command := Commands (Head);
         Head := (if Head = Queue_Capacity then 1 else Head + 1);
         Count := Count - 1;
      end Dequeue;

      procedure Clear is
      begin
         Count := 0;
         Head := 1;
         Tail := 1;
      end Clear;
   end Command_Queue;

   protected body Execution_State is
      procedure Reset is
      begin
         Last_Executed := 0;
      end Reset;

      procedure Report (Index : Command_Index) is
      begin
         Last_Executed := Index;
      end Report;

      function Last return Command_Index is
      begin
         return Last_Executed;
      end Last;
   end Execution_State;

   function To_Axis_Position (Pos : Motor_Position) return Prunt_Simulator_Samples.Axis_Position is
   begin
      return
        [Prunt_Simulator_Samples.X_Axis => Long_Float (Pos (Prunt_Simulator_Types.X_Motor)),
         Prunt_Simulator_Samples.Y_Axis => Long_Float (Pos (Prunt_Simulator_Types.Y_Motor)),
         Prunt_Simulator_Samples.Z_Axis => Long_Float (Pos (Prunt_Simulator_Types.Z_Motor)),
         Prunt_Simulator_Samples.E_Axis => Long_Float (Pos (Prunt_Simulator_Types.E_Motor))];
   end To_Axis_Position;

   function Sample_Period_Delay return Duration is
      Period : Long_Float;
   begin
      Period := Prunt_Simulator_Samples.Sample_Period_S;
      return Duration (Period);
   end Sample_Period_Delay;

   task body Executor is
      Command : Queued_Command;
   begin
      loop
         Command_Queue.Dequeue (Command);
         delay Sample_Period_Delay;
         Prunt_Simulator_Samples.Append (To_Axis_Position (Command.Pos));
         if Command.Loop_Until_Hit and then Loop_Cycles_Callback /= null then
            Loop_Cycles_Callback.all (Command.Index, 1.0);
         end if;
         if Last_Command_Callback /= null then
            Last_Command_Callback.all (Command.Index);
         end if;
         Execution_State.Report (Command.Index);
      end loop;
   exception
      when Occurrence : others =>
         if Error_Callback /= null then
            Error_Callback.all (Occurrence, True);
         end if;
   end Executor;

   procedure Set_Reporters
     (Last_Command : Last_Command_Reporter; Loop_Cycles : Loop_Cycles_Reporter; Error : Error_Reporter) is
   begin
      Last_Command_Callback := Last_Command;
      Loop_Cycles_Callback := Loop_Cycles;
      Error_Callback := Error;
   end Set_Reporters;

   procedure Enqueue_Command (Command : Queued_Command) is
   begin
      Command_Queue.Enqueue (Command);
   end Enqueue_Command;

   procedure Reset_Position (Pos : Motor_Position) is
   begin
      pragma Unreferenced (Pos);
      null;
   end Reset_Position;

   procedure Wait_Until_Idle (Last_Command_Index : Command_Index) is
   begin
      while Execution_State.Last < Last_Command_Index loop
         delay 0.001;
      end loop;
   end Wait_Until_Idle;

   procedure Reset_Hardware is
   begin
      Command_Queue.Clear;
      Execution_State.Reset;
      Prunt_Simulator_Samples.Reset ([others => 0.0]);
   end Reset_Hardware;

end Prunt_Simulator_Machine;
