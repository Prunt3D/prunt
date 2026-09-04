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

with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with Prunt_Simulator_Hardware;
with Prunt_Simulator_Samples;

package body Prunt_Simulator_Machine is

   Maximum_Tail_Length : constant Positive := Prunt_Simulator_Types.Maximum_Loop_Move_Tail_Length;
   Queue_Capacity      : constant Positive := Maximum_Tail_Length + 1;
   Delay_Batch_Size    : constant Positive := 100;
   Unsafe_Queue_Stall_Timeout : constant Duration := 0.100;

   function Environment_Boolean (Name : String; Default : Boolean) return Boolean;

   function Environment_Boolean (Name : String; Default : Boolean) return Boolean is
      Image : constant String :=
        Ada.Characters.Handling.To_Upper
          (if Ada.Environment_Variables.Exists (Name)
           then Trim (Ada.Environment_Variables.Value (Name), Both)
           else (if Default then "TRUE" else "FALSE"));
   begin
      if Image = "TRUE" then
         return True;
      elsif Image = "FALSE" then
         return False;
      else
         raise Constraint_Error with Name & " must be either true or false.";
      end if;
   end Environment_Boolean;

   Real_Time_Simulation : constant Boolean := Environment_Boolean ("PRUNT_SIM_REALTIME", True);

   type Command_Queue_Item_Kind is (Motion_Command, Logical_Position_Reset);

   type Command_Queue_Item is record
      Kind         : Command_Queue_Item_Kind := Motion_Command;
      Command      : Queued_Command;
      Reset_Pos    : Motor_Position := [others => 0.0];
      Is_Loop_Move : Boolean := False;
      Loop_Setup   : Loop_Move_Setup := (others => <>);
   end record;

   type Command_Array is array (Positive range <>) of Command_Queue_Item;
   type Command_Array_Access is access all Command_Array;

   protected Command_Queue is
      procedure Setup_For_Loop_Move (Setup : Loop_Move_Setup);
      entry Enqueue (Command : Queued_Command);
      entry Enqueue_Reset (Pos : Motor_Position);
      entry Dequeue (Item : out Command_Queue_Item);
      procedure Clear;
   private
      Commands : Command_Array (1 .. Queue_Capacity);
      Count    : Natural := 0;
      Head     : Positive := 1;
      Tail     : Positive := 1;
      Has_Pending_Loop_Setup : Boolean := False;
      Pending_Loop_Setup     : Loop_Move_Setup := (others => <>);
   end Command_Queue;

   protected Execution_State is
      procedure Reset;
      procedure Report (Index : Command_Index);
      procedure Execute (Item : Command_Queue_Item; Physical_Pos : out Motor_Position);
      procedure Reset_Logical_Position (Pos : Motor_Position);
      procedure Set_After_Loop (Logical_Pos, Physical_Pos : Motor_Position);
      function Logical_Position return Motor_Position;
      function Physical_Position return Motor_Position;
      function Last return Command_Index;
   private
      Last_Executed : Command_Index := 0;
      Logical_Pos   : Motor_Position := [others => 0.0];
      Physical_Pos  : Motor_Position := Prunt_Simulator_Hardware.Get_Initial_Motor_Position;
   end Execution_State;

   Last_Command_Callback : Last_Command_Reporter := null;
   Error_Callback        : Error_Reporter := null;

   function Sample_Period_Delay return Duration;
   function To_Axis_Position (Pos : Motor_Position) return Prunt_Simulator_Samples.Axis_Position;
   function Current_Switch_States return Prunt_Simulator_Samples.Input_Switch_State;
   task Executor;

   protected body Command_Queue is
      procedure Setup_For_Loop_Move (Setup : Loop_Move_Setup) is
      begin
         if Has_Pending_Loop_Setup then
            raise Program_Error with "Loop move setup was not consumed by the next command.";
         end if;
         Has_Pending_Loop_Setup := True;
         Pending_Loop_Setup := Setup;
      end Setup_For_Loop_Move;

      entry Enqueue (Command : Queued_Command) when Count < Queue_Capacity is
      begin
         Commands (Tail) :=
           (Kind         => Motion_Command,
            Command      => Command,
            Reset_Pos    => [others => 0.0],
            Is_Loop_Move => Has_Pending_Loop_Setup,
            Loop_Setup   => Pending_Loop_Setup);
         Has_Pending_Loop_Setup := False;
         Pending_Loop_Setup := (others => <>);
         Tail := (if Tail = Queue_Capacity then 1 else Tail + 1);
         Count := Count + 1;
      end Enqueue;

      entry Enqueue_Reset (Pos : Motor_Position) when Count < Queue_Capacity is
      begin
         if Has_Pending_Loop_Setup then
            raise Program_Error with "A logical position reset cannot be inserted between loop setup and its command.";
         end if;

         Commands (Tail) :=
           (Kind         => Logical_Position_Reset,
            Command      => (others => <>),
            Reset_Pos    => Pos,
            Is_Loop_Move => False,
            Loop_Setup   => (others => <>));
         Tail := (if Tail = Queue_Capacity then 1 else Tail + 1);
         Count := Count + 1;
      end Enqueue_Reset;

      entry Dequeue (Item : out Command_Queue_Item) when Count > 0 is
      begin
         Item := Commands (Head);
         Head := (if Head = Queue_Capacity then 1 else Head + 1);
         Count := Count - 1;
      end Dequeue;

      procedure Clear is
      begin
         Count := 0;
         Head := 1;
         Tail := 1;
         Has_Pending_Loop_Setup := False;
         Pending_Loop_Setup := (others => <>);
      end Clear;
   end Command_Queue;

   protected body Execution_State is
      procedure Reset is
      begin
         Last_Executed := 0;
         Logical_Pos := [others => 0.0];
      end Reset;

      procedure Report (Index : Command_Index) is
      begin
         Last_Executed := Index;
      end Report;

      procedure Execute (Item : Command_Queue_Item; Physical_Pos : out Motor_Position)
      is
         Command : Queued_Command renames Item.Command;
      begin
         pragma Assert (Item.Kind = Motion_Command);
         for Motor in Prunt_Simulator_Types.Motor_Name loop
            Execution_State.Physical_Pos (Motor) :=
              @ + Command.Pos (Motor) - Logical_Pos (Motor);
         end loop;
         Logical_Pos := Command.Pos;
         Physical_Pos := Execution_State.Physical_Pos;
      end Execute;

      procedure Reset_Logical_Position (Pos : Motor_Position) is
      begin
         Logical_Pos := Pos;
      end Reset_Logical_Position;

      procedure Set_After_Loop (Logical_Pos, Physical_Pos : Motor_Position) is
      begin
         Execution_State.Logical_Pos := Logical_Pos;
         Execution_State.Physical_Pos := Physical_Pos;
      end Set_After_Loop;

      function Logical_Position return Motor_Position is
      begin
         return Logical_Pos;
      end Logical_Position;

      function Physical_Position return Motor_Position is
      begin
         return Physical_Pos;
      end Physical_Position;

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

   function Current_Switch_States return Prunt_Simulator_Samples.Input_Switch_State is
   begin
      return
        [1 => Prunt_Simulator_Hardware.Get_Input_Switch_State (Prunt_Simulator_Types.X_Endstop),
         2 => Prunt_Simulator_Hardware.Get_Input_Switch_State (Prunt_Simulator_Types.Y_Endstop),
         3 => Prunt_Simulator_Hardware.Get_Input_Switch_State (Prunt_Simulator_Types.Z_Endstop)];
   end Current_Switch_States;

   function Sample_Period_Delay return Duration is
      Period : Long_Float;
   begin
      Period := Prunt_Simulator_Samples.Sample_Period_S;
      return Duration (Period);
   end Sample_Period_Delay;

   task body Executor is
      Item          : Command_Queue_Item;
      Tail_Items    : constant Command_Array_Access := new Command_Array (1 .. Maximum_Tail_Length);
      Tail_Length   : Natural range 0 .. Maximum_Tail_Length := 0;
      Start_Logical_Pos    : Motor_Position;
      Replay_Logical_Pos   : Motor_Position;
      Replay_Physical_Pos  : Motor_Position;
      Executed_Physical_Pos : Motor_Position;
      Loop_Delta           : Motor_Position;
      Pending_Delay_Samples : Natural range 0 .. Delay_Batch_Size := 0;

      procedure Account_For_Tick;
      procedure Flush_Pending_Delay;
      procedure Dequeue_After_Unsafe_Command (Next_Item : out Command_Queue_Item);
      procedure Publish (Physical_Pos : Motor_Position; Command : Command_Index);

      procedure Account_For_Tick is
      begin
         if Real_Time_Simulation then
            Pending_Delay_Samples := @ + 1;
            if Pending_Delay_Samples = Delay_Batch_Size then
               delay Sample_Period_Delay * Pending_Delay_Samples;
               Pending_Delay_Samples := 0;
            end if;
         end if;
      end Account_For_Tick;

      procedure Flush_Pending_Delay is
      begin
         if Real_Time_Simulation and then Pending_Delay_Samples > 0 then
            delay Sample_Period_Delay * Pending_Delay_Samples;
            Pending_Delay_Samples := 0;
         end if;
      end Flush_Pending_Delay;

      procedure Dequeue_After_Unsafe_Command (Next_Item : out Command_Queue_Item) is
      begin
         select
            Command_Queue.Dequeue (Next_Item);
         or
            delay Unsafe_Queue_Stall_Timeout;
            raise Program_Error with
              "Simulator command queue stalled after a command which was not safe to stop after.";
         end select;
      end Dequeue_After_Unsafe_Command;

      procedure Publish (Physical_Pos : Motor_Position; Command : Command_Index) is
      begin
         Prunt_Simulator_Hardware.Set_Current_Motor_Position (Physical_Pos);
         if Last_Command_Callback /= null then
            Last_Command_Callback.all (Command);
         end if;
         Prunt_Simulator_Samples.Append
           (Motor_Position => To_Axis_Position (Physical_Pos),
            Command        => Command,
            Switch_State   => Current_Switch_States);
         Execution_State.Report (Command);
      end Publish;
   begin
      --  Before the first command, and after every safe stop, an empty queue means the machine is simply idle. Once an
      --  unsafe command has executed, however, a persistent gap is a hardware underflow. A short timed entry call
      --  avoids treating an ordinary producer task handoff as an underflow.
      Command_Queue.Dequeue (Item);
      loop
         if Item.Kind = Logical_Position_Reset then
            Execution_State.Reset_Logical_Position (Item.Reset_Pos);
         elsif Item.Is_Loop_Move then
            Tail_Length := 0;
            if not Item.Command.Safe_Stop_After then
               loop
                  if Tail_Length = Maximum_Tail_Length then
                     raise Constraint_Error with "Simulator loop move tail exceeds its advertised maximum length.";
                  end if;
                  Tail_Length := @ + 1;
                  Dequeue_After_Unsafe_Command (Tail_Items (Tail_Length));
                  if Tail_Items (Tail_Length).Kind /= Motion_Command then
                     raise Program_Error with "A logical position reset was queued inside a loop-move tail.";
                  end if;
                  exit when Tail_Items (Tail_Length).Command.Safe_Stop_After;
               end loop;
            end if;

            Start_Logical_Pos := Execution_State.Logical_Position;
            Replay_Logical_Pos := Start_Logical_Pos;
            Replay_Physical_Pos := Execution_State.Physical_Position;
            Loop_Delta :=
              [for Motor in Prunt_Simulator_Types.Motor_Name =>
                 Item.Command.Pos (Motor) - Start_Logical_Pos (Motor)];

            declare
               type Motor_Loop_State is (Stationary, Repeating, Tailing, Complete);
               type Motor_Loop_State_Array is array (Prunt_Simulator_Types.Motor_Name) of Motor_Loop_State;
               type Motor_Tail_Index_Array is array (Prunt_Simulator_Types.Motor_Name) of Natural
                 range 0 .. Maximum_Tail_Length;

               State                    : Motor_Loop_State_Array := [others => Stationary];
               Trigger_Logical_Position : Motor_Position := Start_Logical_Pos;
               Next_Tail_Index          : Motor_Tail_Index_Array := [others => 0];
               Loop_Count               : Loop_Move_Count := 0;
               Timed_Out                : Boolean := False;

               function Has_Moving_Motor return Boolean
               is (for some Offset of Loop_Delta => Offset /= 0.0);

               function All_Motors_Complete return Boolean
               is (for all Motor_State of State => Motor_State in Stationary | Complete);
            begin
               if not Has_Moving_Motor then
                  raise Program_Error with "Loop move has no moving motor.";
               end if;

               for Motor in Prunt_Simulator_Types.Motor_Name loop
                  if Loop_Delta (Motor) /= 0.0 then
                     State (Motor) := Repeating;
                  end if;
               end loop;

               while not All_Motors_Complete loop
                  if (for some Motor_State of State => Motor_State = Repeating) then
                     if Loop_Count = Item.Loop_Setup.Maximum_Loop_Count then
                        Timed_Out := True;
                        for Motor in Prunt_Simulator_Types.Motor_Name loop
                           if State (Motor) = Repeating then
                              Trigger_Logical_Position (Motor) := Replay_Logical_Pos (Motor);
                              if Tail_Length = 0 then
                                 State (Motor) := Complete;
                              else
                                 State (Motor) := Tailing;
                                 Next_Tail_Index (Motor) := 1;
                              end if;
                           end if;
                        end loop;
                     else
                        Loop_Count := @ + 1;
                     end if;
                  end if;

                  for Motor in Prunt_Simulator_Types.Motor_Name loop
                     case State (Motor) is
                        when Repeating                     =>
                           Replay_Logical_Pos (Motor) := @ + Loop_Delta (Motor);
                           Replay_Physical_Pos (Motor) := @ + Loop_Delta (Motor);

                        when Tailing                       =>
                           declare
                              Tail_Index          : constant Positive := Next_Tail_Index (Motor);
                              New_Logical_Position : constant Dimensionless :=
                                Trigger_Logical_Position (Motor)
                                + Tail_Items (Tail_Index).Command.Pos (Motor)
                                - Item.Command.Pos (Motor);
                           begin
                              Replay_Physical_Pos (Motor) :=
                                @ + New_Logical_Position - Replay_Logical_Pos (Motor);
                              Replay_Logical_Pos (Motor) := New_Logical_Position;
                              if Tail_Index = Tail_Length then
                                 State (Motor) := Complete;
                              else
                                 Next_Tail_Index (Motor) := @ + 1;
                              end if;
                           end;

                        when Stationary | Complete         =>
                           null;
                     end case;
                  end loop;

                  Account_For_Tick;
                  for Motor in Prunt_Simulator_Types.Motor_Name loop
                     if State (Motor) = Repeating then
                        declare
                           Condition : Prunt_Simulator_Types.Generic_Types.Stop_Condition renames
                             Item.Loop_Setup.Stop_Conditions (Motor);
                        begin
                           if Prunt_Simulator_Hardware.Get_Input_Switch_State_At_Position
                                (Condition.Input_Switch, Replay_Physical_Pos)
                              = Condition.Stop_State
                           then
                              Trigger_Logical_Position (Motor) := Replay_Logical_Pos (Motor);
                              if Tail_Length = 0 then
                                 State (Motor) := Complete;
                              else
                                 State (Motor) := Tailing;
                                 Next_Tail_Index (Motor) := 1;
                              end if;
                           end if;
                        end;
                     end if;
                  end loop;

                  Publish
                    (Replay_Physical_Pos,
                     (if All_Motors_Complete and then Tail_Length > 0
                      then Tail_Items (Tail_Length).Command.Index
                      else Item.Command.Index));
               end loop;

               if Tail_Length = 0 then
                  for Motor in Prunt_Simulator_Types.Motor_Name loop
                     pragma Assert (State (Motor) in Stationary | Complete);
                  end loop;
               end if;

               Flush_Pending_Delay;
               Execution_State.Set_After_Loop (Replay_Logical_Pos, Replay_Physical_Pos);
               if Timed_Out then
                  raise Constraint_Error with "Simulator homing detector did not trigger before timeout.";
               end if;
            end;
         else
            Account_For_Tick;
            if Item.Command.Safe_Stop_After then
               Flush_Pending_Delay;
            end if;
            Execution_State.Execute (Item, Executed_Physical_Pos);
            Publish (Executed_Physical_Pos, Item.Command.Index);
         end if;

         if Item.Kind = Logical_Position_Reset
           or else Item.Is_Loop_Move
           or else Item.Command.Safe_Stop_After
         then
            Command_Queue.Dequeue (Item);
         else
            Dequeue_After_Unsafe_Command (Item);
         end if;
      end loop;
   exception
      when Occurrence : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Prunt simulator executor error: " & Ada.Exceptions.Exception_Information (Occurrence));
         if Error_Callback /= null then
            Error_Callback.all (Occurrence, True);
         end if;
   end Executor;

   procedure Set_Reporters (Last_Command : Last_Command_Reporter; Error : Error_Reporter) is
   begin
      Last_Command_Callback := Last_Command;
      Error_Callback := Error;
   end Set_Reporters;

   procedure Enqueue_Command (Command : Queued_Command) is
   begin
      Command_Queue.Enqueue (Command);
   end Enqueue_Command;

   procedure Setup_For_Loop_Move (Setup : Loop_Move_Setup) is
   begin
      Command_Queue.Setup_For_Loop_Move (Setup);
   end Setup_For_Loop_Move;

   procedure Reset_Position (Pos : Motor_Position) is
   begin
      --  This is a coordinate reset, not a motor move. Queue it with motion so
      --  it cannot overtake commands which were already accepted.
      Command_Queue.Enqueue_Reset (Pos);
   end Reset_Position;

   procedure Wait_Until_Idle (Last_Command_Index : Command_Index) is
   begin
      while Execution_State.Last < Last_Command_Index loop
         delay 0.001;
      end loop;
   end Wait_Until_Idle;

   procedure Reset_Hardware is
      Physical_Pos : Motor_Position;
   begin
      Command_Queue.Clear;
      Execution_State.Reset;
      Physical_Pos := Execution_State.Physical_Position;
      Prunt_Simulator_Hardware.Set_Current_Motor_Position (Physical_Pos);
      Prunt_Simulator_Samples.Reset
        (Motor_Position => To_Axis_Position (Physical_Pos), Switch_State => Current_Switch_States);
   end Reset_Hardware;

end Prunt_Simulator_Machine;
