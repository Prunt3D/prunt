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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Prunt_Simulator_Samples is

   use Ada.Streams;

   Sample_Capacity    : constant Positive := 65_536;
   Max_Response_Bytes : constant Stream_Element_Offset := 96 * 1_024 * 1_024;
   --  One-tick fifth differences amplify position roundoff enough to swamp the plotted crackle signal.
   Derivative_Step    : constant Positive := 5;
   Derivative_Period  : constant Long_Float := Sample_Period_S * Long_Float (Derivative_Step);

   type Sample is record
      Sequence     : Long_Long_Integer := 0;
      Position     : Axis_Position := [others => 0.0];
      Velocity     : Axis_Position := [others => 0.0];
      Acceleration : Axis_Position := [others => 0.0];
      Jerk         : Axis_Position := [others => 0.0];
      Snap         : Axis_Position := [others => 0.0];
      Crackle      : Axis_Position := [others => 0.0];
   end record;

   type Sample_Array is array (Positive range <>) of Sample;
   type Response_Access is access Stream_Element_Array;
   type Response_Slot_Index is mod 8;
   type Response_Slot_Array is array (Response_Slot_Index) of Response_Access;

   protected Ring_Buffer is
      procedure Reset (Position : Axis_Position);
      procedure Append (Position : Axis_Position);
      procedure Snapshot (Output : out Sample_Array; Length : out Natural);
   private
      Samples       : Sample_Array (1 .. Sample_Capacity);
      Count         : Natural := 0;
      Next          : Positive := 1;
      Next_Sequence : Long_Long_Integer := 0;
   end Ring_Buffer;

   function Axis_Label (Axis : Axis_Name) return String;
   function Image (Value : Long_Float) return String;
   procedure Append_Number (Output : in out Unbounded_String; Value : Long_Float);
   procedure Append_Sample_Number_Array
     (Output : in out Unbounded_String; Samples : Sample_Array; First : Positive; Last : Natural);
   procedure Append_Axis_Number_Arrays
     (Output : in out Unbounded_String;
      Samples : Sample_Array;
      First   : Positive;
      Last    : Natural;
      Choose  : access function (Item : Sample) return Axis_Position);
   function Position_Of (Item : Sample) return Axis_Position;
   function Velocity_Of (Item : Sample) return Axis_Position;
   function Acceleration_Of (Item : Sample) return Axis_Position;
   function Jerk_Of (Item : Sample) return Axis_Position;
   function Snap_Of (Item : Sample) return Axis_Position;
   function Crackle_Of (Item : Sample) return Axis_Position;
   function Build_JSON (Samples : Sample_Array; Count : Natural; Keep : Natural) return String;
   procedure Store_Response (JSON : String);
   procedure Free_Response is new Ada.Unchecked_Deallocation (Stream_Element_Array, Response_Access);

   Snapshot_Buffer : Sample_Array (1 .. Sample_Capacity);
   Response_Slots   : Response_Slot_Array := [others => null];
   Response_Slot    : Response_Slot_Index := 0;
   Current_Response : Response_Access := null;

   protected body Ring_Buffer is
      procedure Reset (Position : Axis_Position) is
      begin
         Count := 0;
         Next := 1;
         Next_Sequence := 0;
         Append (Position);
      end Reset;

      procedure Append (Position : Axis_Position) is
         New_Sample : Sample;

         function Previous_Index (Steps_Back : Positive) return Positive;

         function Previous_Index (Steps_Back : Positive) return Positive is
         begin
            return ((Next - 1 - Steps_Back + Sample_Capacity) mod Sample_Capacity) + 1;
         end Previous_Index;
      begin
         if Count >= Derivative_Step then
            New_Sample.Velocity :=
              [for Axis in Axis_Name =>
                 (Position (Axis) - Samples (Previous_Index (Derivative_Step)).Position (Axis))
                 / Derivative_Period];
         end if;

         if Count >= 2 * Derivative_Step then
            New_Sample.Acceleration :=
              [for Axis in Axis_Name =>
                 (New_Sample.Velocity (Axis) - Samples (Previous_Index (Derivative_Step)).Velocity (Axis))
                 / Derivative_Period];
         end if;

         if Count >= 3 * Derivative_Step then
            New_Sample.Jerk :=
              [for Axis in Axis_Name =>
                 (New_Sample.Acceleration (Axis) - Samples (Previous_Index (Derivative_Step)).Acceleration (Axis))
                 / Derivative_Period];
         end if;

         if Count >= 4 * Derivative_Step then
            New_Sample.Snap :=
              [for Axis in Axis_Name =>
                 (New_Sample.Jerk (Axis) - Samples (Previous_Index (Derivative_Step)).Jerk (Axis))
                 / Derivative_Period];
         end if;

         if Count >= 5 * Derivative_Step then
            New_Sample.Crackle :=
              [for Axis in Axis_Name =>
                 (New_Sample.Snap (Axis) - Samples (Previous_Index (Derivative_Step)).Snap (Axis))
                 / Derivative_Period];
         end if;

         New_Sample.Sequence := Next_Sequence;
         New_Sample.Position := Position;
         Samples (Next) := New_Sample;
         Next_Sequence := Next_Sequence + 1;

         if Count < Sample_Capacity then
            Count := Count + 1;
         end if;

         Next := (if Next = Sample_Capacity then 1 else Next + 1);
      end Append;

      procedure Snapshot (Output : out Sample_Array; Length : out Natural) is
         Start : constant Positive := (if Count = Sample_Capacity then Next else 1);
         Index : Positive;
      begin
         Length := Count;
         for I in 1 .. Count loop
            Index := ((Start - 1 + I - 1) mod Sample_Capacity) + 1;
            Output (I) := Samples (Index);
         end loop;
      end Snapshot;
   end Ring_Buffer;

   function Axis_Label (Axis : Axis_Name) return String is
   begin
      case Axis is
         when X_Axis =>
            return "X";
         when Y_Axis =>
            return "Y";
         when Z_Axis =>
            return "Z";
         when E_Axis =>
            return "E";
      end case;
   end Axis_Label;

   function Image (Value : Long_Float) return String is
   begin
      return Trim (Long_Float'Image (Value), Both);
   end Image;

   procedure Append_Number (Output : in out Unbounded_String; Value : Long_Float) is
   begin
      Append (Output, Image (Value));
   end Append_Number;

   procedure Append_Sample_Number_Array
     (Output : in out Unbounded_String; Samples : Sample_Array; First : Positive; Last : Natural) is
   begin
      Append (Output, "[");
      for I in First .. Last loop
         if I /= First then
            Append (Output, ",");
         end if;
         Append_Number (Output, Long_Float (Samples (I).Sequence - Samples (First).Sequence) * Sample_Period_S);
      end loop;
      Append (Output, "]");
   end Append_Sample_Number_Array;

   procedure Append_Axis_Number_Arrays
     (Output : in out Unbounded_String;
      Samples : Sample_Array;
      First   : Positive;
      Last    : Natural;
      Choose  : access function (Item : Sample) return Axis_Position)
   is
      Values : Axis_Position;
   begin
      Append (Output, "[");
      for Axis in Axis_Name loop
         if Axis /= Axis_Name'First then
            Append (Output, ",");
         end if;

         Append (Output, "[");
         for I in First .. Last loop
            if I /= First then
               Append (Output, ",");
            end if;
            Values := Choose (Samples (I));
            Append_Number (Output, Values (Axis));
         end loop;
         Append (Output, "]");
      end loop;
      Append (Output, "]");
   end Append_Axis_Number_Arrays;

   function Position_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Position;
   end Position_Of;

   function Velocity_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Velocity;
   end Velocity_Of;

   function Acceleration_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Acceleration;
   end Acceleration_Of;

   function Jerk_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Jerk;
   end Jerk_Of;

   function Snap_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Snap;
   end Snap_Of;

   function Crackle_Of (Item : Sample) return Axis_Position is
   begin
      return Item.Crackle;
   end Crackle_Of;

   function Build_JSON (Samples : Sample_Array; Count : Natural; Keep : Natural) return String is
      Output : Unbounded_String;
      First  : constant Positive := Count - Keep + 1;
      Last   : constant Natural := Count;
   begin
      Append (Output, "{""axes"":[");
      for Axis in Axis_Name loop
         if Axis /= Axis_Name'First then
            Append (Output, ",");
         end if;
         Append (Output, """" & Axis_Label (Axis) & """");
      end loop;

      Append (Output, "],""dt_s"":");
      Append_Number (Output, Sample_Period_S);
      Append (Output, ",""first_sequence"":");
      Append (Output, Long_Long_Integer'Image ((if Keep = 0 then 0 else Samples (First).Sequence)));
      Append (Output, ",""last_sequence"":");
      Append (Output, Long_Long_Integer'Image ((if Keep = 0 then -1 else Samples (Last).Sequence)));
      Append (Output, ",""samples"":");
      Append (Output, Natural'Image (Keep));

      Append (Output, ",""t"":");
      if Keep = 0 then
         Append (Output, "[]");
      else
         Append_Sample_Number_Array (Output, Samples, First, Last);
      end if;

      Append (Output, ",""position"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Position_Of'Access);
      end if;

      Append (Output, ",""velocity"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Velocity_Of'Access);
      end if;

      Append (Output, ",""acceleration"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Acceleration_Of'Access);
      end if;

      Append (Output, ",""jerk"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Jerk_Of'Access);
      end if;

      Append (Output, ",""snap"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Snap_Of'Access);
      end if;

      Append (Output, ",""crackle"":");
      if Keep = 0 then
         Append (Output, "[[],[],[],[]]");
      else
         Append_Axis_Number_Arrays (Output, Samples, First, Last, Crackle_Of'Access);
      end if;

      Append (Output, "}");
      return To_String (Output);
   end Build_JSON;

   procedure Store_Response (JSON : String) is
      Length_Offset : constant Stream_Element_Offset := Stream_Element_Offset (JSON'Length);
      Offset        : Stream_Element_Offset := 1;
   begin
      if Response_Slots (Response_Slot) /= null then
         Free_Response (Response_Slots (Response_Slot));
      end if;

      Response_Slots (Response_Slot) := new Stream_Element_Array (1 .. Length_Offset);
      for C of JSON loop
         Response_Slots (Response_Slot) (Offset) := Stream_Element (Character'Pos (C));
         Offset := Offset + 1;
      end loop;

      Current_Response := Response_Slots (Response_Slot);
      Response_Slot := Response_Slot + 1;
   end Store_Response;

   procedure Reset (Position : Axis_Position) is
   begin
      Ring_Buffer.Reset (Position);
   end Reset;

   procedure Append (Position : Axis_Position) is
   begin
      Ring_Buffer.Append (Position);
   end Append;

   function JSON_Content return access constant Stream_Element_Array is
      Count : Natural;
      Keep  : Natural;
      JSON  : Unbounded_String;
   begin
      Ring_Buffer.Snapshot (Snapshot_Buffer, Count);
      Keep := Count;

      loop
         JSON := To_Unbounded_String (Build_JSON (Snapshot_Buffer, Count, Keep));
         exit when Length (JSON) <= Natural (Max_Response_Bytes) or else Keep = 0;
         Keep := Keep / 2;
      end loop;

      Store_Response (To_String (JSON));
      return Current_Response;
   end JSON_Content;

begin
   Ring_Buffer.Reset ([others => 0.0]);
end Prunt_Simulator_Samples;
