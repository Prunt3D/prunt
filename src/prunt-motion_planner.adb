-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body Prunt.Motion_Planner is

   type Feedrate_Profile_Stage_Index is range 1 .. 15;

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time     := Profile (1);
      T2 : constant Time     := Profile (2);
      T3 : constant Time     := Profile (3);
      T4 : constant Time     := Profile (4);
      Cm : constant Crackle  := Max_Crackle;
      Vs : constant Velocity := Start_Vel;
   begin
      return
        (Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4) / 2.0) *
        (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
      --  Symbolically equivalent to: return Distance_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   end Fast_Distance_At_Max_Time;

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time     := Profile (1);
      T2 : constant Time     := Profile (2);
      T3 : constant Time     := Profile (3);
      T4 : constant Time     := Profile (4);
      Cm : constant Crackle  := Max_Crackle;
      Vs : constant Velocity := Start_Vel;
   begin
      return Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4);
      --  Symbolically equivalent to: return Velocity_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   end Fast_Velocity_At_Max_Time;

   function Total_Time (Times : Feedrate_Profile_Times) return Time is
   begin
      return 8.0 * Times (1) + 4.0 * Times (2) + 2.0 * Times (3) + Times (4);
   end Total_Time;

   function Total_Time (Profile : Feedrate_Profile) return Time is
   begin
      return
        Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel) +
        Profile.End_Coast;
   end Total_Time;

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Cm;
      elsif T < T1 + T2 then
         return 0.0 * mm / s**5;
      elsif T < 2.0 * T1 + T2 then
         return -Cm;
      elsif T < 2.0 * T1 + T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 3.0 * T1 + T2 + T3 then
         return -Cm;
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Cm;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return -Cm;
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Cm;
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Cm;
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      else
         return -Cm;
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Snap_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Snap is
      begin
         case Stage is
            when 1 =>
               return Cm * DT;
            when 2 =>
               return Snap_At_Stage (T1, 1);
            when 3 =>
               return Snap_At_Stage (T2, 2) - Cm * DT;
            when 4 =>
               return Snap_At_Stage (T1, 3);
            when 5 =>
               return Snap_At_Stage (T3, 4) - Cm * DT;
            when 6 =>
               return Snap_At_Stage (T1, 5);
            when 7 =>
               return Snap_At_Stage (T2, 6) + Cm * DT;
            when 8 =>
               return Snap_At_Stage (T1, 7);
            when 9 =>
               return Snap_At_Stage (T4, 8) - Cm * DT;
            when 10 =>
               return Snap_At_Stage (T1, 9);
            when 11 =>
               return Snap_At_Stage (T2, 10) + Cm * DT;
            when 12 =>
               return Snap_At_Stage (T1, 11);
            when 13 =>
               return Snap_At_Stage (T3, 12) + Cm * DT;
            when 14 =>
               return Snap_At_Stage (T1, 13);
            when 15 =>
               return Snap_At_Stage (T2, 14) - Cm * DT;
         end case;
      end Snap_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Snap_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Snap_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Snap_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Snap_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Jerk_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Jerk is
      begin
         case Stage is
            when 1 =>
               return Cm * DT**2 / 2.0;
            when 2 =>
               return Jerk_At_Stage (T1, 1) + Cm * DT * T1;
            when 3 =>
               return Jerk_At_Stage (T2, 2) + Cm * DT * (-DT + 2.0 * T1) / 2.0;
            when 4 =>
               return Jerk_At_Stage (T1, 3);
            when 5 =>
               return Jerk_At_Stage (T3, 4) - Cm * DT**2 / 2.0;
            when 6 =>
               return Jerk_At_Stage (T1, 5) - Cm * DT * T1;
            when 7 =>
               return Jerk_At_Stage (T2, 6) + Cm * DT * (DT - 2.0 * T1) / 2.0;
            when 8 =>
               return Jerk_At_Stage (T1, 7);
            when 9 =>
               return Jerk_At_Stage (T4, 8) - Cm * DT**2 / 2.0;
            when 10 =>
               return Jerk_At_Stage (T1, 9) - Cm * DT * T1;
            when 11 =>
               return Jerk_At_Stage (T2, 10) + Cm * DT * (DT - 2.0 * T1) / 2.0;
            when 12 =>
               return Jerk_At_Stage (T1, 11);
            when 13 =>
               return Jerk_At_Stage (T3, 12) + Cm * DT**2 / 2.0;
            when 14 =>
               return Jerk_At_Stage (T1, 13) + Cm * DT * T1;
            when 15 =>
               return Jerk_At_Stage (T2, 14) + Cm * DT * (-DT + 2.0 * T1) / 2.0;
         end case;
      end Jerk_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Jerk_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Jerk_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Jerk_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Jerk_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration
   is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Acceleration_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Acceleration is
      begin
         case Stage is
            when 1 =>
               return Cm * DT**3 / 6.0;
            when 2 =>
               return Acceleration_At_Stage (T1, 1) + Cm * DT * T1 * (DT + T1) / 2.0;
            when 3 =>
               return
                 Acceleration_At_Stage (T2, 2) + Cm * DT * (-DT**2 + 3.0 * DT * T1 + 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;
            when 4 =>
               return Acceleration_At_Stage (T1, 3) + Cm * DT * T1 * (T1 + T2);
            when 5 =>
               return Acceleration_At_Stage (T3, 4) + Cm * DT * (-DT**2 + 6.0 * T1 * (T1 + T2)) / 6.0;
            when 6 =>
               return Acceleration_At_Stage (T1, 5) + Cm * DT * T1 * (-DT + T1 + 2.0 * T2) / 2.0;
            when 7 =>
               return Acceleration_At_Stage (T2, 6) + Cm * DT * (DT**2 - 3.0 * DT * T1 + 3.0 * T1**2) / 6.0;
            when 8 =>
               return Acceleration_At_Stage (T1, 7);
            when 9 =>
               return Acceleration_At_Stage (T4, 8) - Cm * DT**3 / 6.0;
            when 10 =>
               return Acceleration_At_Stage (T1, 9) + Cm * DT * T1 * (-DT - T1) / 2.0;
            when 11 =>
               return
                 Acceleration_At_Stage (T2, 10) + Cm * DT * (DT**2 - 3.0 * DT * T1 - 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;
            when 12 =>
               return Acceleration_At_Stage (T1, 11) - Cm * DT * T1 * (T1 + T2);
            when 13 =>
               return Acceleration_At_Stage (T3, 12) + Cm * DT * (DT**2 - 6.0 * T1 * (T1 + T2)) / 6.0;
            when 14 =>
               return Acceleration_At_Stage (T1, 13) + Cm * DT * T1 * (DT - T1 - 2.0 * T2) / 2.0;
            when 15 =>
               return Acceleration_At_Stage (T2, 14) + Cm * DT * (-DT**2 + 3.0 * DT * T1 - 3.0 * T1**2) / 6.0;
         end case;
      end Acceleration_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Acceleration_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Acceleration_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Acceleration_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Acceleration_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Velocity_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Velocity is
      begin
         case Stage is
            when 1 =>
               return Start_Vel + Cm * DT**4 / 24.0;
            when 2 =>
               return Velocity_At_Stage (T1, 1) + Cm * DT * T1 * (2.0 * DT**2 + 3.0 * DT * T1 + 2.0 * T1**2) / 12.0;
            when 3 =>
               return
                 Velocity_At_Stage (T2, 2) +
                 Cm * DT *
                   (-DT**3 + 4.0 * DT**2 * T1 + 6.0 * DT * T1 * (T1 + 2.0 * T2) +
                    4.0 * T1 * (T1**2 + 3.0 * T1 * T2 + 3.0 * T2**2)) /
                   24.0;
            when 4 =>
               return
                 Velocity_At_Stage (T1, 3) +
                 Cm * DT * T1 * (DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 2.0;
            when 5 =>
               return
                 Velocity_At_Stage (T3, 4) +
                 Cm * DT *
                   (-DT**3 + 12.0 * DT * T1 * (T1 + T2) +
                    12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3)) /
                   24.0;
            when 6 =>
               return
                 Velocity_At_Stage (T1, 5) +
                 Cm * DT * T1 *
                   (-2.0 * DT**2 + 3.0 * DT * (T1 + 2.0 * T2) + 22.0 * T1**2 + 30.0 * T1 * T2 + 12.0 * T1 * T3 +
                    6.0 * T2**2 + 12.0 * T2 * T3) /
                   12.0;
            when 7 =>
               return
                 Velocity_At_Stage (T2, 6) +
                 Cm * DT *
                   (DT**3 - 4.0 * DT**2 * T1 + 6.0 * DT * T1**2 +
                    4.0 * T1 * (11.0 * T1**2 + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * T2**2 + 6.0 * T2 * T3)) /
                   24.0;
            when 8 =>
               return
                 Velocity_At_Stage (T1, 7) + Cm * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3);
            when 9 =>
               return
                 Velocity_At_Stage (T4, 8) +
                 Cm * DT * (-DT**3 + 24.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3)) / 24.0;
            when 10 =>
               return
                 Velocity_At_Stage (T1, 9) +
                 Cm * DT * T1 *
                   (-2.0 * DT**2 - 3.0 * DT * T1 + 22.0 * T1**2 + 36.0 * T1 * T2 + 12.0 * T1 * T3 + 12.0 * T2**2 +
                    12.0 * T2 * T3) /
                   12.0;
            when 11 =>
               return
                 Velocity_At_Stage (T2, 10) +
                 Cm * DT *
                   (DT**3 - 4.0 * DT**2 * T1 - 6.0 * DT * T1 * (T1 + 2.0 * T2) +
                    4.0 * T1 * (11.0 * T1**2 + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * T2**2 + 6.0 * T2 * T3)) /
                   24.0;
            when 12 =>
               return
                 Velocity_At_Stage (T1, 11) +
                 Cm * DT * T1 *
                   (-DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3) / 2.0;
            when 13 =>
               return
                 Velocity_At_Stage (T3, 12) +
                 Cm * DT * (DT**3 - 12.0 * DT * T1 * (T1 + T2) + 12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T2**2)) /
                   24.0;
            when 14 =>
               return
                 Velocity_At_Stage (T1, 13) +
                 Cm * DT * T1 *
                   (2.0 * DT**2 - 3.0 * DT * (T1 + 2.0 * T2) + 2.0 * T1**2 + 6.0 * T1 * T2 + 6.0 * T2**2) / 12.0;
            when 15 =>
               return
                 Velocity_At_Stage (T2, 14) +
                 Cm * DT * (-DT**3 + 4.0 * DT**2 * T1 - 6.0 * DT * T1**2 + 4.0 * T1**3) / 24.0;
         end case;
      end Velocity_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Velocity_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Velocity_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Velocity_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Velocity_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Velocity_At_Time;

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time    := Profile (1);
      T2 : constant Time    := Profile (2);
      T3 : constant Time    := Profile (3);
      T4 : constant Time    := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Distance_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Length is
      begin
         case Stage is
            when 1 =>
               return Start_Vel * T + Cm * DT**5 / 120.0;
            when 2 =>
               return
                 Distance_At_Stage (T1, 1) +
                 Cm * DT * T1 * (DT**3 + 2.0 * DT**2 * T1 + 2.0 * DT * T1**2 + T1**3) / 24.0;
            when 3 =>
               return
                 Distance_At_Stage (T2, 2) +
                 Cm * DT *
                   (-DT**4 + 5.0 * DT**3 * T1 + 10.0 * DT**2 * T1 * (T1 + 2.0 * T2) +
                    10.0 * DT * T1 * (T1**2 + 3.0 * T1 * T2 + 3.0 * T2**2) +
                    5.0 * T1 * (T1**3 + 4.0 * T1**2 * T2 + 6.0 * T1 * T2**2 + 4.0 * T2**3)) /
                   120.0;
            when 4 =>
               return
                 Distance_At_Stage (T1, 3) +
                 Cm * DT * T1 *
                   (2.0 * DT**2 * (T1 + T2) + 3.0 * DT * (2.0 * T1**2 + 3.0 * T1 * T2 + T2**2) + 7.0 * T1**3 +
                    14.0 * T1**2 * T2 + 9.0 * T1 * T2**2 + 2.0 * T2**3) /
                   12.0;
            when 5 =>
               return
                 Distance_At_Stage (T3, 4) +
                 Cm * DT *
                   (-DT**4 + 20.0 * DT**2 * T1 * (T1 + T2) +
                    30.0 * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3) +
                    10.0 * T1 *
                      (7.0 * T1**3 + 14.0 * T1**2 * T2 + 12.0 * T1**2 * T3 + 9.0 * T1 * T2**2 + 18.0 * T1 * T2 * T3 +
                       6.0 * T1 * T3**2 + 2.0 * T2**3 + 6.0 * T2**2 * T3 + 6.0 * T2 * T3**2)) /
                   120.0;
            when 6 =>
               return
                 Distance_At_Stage (T1, 5) +
                 Cm * DT * T1 *
                   (-DT**3 + 2.0 * DT**2 * (T1 + 2.0 * T2) +
                    2.0 * DT * (11.0 * T1**2 + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * T2**2 + 6.0 * T2 * T3) +
                    49.0 * T1**3 + 76.0 * T1**2 * T2 + 48.0 * T1**2 * T3 + 30.0 * T1 * T2**2 + 60.0 * T1 * T2 * T3 +
                    12.0 * T1 * T3**2 + 4.0 * T2**3 + 12.0 * T2**2 * T3 + 12.0 * T2 * T3**2) /
                   24.0;
            when 7 =>
               return
                 Distance_At_Stage (T2, 6) +
                 Cm * DT *
                   (DT**4 - 5.0 * DT**3 * T1 + 10.0 * DT**2 * T1**2 +
                    10.0 * DT * T1 * (11.0 * T1**2 + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * T2**2 + 6.0 * T2 * T3) +
                    5.0 * T1 *
                      (49.0 * T1**3 + 120.0 * T1**2 * T2 + 48.0 * T1**2 * T3 + 96.0 * T1 * T2**2 +
                       84.0 * T1 * T2 * T3 + 12.0 * T1 * T3**2 + 24.0 * T2**3 + 36.0 * T2**2 * T3 +
                       12.0 * T2 * T3**2)) /
                   120.0;
            when 8 =>
               return
                 Distance_At_Stage (T1, 7) +
                 Cm * DT * T1 *
                   (DT * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3) + 8.0 * T1**3 + 16.0 * T1**2 * T2 +
                    6.0 * T1**2 * T3 + 10.0 * T1 * T2**2 + 9.0 * T1 * T2 * T3 + T1 * T3**2 + 2.0 * T2**3 +
                    3.0 * T2**2 * T3 + T2 * T3**2) /
                   2.0;
            when 9 =>
               return
                 Distance_At_Stage (T4, 8) +
                 Cm * DT *
                   (-DT**4 + 60.0 * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3) +
                    60.0 * T1 *
                      (8.0 * T1**3 + 16.0 * T1**2 * T2 + 6.0 * T1**2 * T3 + 4.0 * T1**2 * T4 + 10.0 * T1 * T2**2 +
                       9.0 * T1 * T2 * T3 + 6.0 * T1 * T2 * T4 + T1 * T3**2 + 2.0 * T1 * T3 * T4 + 2.0 * T2**3 +
                       3.0 * T2**2 * T3 + 2.0 * T2**2 * T4 + T2 * T3**2 + 2.0 * T2 * T3 * T4)) /
                   120.0;
            when 10 =>
               return
                 Distance_At_Stage (T1, 9) +
                 Cm * DT * T1 *
                   (-DT**3 - 2.0 * DT**2 * T1 +
                    2.0 * DT * (11.0 * T1**2 + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * T2**2 + 6.0 * T2 * T3) +
                    143.0 * T1**3 + 264.0 * T1**2 * T2 + 96.0 * T1**2 * T3 + 48.0 * T1**2 * T4 + 144.0 * T1 * T2**2 +
                    132.0 * T1 * T2 * T3 + 72.0 * T1 * T2 * T4 + 12.0 * T1 * T3**2 + 24.0 * T1 * T3 * T4 +
                    24.0 * T2**3 + 36.0 * T2**2 * T3 + 24.0 * T2**2 * T4 + 12.0 * T2 * T3**2 + 24.0 * T2 * T3 * T4) /
                   24.0;
            when 11 =>
               return
                 Distance_At_Stage (T2, 10) +
                 Cm * DT *
                   (DT**4 - 5.0 * DT**3 * T1 - 10.0 * DT**2 * T1 * (T1 + 2.0 * T2) +
                    10.0 * DT * T1 * (11.0 * T1**2 + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * T2**2 + 6.0 * T2 * T3) +
                    5.0 * T1 *
                      (143.0 * T1**3 + 308.0 * T1**2 * T2 + 96.0 * T1**2 * T3 + 48.0 * T1**2 * T4 +
                       210.0 * T1 * T2**2 + 156.0 * T1 * T2 * T3 + 72.0 * T1 * T2 * T4 + 12.0 * T1 * T3**2 +
                       24.0 * T1 * T3 * T4 + 44.0 * T2**3 + 60.0 * T2**2 * T3 + 24.0 * T2**2 * T4 + 12.0 * T2 * T3**2 +
                       24.0 * T2 * T3 * T4)) /
                   120.0;
            when 12 =>
               return
                 Distance_At_Stage (T1, 11) +
                 Cm * DT * T1 *
                   (-2.0 * DT**2 * (T1 + T2) +
                    3.0 * DT * (2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3) + 89.0 * T1**3 +
                    178.0 * T1**2 * T2 + 60.0 * T1**2 * T3 + 24.0 * T1**2 * T4 + 111.0 * T1 * T2**2 +
                    90.0 * T1 * T2 * T3 + 36.0 * T1 * T2 * T4 + 6.0 * T1 * T3**2 + 12.0 * T1 * T3 * T4 + 22.0 * T2**3 +
                    30.0 * T2**2 * T3 + 12.0 * T2**2 * T4 + 6.0 * T2 * T3**2 + 12.0 * T2 * T3 * T4) /
                   12.0;
            when 13 =>
               return
                 Distance_At_Stage (T3, 12) +
                 Cm * DT *
                   (DT**4 - 20.0 * DT**2 * T1 * (T1 + T2) + 30.0 * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T2**2) +
                    10.0 * T1 *
                      (89.0 * T1**3 + 178.0 * T1**2 * T2 + 72.0 * T1**2 * T3 + 24.0 * T1**2 * T4 + 111.0 * T1 * T2**2 +
                       108.0 * T1 * T2 * T3 + 36.0 * T1 * T2 * T4 + 12.0 * T1 * T3**2 + 12.0 * T1 * T3 * T4 +
                       22.0 * T2**3 + 36.0 * T2**2 * T3 + 12.0 * T2**2 * T4 + 12.0 * T2 * T3**2 +
                       12.0 * T2 * T3 * T4)) /
                   120.0;
            when 14 =>
               return
                 Distance_At_Stage (T1, 13) +
                 Cm * DT * T1 *
                   (DT**3 - 2.0 * DT**2 * (T1 + 2.0 * T2) + 2.0 * DT * (T1**2 + 3.0 * T1 * T2 + 3.0 * T2**2) +
                    191.0 * T1**3 + 380.0 * T1**2 * T2 + 144.0 * T1**2 * T3 + 48.0 * T1**2 * T4 + 234.0 * T1 * T2**2 +
                    216.0 * T1 * T2 * T3 + 72.0 * T1 * T2 * T4 + 24.0 * T1 * T3**2 + 24.0 * T1 * T3 * T4 +
                    44.0 * T2**3 + 72.0 * T2**2 * T3 + 24.0 * T2**2 * T4 + 24.0 * T2 * T3**2 + 24.0 * T2 * T3 * T4) /
                   24.0;
            when 15 =>
               return
                 Distance_At_Stage (T2, 14) +
                 Cm * DT *
                   (-DT**4 + 5.0 * DT**3 * T1 - 10.0 * DT**2 * T1**2 + 10.0 * DT * T1**3 +
                    5.0 * T1 *
                      (191.0 * T1**3 + 384.0 * T1**2 * T2 + 144.0 * T1**2 * T3 + 48.0 * T1**2 * T4 +
                       240.0 * T1 * T2**2 + 216.0 * T1 * T2 * T3 + 72.0 * T1 * T2 * T4 + 24.0 * T1 * T3**2 +
                       24.0 * T1 * T3 * T4 + 48.0 * T2**3 + 72.0 * T2**2 * T3 + 24.0 * T2**2 * T4 + 24.0 * T2 * T3**2 +
                       24.0 * T2 * T3 * T4)) /
                   120.0;
         end case;
      end Distance_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Distance_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Distance_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Distance_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Distance_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Distance_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Distance_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Distance_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Distance_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Distance_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Distance_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Distance_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Distance_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Distance_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Distance_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Distance_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Distance_At_Time;

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         return 0.0 * mm / s**5;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         return Crackle_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         return 0.0 * mm / s**5;
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel) then
         return 0.0 * mm / s**5;
      else
         return
           Crackle_At_Time
             (Profile.Decel,
              T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast),
              -Max_Crackle);
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         return 0.0 * mm / s**4;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         return Snap_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         return 0.0 * mm / s**4;
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel) then
         return 0.0 * mm / s**4;
      else
         return
           Snap_At_Time
             (Profile.Decel, T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast), -Max_Crackle);
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         return 0.0 * mm / s**3;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         return Jerk_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         return 0.0 * mm / s**3;
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel) then
         return 0.0 * mm / s**3;
      else
         return
           Jerk_At_Time
             (Profile.Decel,
              T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast),
              -Max_Crackle);
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration
   is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         return 0.0 * mm / s**2;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         return Acceleration_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         return 0.0 * mm / s**2;
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel) then
         return 0.0 * mm / s**2;
      else
         return
           Acceleration_At_Time
             (Profile.Decel,
              T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast),
              -Max_Crackle);
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      Mid_Vel : constant Velocity :=
        Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         return Start_Vel;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         return Velocity_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle, Start_Vel);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         return Mid_Vel;
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel)
      then
         return Velocity_At_Time (Profile.Decel, Total_Time (Profile.Decel), -Max_Crackle, Mid_Vel);
      else
         return
           Velocity_At_Time
             (Profile.Decel,
              T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast),
              -Max_Crackle,
              Mid_Vel);
      end if;
   end Velocity_At_Time;

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      Is_Past_Accel_Part : Boolean;
   begin
      return Distance_At_Time (Profile, T, Max_Crackle, Start_Vel, Is_Past_Accel_Part);
   end Distance_At_Time;

   function Distance_At_Time
     (Profile            :     Feedrate_Profile;
      T                  :     Time;
      Max_Crackle        :     Crackle;
      Start_Vel          :     Velocity;
      Is_Past_Accel_Part : out Boolean)
      return Length
   is
      Start_Dist : constant Length   := Profile.Start_Coast * Start_Vel;
      Mid_Vel    : constant Velocity :=
        Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      Accel_Dist : constant Length   :=
        Distance_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      Mid_Dist   : constant Length   := Mid_Vel * Profile.Mid_Coast;
      Decel_Dist : constant Length   :=
        Distance_At_Time (Profile.Decel, Total_Time (Profile.Decel), -Max_Crackle, Mid_Vel);
      End_Vel    : constant Velocity :=
        Velocity_At_Time (Profile.Decel, Total_Time (Profile.Decel), -Max_Crackle, Mid_Vel);
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < Profile.Start_Coast then
         Is_Past_Accel_Part := False;
         return Start_Vel * T;
      elsif T <= Profile.Start_Coast + Total_Time (Profile.Accel) then
         Is_Past_Accel_Part := False;
         return Start_Dist + Distance_At_Time (Profile.Accel, T - Profile.Start_Coast, Max_Crackle, Start_Vel);
      elsif T < Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast then
         Is_Past_Accel_Part := True;
         return Start_Dist + Accel_Dist + Mid_Vel * (T - (Profile.Start_Coast + Total_Time (Profile.Accel)));
      elsif T > Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel)
      then
         Is_Past_Accel_Part := True;
         return
           Start_Dist + Accel_Dist + Mid_Dist + Decel_Dist +
           End_Vel *
             (T -
              (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast + Total_Time (Profile.Decel)));
      else
         Is_Past_Accel_Part := True;
         return
           Start_Dist + Accel_Dist + Mid_Dist +
           Distance_At_Time
             (Profile.Decel, T - (Profile.Start_Coast + Total_Time (Profile.Accel) + Profile.Mid_Coast),
              -Max_Crackle, Mid_Vel);
      end if;
   end Distance_At_Time;

   function Optimal_Profile_For_Distance
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle)
      return Feedrate_Profile_Times
   is
      D     : constant Length       := Distance;
      Vs    : constant Velocity     := Start_Vel;
      Am    : constant Acceleration := Acceleration_Max;
      Jm    : constant Jerk         := Jerk_Max;
      Sm    : constant Snap         := Snap_Max;
      Cm    : constant Crackle      := Crackle_Max;
      Cases : array (Feedrate_Profile_Times_Index) of Feedrate_Profile_Times;

      function Solve_Distance_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Feedrate_Profile_Times
      is
         Result : Feedrate_Profile_Times := Profile;

         Lower : Time := 0.0 * s;
         Upper : Time := 86_400.0 * s;
         --  A maximum of 24 hours should be more than enough.

         type Casted_Time is mod 2**64;
         function Cast_Time is new Ada.Unchecked_Conversion (Time, Casted_Time);
         function Cast_Time is new Ada.Unchecked_Conversion (Casted_Time, Time);
      begin
         --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check for
         --  that.
         pragma Assert (Time'Size = 64);
         pragma Assert (Casted_Time'Size = 64);
         pragma Assert (Cast_Time (86_400.0 * s) = 4_680_673_776_000_565_248);
         pragma Assert (Cast_Time (0.123_45 * s) = 4_593_559_930_647_147_132);

         loop
            Result (Variable) := Cast_Time (Cast_Time (Lower) + (Cast_Time (Upper) - Cast_Time (Lower)) / 2);
            exit when Lower = Result (Variable) or Upper = Result (Variable);
            if Fast_Distance_At_Max_Time (Result, Cm, Vs) <= D then
               Lower := Result (Variable);
            else
               Upper := Result (Variable);
            end if;
         end loop;

         return Result;
      end Solve_Distance_At_Time;

   begin
      --!pp off
      if Sm**2 < Jm * Cm then
         if Am >= Jm * (Jm / Sm + Sm / Cm) then
            Cases :=
            [
               --  Reachable: Sm, Jm, Am
               4 => [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s],
               --  Reachable: Sm, Jm
               3 => [Sm / Cm, Jm / Sm - Sm / Cm, 0.0 * s, 0.0 * s],
               --  Reachable: Sm
               2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]
            ];
         elsif Am >= 2.0 * Sm**3 / Cm**2 then
            Cases :=
            [
               --  Reachable: Sm, Am
               4 => [Sm / Cm, (0.25 * Sm**2 / Cm**2 + Am / Sm)**(1 / 2) - 1.5 * Sm / Cm, 0.0 * s, 0.0 * s],
               --  Impossible case.
               3 => [Sm / Cm, (0.25 * Sm**2 / Cm**2 + Am / Sm)**(1 / 2) - 1.5 * Sm / Cm, 0.0 * s, 0.0 * s],
               --  Reachable: Sm
               2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]
            ];
         else
            Cases :=
            [
               --  Reachable: Am
               4 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               3 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]
            ];
         end if;
      else
         if Am > 2.0 * Jm * (Jm / Cm)**(1 / 2) then
            Cases :=
            [
               --  Reachable: Jm, Am
               4 => [(Jm / Cm)**(1 / 2), 0.0 * s, Am / Jm - 2.0 * (Jm / Cm)**(1 / 2), 0.0 * s],
               --  Reachable: Jm
               3 => [(Jm / Cm)**(1 / 2), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [(Jm / Cm)**(1 / 2), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]
            ];
         else
            Cases :=
            [
               --  Reachable: Am
               4 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               3 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]
            ];
         end if;
      end if;
      --!pp on

      for I in reverse Cases'Range loop
         if I = Cases'First or D > Fast_Distance_At_Max_Time (Cases (I), Cm, Vs) then
            return Solve_Distance_At_Time (Cases (I), I);
            --  There are simple analytical solutions for a lot of these, but this is already fast so there is no
            --  reason to optimise it.
         end if;
      end loop;

      --  Unreachable.
      raise Program_Error;

   end Optimal_Profile_For_Distance;
   function Optimal_Profile_For_Delta_V
     (Delta_V          : Velocity;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle)
      return Feedrate_Profile_Times
   is
      Vd : constant Velocity     := abs Delta_V;
      Am : constant Acceleration := Acceleration_Max;
      Jm : constant Jerk         := Jerk_Max;
      Sm : constant Snap         := Snap_Max;
      Cm : constant Crackle      := Crackle_Max;

      function Solve_Velocity_At_Time
        (Profile  : Feedrate_Profile_Times;
         Variable : Feedrate_Profile_Times_Index;
         Target   : Velocity)
         return Feedrate_Profile_Times
      is
         Result : Feedrate_Profile_Times := Profile;

         Lower : Time := 0.0 * s;
         Upper : Time := 86_400.0 * s;
         --  A maximum of 24 hours should be more than enough.

         type Casted_Time is mod 2**64;
         function Cast_Time is new Ada.Unchecked_Conversion (Time, Casted_Time);
         function Cast_Time is new Ada.Unchecked_Conversion (Casted_Time, Time);
      begin
         --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check for
         --  that.
         pragma Assert (Time'Size = 64);
         pragma Assert (Casted_Time'Size = 64);
         pragma Assert (Cast_Time (86_400.0 * s) = 4_680_673_776_000_565_248);
         pragma Assert (Cast_Time (0.123_45 * s) = 4_593_559_930_647_147_132);

         loop
            Result (Variable) := Cast_Time (Cast_Time (Lower) + (Cast_Time (Upper) - Cast_Time (Lower)) / 2);
            exit when Lower = Result (Variable) or Upper = Result (Variable);
            if Fast_Velocity_At_Max_Time (Result, Cm, 0.0 * mm / s) <= Target then
               Lower := Result (Variable);
            else
               Upper := Result (Variable);
            end if;
         end loop;

         return Result;
      end Solve_Velocity_At_Time;
   begin
      --  This function is called a lot more than Optimal_Profile_For_Distance, so we use simple analytical solutions
      --  where they exist. In the one case where we resort to Solve_Velocity_At_Time, the analytical solution that
      --  Mathematica outputs involves a Cm**18, which is far outside the range of Dimensioned_Float for reasonable
      --  values of Cm.
      --
      --  For reference:
      --  ToRadicals[
      --    Solve[
      --      With[
      --        {T1 = Sm/Cm, T3 = 0, T4 = 0},
      --        v == Cm*T1*(T1 + T2)*(2*T1 + T2 + T3)*(4*T1 + 2*T2 + T3 + T4)
      --      ],
      --      T2,
      --      NonNegativeReals
      --    ]
      --  ]
      if Sm**2 < Jm * Cm then
         if Am >= Jm * (Jm / Sm + Sm / Cm) then
            if Vd > Am * (Am / Jm + Jm / Sm + Sm / Cm) then
               --  Reachable: Sm, Jm, Am
               return [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, Vd / Am - Am / Jm - Jm / Sm - Sm / Cm];
            elsif Vd > 2.0 * Jm * (Jm / Sm + Sm / Cm)**2 then
               --  Reachable: Sm, Jm
               return
                 [Sm / Cm,
                 Jm / Sm - Sm / Cm,
                 0.5 * ((Jm / Sm + Sm / Cm)**2 + 4.0 * Vd / Jm)**(1 / 2) - 1.5 * (Jm / Sm + Sm / Cm),
                 0.0 * s];
            elsif Vd > 8.0 * Sm**4 / Cm**3 then
               --  Reachable: Sm
               return Solve_Velocity_At_Time ([Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s], 2, Vd);
            else
               --  Reachable: None
               return [(0.125 * Vd / Cm)**(1 / 4), 0.0 * s, 0.0 * s, 0.0 * s];
            end if;
         elsif Am >= 2.0 * Sm**3 / Cm**2 then
            if Vd > Am * (2.0 * (0.25 * Sm**2 / Cm**2 + Am / Sm)**(1 / 2) + Sm / Cm) then
               --  Reachable: Sm, Am
               return
                 [Sm / Cm,
                 (0.25 * Sm**2 / Cm**2 + Am / Sm)**(1 / 2) - 1.5 * Sm / Cm,
                 0.0 * s,
                 Vd / Am - Sm / Cm - 2.0 * (0.25 * Sm**2 / Cm**2 + Am / Sm)**(1 / 2)];
            elsif Vd > 8.0 * Sm**4 / Cm**3 then
               --  Reachable: Sm
               return Solve_Velocity_At_Time ([Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s], 2, Vd);
            else
               --  Reachable: None
               return [(0.125 * Vd / Cm)**(1 / 4), 0.0 * s, 0.0 * s, 0.0 * s];
            end if;
         else
            if Vd > 8.0 * Cm * (0.5 * Am / Cm)**(4 / 3) then
               --  Reachable: Am
               return [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, Vd / Am - 4.0 * (0.5 * Am / Cm)**(1 / 3)];
            else
               --  Reachable: None
               return [(0.125 * Vd / Cm)**(1 / 4), 0.0 * s, 0.0 * s, 0.0 * s];
            end if;
         end if;
      else
         if Am > 2.0 * Jm * (Jm / Cm)**(1 / 2) then
            if Vd > Am * (Am / Jm + 2.0 * (Jm / Cm)**(1 / 2)) then
               --  Reachable: Jm, Am
               return
                 [(Jm / Cm)**(1 / 2),
                 0.0 * s,
                 Am / Jm - 2.0 * (Jm / Cm)**(1 / 2),
                 Vd / Am - Am / Jm - 2.0 * (Jm / Cm)**(1 / 2)];
            elsif Vd > 8.0 * Jm**2 / Cm then
               --  Reachable: Jm
               return [(Jm / Cm)**(1 / 2), 0.0 * s, (Jm / Cm + Vd / Jm)**(1 / 2) - 3.0 * (Jm / Cm)**(1 / 2), 0.0 * s];
            else
               --  Reachable: None
               return [(0.125 * Vd / Cm)**(1 / 4), 0.0 * s, 0.0 * s, 0.0 * s];
            end if;
         else
            if Vd > 8.0 * Cm * (0.5 * Am / Cm)**(4 / 3) then
               --  Reachable: Am
               return [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, Vd / Am - 4.0 * (0.5 * Am / Cm)**(1 / 3)];
            else
               --  Reachable: None
               return [(0.125 * Vd / Cm)**(1 / 4), 0.0 * s, 0.0 * s, 0.0 * s];
            end if;
         end if;
      end if;
   end Optimal_Profile_For_Delta_V;

   function Optimal_Full_Profile
     (Start_Vel            : Velocity;
      Start_Coast_Distance : Length;
      Max_Vel              : Velocity;
      End_Vel              : Velocity;
      End_Coast_Distance   : Length;
      Mid_Distance         : Length;
      Acceleration_Max     : Acceleration;
      Jerk_Max             : Jerk;
      Snap_Max             : Snap;
      Crackle_Max          : Crackle)
      return Feedrate_Profile
   is
      Profile : Feedrate_Profile;
   begin
      if Max_Vel < Start_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than Start_Vel.";
      end if;

      if Max_Vel < End_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than End_Vel.";
      end if;

      if Mid_Distance = 0.0 * mm then
         return
           (Start_Coast => (if Start_Vel = 0.0 * mm / s then 0.0 * s else Start_Coast_Distance / Start_Vel),
            Accel       => (others => 0.0 * s),
            Mid_Coast   => 0.0 * s,
            Decel       => (others => 0.0 * s),
            End_Coast   => (if End_Vel = 0.0 * mm / s then 0.0 * s else End_Coast_Distance / End_Vel));
      end if;

      declare
         Profile : constant Feedrate_Profile_Times :=
           Optimal_Profile_For_Delta_V (Start_Vel - End_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);

         Profile_Distance : constant Length :=
           Fast_Distance_At_Max_Time (Profile, (if Start_Vel < End_Vel then Crackle_Max else -Crackle_Max), Start_Vel);
      begin
         if Mid_Distance < Profile_Distance then
            raise Constraint_Error with "End_Vel is not reachable under given constraints.";
         end if;
      end;

      Profile.Start_Coast := (if Start_Vel = 0.0 * mm / s then 0.0 * s else Start_Coast_Distance / Start_Vel);
      Profile.End_Coast   := (if End_Vel = 0.0 * mm / s then 0.0 * s else End_Coast_Distance / End_Vel);
      Profile.Accel       :=
        Optimal_Profile_For_Delta_V (Start_Vel - Max_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      Profile.Decel       :=
        Optimal_Profile_For_Delta_V (End_Vel - Max_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);

      declare
         Accel_Distance : Length            := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
         Decel_Distance : Length            := Fast_Distance_At_Max_Time (Profile.Decel, -Crackle_Max, Max_Vel);
      begin
         if Accel_Distance + Decel_Distance <= Mid_Distance then
            Profile.Mid_Coast := (Mid_Distance - Accel_Distance - Decel_Distance) / Max_Vel;
         else
            Profile.Mid_Coast := 0.0 * s;
            declare
               type Casted_Vel is mod 2**64;
               function Cast_Vel is new Ada.Unchecked_Conversion (Velocity, Casted_Vel);
               function Cast_Vel is new Ada.Unchecked_Conversion (Casted_Vel, Velocity);
               Upper : Velocity := Max_Vel;
               Lower : Velocity := Velocity'Max (Start_Vel, End_Vel);
               Mid   : Velocity;
            begin
               --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check
               --  for that.
               pragma Assert (Velocity'Size = 64);
               pragma Assert (Casted_Vel'Size = 64);
               pragma Assert (Cast_Vel (86_400.0 * mm / s) = 4_680_673_776_000_565_248);
               pragma Assert (Cast_Vel (0.123_45 * mm / s) = 4_593_559_930_647_147_132);

               loop
                  Mid := Cast_Vel (Cast_Vel (Lower) + (Cast_Vel (Upper) - Cast_Vel (Lower)) / 2);
                  exit when Lower = Mid or Upper = Mid;

                  Profile.Accel :=
                    Optimal_Profile_For_Delta_V (Start_Vel - Mid, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
                  Profile.Decel :=
                    Optimal_Profile_For_Delta_V (End_Vel - Mid, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);

                  Accel_Distance := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
                  Decel_Distance := Fast_Distance_At_Max_Time (Profile.Decel, Crackle_Max, End_Vel);

                  if Accel_Distance + Decel_Distance <= Mid_Distance then
                     Lower := Mid;
                  else
                     Upper := Mid;
                  end if;
               end loop;
            end;
         end if;
      end;

      return Profile;
   end Optimal_Full_Profile;

end Prunt.Motion_Planner;
