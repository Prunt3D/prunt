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

--  This package provides a 5th-order (bounded crackle) motion planner with adjustable velocity, acceleration, jerk,
--  snap, and crackle limits. Blending of corners is also provided to limit axial acceleration through crackle.
--
--  The package works by collecting a series of corners before processing them as a single batch, called an
--  `Execution_Block` which starts and ends at zero velocity. Corners are collected until a flush command is received
--  or the block is full. Once a block is filled, it passes through a multi-stage planning pipeline, each of which is
--  implemented in a child package of this one:
--
--  1. Corner_Blender: Sharp corners are replaced with C⁴ continuous Pythagorean-Hodograph (PH) Bézier
--     curves. The maximal deviation of these curves from the original path is determined by the `Chord_Error_Max`
--     parameter. Corners may optionally be shifted so that the midpoint of the curve is equal to the original corner,
--     this can produce paths which more closely match original CAD files before conversion to triangulated surface
--     formats such as STL files.
--
--  2. Early_Kinematic_Limiter: The programmed feed-rate is adjusted if `Ignore_E_In_XYZE` is set so that it is equal
--     to the desired feedrate when the E axis movement is included. After this the total time of each move is adjusted
--     such that no move will be less than `Interpolation_Time`, This ensures that the step generator will not have to
--     skip over many segments in a row, which could cause the command queue to run dry.. Finally the axial limits
--     defined in `Axial_Velocity_Maxes` are applied.
--
--  3. Kinematic_Limiter: A forward and backward pass are performed to generate corner velocities that confirm to the
--    specified kinematic limits. The forward pass starts from zero velocity and generates a series of time-optimal
--    profiles for each segment to find the maximum reachable corner velocity, these are also clamped based on the
--    curvature of the corner to maintain axial kinematic limits. The backward pass sets a velocity of zero on the
--    final corner and then goes back one corner at a time, limiting the corner velocities such that the next corner
--    can be reached without violating the kinematic limits.
--
--  4. Feedrate_Profile_Generator: Using the corner velocities, an optimal velocity profile is generated for each
--     segment.
--
--  5. Homing move limits: If the move is a homing sequence, an inner loop first checks if the generated profile has a
--     sufficiently long constant-velocity (coast) phase, as defined by `Home_Move_Minimum_Coast_Time`. If the coast
--     time is too short, the segment's maximum velocity is reduced before going back to stage 3 (Kinematic_Limiter).
--
--  6. Step_Rate_Limiter: The planner simulates the complete motion of the block, including input shaping. It
--     calculates the required steps for each motor at each interpolation interval. If any motor's maximum step rate is
--     exceeded, the planner reduces the velocity of the corresponding segment. If any segments are reduced then the
--     planner goes back to stage 3 (Kinematic_Limiter).
--
--  The fully processed `Execution_Block` is then made available via the `Dequeue` procedure.

with System.Multiprocessors;
with Ada.Containers;
with Prunt.Input_Shapers;
private with Prunt.Motion_Planner.PH_Beziers;

pragma Warnings (Off, "formal object * is not referenced");
--  Silence bogus warnings related to generic parameters used in child packages.

generic
   type Flush_Resetting_Data_Type is private;
   --  Data to be included in each `Execution_Block` which is reset to a default value at the start of each block. Can
   --  be used to indicate if a move is a homing move or if the machine should pause after completion.

   Flush_Resetting_Data_Default : Flush_Resetting_Data_Type;
   --  Default value for the resetting data included in each block if no value is specified.

   type Block_Persistent_Data_Type is private;
   --  Data to be included in each `Execution_Block` which is not reset between blocks. Can be used for holding shaper
   --  parameters or the name of the currently executing file.
   --
   --  TODO: This should be passed around everywhere when a block finishes like Flush_Resetting_Data_Type is.

   Block_Persistent_Data_Default : Block_Persistent_Data_Type;
   --  Default value for persistent data to be included in the first block and all subsequent blocks until a new value
   --  is provided.

   type Corner_Extra_Data_Type is private;
   --  Data to be included with each corner such as heater targets or the current file line number.

   Home_Move_Minimum_Coast_Time : Time;
   --  The minimum time that should be used for the coasting phase of a move where `Is_Homing_Move` returns True. This
   --  can be used to have a section that can be repeated in a loop until a switch is hit.

   with function Is_Homing_Move (Data : Flush_Resetting_Data_Type) return Boolean;
   --  Indicates whether a move is a homing move for the purposes of applying `Home_Move_Minimum_Coast_Time`. Currently
   --  a block containing a homing move must have exactly 2 corners, however this is trivial to change if required as
   --  the planner does not do anything with homing moves beyond setting the minimum coast time.

   Interpolation_Time : Time;
   --  The length of each interpolation period to be used by the step rate checker. This also determines the minimum
   --  time of a segment.

   type Stepper_Name is (<>);

   type Stepper_Position is array (Stepper_Name) of Dimensionless;

   Maximum_Stepper_Delta : Stepper_Position;
   --  The maximum change in position for each axis within a single interpolation period. Step generation will be
   --  simulated and any moves that result in these limits being exceeded will be slowed down.

   with
     function Get_Axial_Shaper_Parameters
       (Data : Block_Persistent_Data_Type) return Input_Shapers.Axial_Shaper_Parameters;
   --  Retrieve the shaper parameters for a given block. These are used during step rate limiting as shapers can change
   --  the number of steps within an interpolation period.

   with procedure Log (Message : String);
   --  Used to warn the user if the step rate is limited.

   Runner_CPU : System.Multiprocessors.CPU_Range := System.Multiprocessors.Not_A_Specific_CPU;
   --  CPU to run all motion planning on.

   Max_Corners : Max_Corners_Type := 50_000;
   --  The maximum number of corners that can be processed in a single execution block. This impacts the memory usage
   --  of the planner. Memory is allocated for the maximum block size during initialisation, memory is not allocated
   --  per-block.

   --  Preprocessor_Minimum_Move_Distance : Length := 0.001 * mm;
   --  Unused. Can be uncommented in the preprocessor package if required.

   Corner_Blender_Max_Computational_Error : Length := 0.001 * mm;
   --  The maximum allowed distance between a corner and a blended corner's Bézier curve midpoint when shifting of
   --  blended corners is enabled. A smaller value will require more iterations.

   Corner_Blender_Min_Corner_Angle_To_Blend : Angle := 1.0 * deg;
   --  The minimum angle that will be blended in to a rounded corner instead of being left as a sharp corner. Corner
   --  angles are always considered to be between 0° and 180° with 180° being a straight line.

   Input_Queue_Length : Ada.Containers.Count_Type := 1_000;
   --  The maximum number of corners that can be enqueued before the enqueue procedure begins to block. This queue acts
   --  as a buffer while the planner is processing another block or waiting for a block to be consumed, when a block is
   --  not being processed or waiting to be consumed commands are rapidly transferred out of the queue. This impacts
   --  the memory usage of the planner. Memory is allocated for the maximum queue size during initialisation, memory is
   --  not allocated per-item.

   Initial_Position : Position := [others => 0.0 * mm];
package Prunt.Motion_Planner.Planner is

   type Stepper_Pos_Map is array (Axis_Name, Stepper_Name) of Length;
   --  Defines how each axis moves in response to a step from a given motor. This is used during step simulation.

   type Command_Kind is
     (Move_Kind,
      Dummy_Corner_Kind,
      Flush_Kind,
      Flush_And_Reset_Position_Kind,
      Flush_And_Change_Parameters_Kind,
      Flush_And_Update_Persistent_Data_Kind);

   type Command (Kind : Command_Kind := Move_Kind) is record
      case Kind is
         when Flush_Kind | Flush_And_Reset_Position_Kind | Flush_And_Change_Parameters_Kind =>
            Flush_Resetting_Data : Flush_Resetting_Data_Type;
            case Kind is
               when Flush_And_Reset_Position_Kind =>
                  Reset_Pos : Position;

               when Flush_And_Change_Parameters_Kind =>
                  New_Params : Kinematic_Parameters;

               when others =>
                  null;
            end case;

         when Move_Kind | Dummy_Corner_Kind =>
            Corner_Extra_Data : Corner_Extra_Data_Type;
            Dwell_After       : Time := 0.0 * s;
            case Kind is
               when Move_Kind =>
                  Pos      : Position;
                  Feedrate : Velocity;

               when others =>
                  null;
            end case;

         when Flush_And_Update_Persistent_Data_Kind =>
            New_Persistent_Data : Block_Persistent_Data_Type;
      end case;
   end record;

   type Corners_Index is new Max_Corners_Type'Base range 1 .. Max_Corners;

   type Execution_Block (N_Corners : Corners_Index := 1) is private;
   --  N_Corners may be 1, in which case there are no segments.

   --  First Finishing_Corner = 2. If N_Corners < 2 then these functions must not be called.

   function Segment_Time (Block : Execution_Block; Finishing_Corner : Corners_Index) return Time;
   --  Returns the total time for a given segment.

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length;
   --  Returns the distance between the two original corners for a given segment.

   function Segment_Pos_At_Time
     (Block              : Execution_Block;
      Finishing_Corner   : Corners_Index;
      Time_Into_Segment  : Time;
      Is_Past_Accel_Part : out Boolean) return Position
   with Pre => Time_Into_Segment <= Segment_Time (Block, Finishing_Corner) and Time_Into_Segment >= 0.0 * s;
   --  Returns the position at a given time in to a segment. Is_Past_Accel_Part indicates if the given time is past the
   --  acceleration part of the segment.

   function Segment_Vel_Ratio_At_Time
     (Block : Execution_Block; Finishing_Corner : Corners_Index; Time_Into_Segment : Time) return Dimensionless
   with Pre => Time_Into_Segment <= Segment_Time (Block, Finishing_Corner) and Time_Into_Segment >= 0.0 * s;
   --  Returns the velocity at the given time in to a segment divided by the target velocity for the given segment.
   --  Always returns 1.0 inside dwell parts.

   function Next_Block_Pos (Block : Execution_Block) return Position;
   --  Returns the start position of the next block. At the end of a block, the motion executor should assume it is at
   --  this position, even if is not.

   function Block_Start_Pos (Block : Execution_Block) return Position;
   --  Returns the start position of this block.

   function Flush_Resetting_Data (Block : Execution_Block) return Flush_Resetting_Data_Type;
   --  Return the data passed to the Enqueue procedure, or Flush_Resetting_Data_Default if the block was filled before
   --  receiving a flush command. This data resets for each block.

   function Block_Persistent_Data (Block : Execution_Block) return Block_Persistent_Data_Type;
   --  Return the latest data passed to the Enqueue procedure, or Block_Persistent_Data_Default if the no data has been
   --  received. This data persists between blocks.

   function Segment_Accel_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length;
   --  Returns the length of the acceleration part of a segment.

   function Corner_Extra_Data (Block : Execution_Block; Corner : Corners_Index) return Corner_Extra_Data_Type;
   --  Returns the extra data for a corner. It is illegal to call this function with Corner = 1.

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False);
   --  Send a new command to the planner queue. May be called before Setup, but will block once the queue if full.

   procedure Reset;

   procedure Dequeue (Block : out Execution_Block; Timed_Out : out Boolean);
   --  Pop a block from the queue of processed blocks. If a block is not ready then Timed_Out will be set to True,
   --  otherwise it will be set to False and Block will be set.

   Out_Of_Bounds_Error : exception;

   task Runner
     with CPU => Runner_CPU, Storage_Size => 32 * 1024 * 1024 is
      --  Large Storage_Size to allow for large shapers in the step rate limiter.
      entry Setup (In_Params : Kinematic_Parameters; In_Map : Stepper_Pos_Map);
      entry Reset_Do_Not_Call_From_Other_Packages;
      --  Call the Reset procedure rather than this entry to avoid blocking and reset the preprocessor.
      --  TODO: There must be some way to hide this while still exposing the task.
      entry Dequeue_Do_Not_Call_From_Other_Packages (Out_Block : out Execution_Block);
      --  Call the Dequeue procedure rather than this entry as it may be replaced with a queue in the future.
   end Runner;

private

   use Prunt.Motion_Planner.PH_Beziers;

   --  Preprocessor
   type Block_Plain_Corners is array (Corners_Index range <>) of Scaled_Position;
   type Block_Segment_Feedrates is array (Corners_Index range <>) of Velocity;
   type Block_Corners_Extra_Data is array (Corners_Index range <>) of Corner_Extra_Data_Type;
   type Block_Corner_Dwell_Times is array (Corners_Index range <>) of Time;

   --  Corner_Blender
   type Block_Beziers is array (Corners_Index range <>) of PH_Bezier;

   --  Feedrate_Profile_Generator
   type Block_Feedrate_Profiles is array (Corners_Index range <>) of Feedrate_Profile;

   --  Kinematic_Limiter
   type Block_Corner_Velocity_Limits is array (Corners_Index range <>) of Velocity;

   type Execution_Block (N_Corners : Corners_Index := 1) is record
      --  TODO: Having all these fields accessible before the relevant stage is called is not ideal, but using a
      --  discriminated type with a discriminant to indicate the stage causes a stack overflow when trying to change
      --  the discriminant without making a copy as GCC tries to copy the whole thing to the stack. In the future we
      --  could possibly use SPARK to ensure stages do not touch fields that are not yet assigned.

      --  Having so many discriminated types here may seem like it will cause performance issues, but in practice it is
      --  faster than the same code without discriminated types (refer to the no-discriminated-records branch).

      --  Preprocessor
      Flush_Resetting_Data       : Flush_Resetting_Data_Type;
      Block_Persistent_Data      : Block_Persistent_Data_Type;
      Next_Block_Pos             : Scaled_Position;
      Params                     : Kinematic_Parameters;
      Corners                    : Block_Plain_Corners (1 .. N_Corners);  --  Adjusted with scaler.
      Original_Segment_Feedrates : Block_Segment_Feedrates (2 .. N_Corners);
      --  Adjusted with scaler in Kinematic_Limiter.
      Limited_Segment_Feedrates  : Block_Segment_Feedrates (2 .. N_Corners);
      --  Adjusted with scaler in Kinematic_Limiter and limited by maximum velocity and step rate.
      Corners_Extra_Data         : Block_Corners_Extra_Data (2 .. N_Corners);
      Corner_Dwell_Times         : Block_Corner_Dwell_Times (2 .. N_Corners);

      --  Corner_Blender
      Beziers : Block_Beziers (1 .. N_Corners);

      --  Kinematic_Limiter
      Corner_Velocity_Limits : Block_Corner_Velocity_Limits (1 .. N_Corners);

      --  Feedrate_Profile_Generator
      Feedrate_Profiles : Block_Feedrate_Profiles (2 .. N_Corners);
   end record;

end Prunt.Motion_Planner.Planner;
