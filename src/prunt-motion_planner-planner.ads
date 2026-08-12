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

--  This package provides a 5th-order (bounded crackle) motion planner with adjustable velocity, acceleration, jerk,
--  snap, and crackle limits. Corners use an explicitly selected geometric transition and junction policy; the selected
--  family determines which derivative limits remain meaningful at its joins.
--
--  The package works by collecting a series of corners before processing them as a single batch, called an
--  Execution_Block which starts and ends at zero velocity. Corners are collected until a flush command is received
--  or the block is full. Once a block is filled, it passes through a multi-stage planning pipeline, each of which is
--  implemented in a child package of this one:
--
--  1. Preprocessor:
--
--     The preprocessor is responsible for taking the incoming commands and converting them in to a series of corners
--     that can be used by the later stages.
--
--  2. Corner-transition construction:
--
--     The configured family is constructed without per-corner allocation. Stereographic transitions support the
--     existing line/helix combinations and match position through its fourth distance derivative at their endpoints.
--     Circular and Parabolic transitions support line-to-line corners and are C1. Biarc transitions are C1 and support
--     lines and helices when their two arcs can be certified. Circular and Parabolic waive jerk, snap, and crackle at
--     their endpoints; Biarc also waives them at its internal splice. Sharp_SCV retains the commanded corner exactly,
--     is C0, and waives acceleration and every higher derivative limit at the junction. Unsupported or uncertifiable
--     geometry becomes a hard stop; nearly straight geometry is represented explicitly as a passthrough.
--
--  3. Early_Kinematic_Limiter:
--
--     The programmed feed-rate is adjusted if Ignore_E_In_XYZE is set so that it is equal to the desired feedrate
--     when the E axis movement is included. After this the total time of each move is adjusted such that no move will
--     be less than Interpolation_Time, This ensures that the step generator will not have to skip over many segments
--     in a row, which could cause the command queue to run dry. Finally the axial limits defined in
--     Axial_Velocity_Maxes are applied.
--
--  4. Kinematic_Limiter:
--
--     A forward and backward pass are performed to generate corner velocities that conform to the specified kinematic
--     limits. The forward pass starts from zero velocity and generates a series of time-optimal profiles for each
--     segment to find the maximum reachable corner velocity. These are also clamped by the component-wise derivative
--     bounds exposed by each corner transition. The backward pass sets a velocity of zero on the final corner and then
--     goes back one corner at a time, limiting the corner velocities such that the next corner can be reached without
--     violating the kinematic limits.
--
--  5. Feedrate_Profile_Generator:
--
--     Using the corner velocities, an optimal velocity profile is generated for each segment.
--
--  6. Homing move limits:
--
--     If the move is a homing sequence, an inner loop first checks if the generated profile has a sufficiently long
--     constant-velocity (coast) phase, as defined by Home_Move_Minimum_Coast_Time. If the coast time is too short,
--     the segment's maximum velocity is reduced before going back to stage 4 (Kinematic_Limiter).
--
--  The fully processed Execution_Block is then made available via the Dequeue procedure.

pragma Extensions_Allowed (On);

with Ada.Containers;
with System.Multiprocessors;
with System.Storage_Elements;

private with Prunt.Bounded_Indefinite_Vectors;
private with Prunt.Motion_Planner.Corner_Transitions;
private with Prunt.Motion_Planner.Stereographic_Curves;

pragma Warnings (Off, "formal object * is not referenced");
--  Silence bogus warnings related to generic parameters used in child packages.

generic
   type Motor_Name is (<>);

   type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;

   type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

   Maximum_Deltas_Per_Command : Motor_Delta_Limits;

   type Flush_Resetting_Data_Type is private;
   --  Data to be included in each Execution_Block which is reset to a default value at the start of each block. Can
   --  be used to indicate if a move is a homing move or if the machine should pause after completion.

   Flush_Resetting_Data_Type_Default : Flush_Resetting_Data_Type;

   type Corner_Extra_Data_Type (<>) is private;
   --  Data to be included with each corner such as heater targets or the current file line number.

   Home_Move_Minimum_Coast_Time : Time;
   --  The minimum time that should be used for the coasting phase of a move where Is_Homing_Move returns True. This
   --  can be used to have a section that can be repeated in a loop until a switch is hit.

   Interpolation_Time : Time;
   --  The length of each interpolation period used by the step generator. This also determines the minimum time of a
   --  segment.

   Runner_CPU : System.Multiprocessors.CPU_Range := System.Multiprocessors.Not_A_Specific_CPU;
   --  CPU to run all motion planning on.

   Max_Corners : Max_Corners_Type := 50_000;
   --  The maximum number of corners that can be processed in a single execution block. This impacts the memory usage
   --  of the planner. Memory is allocated for the maximum block size during initialisation, memory is not allocated
   --  per-block.

   Max_Corners_Extra_Data_Count : Max_Corners_Extra_Data_Type := 1_000;

   Max_Corners_Extra_Data_Storage : System.Storage_Elements.Storage_Count := 1_000_000;
   --  The maximum amount of data in storage elements that the vector of Corner_Extra_Data_Type for a block may use for
   --  its backing storage. If a motion block runs out of space for extra data, the remaining data for the same corner
   --  is emitted in one or more Extra_Data_Overflow_Block_Kind blocks. If a Corner_Extra_Data_Type does not fit in an
   --  empty block then an error will be raised.

   Max_Corners_Extra_Data_Per_Corner : Max_Corners_Extra_Data_Type := 10;
   --  The maximum amount of extra data that can be processed for a corner before forcing a motion block boundary.
   --  Remaining extra data for the same corner is emitted through Extra_Data_Overflow_Block_Kind blocks after the
   --  machine has reached the block's stopping point.

   Corner_Transition_Max_Computational_Error : Length := 0.001 * mm;
   --  Caps the certified numerical position error requested from a generated corner transition.

   Corner_Transition_Min_Corner_Angle : Angle := 1.0 * deg;
   --  The minimum geometric corner angle for which a rounded transition may be generated. Sharper corners become hard
   --  stops. Corner angles are between 0° and 180°, with 180° denoting a straight passthrough.

   Input_Queue_Length : Ada.Containers.Count_Type := 1_000;
   --  The maximum number of corners that can be enqueued before the enqueue procedure begins to block. This queue acts
   --  as a buffer while the planner is processing another block or waiting for a block to be consumed, when a block is
   --  not being processed or waiting to be consumed commands are rapidly transferred out of the queue. This impacts
   --  the memory usage of the planner. Memory is allocated for the maximum queue size during initialisation, memory is
   --  not allocated per-item.

   Initial_Position : Position := [others => 0.0 * mm];
package Prunt.Motion_Planner.Planner is

   type Corners_Index is new Max_Corners_Type'Base range 1 .. Max_Corners;
   subtype Finishing_Corners_Index is Corners_Index range 2 .. Corners_Index'Last;

   type Execution_Block_Kind is (Motion_Block_Kind, Extra_Data_Overflow_Block_Kind);

   type Execution_Block (N_Corners : Corners_Index := 1) is private;
   --  N_Corners may be 1, in which case there are no segments.

   --  First Finishing_Corner = 2. If N_Corners < 2 then these functions must not be called.

   function Segment_Time
     (Block : not null access constant Execution_Block; Finishing_Corner : Corners_Index) return Time;
   --  Returns the total time for a given segment.

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length;
   --  Returns the distance between the two original corners for a given segment.

   function Segment_Pos_At_Time
     (Block              : not null access constant Execution_Block;
      Finishing_Corner   : Finishing_Corners_Index;
      Time_Into_Segment  : Time;
      Is_Past_Accel_Part : out Boolean) return Position
   with
     Pre =>
       Finishing_Corner <= Block.N_Corners
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;
   --  Returns the position at a given time in to a segment. Is_Past_Accel_Part indicates if the given time is past the
   --  acceleration part of the segment.

   function Segment_Vel_Ratio_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Dimensionless
   with
     Pre =>
       Finishing_Corner <= Block.N_Corners
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;
   --  Returns the velocity at the given time in to a segment divided by the target velocity for the given segment.
   --  Always returns 1.0 inside dwell parts.

   function Next_Block_Pos (Block : not null access constant Execution_Block) return Position;
   --  Returns the start position of the next block. At the end of a block, the motion executor should assume it is at
   --  this position, even if is not.

   function Block_Start_Pos (Block : not null access constant Execution_Block) return Position;
   --  Returns the start position of this block.

   function Flush_Resetting_Data (Block : not null access constant Execution_Block) return Flush_Resetting_Data_Type;
   --  Return the data passed to the Enqueue procedure. This data resets for each block.

   function Segment_Accel_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length
   with Pre => Finishing_Corner <= Block.N_Corners;
   --  Returns the length of the acceleration part of a segment.

   function Block_Kind (Block : Execution_Block) return Execution_Block_Kind;
   --  Return whether Block contains planned motion or only overflow corner data.

   function Corner_ID
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Planner_Corner_ID
   with Pre => Corner <= Block.N_Corners;
   --  Return the monotonic planner identifier assigned to Corner in Block.

   procedure Corner_Extra_Data
     (Block   : not null access constant Execution_Block;
      Corner  : Corners_Index;
      Process : not null access procedure (Data : in out Corner_Extra_Data_Type))
   with Pre => Corner <= Block.N_Corners;
   --  Allows the caller to process the extra data for a corner.

   function Has_Associated_Overflow_Block (Block : not null access constant Execution_Block) return Boolean;
   --  Returns True if this block's final corner has more extra data in a following overflow block, so its corner ID
   --  publication must be delayed.

   function Block_Kinematic_Parameters (Block : not null access constant Execution_Block) return Kinematic_Parameters;
   --  Returns the kinematic parameters used for the given block.

   function Is_Homing_Move (Block : not null access constant Execution_Block) return Boolean;
   --  Returns True if the block contains a homing move a specified by the relevant flush command.

   procedure Enqueue_Move
     (Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s; Ignore_Bounds : Boolean := False);
   --  Queue a linear move ending at Pos, optionally followed by a dwell.

   procedure Enqueue_Helix
     (Pos           : Position;
      Center        : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Ignore_Bounds : Boolean := False);
   --  Queue a clockwise or counter-clockwise XY helix ending at Pos around Center.

   function Get_Last_Assigned_Corner_ID return Planner_Corner_ID;
   --  Returns the highest corner ID assigned to accepted planner input. This value is monotonic and is not reset by
   --  Reset.

   procedure Enqueue_Corner_Extra_Data (Data : Corner_Extra_Data_Type);
   --  This may be emitted in an Extra_Data_Overflow_Block_Kind block if the current motion block has no room for the
   --  data.

   procedure Enqueue_Flush (Data : Flush_Resetting_Data_Type; Is_Homing_Move : Boolean := False);
   --  End the current input block and attach Data for processing after its motion stops.

   procedure Enqueue_Flush_And_Reset_Position
     (Data           : Flush_Resetting_Data_Type;
      Pos            : Position;
      Is_Homing_Move : Boolean := False;
      Ignore_Bounds  : Boolean := False);
   --  End the current input block and make Pos the start position for subsequently queued motion.

   procedure Enqueue_Flush_And_Change_Kinematic_Parameters
     (Data : Flush_Resetting_Data_Type; New_Params : Kinematic_Parameters; Is_Homing_Move : Boolean := False);
   --  End the current input block and make New_Params apply to subsequently queued motion.

   procedure Reset;
   --  Reset preprocessing and asynchronously discard planner work that has not been dequeued for execution.

   procedure Dequeue (Block : out Execution_Block; Timed_Out : out Boolean);
   --  Pop a block from the queue of processed blocks. If a block is not ready then Timed_Out will be set to True,
   --  otherwise it will be set to False and Block will be set.

   Out_Of_Bounds_Error : exception;

   task Runner
     with CPU => Runner_CPU, Storage_Size => 32 * 1024 * 1024 is
      --  TODO: We can probably lower Storage_Size now that the step rate limiter is gone.
      entry Setup (In_Params : Kinematic_Parameters; In_Motor_Map : Motor_Position_Map);
      entry Reset_Do_Not_Call_From_Other_Packages;
      --  Call the Reset procedure rather than this entry to avoid blocking and reset the preprocessor.
      --  TODO: There must be some way to hide this while still exposing the task.
      entry Dequeue_Do_Not_Call_From_Other_Packages (Out_Block : out Execution_Block);
      --  Call the Dequeue procedure rather than this entry as it may be replaced with a queue in the future.
   end Runner;

private

   type Command_Kind is
     (Move_Kind,
      Helix_Move_Kind,
      Corner_Extra_Data_Kind,
      Flush_Kind,
      Flush_And_Reset_Position_Kind,
      Flush_And_Change_Parameters_Kind);

   type Command (Kind : Command_Kind := Move_Kind) is record
      case Kind is
         when Flush_Kind | Flush_And_Reset_Position_Kind | Flush_And_Change_Parameters_Kind =>
            Flush_Resetting_Data : Flush_Resetting_Data_Type;
            Is_Homing_Move       : Boolean := False;
            --  Indicates whether a move is a homing move for the purposes of applying Home_Move_Minimum_Coast_Time.
            --  Currently a block containing a homing move must have exactly 2 corners, however this is trivial to
            --  change if required as the planner does not do anything with homing moves beyond setting the minimum
            --  coast time.
            case Kind is
               when Flush_And_Reset_Position_Kind =>
                  Reset_Pos : Position;

               when Flush_And_Change_Parameters_Kind =>
                  New_Params : Kinematic_Parameters;

               when others =>
                  null;
            end case;

         when Move_Kind | Helix_Move_Kind =>
            Dwell_After : Time := 0.0 * s;
            Pos         : Position;
            Feedrate    : Velocity;
            case Kind is
               when Helix_Move_Kind =>
                  Center    : Position;
                  Clockwise : Boolean;

               when others =>
                  null;
            end case;

         when Corner_Extra_Data_Kind =>
            --  We have to transfer the extra data into the queue separately to avoid requiring Unchecked_Access as it
            --  is an indefinite type which we can not store in the record. We use this variant as a flag.
            null;
      end case;
   end record;

   use Prunt.Motion_Planner.Stereographic_Curves;
   use Prunt.Motion_Planner.Corner_Transitions;

   --  Preprocessor
   type Corners_Extra_Data_Index is new Max_Corners_Extra_Data_Type'Base range 1 .. Max_Corners_Extra_Data_Count;
   package Corner_Extra_Data_Vectors is new
     Bounded_Indefinite_Vectors
       (Element_Type => Corner_Extra_Data_Type,
        Index_Type   => Corners_Extra_Data_Index,
        Storage_Size => Max_Corners_Extra_Data_Storage);

   type Block_Plain_Corners is array (Corners_Index range <>) of Position;
   type Block_Segment_Feedrates is array (Corners_Index range <>) of Velocity;
   type Block_Corners_Extra_Data_End_Indices is
     array (Corners_Index range <>) of Corner_Extra_Data_Vectors.Extended_Index;
   type Block_Corner_Dwell_Times is array (Corners_Index range <>) of Time;

   type Path_Primitive_Kind is (Line_Primitive_Kind, Helix_Primitive_Kind);

   type Path_Primitive (Kind : Path_Primitive_Kind := Line_Primitive_Kind) is record
      case Kind is
         when Line_Primitive_Kind =>
            null;

         when Helix_Primitive_Kind =>
            Center    : Position := [others => 0.0 * mm];
            Clockwise : Boolean := False;
      end case;
   end record;

   type Derived_Path_Primitive (Kind : Path_Primitive_Kind := Line_Primitive_Kind) is record
      Length : Prunt.Length := 0.0 * mm;
      case Kind is
         when Line_Primitive_Kind =>
            Direction : Position_Scale := [others => 0.0];

         when Helix_Primitive_Kind =>
            Radius            : Prunt.Length := 0.0 * mm;
            Theta_Start       : Dimensionless := 0.0;
            Theta_Delta       : Dimensionless := 0.0;
            Axial_Per_Phase   : Position_Offset := [others => 0.0 * mm];
            Length_Per_Radian : Prunt.Length := 0.0 * mm;
      end case;
   end record;
   --  Cached geometric values derived from a path primitive and its segment endpoints.

   function Derive_Path_Primitive
     (Primitive : Path_Primitive; Start_Point, End_Point : Position) return Derived_Path_Primitive;
   --  Derive the line direction or helix geometry needed to evaluate Primitive between the supplied endpoints.

   function Primitive_Phase_At_Distance (Primitive : Derived_Path_Primitive; Distance : Length) return Dimensionless;
   --  Return the helix phase at Distance after clamping the distance to Primitive's extent.

   type Block_Path_Primitives is array (Corners_Index range <>) of Path_Primitive;

   --  Corner-transition construction
   type Block_Corner_Transitions is array (Corners_Index range <>) of Corner_Transition_Evaluator;

   --  Feedrate_Profile_Generator
   type Block_Feedrate_Profiles is array (Corners_Index range <>) of Feedrate_Profile;

   --  Kinematic_Limiter
   type Block_Corner_Velocity_Limits is array (Corners_Index range <>) of Velocity;

   type Block_Segment_Lengths is array (Corners_Index range <>) of Length;
   type Block_Primitive_Derivative_Bounds is array (Corners_Index range <>) of Unit_Speed_Axial_Derivative_Bounds;
   type Profile_Window is record
      Start_Distance : Length := 0.0 * mm;
      Distance       : Length := 0.0 * mm;
   end record;

   type Profile_Window_Candidate_Index is range 1 .. 4;
   type Profile_Window_Candidates is array (Profile_Window_Candidate_Index) of Profile_Window;
   type Stored_Profile_Window_Selection is mod 2 ** 8 with Size => 8;
   type Block_Profile_Window_Selections is array (Corners_Index range <>) of Stored_Profile_Window_Selection;
   type Block_Profile_Crackles is array (Corners_Index range <>) of Crackle;

   type Planning_Workspace is record
      Corner_Derivative_Bounds : Block_Primitive_Derivative_Bounds (Corners_Index);
   end record;
   --  Data needed while planning a block but not while executing it. One workspace is allocated per planner instance
   --  and reused after the corresponding Execution_Block has been dequeued.

   type Profile_Window_Evaluation is record
      Valid   : Boolean := False;
      Window  : Profile_Window;
      Limits  : Scalar_Derivative_Limits;
      Max_Vel : Velocity := 0.0 * mm / s;
   end record;

   function Point_At_Segment_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position;
   --  Return the point at Distance along the complete segment path, including its adjacent transition portions.

   function Interval_Contains_Phase (Low, High, Base, Period : Dimensionless) return Boolean;
   --  Return True when the closed interval Low through High contains a phase congruent to Base modulo Period.

   function Motor_Delta_Ceiling_For_Window
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Velocity;
   --  Limit Max_Vel so every motor delta remains within its per-command bound throughout Window.

   function Window_Axial_Derivative_Bounds
     (Block            : not null access constant Execution_Block;
      Workspace        : not null access constant Planning_Workspace;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window) return Unit_Speed_Axial_Derivative_Bounds;
   --  Merge the axial derivative bounds of every transition or primitive portion overlapped by Window.

   function Motor_Projection_Coefficients
     (Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map; Motor : Motor_Name) return Projection_Coefficients;
   --  Return the Cartesian-to-motor projection coefficients for Motor, omitting unmapped axes.

   function Maximum_Absolute_Cosine (Start_Phase, End_Phase : Dimensionless) return Dimensionless;
   --  Conservatively bound the absolute cosine over the closed phase interval.

   function Maximum_Absolute_Offset_Sine
     (Start_Phase, End_Phase : Dimensionless; Amplitude, Offset : Curvature; Phase_Shift : Dimensionless)
      return Curvature;
   --  Conservatively bound the absolute value of Offset plus a shifted sinusoid over the phase interval.

   function Maximum_Absolute_Sine (Start_Phase, End_Phase : Dimensionless) return Dimensionless;
   --  Conservatively bound the absolute sine over the closed phase interval.

   function Segment_Start_Transition_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length;
   --  Return the previous corner transition distance assigned to this segment after its family-specific split.

   function Segment_End_Transition_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length;
   --  Return the finishing corner transition distance assigned to this segment before its family-specific split.

   function Segment_Straight_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length;
   --  Return the distance along the retained path primitive between the two adjacent corner transitions.

   function Segment_Total_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length;
   --  Return the full profileable path distance between the family-specific splits of adjacent corner transitions.

   function Segment_Profile_Window_Candidates
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window_Candidates;
   --  Return the deterministic candidate profile windows for a segment.

   function Evaluate_Profile_Window
     (Block            : not null access constant Execution_Block;
      Workspace        : not null access constant Planning_Workspace;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Profile_Window_Evaluation;
   --  Return mixed chain-rule limits and velocity ceiling for a candidate profile window.

   function Motor_Delta_Ceiling_For_Projection
     (Params : Kinematic_Parameters; Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map; Max_Vel : Velocity)
      return Velocity;
   --  Apply a unit-tangent Cauchy--Schwarz projection bound to a curved corner transition without retaining
   --  construction data, enlarged analytically when independently shaped axes share a motor.

   function Shaper_Aware_Projection_Bound
     (Params : Kinematic_Parameters; Coefficients : Projection_Coefficients; Raw_Bound : Curvature) return Curvature;
   --  Enlarge Raw_Bound when independently delayed basic-shaper impulses can combine in motor space.

   function Make_Line_Primitive return Path_Primitive;
   --  Return the descriptor for a straight path primitive.

   function Make_Helix_Primitive
     (Start_Point, End_Point, Center : Position; Clockwise : Boolean) return Path_Primitive;
   --  Return a helix descriptor for the supplied geometry, falling back to a line when the arc is not usable.

   function Primitive_Length
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length;
   --  Return the length of the line or helix primitive ending at Finishing_Corner.

   function Primitive_Point_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position;
   --  Return the position at Distance along the primitive, clamped to its endpoints.

   function Primitive_Direction_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position_Scale;
   --  Return the unit tangent at Distance along the primitive.

   function Primitive_Derivative_Jets_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Endpoint_Tangent_Jet;
   --  Return the primitive's unit-tangent derivative jet at Distance.

   function Primitive_Derivative_Bounds
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Start_Distance   : Length;
      Distance         : Length) return Unit_Speed_Axial_Derivative_Bounds;
   --  Bound the primitive's unit-speed axial derivatives over the requested distance interval.

   function Primitive_Motor_Delta_Ceiling
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Start_Distance   : Length;
      Distance         : Length;
      Max_Vel          : Velocity) return Velocity;
   --  Limit Max_Vel so the primitive respects every motor's per-command delta over the requested interval.

   function Reachable_Velocity
     (Start_Vel : Velocity; Max_Vel : Velocity; Distance : Length; Limits : Scalar_Derivative_Limits) return Velocity;
   --  Return the greatest velocity reachable from Start_Vel across Distance without exceeding Limits or Max_Vel.

   function Endpoint_Delta_V_Distance
     (Start_Vel : Velocity; End_Vel : Velocity; Limits : Scalar_Derivative_Limits) return Length;
   --  Return the minimum distance required to transition between the two endpoint velocities under Limits.

   function Constant_Speed_Time (Distance : Length; Speed : Velocity) return Time;
   --  Return the time needed to cover Distance at Speed, raising Constraint_Error for nonzero distance at zero speed.

   function Selected_Profile_Window
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window;
   --  Return the profile window selected while planning the segment.

   type Execution_Block (N_Corners : Corners_Index := 1) is record
      --  This record contains all the data for a single execution block. It is passed through the planning pipeline,
      --  with each stage adding more data to it.

      --  TODO: Having all these fields accessible before the relevant stage is called is not ideal, but using a
      --  discriminated type with a discriminant to indicate the stage causes a stack overflow when trying to change
      --  the discriminant without making a copy as GCC tries to copy the whole thing to the stack. In the future we
      --  could possibly use SPARK to ensure stages do not touch fields that are not yet assigned.

      --  Having so many discriminated types here may seem like it will cause performance issues, but in practice it is
      --  faster than the same code without discriminated types (refer to the no-discriminated-records branch).

      --  Preprocessor
      Kind                           : Execution_Block_Kind := Motion_Block_Kind;
      Flush_Resetting_Data           : Flush_Resetting_Data_Type;
      Next_Block_Pos                 : Position;
      Params                         : Kinematic_Parameters;
      Corners_Extra_Data             : Corner_Extra_Data_Vectors.Vector;
      Corners_Extra_Data_End_Indices : Block_Corners_Extra_Data_End_Indices (1 .. N_Corners);
      Corners                        : Block_Plain_Corners (1 .. N_Corners);
      Primitives                     : Block_Path_Primitives (2 .. N_Corners);
      Original_Segment_Feedrates     : Block_Segment_Feedrates (2 .. N_Corners);
      First_Corner_ID                : Planner_Corner_ID := 0;
      Associated_Overflow_Block      : Boolean := False;
      Is_Homing_Move                 : Boolean;
      Limited_Segment_Feedrates      : Block_Segment_Feedrates (2 .. N_Corners);
      Corner_Dwell_Times             : Block_Corner_Dwell_Times (2 .. N_Corners);

      --  Corner-transition construction
      Corner_Transitions  : Block_Corner_Transitions (1 .. N_Corners);
      Primitive_Distances : Block_Segment_Lengths (2 .. N_Corners);

      --  Early_Kinematic_Limiter
      Primitive_Start_Distances : Block_Segment_Lengths (2 .. N_Corners);

      --  Kinematic_Limiter
      Corner_Velocity_Limits : Block_Corner_Velocity_Limits (1 .. N_Corners);

      --  Feedrate_Profile_Generator
      Feedrate_Profiles         : Block_Feedrate_Profiles (2 .. N_Corners);
      Profile_Crackles          : Block_Profile_Crackles (2 .. N_Corners);
      Profile_Window_Selections : Block_Profile_Window_Selections (2 .. N_Corners);
   end record;

end Prunt.Motion_Planner.Planner;

pragma Warnings (On, "formal object * is not referenced");
