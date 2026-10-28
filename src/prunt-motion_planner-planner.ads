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

pragma Check_Float_Overflow;

--  This package provides a 5th-order (bounded crackle) motion planner with adjustable velocity, acceleration, jerk,
--  snap, and crackle limits. Corners use an explicitly selected geometric transition and junction policy; the selected
--  family determines which derivative limits remain meaningful at its joins.
--
--  The package works by collecting a series of corners before processing them as a single batch, called an
--  Execution_Block with a stopped fallback plan. Adjacent queued blocks without end data can be linked by a
--  certified local blend before execution borrows them. Corners are collected until a flush command is received
--  or the block is full. Once a block holds the last tenth of Max_Corners, Max_Corners_Extra_Data_Count or
--  Max_Corners_Extra_Data_Storage, it also ends before any move that does not continue its last extrusion run: a
--  travel, E reversal or E-only move, any move after a travel or E-only move, or any move after a dwell. A full block
--  therefore rarely splits an extrusion run. A limit below 10 has no such window. A full block waits for the next
--  command to establish whether its final corner has overflow data before being published. Each stage of the planning
--  pipeline is implemented in a child package of this one:
--
--  1. Preprocessor:
--
--     The preprocessor is responsible for taking the incoming commands and converting them in to a series of corners
--     that can be used by the later stages.
--
--  2. Extrusion_Density_Normalizer:
--
--     Adjacent same-direction spatial extrusion segments can share one density if each segment's extrusion adjustment
--     fits Extrusion_Normalization: a fraction of its extrusion, or what rounding of the commanded coordinates could
--     explain at that density. Runs retain their total extrusion before path shortening. The assigned densities remain
--     authoritative through all later stages, including geometry recovery.
--
--  3. Corner-transition construction:
--
--     The configured family is constructed without per-corner allocation. Stereographic transitions support the
--     existing line/helix combinations and match position through its fourth distance derivative at their endpoints.
--     Circular transitions support line-to-line corners and are C1. Biarc transitions are C1 and support lines and
--     helices when their two arcs can be certified. Circular transitions waive jerk, snap, and crackle at
--     their endpoints; Biarc also waives them at its internal splice. Sharp_SCV retains the commanded corner exactly,
--     is C0, and waives acceleration and every higher derivative limit at the junction. Unsupported or uncertifiable
--     geometry becomes a hard stop; nearly straight geometry is represented explicitly as a passthrough.
--     Stereographic passthroughs also require matching distance derivatives through order four.
--
--  4. Early_Kinematic_Limiter:
--
--     Feedrate uses XYZ distance, or E distance for E-only moves. When Ignore_E_In_XYZE is False, the programmed
--     XYZE feedrate is converted to these coordinates. The total time of each move is adjusted such that no move will
--     be less than Interpolation_Time, This ensures that the step generator will not have to skip over many segments
--     in a row, which could cause the command queue to run dry. Then the axial limits defined in
--     Axial_Velocity_Maxes are applied.
--
--     Finally, in Smooth_Deviation mode, each moving density change that its ratios allow gets an E transition: a
--     fixed smooth density curve over the path either side of its corner, just wide enough that E's limits do not
--     lower the corner speed that the feedrates and XYZ transition allow, within Transition_Distance_Max, the E
--     deviation and the neighbouring segments. E stays a function of path position, so the later stages bound its
--     distance derivatives just as they bound curved XYZ geometry, and no transition changes with the speed through
--     it.
--
--  5. Kinematic_Limiter:
--
--     A forward and backward pass are performed to generate corner velocities that conform to the specified kinematic
--     limits. The forward pass starts from zero velocity and generates a series of time-optimal profiles for each
--     segment to find the maximum reachable corner velocity. These are also clamped by the component-wise derivative
--     bounds exposed by each corner transition. The backward pass sets a velocity of zero on the final corner and then
--     goes back one corner at a time, limiting the corner velocities such that the next corner can be reached without
--     violating the kinematic limits.
--
--  6. Feedrate_Profile_Generator:
--
--     Using the corner velocities, an optimal velocity profile is generated for each segment.
--
--  7. Homing move limits:
--
--     If the move is a homing sequence, an inner loop first checks that the generated profile has a sufficiently long
--     constant-velocity (coast) phase and that the complete tail after the loop command fits within
--     Home_Move_Maximum_Tail_Time. If either check fails, the segment's maximum velocity is reduced before going back
--     to stage 5 (Kinematic_Limiter).
--
--  The fully processed Execution_Block is then made available via the Dequeue procedure. Fixed blocks are reused
--  in a ring; execution borrows a block until Release, while planning continues in the other slots.

pragma Extensions_Allowed (On);

with Ada.Containers;
with System.Multiprocessors;
with System.Storage_Elements;

private with Ada.Exceptions;
private with Ada.Finalization;
private with Prunt.Bounded_Indefinite_Vectors;
private with Prunt.Cleanup_Exceptions;
private with Prunt.Motion_Planner.Corner_Transitions;
private with Prunt.Motion_Planner.Stereographic_Curves;
private with System.Pool_Local;
private with System.Atomic_Operations.Exchange;

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
   --  Data for blocks that end without a flush, at or near a capacity limit. Such boundaries may be blended with the
   --  next block, so a flush whose Data equals this value does not guarantee a stop. Use a distinct default if every
   --  explicit flush must stop.

   type Corner_Extra_Data_Type (<>) is private;
   --  Data to be included with each corner such as heater targets or the current file line number.

   Home_Move_Minimum_Coast_Time : Time;
   --  The minimum constant-velocity time reserved on each side of the loop command of a move where Is_Homing_Move
   --  returns True. This keeps the repeated command clear of both acceleration and deceleration.

   Home_Move_Maximum_Tail_Time : Time;
   --  Maximum duration represented by commands retained after a homing loop command. The planner places the loop
   --  command late enough in a long coast that the remaining coast, deceleration, constant-speed suffix, and dwell all
   --  fit within this bound. It also lowers the homing feedrate until the part of that tail which cannot be moved
   --  earlier in the coast fits within this bound.

   Interpolation_Time : Time;
   --  The length of each interpolation period used by the step generator. This also determines the minimum time of a
   --  segment.

   Runner_CPU : System.Multiprocessors.CPU_Range := System.Multiprocessors.Not_A_Specific_CPU;
   --  CPU to run all motion planning on.

   Max_Corners : Max_Corners_Type := 50_000;
   --  The maximum number of corners in one execution block. Each planner allocates Block_Capacity blocks of this
   --  size, plus its planning workspace, during initialisation. Increasing this limit increases every slot's storage;
   --  execution blocks are reused without per-block allocation. Once a block reaches the last tenth of this limit it
   --  ends before the first move that does not continue its last extrusion run, including any move after a travel or
   --  E-only move. Limits below 10 have no such window.

   Max_Corners_Extra_Data_Count : Max_Corners_Extra_Data_Type := 1_000;
   --  The maximum number of Corner_Extra_Data_Type items in one block. Like Max_Corners, a block holding the last
   --  tenth of this limit ends before the first move that does not continue its last extrusion run.

   Max_Corners_Extra_Data_Storage : System.Storage_Elements.Storage_Count := 1_000_000;
   --  The maximum amount of data in storage elements that the vector of Corner_Extra_Data_Type for a block may use for
   --  its backing storage. If a motion block runs out of space for extra data, the remaining data for the same corner
   --  is emitted in one or more Extra_Data_Overflow_Block_Kind blocks. If a Corner_Extra_Data_Type does not fit in an
   --  empty block then an error will be raised. Like Max_Corners, a block using the last tenth of this storage ends
   --  before the first move that does not continue its last extrusion run.

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

   with function Allow_Blended_Entry (Data : Flush_Resetting_Data_Type) return Boolean;
   --  Planner-side policy for data which also changes execution of the block itself (for example pinning individual
   --  motors). False preserves its stopped entry. Ordinary end-only callbacks need no restriction.
package Prunt.Motion_Planner.Planner is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);
   --  These local pragmas survive instantiation; configuration pragmas above the generic do not.
   --  The instantiating compilation unit must also enable floating-point checking with -gnateF.

   type Corners_Index is new Max_Corners_Type'Base range 1 .. Max_Corners_Type'Max (Max_Corners, 3);
   --  Three indices are needed for the seam workspace even when input blocks are limited to two corners.

   subtype Finishing_Corners_Index is Corners_Index range 2 .. Corners_Index'Last;

   Block_Capacity : constant Positive;
   --  Number of slots in each planner's ring. Configure this by changing the private Block_Index modulus;
   --  storage, queue limits and wraparound all derive from that type.

   type Execution_Block_Kind is (Motion_Block_Kind, Extra_Data_Overflow_Block_Kind);

   type Block_Handle is tagged limited private;
   --  Exclusive ownership of one immutable block and its accepted outgoing blend in the planner's ring. A handle
   --  starts empty and cannot be copied. Release returns its remaining slots; finalization also releases them on
   --  scope exit, exceptions or abort. A populated handle remains valid across Reset. Do not access the same handle
   --  concurrently from multiple tasks.

   function Is_Empty (Block : Block_Handle) return Boolean;

   function Corner_Count (Block : Block_Handle) return Corners_Index
   with Pre => not Is_Empty (Block);
   --  May be 1, in which case there are no segments.

   --  First Finishing_Corner = 2. If Corner_Count is 1 then there are no segments to evaluate.

   function Segment_Time (Block : Block_Handle; Finishing_Corner : Corners_Index) return Time
   with Pre => not Is_Empty (Block);
   --  Returns the total time for a given segment.

   function Segment_Corner_Distance (Block : Block_Handle; Finishing_Corner : Corners_Index) return Length
   with Pre => not Is_Empty (Block);
   --  Returns the distance between the two original corners for a given segment.

   function Segment_Pos_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Position
   with
     Pre =>
       not Is_Empty (Block)
       and then Finishing_Corner <= Corner_Count (Block)
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;
   --  Return the position at a given time into a segment.

   function Segment_Vel_Ratio_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Dimensionless
   with
     Pre =>
       not Is_Empty (Block)
       and then Finishing_Corner <= Corner_Count (Block)
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;
   --  Returns the velocity at the given time in to a segment divided by the target velocity for the given segment.
   --  Always returns 1.0 inside dwell parts.

   function Next_Block_Pos (Block : Block_Handle) return Position
   with Pre => not Is_Empty (Block);
   --  Returns the commanded start position of the next block. After a block with no accepted successor stops, the
   --  motion executor should assume it is at this position, even if it is not. This also rebases E after path-length
   --  extrusion correction so subsequent blocks continue to accept the commanded absolute E coordinates.

   function Block_Start_Pos (Block : Block_Handle) return Position
   with Pre => not Is_Empty (Block);
   --  Returns the start position of this block.

   function Flush_Resetting_Data (Block : Block_Handle) return Flush_Resetting_Data_Type
   with Pre => not Is_Empty (Block);
   --  Return the data passed to the Enqueue procedure. This data resets for each block.

   function Is_Homing_Move (Block : Block_Handle) return Boolean
   with Pre => not Is_Empty (Block);
   --  Returns True if the block contains a homing move as specified by the relevant flush command.

   function Loop_Move_Minimum_Time (Block : Block_Handle) return Time
   with Pre => not Is_Empty (Block) and then Is_Homing_Move (Block);
   --  Return the earliest interpolation time at which the homing loop command may be emitted.

   function Block_Kind (Block : Block_Handle) return Execution_Block_Kind
   with Pre => not Is_Empty (Block);
   --  Return whether Block contains planned motion or only overflow corner data.

   function Corner_ID (Block : Block_Handle; Corner : Corners_Index) return Planner_Corner_ID
   with Pre => not Is_Empty (Block) and then Corner <= Corner_Count (Block);
   --  Return the monotonic planner identifier assigned to Corner in Block.

   procedure Corner_Extra_Data
     (Block   : Block_Handle;
      Corner  : Corners_Index;
      Process : not null access procedure (Data : in out Corner_Extra_Data_Type))
   with Pre => not Is_Empty (Block) and then Corner <= Corner_Count (Block);
   --  Allows the caller to process the extra data for a corner.

   function Has_Associated_Overflow_Block (Block : Block_Handle) return Boolean
   with Pre => not Is_Empty (Block);
   --  Returns True if this block's final corner has more extra data in a following overflow block, so its corner ID
   --  publication must be delayed.

   function Block_Kinematic_Parameters (Block : Block_Handle) return Kinematic_Parameters
   with Pre => not Is_Empty (Block);
   --  Returns the kinematic parameters used for the given block.

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

   procedure Enqueue_Flush (Data : Flush_Resetting_Data_Type);
   --  End the current input block and attach Data for processing after its motion stops. Data equal to
   --  Flush_Resetting_Data_Type_Default only marks a boundary, which may be blended with the next block without
   --  stopping.

   function Enqueue_Homing_Flush (Data : Flush_Resetting_Data_Type) return Position_Offset;
   --  End the current input block as a homing loop move, prevent further motion enqueueing, and return the planned
   --  displacement from the detector-hit command to the end of the retained tail.

   procedure Resolve_Homing_Position (Pos : Position);
   --  Supply the homing move's true stopped position and allow subsequent motion to be enqueued.

   procedure Enqueue_Flush_And_Reset_Position
     (Data : Flush_Resetting_Data_Type; Pos : Position; Ignore_Bounds : Boolean := False);
   --  End the current input block and make Pos the start position for subsequently queued motion.

   procedure Enqueue_Flush_And_Change_Kinematic_Parameters
     (Data : Flush_Resetting_Data_Type; New_Params : Kinematic_Parameters);
   --  End the current input block and make New_Params apply to subsequently queued motion.

   procedure Reset;
   --  Reset preprocessing and discard planner work that has not been dequeued. Borrowed blocks remain valid until
   --  released and continue to occupy their slots across the next Setup.

   procedure Dequeue (Block : in out Block_Handle; Timed_Out : out Boolean)
   with Pre => Is_Empty (Block);
   --  Borrow the next independently stopped block. Prepared successors are claimed one at a time by Advance_Block.
   --  Call Prepare_Segment before evaluating each segment; it chooses a prepared tail at its first affected segment.
   --  If no block becomes ready within one second, Timed_Out is True and Block remains empty. After Release discards
   --  an unconsumed successor planned since the last Reset, no block is returned until the next Reset, because queued
   --  blocks would continue from motion that never ran.

   procedure Prepare_Segment (Block : in out Block_Handle; Finishing_Corner : Finishing_Corners_Index)
   with Pre => not Is_Empty (Block);
   --  Atomically commit to a prepared blend or the original stopping tail before evaluating the affected region.
   --  Never waits or calls a protected object. Once chosen, the trajectory cannot change underneath the consumer.

   function Has_Next_Block (Block : Block_Handle) return Boolean
   with Pre => not Is_Empty (Block);
   --  True only after Prepare_Segment has accepted this block's outgoing blend.

   procedure Advance_Block (Block : in out Block_Handle)
   with Pre => not Is_Empty (Block) and then Has_Next_Block (Block);
   --  Claim the prepared successor and atomically retire the completed slot. A background cleaner finalizes it, so
   --  extra-data finalizers never run on Runner while it plans. No allocation, finalization, protected call or waiting
   --  occurs on this moving handoff.

   procedure Release (Block : in out Block_Handle);
   --  At a safe stop, release this block and any unconsumed linked successors, leaving the handle empty. Discarding
   --  a successor planned since the last Reset makes Dequeue wait for Reset. Finalizers run outside the ring lock.
   --  Finish cleanup, including background cleanup of slots retired by Advance_Block, before propagating the first
   --  cleanup exception.

   Out_Of_Bounds_Error : exception;

   task Runner
     with CPU => Runner_CPU, Storage_Size => 32 * 1024 * 1024 is
      --  TODO: We can probably lower Storage_Size now that the step rate limiter is gone.
      entry Setup (In_Params : Kinematic_Parameters; In_Motor_Map : Motor_Position_Map);
      entry Reset_Do_Not_Call_From_Other_Packages;
      --  Call the Reset procedure rather than this entry to stop dequeueing and reset the preprocessor.
      --  TODO: There must be some way to hide this while still exposing the task.
   end Runner;

private

   type Execution_Block (N_Corners : Corners_Index := 1);
   type Execution_Block_Access is access constant Execution_Block with Storage_Size => 0;

   type Block_Index is mod 5;
   Block_Capacity : constant Positive := Block_Index'Modulus;
   subtype Block_Count is Natural range 0 .. Block_Capacity;

   type Block_Handle is new Ada.Finalization.Limited_Controlled with record
      Data         : Execution_Block_Access := null;
      Index        : Block_Index := Block_Index'First;
      Use_Tail     : Boolean := False;
      Tail_Decided : Boolean := False;
      --  Slot designated by Data. Only meaningful while Data is not null.
   end record;

   overriding
   procedure Finalize (Block : in out Block_Handle);

   type Command_Kind is
     (Move_Kind,
      Helix_Move_Kind,
      Corner_Extra_Data_Kind,
      Flush_Kind,
      Homing_Flush_Kind,
      Flush_And_Reset_Position_Kind,
      Flush_And_Change_Parameters_Kind);

   type Command (Kind : Command_Kind := Move_Kind) is record
      case Kind is
         when Flush_Kind | Homing_Flush_Kind | Flush_And_Reset_Position_Kind | Flush_And_Change_Parameters_Kind =>
            Flush_Resetting_Data : Flush_Resetting_Data_Type;
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

   Motor_Delta_Numerical_Safety_Factor : constant Dimensionless := 0.999;
   Maximum_Homing_Feedrate_Adjustments : constant Positive := 128;
   Home_Move_Required_Coast_Time       : constant Time := 2.0 * Home_Move_Minimum_Coast_Time + Interpolation_Time;
   --  Reserve the configured leeway on both sides of the loop command plus one interpolation period for rounding the
   --  minimum time up to an emitted command.

   function Scaled_Curvature_Norm (Coefficients : Projection_Coefficients) return Curvature;
   function Scaled_Curvature_Hypot (Left, Right : Curvature) return Curvature;

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

   type Run_Direction is (No_Run, Extruding, Retracting);
   --  The E direction of a run of segments that Extrusion_Density_Normalizer may give one common density.

   function Segment_Run_Direction (Spatial_Length, E_Change : Length) return Run_Direction;
   --  The run direction of a segment with XYZ path length Spatial_Length and E displacement E_Change. Travel and
   --  E-only moves join no run.

   function Primitive_Run_Direction
     (Primitive : Path_Primitive; Start_Point, End_Point : Position) return Run_Direction;
   --  The Segment_Run_Direction of Primitive between the supplied endpoints, measuring its path with E ignored.

   function Continues_Run (Run, Next : Run_Direction; Dwell_Between : Time) return Boolean;
   --  Whether a segment in direction Next can join a run in direction Run across a corner that dwells for
   --  Dwell_Between. A travel, E reversal, E-only move or dwell ends the run.

   type Block_Path_Primitives is array (Corners_Index range <>) of Path_Primitive;

   --  Corner-transition construction
   type Block_Corner_Transitions is array (Corners_Index range <>) of Corner_Transition_Evaluator;

   --  Feedrate_Profile_Generator
   type Block_Feedrate_Profiles is array (Corners_Index range <>) of Feedrate_Profile;

   --  Kinematic_Limiter
   type Block_Corner_Velocity_Limits is array (Corners_Index range <>) of Velocity;

   type Block_Segment_Lengths is array (Corners_Index range <>) of Length;
   type Block_Segment_Times is array (Corners_Index range <>) of Time;
   type Block_Timing_Run_Offsets is array (Corners_Index range <>) of Corners_Index'Base
   with Default_Component_Value => 0;
   --  Segment counts to the ends of a timing run. Zero by default, so a block that was never assigned runs has only
   --  single-segment runs.
   type Block_Primitive_Derivative_Bounds is array (Corners_Index range <>) of Unit_Speed_Axial_Derivative_Bounds;
   type Block_Extrusion_Densities is array (Corners_Index range <>) of Dimensionless;
   --  Signed E displacement per unit of commanded path length. E-only moves use |Delta_E| as path length.

   type Extrusion_Transition is record
      Half_Width     : Length := 0.0 * mm;
      Density_Change : Dimensionless := 0.0;
   end record;
   --  A Smooth_Deviation density change at a corner, fixed in path distance rather than time. Over Half_Width of path
   --  on each side of the corner, E's density follows rho_in + Density_Change * h (u), where u runs from 0 to 1 across
   --  the 2 * Half_Width of the transition and h (u) = 35u^4 - 84u^5 + 70u^6 - 20u^7. h matches the two densities
   --  through h''' at both edges, so E stays a function of path position with bounded distance derivatives through
   --  order five, and the existing mixed-derivative certificates bound its time derivatives at any path speed.
   --
   --  Density_Change is the outgoing density minus the incoming one, so each segment can evaluate the transitions at
   --  both of its corners from its own density alone. A zero Half_Width means no transition: E's velocity steps at the
   --  corner, which Extrusion_Junction_Ceiling limits.

   type Block_Extrusion_Transitions is array (Corners_Index range <>) of Extrusion_Transition;

   type Profile_Window is record
      Start_Distance : Length := 0.0 * mm;
      Distance       : Length := 0.0 * mm;
   end record;

   type Profile_Window_Candidate_Index is range 1 .. 4;
   type Profile_Window_Candidates is array (Profile_Window_Candidate_Index) of Profile_Window;
   type Block_Profile_Windows is array (Corners_Index range <>) of Profile_Window;
   type Block_Profile_Crackles is array (Corners_Index range <>) of Crackle;

   type Profile_End is record
      Profile           : Feedrate_Profile :=
        (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
      Max_Crackle       : Crackle := 0.0 * mm / s**5;
      Decel_Crackle     : Crackle := 0.0 * mm / s**5;
      Boundary_Velocity : Velocity := 0.0 * mm / s;
   end record;
   --  Generated motion outside the central profile window. Boundary_Velocity is the velocity at the join with the
   --  central profile; its acceleration, jerk and snap are zero, as at every full-profile endpoint. Max_Crackle
   --  drives the acceleration ramp and Decel_Crackle the deceleration ramp; they differ only for cellular runs.

   type Profile_End_Pair is record
      Enabled        : Boolean := False;
      Prefix, Suffix : Profile_End;
   end record;
   type Block_Profile_Ends is array (Corners_Index range <>) of Profile_End_Pair;

   type Profile_Window_Evaluation is record
      Valid         : Boolean := False;
      Window        : Profile_Window;
      Limits        : Scalar_Derivative_Limits;
      Max_Vel       : Velocity := 0.0 * mm / s;
      Start_Max_Vel : Velocity := 0.0 * mm / s;
      End_Max_Vel   : Velocity := 0.0 * mm / s;
      Bounds        : Unit_Speed_Axial_Derivative_Bounds;
      Curved        : Boolean := False;
      Cellular      : Boolean := False;
   end record;
   --  Limits contains the tangent-only envelope. Curved windows retain geometry so each candidate velocity
   --  change can be certified at its own speed and derivative peaks, rather than at an unreachable ceiling.
   --  Start_Max_Vel and End_Max_Vel admit the speeds at the window's start and end.
   --
   --  A Cellular window belongs to a multi-segment timing run, and its profiles are certified against the run's
   --  Path_Cells. Each endpoint ceiling comes from the cells at that end, Max_Vel is the lower of the two, and the
   --  cells cap the peak. Its Bounds still merge the whole window, for the conservative retry only. In any other
   --  window both endpoint ceilings equal Max_Vel.

   type Window_Profile_Result is record
      Valid         : Boolean := False;
      Profile       : Feedrate_Profile :=
        (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
      Max_Crackle   : Crackle := 0.0 * mm / s**5;
      Decel_Crackle : Crackle := 0.0 * mm / s**5;
   end record;
   --  Max_Crackle drives the acceleration ramp and Decel_Crackle the deceleration ramp.

   type Path_Cell is record
      Start, Stop : Length := 0.0 * mm;
      Bounds      : Unit_Speed_Axial_Derivative_Bounds;
      Ceiling     : Velocity := 0.0 * mm / s;
      Curved      : Boolean := False;
   end record;
   --  One geometric piece of a timing run, in run distance: a transition portion, a primitive, or a zero-length point
   --  where E's velocity jumps. Ceiling combines the constant-speed axial ceiling of Bounds, the motor-delta ceiling
   --  and the feedrate; a point cell has only its E junction ceiling. Curved is False when Bounds have only velocity
   --  terms; the tangent-only envelope of a window then already certifies every ramp built from it.

   type Path_Cells is array (Positive range <>) of Path_Cell;

   type Path_Cell_Caches is array (Positive range <>) of Mixed_Derivative_Cache;
   --  One cache per cell, reusable across every certificate for the same cells and parameters.

   function Cell_Overlaps (Cell : Path_Cell; Low, High : Length) return Boolean
   is (if Cell.Stop > Cell.Start
       then Cell.Start < High and then Cell.Stop > Low
       else Cell.Start >= Low and then Cell.Start <= High);
   --  Whether a piece overlaps the interval with positive length, or a point cell lies inside it.

   type Corner_Flags is array (Corners_Index range <>) of Boolean with Default_Component_Value => False;

   type Profile_Window_Evaluation_Set is array (Profile_Window_Candidate_Index) of Profile_Window_Evaluation;
   type Profile_Window_Evaluation_Cache is array (Finishing_Corners_Index range <>) of Profile_Window_Evaluation_Set;
   type Block_Planning_Attempts is array (Finishing_Corners_Index range <>) of Natural;

   type Reachable_Velocity_Memo is record
      Fixed_Vel : Velocity := 0.0 * mm / s;
      Reachable : Velocity := 0.0 * mm / s;
      Valid     : Boolean := False;
   end record;
   type Reachable_Velocity_Memo_Pair is array (Boolean) of Reachable_Velocity_Memo;
   --  Indexed by whether the fixed velocity is at the start of the segment.
   type Reachable_Velocity_Memo_Cache is array (Finishing_Corners_Index range <>) of Reachable_Velocity_Memo_Pair;

   type Planning_Workspace (Last_Corner : Corners_Index) is record
      Check_Extrusion_Position : Boolean := True;
      --  Whether all commanded E coordinates are in bounds. Compute once per pass; homing can bypass bounds.
      Local_Comparison_Corners : Natural := 0;
      Local_Stops_Selected     : Natural := 0;

      --  Extrusion_Density_Normalizer
      Unblended_Segment_Lengths     : Block_Segment_Lengths (2 .. Last_Corner);
      Unblended_Extrusion_Positions : Block_Segment_Lengths (1 .. Last_Corner);
      --  Original scalar path lengths and normalized E boundary positions, before XYZ corner shortening.
      --  Corner_Blender uses these on the initial pass and on geometry recovery without rederiving densities.
      Unblended_Length_Spreads      : Block_Extrusion_Densities (2 .. Last_Corner);
      --  A first-order bound on how far rounding the XYZ coordinates changes each original path length, per unit of
      --  XYZ_Resolution. Used only while normalizing densities.

      --  Kinematic_Limiter
      Static_Corner_Limits     : Block_Corner_Velocity_Limits (1 .. Last_Corner);
      Window_Evaluations       : Profile_Window_Evaluation_Cache (2 .. Last_Corner);
      --  Cache the limits for each corner and candidate window across forward/reverse lookahead passes.
      Reachable_Velocity_Memos : Reachable_Velocity_Memo_Cache (2 .. Last_Corner);
      --  The latest reachability across each segment in each direction. Lookahead cleanup passes revisit most
      --  segments with an unchanged neighbouring junction speed.
      Timing_Joins             : Corner_Flags (1 .. Last_Corner);
      --  Corners that the current timing-run assignment joins.
      Timing_Join_Vetoes       : Corner_Flags (1 .. Last_Corner);
      --  Corners that must stay junctions because a run joined across them could not be certified. Cleared with the
      --  planning attempts.

      --  Corner_Blender
      Original_Extrusion_Positions : Block_Segment_Lengths (1 .. Last_Corner);
      --  Preserve commanded E while constructing geometry from XYZ alone, including exception recovery.
      Corner_Derivative_Bounds     : Block_Primitive_Derivative_Bounds (1 .. Last_Corner);

      --  Planner
      Planning_Attempts          : Block_Planning_Attempts (2 .. Last_Corner);
      --  Count certification retries per segment. Reset for each block and when falling back to a stopped path.
      Suffix_E_Min, Suffix_E_Max : Block_Segment_Lengths (2 .. Last_Corner);
      --  Bounds on the original remaining E trajectory, used to certify cumulative shifts after local untrimming.
   end record;
   --  Data needed while planning a block but not while executing it. One workspace is allocated per planner instance
   --  and reused after the corresponding Execution_Block has finished planning.

   type Profile_Planning_Result (Valid : Boolean := True) is record
      case Valid is
         when True =>
            null;

         when False =>
            Failed_Segment : Finishing_Corners_Index;
      end case;
   end record;

   procedure Blend_Blocks
     (Left, Right : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Blended     : out Boolean);
   --  Replace the stop where Left ends and Right begins with a blended corner, replanning at most Blend_Radius
   --  segments on each side with the outer speeds fixed. Blended is True only if the replacement is certified and
   --  faster, in which case Right's later E references are also translated; otherwise neither block is modified.
   --  The caller first checks Blendable_Boundary and Allow_Blended_Entry, before copying any window.

   procedure Plan_Fixed_Geometry
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace);
   --  Certify the supplied geometry, retaining the bounded retry and stopped recovery policy.

   procedure Plan_Kinematics
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace);
   --  Certify junctions and profiles, then try stopping at each stereographic corner in a forward sweep.
   --  Replan its two adjoining segments with the outer junctions fixed; keep only certified, faster pairs.
   --  If that fails after the first segment, try stopping at the next corner as well, replanning three segments.
   --  Keep the pair only if it beats both the current plan and stopping at the next corner alone.
   --  With Params.Continuous_Timing, a second sweep tries joining the timing runs on either side of each non-binding
   --  curved junction (Can_Merge_Timing_Runs), again with the outer junctions fixed, and keeps only certified, faster
   --  runs of at most Blend_Radius segments.
   --  Homing bypasses selection. These local searches need not find the best combination of corner policies.

   Negligible_Density_Change : constant Dimensionless := 1.0E-12;
   --  Relative density changes this small are roundoff between nominally equal densities, as when commanded E is not
   --  normalized. Smooth_Deviation lets E's velocity step by such a change rather than smoothing it or stopping.

   function Density_Change_Is_Negligible
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Boolean;
   --  Whether the density change at interior Corner is within Negligible_Density_Change of both densities.

   function Extrusion_Deviation (Params : Kinematic_Parameters) return Length;
   --  The E deviation allowed for Smooth_Deviation transitions: the cornering model's E entry, or zero for Sharp_SCV
   --  and in Instantaneous_Velocity_Change mode.

   function Smooths_Density_Change
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Boolean;
   --  Whether the density change at interior Corner may get a Smooth_Deviation transition: one that is not negligible,
   --  with a nonzero Extrusion_Deviation, whose density ratio is at least the matching Smoothed ratio minimum.

   function Extrusion_Junction_Ceiling
     (Block : not null access constant Execution_Block; Corner : Corners_Index; Limit : Velocity) return Velocity;
   --  Lower Limit so that E's instantaneous velocity change at an interior corner fits Velocity_Change_Max. A
   --  Smooth_Deviation transition has no velocity change, since its derivative bounds limit the coasts through it
   --  rather than the corner itself, and Smooth_Deviation lets roundoff step E's velocity.

   function Corner_Policy_Ceiling
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Corner    : Corners_Index;
      Bounds    : Unit_Speed_Axial_Derivative_Bounds;
      Limit     : Velocity) return Velocity;
   --  Lower Limit to what Corner's transition policy allows: zero for a hard stop or an empty curve, the junction
   --  velocity of a square corner, or the constant-speed ceiling of a curve's derivative Bounds and XYZ motor deltas.

   function Extrusion_Transition_Reach
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Corner    : Corners_Index) return Length;
   --  The half-width of the transition that interior Corner would take with unlimited room: the narrowest whose E
   --  derivative bounds do not lower the corner's ceiling from its feedrates and Corner_Policy_Ceiling, but within
   --  Transition_Distance_Max and Extrusion_Deviation. Zero when Smooths_Density_Change is False or the corner stops.

   function Plan_Extrusion_Transition
     (Block : not null access constant Execution_Block; Corner : Corners_Index; Reach, Incoming, Outgoing : Length)
      return Extrusion_Transition;
   --  The transition at interior Corner of half-width Reach, narrowed to reach at most Incoming before the corner and
   --  Outgoing after it. Zero when that leaves nothing or its bounds would overflow. Narrowing lowers the corner's
   --  ceiling, but never breaks the deviation or Transition_Distance_Max that Reach already fits.

   procedure Assign_Extrusion_Transitions
     (Block : aliased in out Execution_Block; Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map);
   --  Plan every interior corner's transition from the blended geometry and limited feedrates. Corners are fitted in
   --  order: each takes what the previous corner's final transition leaves of its incoming segment, and leaves the
   --  next corner the smaller of that corner's reach and half of their shared segment. A block end always keeps half
   --  of its segment for a later inter-block blend. Transitions may overlap XYZ transitions, since each axis merges
   --  its own bounds.

   function Extrusion_Reference_At_Distance
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Distance : Length)
      return Length;
   --  E position after correction for path shortening, before density transitions.

   function Extrusion_At_Distance
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Distance : Length)
      return Length;
   --  E position after correction for path shortening, including the transitions at both of the segment's corners.

   function Extrusion_Bounds
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Window : Profile_Window)
      return Unit_Speed_Axial_Derivative_Bounds;
   --  E's distance derivative bounds over Window of the segment: its density between transitions, and the analytical
   --  bounds of any transition Window overlaps. A zero-length window is a point, inside a transition at its edges.

   function XYZE_Spatial_Scale (Density : Dimensionless) return Dimensionless;
   --  The ratio of path speed to XYZE speed at the given E density, for converting XYZE feedrates and caps.

   function Spatial_Primitive
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index)
      return Derived_Path_Primitive;

   function Derived_Primitive_Point_At_Distance
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Primitive        : Derived_Path_Primitive;
      Distance         : Length) return Position;
   --  Primitive_Point_At_Distance for a Primitive already returned by Spatial_Primitive (Block, Finishing_Corner), so
   --  that callers sampling one primitive many times derive its helix once.

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
   --  Limit Max_Vel so every motor delta remains within its per-command bound throughout Window. Where a density
   --  transition in Window raises E's density above the segment's own, also keep the XYZE speed within
   --  Tangential_Velocity_Max, which Limited_Segment_Feedrates converted at the segment's density.

   function Window_Axial_Derivative_Bounds
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window) return Unit_Speed_Axial_Derivative_Bounds;
   --  Merge the axial derivative bounds of every transition or primitive portion overlapped by Window.

   function Spatial_Motor_Map
     (Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map)
      return Prunt.Motion_Planner.Planner.Motor_Position_Map;
   --  Project XYZ independently; E's reference is bounded separately.

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

   function Segment_Prefix_Time
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Time;

   function Segment_Suffix_Time
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Time;

   function Central_Profile_Start_Velocity
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Velocity;

   --  Continuous timing. A timing run is identified by its first segment. With Params.Continuous_Timing False every
   --  run has one segment, and each function below reduces to its per-segment counterpart exactly.

   Timing_Run_Feedrate_Tolerance : constant Dimensionless := 1.0E-9;
   --  Relative feedrate difference below which Joins_Timing_Run joins two segments.

   Timing_Merge_Margin : constant Dimensionless := 0.05;
   --  A curved junction is merged only if its planned speed is below its static limit by at least this fraction.
   --  Such a junction cannot be a speed minimum, so stopping its acceleration there is pure cost.

   Timing_Merge_Minimum_Ratio : constant Dimensionless := 1.0E-6;
   --  A junction planned at a negligible fraction of its limit is effectively a stop, and stays a junction.

   Cellular_Search_Tolerance : constant Dimensionless := 1.0E-3;
   --  Relative width at which the cellular speed bisections stop. Every step costs a full cellular certificate, and
   --  Reachable_Cellular_Velocity keeps only 0.999 of its result, so a finer search buys almost nothing.

   function Timing_Run_First
     (Block : not null access constant Execution_Block; Segment : Finishing_Corners_Index)
      return Finishing_Corners_Index;

   function Timing_Run_Last
     (Block : not null access constant Execution_Block; Segment : Finishing_Corners_Index)
      return Finishing_Corners_Index;

   function Joins_Timing_Run
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Boolean;
   --  Whether the segments ending and starting at interior Corner can share one profile: an exact line passthrough
   --  with no dwell, matching feedrates and a non-binding E velocity jump.

   function Can_Merge_Timing_Runs
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Boolean;
   --  Whether a curved or helical junction at interior Corner could be merged into one run: a passthrough or a
   --  derivative-bounded transition with no dwell, positive feedrates and positive distance on both sides.
   --  Plan_Kinematics tries only those junctions that its plan shows are not binding, and keeps a merge only if it
   --  is faster.

   procedure Assign_Timing_Runs (Block : aliased in out Execution_Block; Joins : Corner_Flags);
   --  Group segments into maximal runs joined at the corners flagged in Joins. Every run has one segment unless
   --  Params.Continuous_Timing.

   procedure Assign_Timing_Runs (Block : aliased in out Execution_Block; Forced_Junction : Corners_Index'Base);
   --  Group segments into maximal runs joined by Joins_Timing_Run corners. Forced_Junction, when it is a corner of
   --  Block, always ends a run; pass zero for none. Every run has one segment unless Params.Continuous_Timing.

   function Is_Cellular_Run
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Boolean;
   --  Whether the run has more than one segment, so its profiles are certified cell by cell.

   function Timing_Run_Cell_Count
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Natural;

   function Timing_Run_Cells
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Run_First : Finishing_Corners_Index;
      Max_Vel   : Velocity) return Path_Cells;
   --  The run's pieces in run distance, each with its own bounds and ceiling (capped by Max_Vel and its segment's
   --  feedrate), and a point cell at every interior corner whose E velocity jump limits the speed. Pieces also end at
   --  the edges of E density transitions, so a transition's bounds only cover its own part of a primitive.

   function Timing_Total_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;

   function Timing_Start_Transition_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;

   function Timing_End_Transition_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;

   function Timing_Start_Curve_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;
   --  The run's curved start for split profiles: its XYZ transition portion, or the E density transition at its first
   --  corner when that reaches further. A split's prefix then holds a tight E transition, while ordinary windows cut
   --  only at XYZ transitions and accelerate through gentle ones.

   function Timing_End_Curve_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;
   --  The run's curved end, like Timing_Start_Curve_Distance. An E transition never reaches past the curved start.

   function Timing_Straight_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length;

   function Timing_Feedrate
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Velocity;
   --  The highest limited feedrate of the run's segments. Each of the run's cells keeps its own segment's feedrate.

   function Evaluate_Timing_Window
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Run_First : Finishing_Corners_Index;
      Window    : Profile_Window;
      Max_Vel   : Velocity) return Profile_Window_Evaluation;
   --  Evaluate_Profile_Window for a window in run distance. A single-segment run is evaluated exactly as before; a
   --  longer run gives a Cellular evaluation whose Bounds merge every overlapped segment's bounds, and whose
   --  endpoint ceilings come from the cells containing each end.

   function Timing_Motion_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Time;
   --  The run's planned motion time, excluding the dwell at its last corner.

   function Timing_Distance_At_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index; Run_Time : Time)
      return Length;

   function Timing_Velocity_At_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index; Run_Time : Time)
      return Velocity;

   procedure Assign_Timing_Members (Block : not null access Execution_Block; Run_First : Finishing_Corners_Index);
   --  After the profile of the multi-segment run starting at Run_First is selected, solve each segment's offsets and
   --  duration, and record the run speed at each interior corner.

   function Window_Between (First, Last : Length) return Profile_Window;
   --  The window from First to Last, shortened by roundoff where needed so that it never reaches past Last.

   type Split_Profile_Windows is array (Positive range 1 .. 3) of Profile_Window;

   function Timing_Split_Windows
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index)
      return Split_Profile_Windows;
   --  The curved start, the middle and the curved end of the timing run starting at Run_First, as consecutive windows
   --  covering the run. In a multi-segment run the middle never reaches past the cut where the end curve's cells
   --  start, and where the curves meet it is empty rather than negative through roundoff.

   function Segment_Profile_Window_Candidates
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window_Candidates;
   --  Return the deterministic candidate profile windows, in run distance, for the timing run starting at
   --  Finishing_Corner.

   function Evaluate_Profile_Window
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Profile_Window_Evaluation;
   --  Cache geometry, tangent-only limits and the constant-speed/motor ceiling for a candidate window.

   function Profile_At_Peak
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel, End_Vel, Peak : Velocity)
      return Window_Profile_Result;
   --  Construct and certify a fixed-peak profile using a common time stretch of its velocity transitions.

   function Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity;
      Cache                    : in out Mixed_Derivative_Cache) return Window_Profile_Result;
   --  Identical to the uncached form. Cache must only be shared between calls with the same Params and Eval.

   function Generate_Window_Profile
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel, End_Vel : Velocity)
      return Window_Profile_Result;
   --  Search feasible peak speeds for a fast certified profile, retaining the maximum-speed coast candidate.

   function Reachable_Window_Velocity
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel : Velocity) return Velocity;
   --  Search velocity changes using the same trajectory certificate as final profile generation.

   function Cellular_Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Cells                    : Path_Cells;
      Caches                   : in out Path_Cell_Caches;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity) return Window_Profile_Result
   with Pre => Caches'First = Cells'First and then Caches'Last = Cells'Last;
   --  Profile_At_Peak for a cellular window. Each ramp is certified separately, in every cell it crosses, at the
   --  highest speed it reaches there; a ramp that fails is stretched in time and certified again. The coast must stay
   --  below every cell ceiling it crosses. Caches must only be reused with the same Params and Cells.

   function Generate_Cellular_Profile
     (Params             : Kinematic_Parameters;
      Cells              : Path_Cells;
      Eval               : Profile_Window_Evaluation;
      Start_Vel, End_Vel : Velocity) return Window_Profile_Result;
   --  Generate_Window_Profile with the cellular certificate.

   function Reachable_Cellular_Velocity
     (Params    : Kinematic_Parameters;
      Cells     : Path_Cells;
      Eval      : Profile_Window_Evaluation;
      Fixed_Vel : Velocity;
      Forward   : Boolean) return Velocity;
   --  Reachable_Window_Velocity with the cellular certificate. A cellular window is not symmetric, so Forward selects
   --  whether Fixed_Vel is at the start (reach the end) or at the end (reach the start). The result never exceeds
   --  the reached end's ceiling.

   function Profile_Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Accel_Crackle, Decel_Crackle : Crackle; Start_Vel : Velocity)
      return Length;
   --  Distance_At_Time with separate ramp crackles. Equal crackles give exactly Distance_At_Time.

   function Profile_Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Accel_Crackle, Decel_Crackle : Crackle; Start_Vel : Velocity)
      return Velocity;
   --  Velocity_At_Time with separate ramp crackles. Equal crackles give exactly Velocity_At_Time.

   function Identical (Left, Right : Velocity) return Boolean;
   --  Compare representations, including the sign of zero, so a deterministic result is reused only for the
   --  exact input that produced it.

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

   function Homing_Profile_Violation (Block : not null access constant Execution_Block) return Time;
   --  Return the combined short-coast and over-budget-tail duration for a homing block.

   function Homing_Unavoidable_Tail_Time (Block : not null access constant Execution_Block) return Time;
   --  Return the part of a homing tail which cannot be placed before the loop command: the required final coast,
   --  deceleration, suffix motion, and dwell.

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
      Blendable_End             : Boolean := False;
      Kind                      : Execution_Block_Kind := Motion_Block_Kind;
      Flush_Resetting_Data      : Flush_Resetting_Data_Type := Flush_Resetting_Data_Type_Default;
      Next_Block_Pos            : Position;
      Params                    : Kinematic_Parameters;
      First_Corner_ID           : Planner_Corner_ID := 0;
      Associated_Overflow_Block : Boolean := False;
      Is_Homing_Move            : Boolean;
      Loop_Move_Minimum_Time    : Time := 0.0 * s;
      --  Earliest interpolation time at which the loop command of a homing block may be emitted.

      Corners_Extra_Data_End_Indices : Block_Corners_Extra_Data_End_Indices (1 .. N_Corners);
      --  Indices into the owning ring slot's Extra_Data. Keeping that vector outside this record means scratch blocks
      --  and tail patches never hold corner extra data. They still carry Flush_Resetting_Data (a controlled holder in
      --  the controller), which Copy_Window and Copy_Motion_Window never copy.
      Corners                        : Block_Plain_Corners (1 .. N_Corners);
      Primitives                     : Block_Path_Primitives (2 .. N_Corners);
      Original_Segment_Feedrates     : Block_Segment_Feedrates (2 .. N_Corners);
      Corner_Dwell_Times             : Block_Corner_Dwell_Times (2 .. N_Corners);

      --  Extrusion_Density_Normalizer
      Extrusion_Densities : Block_Extrusion_Densities (2 .. N_Corners);
      --  Authoritative E displacement per unit path distance for segment I, ending at corner I. Accepted runs share
      --  one stored floating-point value exactly; later stages must never reconstruct it from Corners. At distance D,
      --  E = Extrusion_Reference_Positions (I - 1) + Extrusion_Densities (I) * D. Negative densities represent
      --  retraction; E-only moves retain their original signed density.

      --  Corner_Blender
      Corner_Transitions        : Block_Corner_Transitions (1 .. N_Corners);
      Primitive_Distances       : Block_Segment_Lengths (2 .. N_Corners);
      Primitive_Start_Distances : Block_Segment_Lengths (2 .. N_Corners);

      Extrusion_Reference_Positions : Block_Segment_Lengths (1 .. N_Corners);
      --  Entry I is the E coordinate at boundary I after density normalization and path shortening. Entry 1 is the
      --  block's starting E coordinate.

      Extrusion_Transitions : Block_Extrusion_Transitions (1 .. N_Corners);
      --  Smooth_Deviation transitions, planned with the geometry. Later stages retain them: a local stop clears only
      --  its own corner, and an inter-block blend plans only its new seam corner around the transitions it keeps. The
      --  transitions at a segment's two corners never overlap, so a segment never sees more than one at a point.

      --  Early_Kinematic_Limiter
      Limited_Segment_Feedrates : Block_Segment_Feedrates (2 .. N_Corners);

      --  Kinematic_Limiter
      Corner_Velocity_Limits : Block_Corner_Velocity_Limits (1 .. N_Corners);

      --  Feedrate_Profile_Generator
      Feedrate_Profiles      : Block_Feedrate_Profiles (2 .. N_Corners);
      Profile_Crackles       : Block_Profile_Crackles (2 .. N_Corners);
      Profile_Decel_Crackles : Block_Profile_Crackles (2 .. N_Corners);
      --  Crackles of each central profile's acceleration and deceleration ramps. They differ only for cellular runs.
      Profile_Windows        : Block_Profile_Windows (2 .. N_Corners);
      Profile_Ends           : Block_Profile_Ends (2 .. N_Corners) := [others => <>];
      --  Enabled entries replace the coasts outside Profile_Windows with generated acceleration profiles. This lets
      --  the retained primitive accelerate independently of tight bounds on the curved ends.

      --  Continuous timing (Params.Continuous_Timing)
      Timing_Run_Back  : Block_Timing_Run_Offsets (2 .. N_Corners);
      Timing_Run_Ahead : Block_Timing_Run_Offsets (2 .. N_Corners);
      --  Segment I belongs to the timing run of segments I - Timing_Run_Back (I) .. I + Timing_Run_Ahead (I), assigned
      --  by Kinematic_Limiter and changed by Plan_Kinematics' local searches. A run's interior corners are not
      --  junctions: the run has one profile, window and set of ends, stored at its first segment and expressed in run
      --  distance, and its other entries are unused. Offsets are relative, so copying a window that does not cut a run
      --  keeps them valid.

      Timing_Distance_Offsets : Block_Segment_Lengths (2 .. N_Corners);
      Timing_Time_Offsets     : Block_Segment_Times (2 .. N_Corners);
      Timing_Segment_Times    : Block_Segment_Times (2 .. N_Corners);
      --  For a segment of a multi-segment run: run distance and run time at its start, and its own duration including
      --  any dwell. Each time offset is the previous offset plus the previous duration, so evaluating either side of a
      --  member boundary gives the same run time. Unused for single-segment runs.
   end record;

   Blend_Radius : constant Corners_Index := Corners_Index'Min (32, (Corners_Index'Last - 1) / 2);

   type Tail_State is (Open, Preparing, Offered, Closed, Accepted) with Atomic, Size => 32;
   package Tail_Atomics is new System.Atomic_Operations.Exchange (Tail_State);
   --  Producer: Open -> Preparing -> Offered. Consumer closes the choice before the affected segment and accepts
   --  only an already Offered tail. A producer losing that exchange discards its patch and keeps the stopped plan.

   type Block_State is (Free, Planning, Ready, Linked, Borrowed, Retired, Cleaning) with Atomic, Size => 32;
   type Block_Wrapper is record
      State           : Block_State := Free;
      Choice          : aliased Tail_State := Closed;
      Planner_Reading : Boolean := False with Atomic;
      Next            : Block_Index := Block_Index'First;
      Extra_Data      : aliased Corner_Extra_Data_Vectors.Vector;
      Block           : aliased Execution_Block;
      Tail            : aliased Execution_Block (Blend_Radius + 1);
      --  Block is immutable after publication. Tail and Next are visible only after the atomic Offered publication.
      --  Tail stores at most 32 replacement segments, aligned to the end of this fixed-size scratch block.
      --  Planner_Reading pins the original while a consumer may close its choice and retire it during planning.
      --  Extra_Data holds Block's reference-holding corner data, built in place by the preprocessor.
   end record;

   type Block_Array is array (Block_Index) of Block_Wrapper;
   Block_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   type Block_Array_Access is access Block_Array with Storage_Pool => Block_Pool;
   Blocks     : constant Block_Array_Access := new Block_Array;
   --  Five reusable slots, each with a bounded tail patch; streaming length is independent of this capacity.

   procedure Clear_Block_Extra_Data (Index : Block_Index);
   --  Release callback/configuration references when discarding a slot's block. The block must not be executed
   --  afterwards. Take the slot index rather than the record because discarded blocks may be only partly initialized;
   --  only their reference-holding fields are cleared.

   type Block_Indices is array (Block_Index) of Block_Index;
   type Block_Set is array (Block_Index) of Boolean;

   type Reset_Epoch is mod 2**32;
   type Block_Epochs is array (Block_Index) of Reset_Epoch;

   protected Block_Ring is
      procedure Setup;
      procedure Request_Reset;
      procedure Report_Termination;
      procedure Begin_Discard (Pending : out Block_Set);
      entry Acquire (Index : out Block_Index; Reset_Called : out Boolean);
      procedure Begin_Blend (Previous : out Block_Index; Available : out Boolean);
      procedure Finish_Blend (Previous, Following : Block_Index; Blended : Boolean; Published : out Boolean);
      procedure Publish (Index : Block_Index);
      entry Dequeue (Block : in out Block_Handle);
      entry Begin_Release (Block_Index);
      --  Waits for a producer which lost the choice to finish reading this slot. Releasing a Linked slot discards a
      --  successor that was never claimed; if it was planned since the last Reset, Dequeue then waits for Reset.
      procedure Collect_Retired (Pending : out Block_Set);
      procedure Request_Collection;
      --  Runner asks Retired_Cleaner to reclaim retired slots now rather than at its next poll.
      entry Wait_For_Retirement (Pending : out Block_Set; Stop : out Boolean);
      --  Opens while a consumer can retire a slot without a protected call, on a collection request, or once Runner
      --  has terminated. Only Runner links new successors, which re-evaluates this barrier. Pending returns the
      --  retired slots now owned by Retired_Cleaner, which has no caller to report to. Stop is True once Runner has
      --  terminated and none remain.
      entry Wait_For_Collection_Request;
      --  Lets an idle Retired_Cleaner poll without delaying a collection that Runner requests.
      procedure Finish_Cleanup (Index : Block_Index);
      procedure Defer_Cleanup_Error (Occurrence : Ada.Exceptions.Exception_Occurrence);
      entry Take_Cleanup_Error (Error : in out Cleanup_Exceptions.Holder);
      --  Waits for Retired_Cleaner's slots in progress, then hands their first failure to the next Release.
   private
      Deferred_Error        : Cleanup_Exceptions.Holder;
      Deferred_Cleanup      : Block_Set := [others => False];
      Collection_Requested  : Boolean := False;
      Epoch                 : Reset_Epoch := 0;
      Slot_Epochs           : Block_Epochs := [others => 0];
      --  Begin_Discard advances Epoch. Slots keep the epoch in which Runner acquired them.
      Discontinuous         : Boolean := False;
      --  An unconsumed successor from the current epoch was released. Queued blocks would start where it ended.
      Running               : Boolean := False;
      Runner_Terminated     : Boolean := False;
      Tail_Valid            : Boolean := False;
      Tail_Index            : Block_Index := Block_Index'First;
      Free_Count            : Block_Count := Block_Count'Last;
      Next_Slot             : Block_Index := Block_Index'First;
      Ready_Slots           : Block_Indices := [others => Block_Index'First];
      Next_Read, Next_Write : Block_Index := Block_Index'First;
      Ready_Count           : Block_Count := 0;
   end Block_Ring;

   procedure Copy_Window
     (Source : Execution_Block; First, Last : Corners_Index; Target : in out Execution_Block; Into : Corners_Index);
   --  Copy every per-corner motion field of Source (First .. Last), and every per-segment field between those corners,
   --  into Target starting at corner Into. This is the single list of fields that inter-block windows exchange.

   procedure Copy_Motion_Window
     (Source : Execution_Block; First, Last : Corners_Index; Target : aliased out Execution_Block);
   --  Copy numeric motion fields only, aligning Source (First .. Last) with the end of Target. No user data copying.
   --  Segments of Target ending at or before the corner aligned with First are padding, not motion, and must never be
   --  evaluated.

   procedure Clear_Planned_Fields (Block : not null access Execution_Block);
   --  Initialize every field written by the stages after the preprocessor, in the block's current layout. This is the
   --  single list of planned fields; Copy_Window lists the same fields where windows exchange them. Those fields are
   --  not yet valid, so Block is passed by access rather than as an in out record checked for validity on entry.

   procedure Initialize_Scratch_Block (Block : aliased out Execution_Block);
   --  Give a numeric scratch block zeroed preprocessor fields and no plan. Callers fill the fields they use.

   function Is_Timing_Junction (Block : Execution_Block; Corner : Corners_Index) return Boolean;
   --  Whether Corner ends a timing run, so the full motion state there is a plateau that a window can be cut at.

   function Blendable_Boundary (Left, Right : Execution_Block) return Boolean;
   --  Structural conditions for Blend_Blocks, excluding the Allow_Blended_Entry policy. Prepare_Blend checks both
   --  before copying any window. Both fixed window ends must be timing junctions, so that no run is cut.

   type Runner_Finalizer is new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Finalize (This : in out Runner_Finalizer);

   function Segment_Time
     (Block : not null access constant Execution_Block; Finishing_Corner : Corners_Index) return Time;

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length;

   function Segment_Pos_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Position
   with
     Pre =>
       Finishing_Corner <= Block.N_Corners
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;

   function Segment_Vel_Ratio_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Dimensionless
   with
     Pre =>
       Finishing_Corner <= Block.N_Corners
       and then Time_Into_Segment <= Segment_Time (Block, Finishing_Corner)
       and then Time_Into_Segment >= 0.0 * s;

   function Next_Block_Pos (Block : not null access constant Execution_Block) return Position;

   function Block_Start_Pos (Block : not null access constant Execution_Block) return Position;

   function Flush_Resetting_Data (Block : not null access constant Execution_Block) return Flush_Resetting_Data_Type;

   function Loop_Move_Minimum_Time (Block : not null access constant Execution_Block) return Time
   with Pre => Block.Is_Homing_Move;

   function Block_Kind (Block : Execution_Block) return Execution_Block_Kind;

   function Corner_ID
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Planner_Corner_ID
   with Pre => Corner <= Block.N_Corners;

   procedure Corner_Extra_Data
     (Block      : not null access constant Execution_Block;
      Extra_Data : Corner_Extra_Data_Vectors.Vector;
      Corner     : Corners_Index;
      Process    : not null access procedure (Data : in out Corner_Extra_Data_Type))
   with Pre => Corner <= Block.N_Corners;

   function Has_Associated_Overflow_Block (Block : not null access constant Execution_Block) return Boolean;

   function Block_Kinematic_Parameters (Block : not null access constant Execution_Block) return Kinematic_Parameters;

   function Is_Homing_Move (Block : not null access constant Execution_Block) return Boolean;

end Prunt.Motion_Planner.Planner;

pragma Warnings (On, "formal object * is not referenced");
