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
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Interfaces;

with Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator;
with Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator.Motion;
with Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Elementary;
with Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
with Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;

pragma Extensions_Allowed (On);

package Prunt.Motion_Planner.Stereographic_Curves
  with SPARK_Mode => On
is
   type Stereographic_Curve is private;
   --  A successfully constructed blend together with the certificates and construction data needed for later bounds
   --  queries. Use To_Evaluator when only realtime point evaluation and arc length are required.

   type Stereographic_Curve_Evaluator is private;
   --  Compact data required after construction. Planner blocks store this type rather than the full construction
   --  curve.

   subtype Curve_Parameter is Dimensionless range 0.0 .. 1.0;
   --  Normalized physical distance along a curve. Zero denotes the start and one denotes the finish.

   Maximum_Trim_Asymmetry : constant Dimensionless := 20.0;
   --  Largest supported ratio between the distances trimmed from the incoming and outgoing paths. More asymmetric
   --  requests would require a distance warp whose numerical conditioning this package does not promise.

   Maximum_Trim_Asymmetry_Check : constant Dimensionless := Maximum_Trim_Asymmetry * (1.0 + 1.0E-12);
   --  Comparison threshold used at the inclusive 20:1 boundary. The tiny slack makes construction, serialization,
   --  and planner validation agree when an exact decimal boundary is rounded in binary floating point.

   type Tangent_Derivative_1_Vector is array (Axis_Name) of Curvature;
   --  First derivative of the unit tangent with respect to physical distance, resolved onto the machine axes.

   type Tangent_Derivative_2_Vector is array (Axis_Name) of Curvature_To_2;
   --  Second derivative of the unit tangent with respect to physical distance, resolved onto the machine axes.

   type Tangent_Derivative_3_Vector is array (Axis_Name) of Curvature_To_3;
   --  Third derivative of the unit tangent with respect to physical distance, resolved onto the machine axes.

   subtype Endpoint_Tangent_Derivative_Order is Natural range 0 .. 3;
   --  Order zero denotes tangent value; orders one through three denote its physical-distance derivatives.

   function Satisfies_Unit_Tangent_Identities
     (Tangent              : Position_Scale;
      Tangent_Derivative_1 : Tangent_Derivative_1_Vector;
      Tangent_Derivative_2 : Tangent_Derivative_2_Vector;
      Tangent_Derivative_3 : Tangent_Derivative_3_Vector) return Boolean;
   --  Check that the supplied derivatives could belong to a unit-length tangent field. Because the tangent must
   --  satisfy T·T = 1 at every point on the curve, its first three derivatives are constrained by the identities
   --  obtained by differentiating that equation:
   --
   --     T·T            = 1
   --     T·T′           = 0
   --     T·T″ + T′·T′   = 0
   --     T·T‴ + 3 T′·T″ = 0
   --
   --  Rejecting inconsistent endpoint data here prevents construction from treating an impossible tangent jet as
   --  valid; residuals consistent with floating-point roundoff are accepted. So is no tangent with a component above
   --  two in magnitude, and no derivative with a component above 2¹⁰⁰ in millimetres, which no machine has.

   type Endpoint_Tangent_Jet is record
      Tangent : Position_Scale := [X_Axis => 1.0, others => 0.0];
      --  Unit direction of travel at the endpoint.

      Tangent_Derivative_1 : Tangent_Derivative_1_Vector := [others => 0.0 / mm];
      --  Change in direction per unit of distance.

      Tangent_Derivative_2 : Tangent_Derivative_2_Vector := [others => 0.0 / mm**2];
      --  Change in turn rate per squared unit of distance.

      Tangent_Derivative_3 : Tangent_Derivative_3_Vector := [others => 0.0 / mm**3];
      --  Third distance derivative of the tangent.
   end record;
   --  Raw endpoint direction and its first three distance derivatives. Create_Blend validates this record explicitly
   --  so malformed requests produce Blend_Invalid_Start_Jets or Blend_Invalid_Finish_Jets in every assertion policy,
   --  rather than raising during parameter association. For each tangent-derivative order, successfully retained
   --  curves report a normalized-coordinate endpoint mismatch enclosure through
   --  Retained_Endpoint_Jet_Error_Bound; Dimensionless'Last means that optional enclosure was unavailable.

   type Blend_Endpoint is record
      Point : Position := [others => 0.0 * mm];
      --  Exact position at this end of the blend.

      Jet : Endpoint_Tangent_Jet;
      --  Direction and directional derivatives imposed on the ideal blend and measured after retained compilation.
   end record;
   --  All geometric conditions imposed at one end of a requested blend.

   type Bulge_Policy is
     (Never,
      --  Keep the projection of the authoritative ideal tangent onto the plane of the two endpoint tangents inside
      --  their cone. Travel normal to that plane is unrestricted. The mathematical retained evaluator is within
      --  Retained_Tangent_Error_Bound of that tangent, so its signed projection on any unit in-plane cone normal is
      --  at least -Retained_Tangent_Error_Bound.

      When_Unavoidable,
      --  Apply Never whenever the endpoint data admit a curve inside the cone, and Always otherwise. Leaving the cone
      --  is forced when the chord is outside it, including a chord off the line of two parallel endpoint tangents, or
      --  when an endpoint curvature turns the tangent out of it.

      Always
      --  Retain cone-compliant curves and additionally permit leaving the cone when non-negative progress along the
      --  chord can be established.
     );
   --  How far a blend may turn beyond its endpoint tangents.

   type Blend_Request is record
      Start : Blend_Endpoint;
      --  Position and tangent conditions at distance zero.

      Finish : Blend_Endpoint;
      --  Position and tangent conditions at the final distance.

      Maximum_Position_Error : Length := 0.0 * mm;
      --  Allowed ideal-to-realtime-representation error. Construction proves a uniform Bernstein bound for the
      --  rational tangent representation, then adds endpoint and floating-point evaluation allowances.

      Maximum_Arc_Length : Length := 0.0 * mm;
      --  Hard upper bound on the constructed curve length.

      Bulge : Bulge_Policy := Never;
      --  Whether the curve may turn outside the cone spanned by the two endpoint tangents.
   end record;

   type Blend_Result_Kind is
     (Blend_Not_Attempted,
      --  Default storage state. Create_Blend never returns this.

      Blend_Success,
      --  A complete checked curve is present.

      Blend_Invalid_Start_Point,
      --  The start position is non-finite or too large for safe arithmetic.

      Blend_Invalid_Finish_Point,
      --  The finish position is non-finite or too large for safe arithmetic.

      Blend_Endpoints_Too_Close,
      --  The chord cannot be normalised reliably at the coordinate scale.

      Blend_Invalid_Start_Jets,
      --  The start tangent conditions are unusable.

      Blend_Invalid_Finish_Jets,
      --  The finish tangent conditions are unusable.

      Blend_Invalid_Position_Error,
      --  The representation-error budget is not finite and positive.

      Blend_Invalid_Arc_Length_Limit,
      --  The length limit is unsafe or shorter than the chord.

      Blend_Closure_Failed,
      --  No supported frame and warp reached the requested finish.

      Blend_Representation_Failed,
      --  A closed ideal curve could not be compiled within the representation-error budget.

      Blend_Numerically_Unsafe
      --  Finite, trustworthy arithmetic could not be established.
     );
   --  Overall outcome of validating and constructing a blend request.

   function Arc_Length (Curve : Stereographic_Curve) return Length
   with Post => Arc_Length'Result >= 0.0 * mm;
   --  Return the authoritative physical length of a full construction result.

   function Arc_Length (Evaluator : Stereographic_Curve_Evaluator) return Length
   with Post => Arc_Length'Result >= 0.0 * mm;
   --  Return the authoritative physical length retained by a compact evaluator.

   type Blend_Result (Kind : Blend_Result_Kind := Blend_Not_Attempted) is record
      case Kind is
         when Blend_Success =>
            Curve : Stereographic_Curve;
            --  Complete curve and certificates produced for a successful request.

         when others =>
            null;
      end case;
   end record;
   --  A construction status with a curve payload only when construction succeeded.

   function Geometry_Certificate
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Certified_Geometry.Certificate;
   --  The proof-carrying certificate of the ideal tangent's geometry. Certified_Geometry states what it guarantees
   --  at every point of the curve: Lemma_In_Cone, Lemma_Chord_Progress and Lemma_Unit_Speed.

   function Certificate_Matches (Curve : Stereographic_Curve) return Boolean;
   --  Whether the certificate was made for the frame and chart which this curve itself uses.

   function Evaluator_Certificate
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Certified_Evaluator.Certificate;
   --  The proof-carrying certificate of the tangent which the realtime evaluator integrates. Certified_Evaluator
   --  and its child Motion state what it guarantees at every point of the curve.

   function Evaluator_Matches (Curve : Stereographic_Curve) return Boolean;
   --  Whether that certificate was made for the geometry certificate, the distance warp and the evaluator cache
   --  which this curve itself uses.

   function Finish_In_Millimetres
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Retained_Tangent.Dimensionless_Axis_Vector;
   --  The finish point which the evaluator returns at the end of the curve.

   function Uncorrected_Finish_In_Millimetres
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Retained_Tangent.Dimensionless_Axis_Vector;
   --  Where the evaluator's integral of the retained tangent ends, before its endpoint correction.

   function Length_In_Millimetres (Curve : Stereographic_Curve) return Dimensionless;
   --  Arc_Length as a plain number.

   function Executed_Tangent_Error_Bound (Curve : Stereographic_Curve) return Dimensionless;
   --  A proved uniform bound on the Euclidean distance between the ideal unit tangent and the tangent of the
   --  executed motion, endpoint correction included: Finish_Correction.Total_Tangent_Error_Bound of the
   --  evaluator certificate's own bound. It equals Retained_Tangent_Error_Bound except where construction kept a
   --  smaller bound from the Bernstein reconstruction, which is not proved.

   package Certified_Construction is

      pragma Assertion_Policy (Post => Ignore);
      --  The postcondition of Create_Blend speaks of ghost functions, which are never compiled, so it is proved and
      --  never executed, whatever the build profile. This package is only there to keep that policy from applying
      --  to the other contracts of Stereographic_Curves, which are executed where assertions are enabled.

      function Create_Blend (Request : Blend_Request) return Blend_Result
      with
        Post =>
          Create_Blend'Result.Kind /= Blend_Not_Attempted
          and then
            (if Create_Blend'Result.Kind = Blend_Success
             then
               Arc_Length (Create_Blend'Result.Curve) > 0.0 * mm
               and then Arc_Length (Create_Blend'Result.Curve) <= Request.Maximum_Arc_Length
               and then Certificate_Matches (Create_Blend'Result.Curve)
               and then
                 (case Stereographic_Kernel.Certified_Geometry.Kind
                         (Geometry_Certificate (Create_Blend'Result.Curve))
                  is
                    when Stereographic_Kernel.Certified_Geometry.Not_Certified  => False,
                    when Stereographic_Kernel.Certified_Geometry.In_Cone        => True,
                    when Stereographic_Kernel.Certified_Geometry.Chord_Progress => Request.Bulge /= Never)
               and then Evaluator_Matches (Create_Blend'Result.Curve)
               and then
                 Stereographic_Kernel.Certified_Evaluator.Motion.Is_Evaluator
                   (Evaluator_Certificate (Create_Blend'Result.Curve),
                    Finish_In_Millimetres (Create_Blend'Result.Curve),
                    Uncorrected_Finish_In_Millimetres (Create_Blend'Result.Curve),
                    Length_In_Millimetres (Create_Blend'Result.Curve),
                    Executed_Tangent_Error_Bound (Create_Blend'Result.Curve)));

   end Certified_Construction;

   function Create_Blend (Request : Blend_Request) return Blend_Result renames Certified_Construction.Create_Blend;
   --  Validate Request, solve the endpoint-closure problem, and compile the result into a bounded realtime
   --  representation. Failure is reported through the result kind; invalid input or an unsupported geometry does not
   --  raise an exception.
   --
   --  A successful result carries a certificate for its own ideal tangent: it stays in the cone of the endpoint
   --  tangents, or, only when the request allows bulging, it never travels backwards along the chord. The
   --  postcondition of Certified_Construction.Create_Blend, which states this, is proved without relying on the
   --  construction itself being proved: Create_Blend returns a curve only after checking the certificate which the
   --  proved certificate functions issued for it.
   --
   --  It also carries a certificate for the tangent which its realtime evaluator integrates, made for its own geometry
   --  certificate, warp and cache, with a finite bound, Executed_Tangent_Error_Bound, on the distance between the
   --  ideal tangent and the tangent of the executed motion. Stereographic_Kernel.Certified_Evaluator.Motion states
   --  what follows: the executed motion stays in the cone, or progresses along the chord, up to that bound, and has
   --  unit speed up to that bound and the frame's defect.

   function Derivative_Bounds (Curve : Stereographic_Curve) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return certified absolute bounds for each machine-axis component of the tangent and its first four distance
   --  derivatives over the complete curve.

   function Derivative_Bounds
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds
   with
     Pre =>
       Start_Distance >= 0.0 * mm and then Start_Distance <= End_Distance and then End_Distance <= Arc_Length (Curve);
   --  Return the same certified derivative bounds restricted to the closed physical-distance interval. A narrower
   --  interval can produce tighter bounds than the whole-curve query.

   function Point_At_Distance (Curve : Stereographic_Curve; Distance : Length) return Position
   with Pre => Distance >= 0.0 * mm and then Distance <= Arc_Length (Curve);
   --  Evaluate the retained realtime representation at a physical distance along a full construction result.

   function Point_At_Distance (Evaluator : Stereographic_Curve_Evaluator; Distance : Length) return Position
   with Pre => Distance >= 0.0 * mm and then Distance <= Arc_Length (Evaluator);
   --  Evaluate a compact realtime representation at a physical distance along the curve.

   function Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position;
   --  Evaluate the retained realtime representation using normalized physical distance.

   function Point_At_Parameter
     (Evaluator : Stereographic_Curve_Evaluator; Parameter : Curve_Parameter) return Position;
   --  Evaluate a compact realtime representation using normalized physical distance.

   function To_Evaluator (Curve : Stereographic_Curve) return Stereographic_Curve_Evaluator;
   --  Discard construction-only geometry and certificates, retaining the compact immutable data needed during motion
   --  execution.

   function Retained_Tangent_Error_Bound (Curve : Stereographic_Curve) return Dimensionless;
   --  Return a uniform Euclidean bound between the authoritative ideal unit tangent and the mathematical tangent of
   --  the retained evaluator. This includes both the certified rational-cache error and the endpoint-flat finish
   --  correction. Consequently an ideal no-bulge half-space p*T >= 0 with unit p becomes p*T_retained >= -Result.

   function Retained_Endpoint_Jet_Error_Bound
     (Curve : Stereographic_Curve; Order : Endpoint_Tangent_Derivative_Order) return Dimensionless;
   --  Compute an outward-rounded Euclidean bound on the larger endpoint error on demand. The requested tangent
   --  derivative order is expressed in normalized-distance coordinates. For Order > 0, dividing by
   --  Arc_Length(Curve)**Order converts the bound to the corresponding physical-distance derivative units.
   --  Dimensionless'Last is a saturated "certificate unavailable" marker, not a finite physical error claim.

   function Position_Error_Bound (Curve : Stereographic_Curve) return Length;
   --  Uniform certified bound on the difference between the mathematical ideal tangent integral and the retained
   --  realtime evaluator.  This is public so heterogeneous corner-transition storage can propagate the certificate.

   type Projection_Coefficients is array (Axis_Name) of Curvature
   with
     Dynamic_Predicate =>
       (for all Axis in Axis_Name =>
          Projection_Coefficients (Axis) >= (-Dimensionless'Last / 4.0) / mm
          and then Projection_Coefficients (Axis) <= (Dimensionless'Last / 4.0) / mm);
   --  Coefficients of a linear projection of the unit tangent. Their curvature units make the projected value a
   --  curvature, which is convenient for directional motion constraints.

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Coefficients : Projection_Coefficients) return Curvature;
   --  Return a certified upper bound on the absolute value of the requested linear projection of the tangent over the
   --  complete curve.

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length; Coefficients : Projection_Coefficients)
      return Curvature
   with
     Pre =>
       Start_Distance >= 0.0 * mm and then Start_Distance <= End_Distance and then End_Distance <= Arc_Length (Curve);
   --  Return the absolute projected-tangent bound restricted to the supplied closed physical-distance interval.

   function Zero_Blend (Point : Position) return Stereographic_Curve;
   --  Construct a zero-length sentinel located at Point. It is useful for default or degenerate planner storage but is
   --  not a successful result from Create_Blend.

   function Axis_Is_Structurally_Constant (Curve : Stereographic_Curve; Axis : Axis_Name) return Boolean;
   --  Return True when the representation proves algebraically that the selected coordinate never changes. This is an
   --  exact structural property, not a tolerance-based comparison of sampled positions.

   function Flat_Line_Blend_Miss_Ratio (Cos_Half_Theta : Dimensionless) return Dimensionless
   with Post => Flat_Line_Blend_Miss_Ratio'Result >= 0.0;
   --  The distance from the corner to the nearest point of the blend which Create_Blend builds between two lines
   --  turning by θ, from points the same trim either side of the corner with flat endpoint jets, divided by that trim.
   --  Cos_Half_Theta is cos (θ/2) and is clamped to [0, 1]. This is a model for choosing trims, not a certificate.

private

   use Stereographic_Kernel.Elementary;
   --  The elementary functions, Dimensionless_Math, with what the proofs of this unit take on trust about them.

   use Stereographic_Kernel.Ideal_Geometry;
   use Stereographic_Kernel.Intervals;
   use Stereographic_Kernel.Retained_Tangent;

   pragma
     Compile_Time_Error
       (not Dimensionless'Machine_Rounds, "Stereographic_Curves requires rounded floating-point arithmetic");

   ---------------------------------------------------------------------------
   --  Ideal tangent
   ---------------------------------------------------------------------------

   Maximum_Tangent_Numerator_Degree : constant := 2 * Fixed_Chart_Degree;
   --  Highest polynomial degree produced when the degree-eight chart is mapped back to a unit tangent through inverse
   --  stereographic projection.

   type Chart_Vector is array (Chart_Component_Index) of Dimensionless;
   --  One point in the three-dimensional stereographic chart. Unlike Position_Scale, these components are chart
   --  coordinates rather than machine-axis components and are not constrained to form a unit vector.

   type Power_Chart is array (Chart_Coefficient_Index, Chart_Component_Index) of Dimensionless;
   --  The same three chart polynomials stored as ordinary power coefficients for Horner evaluation. Keeping the power
   --  and Bernstein bases as distinct types makes an accidental basis mix-up a compile-time error.

   subtype Tangent_Numerator_Index is Natural range 0 .. Maximum_Tangent_Numerator_Degree;
   --  Index of a coefficient in a polynomial numerator produced by inverse stereographic projection.

   type Raw_Vector_3 is array (Chart_Component_Index) of Dimensionless;
   --  A dimensionless vector in chart coordinates. The "Raw" name distinguishes it from vectors indexed by machine
   --  axes and from quantities carrying physical units.

   type Raw_Vector_4 is array (Frame_Component_Index) of Dimensionless;
   --  A dimensionless four-vector expressed in local frame coordinates.

   type Raw_Matrix_4 is array (Frame_Component_Index, Frame_Component_Index) of Dimensionless;
   --  A dense four-by-four matrix used by the closure solver.

   type Raw_Matrix_4_3 is array (Frame_Component_Index, Chart_Component_Index) of Dimensionless;
   --  A dense matrix mapping three chart adjustments to a four-component closure residual.

   type Raw_Stereo_Jacobian is array (Frame_Component_Index, Chart_Component_Index) of Dimensionless;
   --  Jacobian of inverse stereographic projection: four tangent components by three chart components.

   type Raw_Taylor is array (Natural range 0 .. 3) of Dimensionless;
   --  A fixed-size, cubic Taylor series used for internal jet arithmetic. Element K stores f^(K)(0)/K!, so the
   --  represented function is approximated by Raw_Taylor (0) + Raw_Taylor (1)·U + Raw_Taylor (2)·U² +
   --  Raw_Taylor (3)·U³ near U = 0. Four coefficients are sufficient because endpoint tangent data stops at the
   --  third derivative. Operations on this type deliberately discard terms above degree three.

   type Scaled_Tangent_Jet is array (Natural range 0 .. 3) of Position_Scale;
   --  The tangent and its first three Taylor coefficients after changing from physical distance S to the normalized
   --  coordinate U = S / Chord_Length. Element K stores (1/K!) times the Kth derivative with respect to U. Scaling by
   --  the chord length removes the physical units, so every element can use Position_Scale.

   type Endpoint_Jet_Error_Bounds is array (Endpoint_Tangent_Derivative_Order) of Dimensionless;
   --  Euclidean endpoint error enclosures for tangent derivative orders zero through three in normalized-distance
   --  coordinates; Dimensionless'Last is reserved for an unavailable enclosure.

   type Chart_Jet_Array is array (Natural range 0 .. 3) of Chart_Vector;
   --  A chart-space Taylor jet through third order. Element zero is the chart position, and elements one through three
   --  are the corresponding factorial-scaled derivatives with respect to normalized chord distance.

   type Frame_Candidate_Array is array (Positive range <>) of Position_Scale;
   --  A nonempty sequence of proposed directions for the first vector of the local tangent frame. Entries need not be
   --  normalized and may describe nearly identical directions; Create_Blend normalizes and deduplicates them before
   --  attempting frame construction.

   type Raw_Bernstein_7 is array (Natural range 0 .. 7) of Dimensionless;
   --  Temporary controls for the degree-seven Hermite interpolant that exactly consumes the eight endpoint conditions.

   type Raw_Bernstein is array (Chart_Coefficient_Index) of Dimensionless;
   --  One scalar degree-eight Bernstein polynomial, used for basis conversion and the closure envelope.

   ---------------------------------------------------------------------------
   --  Magnitude limits of the construction
   ---------------------------------------------------------------------------
   --  The construction is free of run-time errors because each stage tests the numbers it is given, or the numbers
   --  it has produced, against a limit before the next stage computes with them, and fails closed beyond it. Within
   --  a stage the limits bound every sum, product and quotient far below the largest floating-point number. The
   --  limits are far above anything a real machine's corner produces, so they change no result which matters; they
   --  replace the handlers of Constraint_Error with which this code used to recover from overflow.

   Tame_Limit : constant := 2.0**100;
   --  The magnitude within which the stages hand numbers to one another.

   Unit_Component_Limit : constant := 2.0;
   --  Above every component of a vector which is a unit vector to within rounding, such as a frame vector.

   Jet_Derivative_Limit : constant := 2.0**100;
   --  Largest magnitude, in millimetres, of a component of a requested tangent derivative.

   Norm_Operand_Limit : constant := 2.0**500;
   --  Largest magnitude of a component whose vector Safe_Norm measures.

   Taylor_Operand_Limit : constant := 2.0**470;
   Taylor_Product_Limit : constant := 2.0**942;
   --  Largest magnitude of a coefficient of a factor of Raw_Taylor_Multiply, and of a coefficient of its product.

   Taylor_Divisor_Limit    : constant := 2.0**110;
   Taylor_Reciprocal_Limit : constant := 2.0**468;
   --  Largest magnitude of a coefficient of a series which Raw_Taylor_Reciprocal inverts, and of a coefficient of its
   --  reciprocal: each order multiplies by a coefficient and divides by the constant term again.

   Chord_Length_Limit : constant Length := 2.0**102 * mm;
   --  Above the distance between two points whose coordinates are within Coordinate_Limit.

   Candidate_Component_Limit : constant := 16.0;
   --  Largest magnitude of a component of a direction offered to Complete_Frame: a sum of a few unit vectors.

   Chart_Build_Limit : constant := 2.0**560;
   --  Above every coefficient of a chart built from jets, a length ratio and closure coefficients within Tame_Limit:
   --  the cube of the length ratio and of the warp factor's reciprocal times a jet, with room for the sums.

   Power_Basis_Limit : constant := 2.0**602;
   --  Above every power coefficient of a chart within Chart_Build_Limit: an eighth difference times a binomial
   --  coefficient.

   Chart_Point_Limit : constant := 2.0**105;
   --  Above a point of a chart whose coefficients are within Tame_Limit, and the largest magnitude of a chart
   --  point which is mapped to the sphere.

   Lambda_Limit : constant := 2.0**40;
   --  Largest ratio of a curve's length to its chord which the closure solve considers.

   Lambda_Terms_Limit : constant := 2.0**606;
   --  Above every coefficient of the charts which multiply the powers of the length ratio: a power coefficient
   --  less up to six times another.

   Maximum_Panel_Count : constant := 64;
   --  Largest number of quadrature panels of a closure residual.

   Closure_Chart_Limit : constant := 2.0**224;
   --  Above every power coefficient of the chart at a length ratio within Lambda_Limit, for terms and closure
   --  coefficients within Tame_Limit: four terms times powers of the ratio up to its cube, and the closure mode.

   Frame_Operand_Limit : constant := 2.0**800;
   Frame_Product_Limit : constant := 2.0**804;
   --  Largest magnitude of a component which is resolved in a frame, or out of one, and of the result.

   function Within (Value : Position_Scale; Limit : Dimensionless) return Boolean
   is (for all Axis in Axis_Name => abs Value (Axis) <= Limit);

   function Within (Value : Raw_Taylor; Limit : Dimensionless) return Boolean
   is (for all Order in Raw_Taylor'Range => abs Value (Order) <= Limit);

   function Within (Value : Raw_Vector_3; Limit : Dimensionless) return Boolean
   is (for all Component in Chart_Component_Index => abs Value (Component) <= Limit);

   function Within (Value : Chart_Vector; Limit : Dimensionless) return Boolean
   is (for all Component in Chart_Component_Index => abs Value (Component) <= Limit);

   function Within (Value : Raw_Vector_4; Limit : Dimensionless) return Boolean
   is (for all Component in Frame_Component_Index => abs Value (Component) <= Limit);

   function Within (Value : Scaled_Tangent_Jet; Limit : Dimensionless) return Boolean
   is (for all Order in Scaled_Tangent_Jet'Range => (for all Axis in Axis_Name => abs Value (Order) (Axis) <= Limit));

   function Within (Value : Chart_Jet_Array; Limit : Dimensionless) return Boolean
   is (for all Order in Chart_Jet_Array'Range =>
         (for all Component in Chart_Component_Index => abs Value (Order) (Component) <= Limit));

   function Within (Value : Frame_Vector_Array; Limit : Dimensionless) return Boolean
   is (for all Component in Frame_Component_Index =>
         (for all Axis in Axis_Name => abs Value (Component) (Axis) <= Limit));

   function Within (Value : Bernstein_Chart; Limit : Dimensionless) return Boolean
   is (for all Index in Chart_Coefficient_Index =>
         (for all Component in Chart_Component_Index => abs Value (Index, Component) <= Limit));

   function Within (Value : Power_Chart; Limit : Dimensionless) return Boolean
   is (for all Index in Chart_Coefficient_Index =>
         (for all Component in Chart_Component_Index => abs Value (Index, Component) <= Limit));
   --  Whether every component is within Limit in magnitude, which no NaN is.

   type Interval_Position_Scale is array (Axis_Name) of Interval;
   --  One independently certified interval for each machine-axis component.

   subtype GL16_Positive_Index is Positive range 1 .. 8;
   --  Index of a positive node in the symmetric 16-point Gauss–Legendre quadrature rule. The corresponding negative
   --  node is generated at the point of use, so only half of the rule needs to be stored.

   type GL16_Constant_Array is array (GL16_Positive_Index) of Dimensionless;
   --  Storage shared by the positive quadrature nodes and their matching weights.

   GL16_Nodes : constant GL16_Constant_Array :=
     [0.095_012_509_837_637_440_19,
      0.281_603_550_779_258_913_23,
      0.458_016_777_657_227_386_34,
      0.617_876_244_402_643_748_45,
      0.755_404_408_355_003_033_90,
      0.865_631_202_387_831_743_88,
      0.944_575_023_073_232_576_08,
      0.989_400_934_991_649_932_60];
   --  Positive sample locations for 16-point Gauss-Legendre integration on the interval from -1 to 1.

   GL16_Weights : constant GL16_Constant_Array :=
     [0.189_450_610_455_068_496_29,
      0.182_603_415_044_923_588_87,
      0.169_156_519_395_002_538_19,
      0.149_595_988_816_576_732_08,
      0.124_628_971_255_533_872_05,
      0.095_158_511_682_492_784_81,
      0.062_253_523_938_647_892_86,
      0.027_152_459_411_754_094_85];
   --  Integration weights corresponding element-for-element with GL16_Nodes. Each weight is used for both the stored
   --  positive node and its reflected negative node.
   --
   --    import mpmath as mp
   --
   --    mp.mp.dps = 80
   --    nodes, weights = mp.gauss_quadrature(16, "legendre")
   --    positive = [(nodes[i], weights[i]) for i in range(16) if nodes[i] > 0]
   --
   --    def ada(value):
   --        digits = f"{int(mp.nint(value * 10**20)):020d}"
   --        return "0." + "_".join(digits[i : i + 3] for i in range(0, 20, 3))
   --
   --    def emit(name, values):
   --        print(f"{name} := [")
   --        print(",\n".join(f"  {ada(value)}" for value in values) + "];")
   --
   --    emit("GL16_Nodes", (node for node, _ in positive))
   --    emit("GL16_Weights", (weight for _, weight in positive))

   Maximum_Binomial_Degree : constant := 2 * Maximum_Tangent_Numerator_Degree;
   --  Largest degree needed by this package's polynomial conversions and products.

   type Binomial_Table_Type is
     array (Natural range 0 .. Maximum_Binomial_Degree, Natural range 0 .. Maximum_Binomial_Degree) of Dimensionless;
   --  Pascal's triangle through Maximum_Binomial_Degree. Entries outside the mathematical lower triangle remain zero.

   Binomial_Limit : constant := 2.0**32;
   --  Above every binomial coefficient of the table, whose largest is C(32, 16), about 2²⁹·².

   function Build_Binomial_Table return Binomial_Table_Type
   with
     Post =>
       (for all N in Binomial_Table_Type'Range (1) =>
          (for all K in Binomial_Table_Type'Range (2) =>
             Build_Binomial_Table'Result (N, K) >= 0.0
             and then Build_Binomial_Table'Result (N, K) <= Binomial_Limit
             and then (if K <= N then Build_Binomial_Table'Result (N, K) >= 1.0)));
   --  Construct Pascal's triangle once during package elaboration using exact additions of integer-valued
   --  coefficients.

   ---------------------------------------------------------------------------
   --  Distance warp
   ---------------------------------------------------------------------------

   Distance_Warp_Log_Lower_Bound : constant Dimensionless := -16.0;
   Distance_Warp_Log_Upper_Bound : constant Dimensionless := 16.0;
   --  Search limits for log(W), where W is the positive distance-warp factor. Searching in logarithmic space treats a
   --  warp and its reciprocal symmetrically and guarantees that every proposed factor remains positive.

   Minimum_Distance_Warp_Factor : constant Dimensionless := Stereographic_Kernel.Elementary.Warp_Factor_Min;
   Maximum_Distance_Warp_Factor : constant Dimensionless := Stereographic_Kernel.Elementary.Warp_Factor_Max;
   --  Inclusive factor bounds, the powers of two just outside the exponentials of the logarithmic search interval
   --  (2⁻²⁴ and 2²⁴ for e⁻¹⁶ and e¹⁶). Keeping W inside this range bounds the conditioning of the parameter
   --  transformation and its derivatives.

   subtype Distance_Warp_Factor is Dimensionless range Minimum_Distance_Warp_Factor .. Maximum_Distance_Warp_Factor;
   --  A positive, supported factor for the endpoint-preserving distance transformation.

   Minimum_Safe_Reciprocal_Denominator : constant Dimensionless := 1.0E-10;
   --  Smallest permitted magnitude for the constant term of a Taylor series being inverted. Dividing below this
   --  threshold could amplify floating-point error enough to make the remaining coefficients unreliable.

   Jet_Tolerance_Factor : constant Dimensionless := 32_768.0;
   --  Multiplier applied to machine epsilon when checking the unit-tangent identities. It allows for accumulated
   --  roundoff from dot products while still rejecting endpoint derivatives with a material mismatch.

   Frame_Residual_Tolerance : constant Dimensionless := 2.0E-12;
   --  Maximum accepted error when the completed frame is checked against an identity Gram matrix. This final check
   --  catches loss of orthogonality that remains after normalization and the two Gram-Schmidt passes.

   Closure_Absolute_Floor : constant Dimensionless := 2.0 * Dimensionless'Model_Epsilon;
   --  Smallest residual tolerance requested from the closure solver. Tolerances derived from a very small position
   --  budget are clamped here because asking Newton iteration to resolve below floating-point noise cannot establish a
   --  more trustworthy result.

   ---------------------------------------------------------------------------
   --  Compiled rational antiderivative
   ---------------------------------------------------------------------------

   type Structural_Constant_Axis_Array is array (Axis_Name) of Boolean;
   --  Per-axis proof flags recording that the represented coordinate is algebraically constant.

   Maximum_Rational_Degree : constant := Maximum_Tangent_Numerator_Degree;
   --  Maximum denominator degree of the rational tangent representation.

   Maximum_Derivative_Bernstein_Degree : constant := 5 * Maximum_Rational_Degree + 4;
   --  Largest Bernstein degree needed to certify the retained tangent through fourth derivative order. If Q is the
   --  common denominator, the factorial-scaled derivative of order K has a numerator of degree
   --  (K + 1) * Degree (Q) + K; degree sixteen and K = 4 therefore require degree eighty-four.

   subtype Exact_Binomial_Value is Interfaces.Unsigned_128;
   --  Integer representation used while combining Bernstein coefficients. All binomial coefficients needed through
   --  degree eighty-four fit in 128 bits, so coefficient ratios can be formed without first rounding the integers.

   subtype Packed_Binomial_Index is Natural range 0 .. (Maximum_Derivative_Bernstein_Degree + 2)**2 / 4 - 1;
   --  Each row stores only K <= N/2, using the symmetry C(N,K) = C(N,N-K).

   type Exact_Binomial_Table_Type is array (Packed_Binomial_Index) of Exact_Binomial_Value;
   --  The symmetric half of Pascal's triangle through the degree required by derivative certificates.

   function Build_Exact_Binomial_Table return Exact_Binomial_Table_Type;
   --  Construct the exact Pascal triangle once during package elaboration using integer additions.

   function Exact_Binomial (N, K : Natural) return Exact_Binomial_Value;
   --  Return the exact binomial coefficient "N choose K". Requests outside the stored triangle return zero, matching
   --  the bounded polynomial-summation convention used by Binomial.

   type Interval_Polynomial is array (Natural range <>) of Interval;
   --  Bernstein controls with outward-rounded coefficient enclosures.

   function Multiply_Bernstein (Left, Right : Interval_Polynomial) return Interval_Polynomial
   with
     Pre =>
       Left'First = 0
       and then Right'First = 0
       and then Left'Length > 0
       and then Right'Length > 0
       and then Left'Last + Right'Last <= Maximum_Derivative_Bernstein_Degree;
   pragma Annotate (GNATprove, Skip_Proof, Multiply_Bernstein);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Multiply by convolving binomial-scaled controls. Fall back to individual enclosing weights when scaling
   --  cannot produce finite intervals, so intermediate overflow does not reject an otherwise certifiable product.

   pragma
     Compile_Time_Error
       (Stereographic_Kernel.Retained_Tangent.Maximum_Rational_Degree /= Maximum_Rational_Degree,
        "the retained tangent has another degree");

   function Complex_Pair_Real_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   with
     Pre  => Pair <= Cache.Pair_Count and then Cache.Real_Pole_Count + 2 * Natural (Pair) <= Maximum_Rational_Degree,
     Post => Complex_Pair_Real_Slot'Result = Cache.Real_Pole_Count + 2 * Natural (Pair) - 1;
   --  Return the slot containing the real components of the selected conjugate pole and residue pair.

   function Complex_Pair_Imaginary_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   with
     Pre  => Pair <= Cache.Pair_Count and then Cache.Real_Pole_Count + 2 * Natural (Pair) <= Maximum_Rational_Degree,
     Post => Complex_Pair_Imaginary_Slot'Result = Cache.Real_Pole_Count + 2 * Natural (Pair);
   --  Return the slot containing the positive-imaginary components of the selected conjugate pair.

   ---------------------------------------------------------------------------
   --  Magnitude limits of the evaluator
   ---------------------------------------------------------------------------
   --  The evaluator is free of run-time errors because every number it stores is tested against these limits when
   --  the curve is constructed, before any arithmetic is done on it. A curve of a real machine is far inside all
   --  of them; a curve outside them is not built.

   Pole_Magnitude_Limit : constant := 2.0**100;
   --  Largest magnitude of a pole component.

   Residue_Magnitude_Limit : constant := 2.0**100;
   --  Largest magnitude of a residue component and of a component of the constant tangent.

   Pole_Clearance : constant := 2.0**(-100);
   --  A pole is at least this far from the unit interval on its left side or, for a conjugate pair, above it.

   Right_Of_Unit : constant := 1.0 + 2.0**(-52);
   --  The smallest number above one: a pole on the right of the unit interval is at least this.

   Coordinate_Limit : constant Length := 2.0**99 * mm;
   --  Largest magnitude of a coordinate of an endpoint of a positive-length curve. The difference of two such
   --  coordinates is within the common limit of the construction.

   Arc_Length_Floor : constant Length := 2.0**(-100) * mm;
   Arc_Length_Limit : constant Length := 2.0**104 * mm;
   --  The length of a positive-length curve lies between these.

   function Is_Clear_Of_Unit_Interval (Pole : Dimensionless) return Boolean
   is (Pole <= -Pole_Clearance or else Pole >= Right_Of_Unit);
   --  Whether a real pole, or the real part of a pair, lies outside the unit interval with the clearance above.

   function Pair_Is_Clear (Pole_Real, Pole_Imaginary : Dimensionless) return Boolean
   is (abs Pole_Real <= Pole_Magnitude_Limit
       and then Pole_Imaginary > 0.0
       and then Pole_Imaginary <= Pole_Magnitude_Limit
       and then (Pole_Imaginary >= Pole_Clearance or else Is_Clear_Of_Unit_Interval (Pole_Real)));
   --  Whether a conjugate pair of poles is within the magnitude limit and clear of the unit interval.

   function Slot_Is_Tame (Slot : Rational_Pole_Slot) return Boolean
   is (abs Slot.Pole_Component <= Pole_Magnitude_Limit
       and then (for all Axis in Axis_Name => abs Slot.Residue_Component (Axis) <= Residue_Magnitude_Limit));

   function Slot_Is_Zero (Slot : Rational_Pole_Slot) return Boolean
   is (Slot.Pole_Component = 0.0 and then (for all Axis in Axis_Name => Slot.Residue_Component (Axis) = 0.0));

   function Rational_Antiderivative_Is_Well_Formed (Cache : Rational_Antiderivative) return Boolean
   is (Cache.Real_Pole_Count + 2 * Cache.Pair_Count <= Maximum_Rational_Degree
       and then (for all Axis in Axis_Name => abs Cache.Constant_Tangent (Axis) <= Residue_Magnitude_Limit)
       and then (for all Slot in Rational_Degree_Slot => Slot_Is_Tame (Cache.Pole_Slots (Slot)))
       and then
         (for all Slot in 1 .. Cache.Real_Pole_Count =>
            Is_Clear_Of_Unit_Interval (Cache.Pole_Slots (Slot).Pole_Component))
       and then
         (for all Pair in 1 .. Cache.Pair_Count =>
            Pair_Is_Clear
              (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair - 1).Pole_Component,
               Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component))
       and then
         (for all Slot in Rational_Degree_Slot =>
            (if Slot > Cache.Real_Pole_Count + 2 * Cache.Pair_Count then Slot_Is_Zero (Cache.Pole_Slots (Slot)))));
   --  What the evaluator needs of its cache, tested once during construction: the slots first hold the real poles
   --  and then the real and imaginary records of each conjugate pair, with all unused slots zero; every stored
   --  number is within the limits above (which no NaN or infinity is); and every pole is clear of the unit
   --  interval. With this no logarithm, arctangent, product or sum of an evaluation can fail or overflow.

   function Rational_Antiderivative_Is_Canonical_Zero (Cache : Rational_Antiderivative) return Boolean
   is (Cache.Real_Pole_Count = 0
       and then Cache.Pair_Count = 0
       and then (for all Axis in Axis_Name => Cache.Constant_Tangent (Axis) = 0.0)
       and then (for all Slot in Rational_Degree_Slot => Slot_Is_Zero (Cache.Pole_Slots (Slot))));
   --  Return True only for the unique all-zero cache used by a zero-length evaluator.

   subtype Stored_Majorant_Order is Natural range 0 .. 4;
   --  Derivative orders retained in a curve certificate, from tangent order zero through order four.

   type Stored_Scalar_Majorants is array (Stored_Majorant_Order) of Dimensionless;
   --  Absolute upper bounds for one scalar function and its first four derivatives.

   type Stored_Axis_Majorants is array (Axis_Name) of Stored_Scalar_Majorants;
   --  Stored tangent-derivative majorants for every machine axis.

   Maximum_Majorant_Order : constant := Stored_Majorant_Order'Last;
   --  Highest derivative order supported by the majorant propagation routines.

   subtype Majorant_Order is Stored_Majorant_Order;
   subtype Scalar_Majorants is Stored_Scalar_Majorants;
   subtype Axis_Majorants is Stored_Axis_Majorants;
   --  Short internal names for the fixed-size certificate types.

   subtype Retained_Bernstein_Index is Natural range 0 .. Maximum_Rational_Degree;
   --  Fixed-capacity coefficient index for the common-denominator tangent certificate.

   type Retained_Bernstein_Polynomial is array (Retained_Bernstein_Index) of Interval;
   --  Outward-rounded Bernstein controls for one polynomial. Only indices through the certificate's Degree are active;
   --  the fixed upper bound keeps Stereographic_Curve allocation-free.

   type Retained_Axis_Bernstein_Polynomials is array (Axis_Name) of Retained_Bernstein_Polynomial;
   --  One numerator polynomial for each machine-axis component of the retained rational tangent.

   type Retained_Tangent_Bernstein_Certificate is record
      Valid : Boolean := False;
      --  True only when all following fields form a usable common-denominator certificate.

      Degree : Natural range 0 .. Maximum_Rational_Degree := 0;
      --  Active degree shared by Denominator and every Axis_Numerators polynomial.

      Denominator : Retained_Bernstein_Polynomial := [others => <>];
      --  Outward-rounded Bernstein controls of the common denominator in chart coordinate V.

      Axis_Numerators : Retained_Axis_Bernstein_Polynomials := [others => [others => <>]];
      --  Outward-rounded numerator controls corresponding to Denominator for each machine axis.

      Minimum_Denominator : Dimensionless := 0.0;
      --  Certified positive lower bound for Denominator over the complete chart interval.
   end record;
   --  Construction-only common-denominator form of the retained tangent in chart coordinate V. The realtime
   --  evaluator remains the compact pole/residue cache. This form exists only when the Bernstein reconstruction ran:
   --  as the tangent-error fallback when the sampled proof is not tight enough, or for the Bernstein derivative
   --  fallback when the pole-based bounds are not sharp.

   type Chart_Majorants is array (Chart_Component_Index) of Scalar_Majorants;
   --  Derivative majorants for each coordinate of a stereographic chart.

   type Frame_Majorants is array (Frame_Component_Index) of Scalar_Majorants;
   --  Derivative majorants for each component of a tangent in local frame coordinates.

   type Curve_Kind is (Zero_Curve_Kind, Positive_Curve_Kind);
   --  Distinguishes the explicit zero-length sentinel from a constructed positive-length curve.

   type Stereographic_Curve_Evaluator is record
      Kind : Curve_Kind := Zero_Curve_Kind;
      --  Distinguishes a zero sentinel from an executable curve.

      Start_Point : Position := [others => 0.0 * mm];
      --  Returned exactly at distance zero.

      Finish_Point : Position := [others => 0.0 * mm];
      --  Returned exactly at Arc_Length.

      Length_Value : Length := 0.0 * mm;
      --  Planner-authoritative physical distance.

      Antiderivative_Cache : Rational_Antiderivative;
      --  Fixed-capacity real partial-fraction antiderivative in normalised physical distance.

      Uncorrected_Finish_Point : Position := [others => 0.0 * mm];
      --  Bit-exact value of the retained antiderivative at normalized distance one before endpoint correction.
      --  Runtime evaluation uses this anchor to apply the degree-eleven endpoint-flat correction without requiring
      --  Finish_Point - Uncorrected_Finish_Point to fit in one floating-point value.
   end record
   with
     Dynamic_Predicate =>
       (for all Axis in Axis_Name =>
          Stereographic_Curve_Evaluator.Start_Point (Axis) >= -Length'Last
          and then Stereographic_Curve_Evaluator.Start_Point (Axis) <= Length'Last
          and then Stereographic_Curve_Evaluator.Finish_Point (Axis) >= -Length'Last
          and then Stereographic_Curve_Evaluator.Finish_Point (Axis) <= Length'Last)
       and then
         (for all Axis in Axis_Name =>
            Stereographic_Curve_Evaluator.Uncorrected_Finish_Point (Axis) >= -Length'Last
            and then Stereographic_Curve_Evaluator.Uncorrected_Finish_Point (Axis) <= Length'Last)
       and then
         (if Stereographic_Curve_Evaluator.Kind = Zero_Curve_Kind
          then
            Stereographic_Curve_Evaluator.Length_Value = 0.0 * mm
            and then Stereographic_Curve_Evaluator.Start_Point = Stereographic_Curve_Evaluator.Finish_Point
            and then Stereographic_Curve_Evaluator.Uncorrected_Finish_Point = Stereographic_Curve_Evaluator.Start_Point
            and then Rational_Antiderivative_Is_Canonical_Zero (Stereographic_Curve_Evaluator.Antiderivative_Cache)
          else
            Stereographic_Curve_Evaluator.Length_Value >= Arc_Length_Floor
            and then Stereographic_Curve_Evaluator.Length_Value <= Arc_Length_Limit
            and then
              (for all Axis in Axis_Name =>
                 abs Stereographic_Curve_Evaluator.Start_Point (Axis) <= Coordinate_Limit
                 and then abs Stereographic_Curve_Evaluator.Finish_Point (Axis) <= Coordinate_Limit
                 and then abs Stereographic_Curve_Evaluator.Uncorrected_Finish_Point (Axis) <= Coordinate_Limit)
            and then Rational_Antiderivative_Is_Well_Formed (Stereographic_Curve_Evaluator.Antiderivative_Cache)),
     Predicate_Failure => "a blend evaluator must contain a coherent zero or positive curve";
   --  Compact, allocation-free curve data retained in planner blocks and used for realtime position evaluation.

   function Finish_Correction_Interval (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Interval;
   --  Enclose the endpoint correction for Axis without directly subtracting two potentially extreme coordinates.

   Log_Ratio_Limit : constant := 2.0 * Stereographic_Kernel.Elementary.Log_Magnitude_Max;
   --  Largest magnitude of a logarithm of a ratio which the evaluator forms.

   Displacement_Limit : constant := 2.0**116;
   --  Largest magnitude of a component of the normalized displacement: the constant tangent and sixteen slots,
   --  each a residue times a primitive below 2¹¹.

   Uncorrected_Point_Limit : constant Length := 2.0**221 * mm;
   --  Largest magnitude of a coordinate before the endpoint correction: a start coordinate plus the length times
   --  the normalized displacement.

   function Stable_Log_One_Plus (Value : Dimensionless) return Dimensionless
   with
     Pre  => abs Value <= 0.125,
     Post => abs Stable_Log_One_Plus'Result <= 0.5 and then (if Value = 0.0 then Stable_Log_One_Plus'Result = 0.0);
   --  Evaluate log(1 + Value) for a small Value without losing the increment.

   function Stable_Real_Log_Ratio (Pole, Normalized_Distance : Dimensionless) return Dimensionless
   with
     Pre  =>
       abs Pole <= Pole_Magnitude_Limit
       and then Is_Clear_Of_Unit_Interval (Pole)
       and then Normalized_Distance >= 0.0
       and then Normalized_Distance <= 1.0,
     Post =>
       abs Stable_Real_Log_Ratio'Result <= Log_Ratio_Limit
       and then (if Normalized_Distance = 0.0 then Stable_Real_Log_Ratio'Result = 0.0);
   --  Evaluate log (abs (U - Pole) / abs Pole) without rounding the argument to zero beside an endpoint pole.

   function Stable_Complex_Log_Ratio
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   with
     Pre  =>
       Pair_Is_Clear (Pole_Real, Pole_Imaginary)
       and then Normalized_Distance >= 0.0
       and then Normalized_Distance <= 1.0,
     Post =>
       abs Stable_Complex_Log_Ratio'Result <= Log_Ratio_Limit
       and then (if Normalized_Distance = 0.0 then Stable_Complex_Log_Ratio'Result = 0.0);
   --  Evaluate log (((U - A)**2 + B**2) / (A**2 + B**2)) without cancelling a small change against one.

   function Stable_Complex_Angle_Delta
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   with
     Pre  =>
       Pair_Is_Clear (Pole_Real, Pole_Imaginary)
       and then Normalized_Distance >= 0.0
       and then Normalized_Distance <= 1.0,
     Post =>
       abs Stable_Complex_Angle_Delta'Result <= Stereographic_Kernel.Elementary.Arctan_Magnitude_Max
       and then (if Normalized_Distance = 0.0 then Stable_Complex_Angle_Delta'Result = 0.0);
   --  Evaluate a conjugate pair's argument change from the cross and dot products of its endpoints.

   function Evaluate_Rational_Displacement
     (Cache : Rational_Antiderivative; Normalized_Distance : Dimensionless) return Dimensionless_Axis_Vector
   with
     Pre  =>
       Rational_Antiderivative_Is_Well_Formed (Cache)
       and then Normalized_Distance >= 0.0
       and then Normalized_Distance <= 1.0,
     Post =>
       (for all Axis in Axis_Name => abs Evaluate_Rational_Displacement'Result (Axis) <= Displacement_Limit)
       and then
         (if Normalized_Distance = 0.0
          then (for all Axis in Axis_Name => Evaluate_Rational_Displacement'Result (Axis) = 0.0));
   --  Evaluate the dimensionless displacement from distance zero by analytically integrating the cached tangent.

   function Evaluate_Uncorrected_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   with
     Pre  =>
       Evaluator.Kind = Positive_Curve_Kind and then Normalized_Distance >= 0.0 and then Normalized_Distance <= 1.0,
     Post =>
       (for all Axis in Axis_Name => abs Evaluate_Uncorrected_Rational_Point'Result (Axis) <= Uncorrected_Point_Limit)
       and then
         (if Normalized_Distance = 0.0
          then
            (for all Axis in Axis_Name =>
               Evaluate_Uncorrected_Rational_Point'Result (Axis) = Evaluator.Start_Point (Axis)));
   --  Scale and anchor the cached displacement, before the endpoint correction.

   function Evaluate_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   with
     Pre  =>
       Evaluator.Kind = Positive_Curve_Kind and then Normalized_Distance >= 0.0 and then Normalized_Distance <= 1.0,
     Post =>
       (if Normalized_Distance = 0.0
        then (for all Axis in Axis_Name => Evaluate_Rational_Point'Result (Axis) = Evaluator.Start_Point (Axis)))
       and then
         (if Normalized_Distance = 1.0
            and then
              (for all Axis in Axis_Name =>
                 Evaluate_Uncorrected_Rational_Point (Evaluator, 1.0) (Axis)
                 = Evaluator.Uncorrected_Finish_Point (Axis))
          then (for all Axis in Axis_Name => Evaluate_Rational_Point'Result (Axis) = Evaluator.Finish_Point (Axis)));
   --  Scale and anchor the cached displacement, then apply the endpoint-flat finish correction. The start point is
   --  returned exactly at normalized distance zero. So is the finish point at normalized distance one, for an
   --  evaluator whose Uncorrected_Finish_Point is the uncorrected point there, which construction makes it.

   type Stereographic_Curve is record
      Evaluator_Data : Stereographic_Curve_Evaluator;
      --  Data retained by To_Evaluator.

      Frame : Frame_Vector_Array := [others => [others => 0.0]];
      --  Local stereographic tangent frame.

      Coefficients : Bernstein_Chart := [others => [others => 0.0]];
      --  Authoritative ideal chart.

      Warp_Factor : Distance_Warp_Factor := 1.0;
      --  Endpoint-preserving map from normalized physical distance to the polynomial chart coordinate. The evaluator
      --  does not retain this: construction transforms every pole into physical-distance space.

      Certified_Frame_Speed_Upper : Dimensionless := 0.0;
      --  Outward-rounded operator-norm bound for Frame. Geometry queries reuse this cached scalar instead of
      --  rebuilding the frame Gram matrix for every enclosure.

      Bounds : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
      --  Whole-curve bounds through tangent derivative order four.

      Whole_Curve_Majorants : Stored_Axis_Majorants := [others => [others => 0.0]];
      --  Construction stores whole-curve tangent bounds here so the planner does not repeat the same analysis.

      Has_Whole_Curve_Majorants : Boolean := False;
      --  Distinguishes a stored certificate from the harmless zero default.

      Structurally_Constant_Axes : Structural_Constant_Axis_Array := [others => False];
      --  Exact structural evidence shared by ideal and compiled forms.

      Certified_Position_Error : Length := 0.0 * mm;
      --  Certified uniform ideal-to-compiled position error, including the continuous finish correction and a
      --  conservative runtime arithmetic allowance.

      Certified_Tangent_Error : Dimensionless := 0.0;
      --  Uniform ideal-to-cache tangent error. This turns ideal unit-vector projection bounds into certificates for
      --  the retained cache; axial derivatives are bounded directly from the pole/residue representation.

      Requested_Start_Jet, Requested_Finish_Jet : Endpoint_Tangent_Jet;
      --  Retain the requests for on-demand endpoint diagnostics; construction does not measure this test-only report.

      Retained_Tangent_Certificate : Retained_Tangent_Bernstein_Certificate := (others => <>);

      Geometry : Stereographic_Kernel.Certified_Geometry.Certificate;

      Evaluator_Evidence : Stereographic_Kernel.Certified_Evaluator.Certificate;
      --  Evidence that the retained tangent of Evaluator_Data's cache stays within a bound of the ideal tangent.
      --  Create_Blend checks that it matches the cache before returning a curve.
      --  Evidence that Frame and Coefficients passed the ideal-geometry certificates. Create_Blend checks that it
      --  matches them before returning a curve.
      --  Rational Bernstein data for the Bernstein derivative certificate. It is valid only when the Bernstein
      --  reconstruction ran: construction normally certifies the tangent from samples instead, and keeps the data
      --  only when its whole-curve derivative bounds needed the Bernstein fallback. Range queries on other curves
      --  reconstruct it on demand with Retain_Bernstein_Tangent_Certificate. The compact evaluator never carries it.
   end record
   with
     Dynamic_Predicate =>
       Stereographic_Curve.Certified_Position_Error >= 0.0 * mm
       and then Stereographic_Curve.Certified_Position_Error <= Length'Last
       and then Stereographic_Curve.Certified_Frame_Speed_Upper >= 0.0
       and then Is_Finite (Stereographic_Curve.Certified_Frame_Speed_Upper)
       and then Stereographic_Curve.Certified_Tangent_Error >= 0.0
       and then Is_Finite (Stereographic_Curve.Certified_Tangent_Error)
       and then
         (Stereographic_Curve.Evaluator_Data.Kind = Positive_Curve_Kind
          or else
            (Stereographic_Curve.Certified_Position_Error = 0.0 * mm
             and then Stereographic_Curve.Certified_Tangent_Error = 0.0)),
     Predicate_Failure => "a stereographic curve must contain nonnegative numerical certificates";
   --  Full construction result: the compact evaluator, ideal geometry, requested endpoint jets, and the
   --  certificates used by construction and diagnostic range queries. Compiled derivatives are bounded from the
   --  pole-based Taylor enclosure, with the Bernstein form as its fallback.

   ---------------------------------------------------------------------------
   --  Construction results used between the private stages
   ---------------------------------------------------------------------------

   type Candidate_Status is
     (Candidate_Success,
      --  This frame produced a complete, checked curve.

      Candidate_Closure_Failed,
      --  The tangent curve could not reach the requested finish point using this frame.

      Candidate_Representation_Failed,
      --  The ideal curve closed, but its realtime representation could not meet the requested position-error budget.

      Candidate_Numerically_Unsafe
      --  Construction encountered non-finite or otherwise untrustworthy arithmetic.
     );
   --  Outcome of attempting construction with one frame candidate.

   type Candidate_Result (Status : Candidate_Status := Candidate_Closure_Failed) is record
      case Status is
         when Candidate_Success =>
            Curve : Stereographic_Curve;
            --  Complete curve produced by this frame candidate.

         when others =>
            null;
      end case;
   end record
   with
     Dynamic_Predicate =>
       Candidate_Result.Status /= Candidate_Success
       or else Candidate_Result.Curve.Evaluator_Data.Kind = Positive_Curve_Kind,
     Predicate_Failure => "a successful candidate must contain a positive-length curve";
   --  Per-frame construction outcome with a curve payload only after every closure and representation check succeeds.

   type Realtime_Compilation_Status is
     (Realtime_Compilation_Succeeded,
      --  The rational representation and all requested certificates were produced.

      Realtime_Representation_Insufficient,
      --  Finite compilation completed, but the representation could not meet the requested error budget.

      Realtime_Compilation_Numerically_Unsafe
      --  Compilation encountered non-finite or otherwise untrustworthy arithmetic.
     );
   --  Outcome of compiling a closed ideal curve into the compact realtime evaluator.

   type Tangent_Certificate_Status is
     (Tangent_Was_Certified,
      --  Error_Bound encloses the tangent error on the complete curve.

      Tangent_Could_Not_Be_Certified,
      --  Finite arithmetic completed, but positivity or accuracy was not established for this representation.

      Tangent_Certificate_Is_Numerically_Unsafe
      --  The certificate arithmetic did not produce trustworthy finite data.
     );
   --  Outcome of comparing the ideal unit tangent with the compiled rational tangent over the complete curve.

   type Tangent_Certificate_Result (Status : Tangent_Certificate_Status := Tangent_Could_Not_Be_Certified) is record
      case Status is
         when Tangent_Was_Certified =>
            Error_Bound : Dimensionless;
            --  Uniform Euclidean tangent-error bound.

         when others =>
            null;
      end case;
   end record
   with
     Dynamic_Predicate =>
       Tangent_Certificate_Result.Status /= Tangent_Was_Certified
       or else
         (Tangent_Certificate_Result.Error_Bound >= 0.0 and then Is_Finite (Tangent_Certificate_Result.Error_Bound)),
     Predicate_Failure => "a successful tangent certificate must contain a finite nonnegative bound";
   --  Tangent-certification outcome with a uniform error bound only when the proof succeeded.

   Tangent_Sample_Count : constant := 2 * Maximum_Rational_Degree + 1;
   --  A polynomial of degree at most 2 * Maximum_Rational_Degree is determined by this many values.

   subtype Tangent_Sample_Index is Positive range 1 .. Tangent_Sample_Count;

   pragma
     Compile_Time_Error
       (Stereographic_Kernel.Sample_Nodes.Sample_Count /= Tangent_Sample_Count, "the sample nodes do not match");

   Tangent_Sample_Lebesgue_Bound : constant Dimensionless := Stereographic_Kernel.Sample_Nodes.Lebesgue_Bound;
   --  Upper bound on the Lebesgue constant of the sample nodes, which is 3.1885 for thirty-three Chebyshev nodes
   --  of the first kind. A polynomial of degree below Tangent_Sample_Count is bounded on [0, 1] by this factor
   --  times its largest magnitude at the nodes.

   function Tangent_Sample_Node (Index : Tangent_Sample_Index) return Dimensionless;
   --  Return one sample node in the chart coordinate, exposed so tests can check the Lebesgue bound.

   function Sampled_Realtime_Tangent_Error
     (Curve             : Stereographic_Curve;
      Final_Cache       : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Evidence          : out Stereographic_Kernel.Certified_Evaluator.Certificate) return Tangent_Certificate_Result
   with Side_Effects;
   --  Bound the Euclidean difference between the ideal tangent of Curve's chart and the rational tangent of
   --  Final_Cache over the complete curve, from their values at the sample nodes alone. Denominator_Scale is the
   --  ideal denominator at the start of the chart. Evidence is the proof-carrying form of a certified bound. A
   --  failure only means that this proof did not apply.

   subtype Conjugate_Root_Index is Positive range 1 .. Maximum_Rational_Degree / 2;
   type Root_Polynomial is array (Natural range 0 .. Maximum_Rational_Degree) of Dimensionless;
   --  Power-basis coefficients of a real polynomial, lowest degree first.
   type Conjugate_Root_Parts is array (Conjugate_Root_Index) of Dimensionless;

   function Iterate_Conjugate_Roots
     (Coefficients : Root_Polynomial; Degree : Natural; Real_Parts, Imaginary_Parts : out Conjugate_Root_Parts)
      return Boolean
   with Side_Effects, Pre => Degree <= Maximum_Rational_Degree;
   --  Find all roots of a real polynomial without real roots by Aberth-Ehrlich iteration. The roots form conjugate
   --  pairs, so only the member of each pair in the upper half-plane is iterated, and its conjugate is implied:
   --  entries 1 .. Degree / 2 of the results hold those members, each with a positive imaginary part. False if the
   --  iteration is not applicable or does not settle, in which case an eigenvalue solve must be used instead.
   --
   --  Nothing here needs to be rigorous: every root is validated against the polynomial afterwards, and the
   --  compiled evaluator is certified against the ideal tangent as a whole.

   type Closure_Seed is record
      Valid : Boolean := False;
      --  False means that the closure solver chooses its own solution. True identifies a complete upstream closure
      --  proposal; Build_Candidate does not replace or refine it, but the normal endpoint and representation checks
      --  still have to certify it.

      Lambda : Dimensionless := 0.0;
      --  Proposed curve length divided by chord length.

      C0 : Raw_Vector_3 := [others => 0.0];
      --  Proposed endpoint-invisible chart adjustment.
   end record
   with
     Dynamic_Predicate =>
       not Closure_Seed.Valid
       or else
         (Closure_Seed.Lambda >= 1.0
          and then Closure_Seed.Lambda <= Lambda_Limit
          and then (for all Component in Chart_Component_Index => abs Closure_Seed.C0 (Component) <= Tame_Limit)),
     Predicate_Failure => "a valid closure seed must contain moderate values and lambda >= 1";
   --  An optional upstream closure solution passed into candidate construction. Build_Candidate treats a valid seed as
   --  final for the closure stage: it is neither replaced nor refined, but the resulting curve still has to pass
   --  endpoint, representation, and numerical-safety checks.

   type Closure_Solution is record
      Success : Boolean := False;
      --  True only when Lambda and C0 satisfy the requested closure tolerance.

      Numerically_Unsafe : Boolean := False;
      --  Distinguishes arithmetic failure from ordinary nonconvergence.

      Lambda : Dimensionless := 0.0;
      --  Curve length divided by chord length.

      C0 : Raw_Vector_3 := [others => 0.0];
      --  Endpoint-invisible chart adjustment.
   end record
   with
     Dynamic_Predicate =>
       not Closure_Solution.Success
       or else
         (not Closure_Solution.Numerically_Unsafe
          and then Closure_Solution.Lambda >= 1.0
          and then Closure_Solution.Lambda <= Lambda_Limit
          and then (for all Component in Chart_Component_Index => abs Closure_Solution.C0 (Component) <= Tame_Limit)),
     Predicate_Failure => "a successful closure solution must contain moderate values and lambda >= 1";
   --  Result of solving the four position-closure equations for curve-length ratio and the three interior chart
   --  adjustments. Numerically_Unsafe lets callers distinguish arithmetic failure from ordinary nonconvergence.

   type Warp_Selection_Status is
     (Warp_Was_Selected,
      --  Factor and the optional flat-jet closure seed are available.

      Warp_Trim_Asymmetry_Is_Unsupported,
      --  The endpoint-plane trim ratio exceeds Maximum_Trim_Asymmetry.

      Warp_Closure_Failed,
      --  The bounded scalar Möbius solve could not reproduce the trim ratio.

      Warp_Selection_Is_Numerically_Unsafe
      --  Finite trustworthy warp arithmetic could not be established.
     );
   --  Outcome of the bounded scalar solve for the single Möbius distance warp shared by every frame candidate.

   type Warp_Selection is record
      Status : Warp_Selection_Status := Warp_Selection_Is_Numerically_Unsafe;
      --  Outcome of the bounded selection stage.

      Factor : Dimensionless := 0.0;
      --  Positive endpoint-preserving Möbius factor on success.

      Seed : Closure_Seed;
      --  Optional exact flat-jet proposal for the first frame only.
   end record
   with
     Dynamic_Predicate =>
       (if Warp_Selection.Status = Warp_Was_Selected
        then
          Warp_Selection.Factor >= Minimum_Distance_Warp_Factor
          and then Warp_Selection.Factor <= Maximum_Distance_Warp_Factor
        else Warp_Selection.Factor = 0.0 and then not Warp_Selection.Seed.Valid),
     Predicate_Failure => "a selected warp must have a supported factor; a failed selection must contain no proposal";
   --  The selected Möbius factor and, for a flat endpoint-jet case, an optional exact closure proposal for the first
   --  frame. The predicate prevents failed selections from carrying stale factor or seed data.

   ---------------------------------------------------------------------------
   --  Private construction operations
   ---------------------------------------------------------------------------

   function Binomial (N, K : Natural) return Dimensionless
   with
     Post =>
       Binomial'Result >= 0.0
       and then Binomial'Result <= Binomial_Limit
       and then (if K <= N and then N <= Maximum_Binomial_Degree then Binomial'Result >= 1.0);
   --  Return the binomial coefficient "N choose K" from Binomial_Table. Requests with K greater than N or N beyond the
   --  stored degree return zero, which simplifies bounded polynomial summations.

   function Closure_Envelope_Bernstein return Raw_Bernstein
   with
     Post =>
       (for all J in Raw_Bernstein'Range =>
          Closure_Envelope_Bernstein'Result (J) >= 0.0 and then Closure_Envelope_Bernstein'Result (J) <= 4.0);
   --  Return the Bernstein coefficients of 256·U⁴·(1 - U)⁴. Its value and first three derivatives vanish at both
   --  endpoints, so scaling it can adjust interior geometry without changing either endpoint tangent jet.

   function Closure_Envelope_Value (U : Dimensionless) return Dimensionless
   with Pre => U >= 0.0 and then U <= 1.0;
   --  Evaluate the endpoint-invisible closure envelope directly at U.

   function Power_Basis (Coefficients : Bernstein_Chart) return Power_Chart
   with Pre => Within (Coefficients, Chart_Build_Limit), Post => Within (Power_Basis'Result, Power_Basis_Limit);
   --  Convert all three chart components from Bernstein controls to ordinary power coefficients using forward
   --  differences. The returned polynomial represents the same chart in a basis suited to Horner evaluation.

   function Chord_Is_Numerically_In_Endpoint_Plane
     (Request : Blend_Request; Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale) return Boolean;
   --  Check whether the endpoint chord is consistent with the plane spanned by the two endpoint tangents. It first
   --  accepts residuals covered by normalized arithmetic error; otherwise it projects the rounding cells of the stored
   --  endpoint coordinates through the plane's orthogonal complement. This admits cancellation caused by large
   --  accumulated coordinates without allowing uncertainty on an unrelated axis to hide a genuine out-of-plane chord.

   function Build_Candidate
     (Request           : Blend_Request;
      Chord_Length      : Length;
      Chord_Direction   : Position_Scale;
      Frame             : Frame_Vector_Array;
      Start_Chart_Jets  : Chart_Jet_Array;
      Finish_Chart_Jets : Chart_Jet_Array;
      Warp_Factor       : Dimensionless;
      Initial_Closure   : Closure_Seed) return Candidate_Result
   with
     Pre =>
       Chord_Length >= Arc_Length_Floor
       and then Chord_Length <= Chord_Length_Limit
       and then Request.Maximum_Position_Error > 0.0 * mm
       and then Request.Maximum_Position_Error <= Arc_Length_Limit
       and then Request.Maximum_Arc_Length >= Chord_Length
       and then Request.Maximum_Arc_Length <= Arc_Length_Limit
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Chord_Direction, Candidate_Component_Limit)
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Start_Chart_Jets, Tame_Limit)
       and then Within (Finish_Chart_Jets, Tame_Limit);
   --  Attempt complete construction for one frame and its endpoint chart jets. Initial_Closure may provide a
   --  flat-planar solution discovered during warp selection; a valid seed is used without replacing or refining it,
   --  while an absent seed makes this stage solve closure itself.
   --
   --  For flat endpoint jets whose chord is planar within stored-coordinate resolution, this stage removes normal
   --  chart modes proved to be numerical noise before compiling the curve. The ordinary representation and
   --  endpoint-correction certificates still have to accept the canonicalized result.
   --
   --  The result separates failure to close the geometry from failure to compile an accurate realtime representation.
   --  Numerically unsafe arithmetic is reported separately so Create_Blend can retain the most informative failure
   --  seen while trying alternative frames.

   type Ideal_Candidate is record
      Status : Candidate_Status := Candidate_Closure_Failed;
      --  Candidate_Success, Candidate_Closure_Failed or Candidate_Numerically_Unsafe. The fields below are meaningful
      --  only on success.

      Coefficients : Bernstein_Chart := [others => [others => 0.0]];
      --  Authoritative Bernstein chart after closure and structural canonicalization.

      Effective_C0 : Raw_Vector_3 := [others => 0.0];
      --  Closure adjustment matching Coefficients.

      Length_Value : Length := 0.0 * mm;
      --  Curve length, clamped to the chord and the requested maximum.

      Bulge_Allowed : Boolean := False;
      --  Whether Request.Bulge admits leaving the endpoint tangent cone for this geometry.
   end record;
   --  The ideal tangent curve of one frame candidate, before any realtime representation is compiled.

   function Build_Ideal_Candidate
     (Request           : Blend_Request;
      Chord_Length      : Length;
      Chord_Direction   : Position_Scale;
      Frame             : Frame_Vector_Array;
      Start_Chart_Jets  : Chart_Jet_Array;
      Finish_Chart_Jets : Chart_Jet_Array;
      Warp_Factor       : Dimensionless;
      Initial_Closure   : Closure_Seed) return Ideal_Candidate
   with
     Pre  =>
       Chord_Length >= Arc_Length_Floor
       and then Chord_Length <= Chord_Length_Limit
       and then Request.Maximum_Position_Error > 0.0 * mm
       and then Request.Maximum_Position_Error <= Arc_Length_Limit
       and then Request.Maximum_Arc_Length >= Chord_Length
       and then Request.Maximum_Arc_Length <= Arc_Length_Limit
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Chord_Direction, Candidate_Component_Limit)
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Start_Chart_Jets, Tame_Limit)
       and then Within (Finish_Chart_Jets, Tame_Limit),
     Post =>
       Within (Build_Ideal_Candidate'Result.Coefficients, Tame_Limit)
       and then Within (Build_Ideal_Candidate'Result.Effective_C0, Tame_Limit)
       and then
         (if Build_Ideal_Candidate'Result.Status = Candidate_Success
          then
            Build_Ideal_Candidate'Result.Length_Value >= Chord_Length
            and then Build_Ideal_Candidate'Result.Length_Value <= Request.Maximum_Arc_Length);
   --  The closure solve, length clamp, chart construction and normal-mode canonicalization of Build_Candidate, which
   --  then compiles the realtime representation of this ideal curve.

   subtype Blend_Frame_Index is Positive range 1 .. 7;
   --  Create_Blend tries at most seven distinct stereographic projection poles.

   type Blend_Preparation is record
      Status : Blend_Result_Kind := Blend_Not_Attempted;
      --  Blend_Success once every request check and the warp selection have passed; otherwise the failure that
      --  Create_Blend reports.

      Chord_Length                      : Length := 0.0 * mm;
      Chord_Direction                   : Position_Scale := [others => 0.0];
      Start_Canonical, Finish_Canonical : Scaled_Tangent_Jet := [others => [others => 0.0]];
      Selection                         : Warp_Selection;

      Directions      : Frame_Candidate_Array (Blend_Frame_Index) := [others => [others => 0.0]];
      Direction_Count : Natural range 0 .. Blend_Frame_Index'Last := 0;
      --  Distinct unit first-frame directions, in the order Create_Blend tries them.

      Seeded_Direction : Natural range 0 .. Blend_Frame_Index'Last := 0;
      --  The direction which receives Selection.Seed, or zero if none does.
   end record;
   --  Everything Create_Blend establishes before trying frames.

   function Arc_Length (Evaluator : Stereographic_Curve_Evaluator) return Length
   is (if Evaluator.Kind = Zero_Curve_Kind then 0.0 * mm else Evaluator.Length_Value);

   function Arc_Length (Curve : Stereographic_Curve) return Length
   is (Arc_Length (Curve.Evaluator_Data));

   function Geometry_Certificate
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Certified_Geometry.Certificate
   is (Curve.Geometry);

   function Certificate_Matches (Curve : Stereographic_Curve) return Boolean
   is (Stereographic_Kernel.Ideal_Geometry."="
         (Stereographic_Kernel.Certified_Geometry.Frame (Curve.Geometry), Curve.Frame)
       and then
         Stereographic_Kernel.Ideal_Geometry."="
           (Stereographic_Kernel.Certified_Geometry.Coefficients (Curve.Geometry), Curve.Coefficients));

   function Evaluator_Certificate
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Certified_Evaluator.Certificate
   is (Curve.Evaluator_Evidence);

   function In_Millimetres (Value : Position) return Stereographic_Kernel.Retained_Tangent.Dimensionless_Axis_Vector
   is ([for Axis in Axis_Name => Dimensionless (Value (Axis) / mm)]);

   function Finish_In_Millimetres
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Retained_Tangent.Dimensionless_Axis_Vector
   is (In_Millimetres (Curve.Evaluator_Data.Finish_Point));

   function Uncorrected_Finish_In_Millimetres
     (Curve : Stereographic_Curve) return Stereographic_Kernel.Retained_Tangent.Dimensionless_Axis_Vector
   is (In_Millimetres (Curve.Evaluator_Data.Uncorrected_Finish_Point));

   function Length_In_Millimetres (Curve : Stereographic_Curve) return Dimensionless
   is (Dimensionless (Curve.Evaluator_Data.Length_Value / mm));

   function Executed_Tangent_Error_Bound (Curve : Stereographic_Curve) return Dimensionless
   is (Stereographic_Kernel.Finish_Correction.Total_Tangent_Error_Bound
         (Stereographic_Kernel.Certified_Evaluator.Tangent_Error (Curve.Evaluator_Evidence),
          Finish_In_Millimetres (Curve),
          Uncorrected_Finish_In_Millimetres (Curve),
          Length_In_Millimetres (Curve)));

   function Is_Prepared (Preparation : Blend_Preparation) return Boolean
   is (Preparation.Chord_Length >= Arc_Length_Floor
       and then Preparation.Chord_Length <= Chord_Length_Limit
       and then Preparation.Selection.Status = Warp_Was_Selected
       and then Within (Preparation.Chord_Direction, Unit_Component_Limit)
       and then Within (Preparation.Start_Canonical, Tame_Limit)
       and then Within (Preparation.Start_Canonical (0), 4.0)
       and then Within (Preparation.Finish_Canonical, Tame_Limit)
       and then Within (Preparation.Finish_Canonical (0), 4.0)
       and then (for all I in Blend_Frame_Index => Within (Preparation.Directions (I), Unit_Component_Limit)));
   --  What a successful preparation hands to the frame candidates: a chord and a warp within the limits of the
   --  construction, canonical jets within the common limit whose tangents are unit vectors, and unit directions.

   function Construct_Blend (Request : Blend_Request) return Blend_Result;
   --  Everything Create_Blend does except the final check of the result's certificate.

   function Prepare_Blend (Request : Blend_Request) return Blend_Preparation
   with
     Post =>
       (if Prepare_Blend'Result.Status = Blend_Success
        then
          Is_Prepared (Prepare_Blend'Result)
          and then Request.Maximum_Position_Error > 0.0 * mm
          and then Request.Maximum_Position_Error <= Arc_Length_Limit
          and then Request.Maximum_Arc_Length >= Prepare_Blend'Result.Chord_Length
          and then Request.Maximum_Arc_Length <= Arc_Length_Limit);
   --  Validate Request, canonicalize its endpoint jets, select the distance warp and list the frame directions.

   function Frame_Candidate
     (Preparation               : Blend_Preparation;
      Index                     : Blend_Frame_Index;
      Frame                     : out Frame_Vector_Array;
      Start_Chart, Finish_Chart : out Chart_Jet_Array) return Boolean
   with
     Side_Effects,
     Pre  =>
       Preparation.Status = Blend_Success
       and then Index <= Preparation.Direction_Count
       and then Is_Prepared (Preparation),
     Post =>
       Within (Frame, Unit_Component_Limit)
       and then Within (Start_Chart, Tame_Limit)
       and then Within (Finish_Chart, Tame_Limit);
   --  Build the frame for one prepared direction and project both endpoint jets into its chart. False if the frame
   --  is degenerate or puts either endpoint tangent too close to the projection pole.

   function Frame_Seed (Preparation : Blend_Preparation; Index : Blend_Frame_Index) return Closure_Seed;
   --  The closure seed for one prepared direction: the warp selection's proposal for the first frame only.

   function Build_Rational_Representation
     (Curve                  : in out Stereographic_Curve;
      Power_Coefficients     : Power_Chart;
      Maximum_Position_Error : Length;
      Start_Jet, Finish_Jet  : Endpoint_Tangent_Jet) return Realtime_Compilation_Status
   with
     Side_Effects,
     Pre =>
       Curve.Evaluator_Data.Kind = Positive_Curve_Kind
       and then Curve.Evaluator_Data.Length_Value > 0.0 * mm
       and then Curve.Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Curve.Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Maximum_Position_Error > 0.0 * mm
       and then Is_Finite (Dimensionless (Maximum_Position_Error / mm))
       and then
         (for all Degree in Chart_Coefficient_Index =>
            (for all Component in Chart_Component_Index => Is_Finite (Power_Coefficients (Degree, Component))));
   pragma Annotate (GNATprove, Skip_Proof, Build_Rational_Representation);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Convert the ideal rational tangent into the fixed-capacity partial-fraction evaluator, certify its tangent and
   --  position errors, and populate Curve's realtime and bound data. The tangent is certified from samples; only
   --  when that proof does not apply is the common-denominator Bernstein form reconstructed, and then retained.
   --  The status explains whether failure was an accuracy limitation or unsafe arithmetic.

   procedure Build_Chart
     (Start_Jets, Finish_Jets : Chart_Jet_Array;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Coefficients            : out Bernstein_Chart;
      D_Lambda_Coefficients   : out Bernstein_Chart)
   with
     Pre  =>
       Lambda >= 1.0
       and then Lambda <= Tame_Limit
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (C0, Tame_Limit)
       and then Within (Start_Jets, Tame_Limit)
       and then Within (Finish_Jets, Tame_Limit),
     Post => Within (Coefficients, Chart_Build_Limit) and then Within (D_Lambda_Coefficients, Chart_Build_Limit);
   --  Construct the degree-eight Bernstein chart from the two endpoint jets, the curve-length ratio, and the three
   --  endpoint-invisible closure adjustments. D_Lambda_Coefficients contains the analytic change in every coefficient
   --  with respect to Lambda for use by the closure Jacobian.

   procedure Build_Chart
     (Start_Jets, Finish_Jets : Chart_Jet_Array;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Coefficients            : out Bernstein_Chart)
   with
     Pre  =>
       Lambda >= 1.0
       and then Lambda <= Tame_Limit
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (C0, Tame_Limit)
       and then Within (Start_Jets, Tame_Limit)
       and then Within (Finish_Jets, Tame_Limit),
     Post => Within (Coefficients, Chart_Build_Limit);
   --  Build only the chart coefficients when the caller does not need the derivative with respect to Lambda.

   function Canonicalize_And_Validate_Jet
     (Jet : Endpoint_Tangent_Jet; Chord_Length : Length; Canonical : out Scaled_Tangent_Jet) return Boolean
   with
     Side_Effects,
     Post =>
       Within (Canonical, Tame_Limit)
       and then Within (Canonical (0), 4.0)
       and then
         (if not Canonicalize_And_Validate_Jet'Result
          then
            (for all Order in Scaled_Tangent_Jet'Range =>
               (for all Axis in Axis_Name => Canonical (Order) (Axis) = 0.0)));
   --  Convert the physical distance derivatives in Jet into factorial-scaled Taylor coefficients for the
   --  chord-normalised distance coordinate. The unit-tangent identities are checked first in physical units, making
   --  validity independent of chord length, and the accepted Taylor series is normalized onto the unit sphere. Return
   --  False, with Canonical zeroed, if the jet is inconsistent or cannot be processed safely.
   --
   --  Specifically, we check:
   --
   --     T·T′ = 0
   --     T·T″ + T′·T′ = 0
   --     T·T‴ + 3·T′·T″ = 0.

   function Chart_From_Canonical_Jet
     (Frame : Frame_Vector_Array; Jet : Scaled_Tangent_Jet; Result : out Chart_Jet_Array) return Boolean
   with
     Side_Effects,
     Pre  => Within (Frame, Unit_Component_Limit) and then Within (Jet, Tame_Limit),
     Post => Within (Result, Tame_Limit);
   --  Express the complete unit-tangent Taylor jet in Frame and apply inverse stereographic projection through third
   --  order. Each chart component is formed as Y(C) = T(C) / (1 + T(0)); Taylor-series division preserves the endpoint
   --  derivatives instead of projecting only the tangent value.
   --
   --  On success, Result contains the projected chart coefficients and the function returns True. If the tangent is
   --  too close to the projection pole, an input is non-finite, or the Taylor arithmetic is unsafe, Result is cleared
   --  to zero and the function returns False.

   function Chart_Value (Coefficients : Bernstein_Chart; U : Dimensionless) return Chart_Vector
   with Pre => Within (Coefficients, Tame_Limit), Post => Within (Chart_Value'Result, Chart_Point_Limit);
   --  Evaluate the Bernstein chart at U. The implementation chooses its recurrence direction by interval half so it
   --  remains well conditioned near both endpoints.

   function Complete_Frame
     (First           : Position_Scale;
      Start_Tangent   : Position_Scale;
      Finish_Tangent  : Position_Scale;
      Chord_Direction : Position_Scale;
      Frame           : out Frame_Vector_Array) return Boolean
   with
     Side_Effects,
     Pre  =>
       Within (First, Candidate_Component_Limit)
       and then Within (Start_Tangent, Candidate_Component_Limit)
       and then Within (Finish_Tangent, Candidate_Component_Limit)
       and then Within (Chord_Direction, Candidate_Component_Limit),
     Post => Within (Frame, Unit_Component_Limit);
   --  Extend First into an orthonormal four-dimensional frame using the geometric directions and machine axes as
   --  candidates. Return False if stable Gram-Schmidt construction and its final orthogonality check cannot succeed.

   Factorial_Limit : constant := 2.0**62;
   --  Above the factorial of twenty, about 2⁶¹·¹.

   function Factorial (N : Natural) return Dimensionless
   with Pre => N <= 20, Post => Factorial'Result >= 1.0 and then Factorial'Result <= Factorial_Limit;
   --  Return N factorial as Dimensionless for the small derivative orders used by chart construction.

   function Frame_Coordinate
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Value : Position_Scale) return Dimensionless
   with
     Pre  => Within (Frame (Component), Unit_Component_Limit) and then Within (Value, Frame_Operand_Limit),
     Post =>
       abs Frame_Coordinate'Result <= Frame_Product_Limit
       and then (if Within (Value, Tame_Limit) then abs Frame_Coordinate'Result <= 8.0 * Tame_Limit)
       and then (if Within (Value, Unit_Component_Limit) then abs Frame_Coordinate'Result <= 16.0);
   --  Return the coordinate of Value along one basis vector of Frame. For an orthonormal frame this dot product is the
   --  corresponding component of Value in local frame coordinates.

   function Frame_Vector (Frame : Frame_Vector_Array; Components : Raw_Vector_4) return Position_Scale
   with
     Pre  => Within (Frame, Unit_Component_Limit) and then Within (Components, Frame_Operand_Limit),
     Post =>
       Within (Frame_Vector'Result, Frame_Product_Limit)
       and then (if Within (Components, Tame_Limit) then Within (Frame_Vector'Result, 8.0 * Tame_Limit))
       and then (if Within (Components, Unit_Component_Limit) then Within (Frame_Vector'Result, 16.0));
   --  Transform local frame Components back into a vector resolved on the machine axes.

   function Interval_Divide (Numerator, Denominator : Interval) return Interval;
   --  Enclose a quotient; an invalid interval or a denominator containing zero fails closed.

   function Is_Zero_Projection (Coefficients : Projection_Coefficients) return Boolean;
   --  Return True when every projection coefficient is exactly zero.
   function Raw_Taylor_Multiply (Left, Right : Raw_Taylor) return Raw_Taylor
   with
     Pre  => Within (Left, Taylor_Operand_Limit) and then Within (Right, Taylor_Operand_Limit),
     Post => Within (Raw_Taylor_Multiply'Result, Taylor_Product_Limit);
   --  Multiply two cubic Taylor series using a Cauchy product and return the coefficients through degree three. Terms
   --  of degree four and above are intentionally discarded because no later operation consumes derivatives above
   --  third order.

   function Raw_Taylor_Reciprocal (Value : Raw_Taylor) return Raw_Taylor
   with
     Pre  => abs Value (0) > Minimum_Safe_Reciprocal_Denominator and then Within (Value, Taylor_Divisor_Limit),
     Post =>
       Within (Raw_Taylor_Reciprocal'Result, Taylor_Reciprocal_Limit)
       and then Raw_Taylor_Reciprocal'Result (0) = 1.0 / Value (0);
   --  Compute the truncated Taylor series for 1/Value. This solves the coefficient convolution that makes
   --  Value·Raw_Taylor_Reciprocal (Value) equal to 1 through degree three; it does not take the reciprocal of each
   --  array element independently.
   --
   --  Value contains the Taylor coefficients of the denominator. Its constant coefficient must be safely nonzero.
   --  The result contains the Taylor coefficients of the reciprocal through degree three.

   function Safe_Norm (Value : Position_Scale; Success : out Boolean) return Dimensionless
   with
     Side_Effects,
     Post =>
       Safe_Norm'Result >= 0.0
       and then Safe_Norm'Result <= 2.0 * Norm_Operand_Limit
       and then (if Success then Within (Value, Safe_Norm'Result) else Safe_Norm'Result = 0.0)
       and then (if Within (Value, Tame_Limit) then Safe_Norm'Result <= 2.0 * Tame_Limit);
   --  Compute the Euclidean norm of a dimensionless vector after scaling by its largest component. The scaling avoids
   --  overflow and underflow in the sum of squares. Success is False, and the returned norm is zero, when an input is
   --  non-finite or the calculation cannot be completed safely.

   function Safe_Norm (Value : Position_Offset; Success : out Boolean) return Length
   with
     Side_Effects,
     Post =>
       Safe_Norm'Result >= 0.0 * mm
       and then Safe_Norm'Result <= 2.0 * Norm_Operand_Limit * mm
       and then
         (if Success
          then (for all Axis in Axis_Name => abs Value (Axis) <= Safe_Norm'Result)
          else Safe_Norm'Result = 0.0 * mm)
       and then
         (if (for all Axis in Axis_Name => abs Value (Axis) <= Tame_Limit * mm)
          then Safe_Norm'Result <= 2.0 * Tame_Limit * mm);
   --  Compute the Euclidean norm of a physical position offset using the same scaled sum-of-squares algorithm. Success
   --  is False, and the returned length is zero, when an input is non-finite or the calculation cannot be completed
   --  safely.

   function Select_Distance_Warp
     (Request                                        : Blend_Request;
      Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Lambda                                 : Dimensionless) return Warp_Selection
   with
     Pre =>
       Maximum_Lambda >= 1.0
       and then Maximum_Lambda <= Lambda_Limit
       and then Within (Chord_Direction, Unit_Component_Limit)
       and then Within (Start_Tangent, 4.0)
       and then Within (Finish_Tangent, 4.0);
   --  Select the endpoint-preserving transformation V(U) = W·U / (1 - U + W·U) used by every frame candidate.
   --  U measures normalized physical distance, while V locates the corresponding point in the polynomial tangent
   --  chart. W = 1 leaves the parameter unchanged, W < 1 allocates more physical distance near the start tangent, and
   --  W > 1 allocates more near the finish tangent.
   --
   --  Chord_Direction and the endpoint tangents determine the desired incoming-to-outgoing trim ratio. Maximum_Lambda
   --  is the maximum permitted curve length divided by chord length. The result distinguishes unsupported asymmetry,
   --  failure to reproduce that ratio, and unsafe numerical arithmetic. For flat endpoint jets whose chord is planar
   --  within normalized arithmetic and stored-coordinate rounding uncertainty, it may also include a zero-bubble
   --  closure seed for the next construction stage.

   function Stereographic (Y : Chart_Vector) return Raw_Vector_4
   with
     Pre  => Within (Y, Chart_Point_Limit),
     Post => abs Stereographic'Result (0) <= 1.0 and then Within (Stereographic'Result, 2.0 * Chart_Point_Limit);
   --  Map a three-dimensional chart point onto the unit sphere in the four-dimensional local tangent frame. This
   --  rational map guarantees a unit tangent for every finite chart point.

   function Stereographic_Jacobian (Y : Chart_Vector) return Raw_Stereo_Jacobian
   with Pre => Within (Y, Chart_Point_Limit);
   --  Return the analytic derivative of Stereographic with respect to the three chart coordinates.

   function Tangent_At
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor : Dimensionless; U : Dimensionless)
      return Position_Scale
   with
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Coefficients, Tame_Limit),
     Post => Within (Tangent_At'Result, Frame_Product_Limit);
   --  Warp normalized physical distance into chart distance, evaluate the unit tangent in the local frame, and convert
   --  it back to machine-axis coordinates.

   function Warp_Parameter (U, Warp_Factor : Dimensionless) return Dimensionless
   with
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor,
     Post => Warp_Parameter'Result >= 0.0 and then Warp_Parameter'Result <= 1.0;
   --  Apply the monotone endpoint-preserving map from normalized physical distance U to polynomial chart coordinate V.

   function Strict_Bernstein_Nonnegative (Coefficients : Interval_Polynomial; Depth : Natural) return Boolean;
   pragma Annotate (GNATprove, Skip_Proof, Strict_Bernstein_Nonnegative);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Whether the polynomial whose Bernstein controls Coefficients encloses is proved nonnegative on [0, 1], from
   --  the controls' lower bounds and up to Depth levels of bisection. False is not a proof of a negative value, except
   --  that an end control whose upper bound is negative is one.

   type Chart_Lambda_Terms is array (Raw_Taylor'Range) of Power_Chart;
   --  Element K holds the power coefficients which multiply λᴷ in the chart without its closure mode.

   function Within (Value : Chart_Lambda_Terms; Limit : Dimensionless) return Boolean
   is (for all Order in Raw_Taylor'Range =>
         (for all Index in Chart_Coefficient_Index =>
            (for all Component in Chart_Component_Index => abs Value (Order) (Index, Component) <= Limit)));

   function Build_Chart_Lambda_Terms
     (Start_Jets, Finish_Jets : Chart_Jet_Array; Warp_Factor : Dimensionless) return Chart_Lambda_Terms
   with
     Pre  =>
       Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Start_Jets, Tame_Limit)
       and then Within (Finish_Jets, Tame_Limit),
     Post => Within (Build_Chart_Lambda_Terms'Result, Lambda_Terms_Limit);
   --  Scaling the normalized distance by λ scales the kth endpoint derivative by λᵏ, and Build_Chart is linear in
   --  those scaled derivatives. The chart of any λ is therefore a cubic in λ whose four coefficient charts depend
   --  only on the endpoint jets and the warp, so the closure solve forms them once rather than rebuilding the
   --  chart at every residual evaluation.

   procedure Chart_At_Lambda
     (Terms                               : Chart_Lambda_Terms;
      Lambda                              : Dimensionless;
      C0                                  : Raw_Vector_3;
      Coefficients, D_Lambda_Coefficients : out Power_Chart)
   with
     Pre  => Within (Terms, Tame_Limit) and then abs Lambda <= Lambda_Limit and then Within (C0, Tame_Limit),
     Post => Within (Coefficients, Closure_Chart_Limit) and then Within (D_Lambda_Coefficients, Closure_Chart_Limit);
   --  The power-basis form of the chart which Build_Chart builds for Lambda and C0, with the rounding residue of its
   --  degree-seven Hermite part removed from V⁸, and the exact derivative of that chart with respect to Lambda.

   function Solve_Closure_From_Terms
     (Terms                     : Chart_Lambda_Terms;
      Frame                     : Frame_Vector_Array;
      Chord_Direction           : Position_Scale;
      Maximum_Lambda, Tolerance : Dimensionless;
      Warp_Factor               : Dimensionless;
      Initial                   : Closure_Solution;
      Panel_Count               : Positive := 1) return Closure_Solution
   with
     Pre  =>
       Maximum_Lambda >= 1.0
       and then Maximum_Lambda <= Lambda_Limit
       and then Tolerance > 0.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Panel_Count <= Maximum_Panel_Count
       and then Within (Terms, Tame_Limit)
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Chord_Direction, Candidate_Component_Limit)
       and then (not Initial.Success or else Initial.Lambda <= Maximum_Lambda),
     Post =>
       (if Solve_Closure_From_Terms'Result.Success then Solve_Closure_From_Terms'Result.Lambda <= Maximum_Lambda);
   --  Solve the fixed-frame closure equations for the chart whose λ-dependence Terms describes, within the
   --  length-ratio and residual limits. Initial supplies the starting point, while Panel_Count selects the quadrature
   --  resolution used for each residual evaluation.

   function Projected_Bound_On_Parameter_Range
     (Curve        : Stereographic_Curve;
      Start_U      : Dimensionless;
      End_U        : Dimensionless;
      Coefficients : Projection_Coefficients) return Curvature;
   --  Certify an upper bound on the absolute projected realtime tangent over a normalized-distance interval, including
   --  ideal-to-cache error and the continuous finish correction.

   --  A degree-seven Hermite chart matches the supplied tangent derivatives through order three. The
   --  endpoint-invisible degree-eight mode
   --
   --     256 * V**4 * (1 - V)**4
   --
   --  then closes the position without changing either endpoint jet. V is a monotone Möbius map of normalized
   --  distance, so asymmetric blends use the complete chart interval without increasing its degree.

   function Ideal_Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position;
   pragma Annotate (GNATprove, Skip_Proof, Ideal_Point_At_Parameter);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Construction-only high-accuracy numerical evaluation of the ideal tangent integral. Realtime code uses
   --  Point_At_Parameter instead.

   function Bounds_On_Parameter_Range
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Unit_Speed_Axial_Derivative_Bounds
   with Pre => Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0;
   --  Construction-only derivative bounds for the retained evaluator on the normalized distance interval.

   function Measure_Evaluator_Endpoint_Jet_Error
     (Evaluator            : Stereographic_Curve_Evaluator;
      Requested_Start_Jet  : Endpoint_Tangent_Jet;
      Requested_Finish_Jet : Endpoint_Tangent_Jet;
      Error_Bounds         : out Endpoint_Jet_Error_Bounds) return Boolean
   with Side_Effects;
   --  Outward-enclose retained endpoint tangent-jet errors through normalized derivative order three. False reports
   --  that at least one finite enclosure was unavailable; Error_Bounds is then saturated.

   function Pole_Taylor_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Sharp : out Boolean) return Axis_Majorants
   with Side_Effects;
   pragma Annotate (GNATprove, Skip_Proof, Pole_Taylor_Tangent_Range_Majorants);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Bound the retained tangent and its first four derivatives directly from the pole/residue evaluator. Each cell
   --  of an adaptive partition uses a fifth-order Taylor expansion about its centre with a rigorous sixth-order
   --  remainder, and cells are trisected until their bound is within half a percent of an attained value. This is
   --  the primary path for every range, including a point-sized one.
   --
   --  The result is always a valid bound. Sharp is False when it is saturated because a pole lies too close to the
   --  range, when the work limits stop refinement while some bound is still more than two percent above a value
   --  known to be attained, or when cancellation between the partial fractions makes the rounding allowance a
   --  visible part of some bound; Bernstein_Tangent_Range_Majorants may then prove a smaller one.

   function Retain_Bernstein_Tangent_Certificate (Curve : in out Stereographic_Curve) return Boolean
   with Side_Effects, Pre => Curve.Evaluator_Data.Kind = Positive_Curve_Kind;
   --  Reconstruct the retained tangent in common-denominator Bernstein form and store it in Curve. Construction
   --  normally certifies the tangent from samples and skips this far costlier reconstruction, so it is run on
   --  demand by the Bernstein derivative fallback. False, with Curve unchanged, if the reconstruction cannot prove
   --  a positive denominator. Whether it also bounds the tangent error within budget does not matter here.

   function Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Success : out Boolean) return Axis_Majorants
   with Side_Effects, Pre => Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0;
   pragma Annotate (GNATprove, Skip_Proof, Bernstein_Tangent_Range_Majorants);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Reconstruct factorial-scaled retained-tangent derivatives from the stored common-denominator Bernstein form.
   --  Exact binomial ratios and outward-rounded interval operations keep coefficient conversions enclosing, while
   --  bounded De Casteljau subdivision tightens the range hull. Success is False when the curve retains no
   --  Bernstein certificate or that certificate cannot prove a finite bound.
   --
   --  This is the fallback for a range on which Pole_Taylor_Tangent_Range_Majorants saturates. It is exposed
   --  privately so child-package tests can exercise it directly.

end Prunt.Motion_Planner.Stereographic_Curves;
