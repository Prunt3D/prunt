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

with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;

pragma Extensions_Allowed (On);

package Prunt.Motion_Planner.Stereographic_Curves is
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
   --  valid; residuals consistent with floating-point roundoff are accepted.

   type Endpoint_Tangent_Jet is record
      Tangent : Position_Scale := [X_Axis => 1.0, others => 0.0];
      --  Unit direction of travel at the endpoint.

      Tangent_Derivative_1 : Tangent_Derivative_1_Vector := [others => 0.0 / mm];
      --  Change in direction per unit of distance.

      Tangent_Derivative_2 : Tangent_Derivative_2_Vector := [others => 0.0 / mm ** 2];
      --  Change in turn rate per squared unit of distance.

      Tangent_Derivative_3 : Tangent_Derivative_3_Vector := [others => 0.0 / mm ** 3];
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

      Allow_Bulge : Boolean := False;
      --  False keeps the authoritative ideal tangent inside the endpoint cone. The mathematical retained evaluator is
      --  within Retained_Tangent_Error_Bound of that tangent, so its signed projection on any unit cone normal is at
      --  least -Retained_Tangent_Error_Bound. True retains cone-compliant curves and additionally permits leaving the
      --  cone when non-negative progress along the chord can be established.
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

   function Create_Blend (Request : Blend_Request) return Blend_Result
   with
     Post =>
       Create_Blend'Result.Kind /= Blend_Not_Attempted
       and then
         (if Create_Blend'Result.Kind = Blend_Success
          then
            Arc_Length (Create_Blend'Result.Curve) > 0.0 * mm
            and then Arc_Length (Create_Blend'Result.Curve) <= Request.Maximum_Arc_Length);
   --  Validate Request, solve the endpoint-closure problem, and compile the result into a bounded realtime
   --  representation. Failure is reported through the result kind; invalid input or an unsupported geometry does not
   --  raise an exception.

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
   --  Return a construction-certified Euclidean bound on the larger of the start and finish endpoint errors for the
   --  requested tangent derivative order, expressed in normalized-distance coordinates. For Order > 0, dividing by
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

private

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   pragma
     Compile_Time_Error
       (not Dimensionless'Machine_Rounds, "Stereographic_Curves requires rounded floating-point arithmetic");

   ---------------------------------------------------------------------------
   --  Ideal tangent
   ---------------------------------------------------------------------------

   Fixed_Chart_Degree : constant := 8;
   --  Degree of the polynomial chart. Degree eight provides nine coefficients: eight satisfy the tangent value and
   --  first three derivatives at both endpoints, while the remaining mode is available to close the curve position.

   Maximum_Tangent_Numerator_Degree : constant := 2 * Fixed_Chart_Degree;
   --  Highest polynomial degree produced when the degree-eight chart is mapped back to a unit tangent through inverse
   --  stereographic projection.

   subtype Chart_Coefficient_Index is Natural range 0 .. Fixed_Chart_Degree;
   --  Index of a coefficient in the fixed degree-eight chart.

   subtype Chart_Component_Index is Positive range 1 .. 3;
   --  Index of one coordinate in the three-dimensional stereographic chart used to represent a unit tangent.

   subtype Frame_Component_Index is Natural range 0 .. 3;
   --  Index of a basis vector in the four-dimensional motion space. Component zero defines the axis opposite the
   --  stereographic projection pole; components one through three correspond to Chart_Component_Index.

   type Chart_Vector is array (Chart_Component_Index) of Dimensionless;
   --  One point in the three-dimensional stereographic chart. Unlike Position_Scale, these components are chart
   --  coordinates rather than machine-axis components and are not constrained to form a unit vector.

   type Bernstein_Chart is array (Chart_Coefficient_Index, Chart_Component_Index) of Dimensionless;
   --  The three chart polynomials stored as Bernstein control coefficients. This basis is used while imposing endpoint
   --  derivatives and adding the endpoint-invisible closure mode.

   type Power_Chart is array (Chart_Coefficient_Index, Chart_Component_Index) of Dimensionless;
   --  The same three chart polynomials stored as ordinary power coefficients for Horner evaluation. Keeping the power
   --  and Bernstein bases as distinct types makes an accidental basis mix-up a compile-time error.

   type Frame_Vector_Array is array (Frame_Component_Index) of Position_Scale;
   --  A complete orthonormal basis for the four machine axes. Each element is one unit basis vector expressed in
   --  machine-axis coordinates.

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

   type Interval is record
      Lower : Dimensionless := 0.0;
      --  Inclusive lower endpoint, rounded toward negative infinity.

      Upper : Dimensionless := 0.0;
      --  Inclusive upper endpoint, rounded toward positive infinity.

      Valid : Boolean := True;
      --  False when finite enclosing arithmetic could not be established.
   end record;
   --  A conservative floating-point enclosure used to certify bounds without trusting ordinary roundoff direction.

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

   function Build_Binomial_Table return Binomial_Table_Type;
   --  Construct Pascal's triangle once during package elaboration using exact additions of integer-valued
   --  coefficients.

   ---------------------------------------------------------------------------
   --  Distance warp
   ---------------------------------------------------------------------------

   Distance_Warp_Log_Lower_Bound : constant Dimensionless := -16.0;
   Distance_Warp_Log_Upper_Bound : constant Dimensionless := 16.0;
   --  Search limits for log(W), where W is the positive distance-warp factor. Searching in logarithmic space treats a
   --  warp and its reciprocal symmetrically and guarantees that every proposed factor remains positive.

   Minimum_Distance_Warp_Factor : constant Dimensionless := Dimensionless_Math.Exp (Distance_Warp_Log_Lower_Bound);
   Maximum_Distance_Warp_Factor : constant Dimensionless := Dimensionless_Math.Exp (Distance_Warp_Log_Upper_Bound);
   --  Inclusive factor bounds obtained from the logarithmic search interval. Keeping W inside this range bounds the
   --  conditioning of the parameter transformation and its derivatives.

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

   type Dimensionless_Axis_Vector is array (Axis_Name) of Dimensionless;
   --  A unit-free vector indexed by machine axes, used for rational residues and displacements normalized by length.

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

   subtype Packed_Binomial_Index is
     Natural range 0 .. (Maximum_Derivative_Bernstein_Degree + 2) ** 2 / 4 - 1;
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
       Left'First = 0 and then Right'First = 0
       and then Left'Length > 0 and then Right'Length > 0
       and then Left'Last + Right'Last <= Maximum_Derivative_Bernstein_Degree;
   --  Multiply by convolving binomial-scaled controls. Fall back to individual enclosing weights when scaling
   --  cannot produce finite intervals, so intermediate overflow does not reject an otherwise certifiable product.

   subtype Rational_Degree_Slot is Positive range 1 .. Maximum_Rational_Degree;
   --  Index of storage associated with one real pole or one component of a complex-conjugate pair.

   subtype Rational_Pair_Index is Positive range 1 .. Maximum_Rational_Degree / 2;
   --  Index of a complex-conjugate pole pair in the compact antiderivative cache.

   type Rational_Pole_Slot is record
      Pole_Component : Dimensionless := 0.0;
      --  A real pole, or one Cartesian component of a complex pole.

      Residue_Component : Dimensionless_Axis_Vector := [others => 0.0];
      --  Matching real residue, or one Cartesian component of a complex residue.
   end record;
   --  One real-valued storage slot. Complex poles and residues use two consecutive slots so runtime evaluation never
   --  requires a complex numeric type.

   type Rational_Pole_Slots is array (Rational_Degree_Slot) of Rational_Pole_Slot;
   --  Fixed-capacity pole and residue storage, avoiding allocation in planner and realtime data.

   type Rational_Antiderivative is record
      Real_Pole_Count : Natural range 0 .. Maximum_Rational_Degree := 0;
      --  Real poles occupy the leading slots, one slot each.

      Pair_Count : Natural range 0 .. Maximum_Rational_Degree / 2 := 0;
      --  Each conjugate pair occupies two slots after the real-pole prefix.

      Constant_Tangent : Dimensionless_Axis_Vector := [others => 0.0];
      --  Constant polynomial quotient. Its primitive is this value times normalised distance.

      Pole_Slots : Rational_Pole_Slots := [others => <>];
      --  A pair stores its real pole/residue components first and its positive-imaginary components in the following
      --  slot.
   end record;
   --  An analytically integrable partial-fraction representation of the compiled tangent. Real poles contribute
   --  logarithms; conjugate pairs contribute real logarithm and arctangent terms.

   function Complex_Pair_Real_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   with Pre => Pair <= Cache.Pair_Count and then Cache.Real_Pole_Count + 2 * Natural (Pair) <= Maximum_Rational_Degree;
   --  Return the slot containing the real components of the selected conjugate pole and residue pair.

   function Complex_Pair_Imaginary_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   with Pre => Pair <= Cache.Pair_Count and then Cache.Real_Pole_Count + 2 * Natural (Pair) <= Maximum_Rational_Degree;
   --  Return the slot containing the positive-imaginary components of the selected conjugate pair.

   function Rational_Antiderivative_Is_Well_Formed (Cache : Rational_Antiderivative) return Boolean;
   --  Cheap structural check used by evaluator contracts: validate counts, slot layout, finiteness, and pole
   --  conditions required on the unit interval.

   function Rational_Antiderivative_Primitives_Are_Safe (Cache : Rational_Antiderivative) return Boolean;
   --  Construction-only check that every retained primitive and accumulation has ample finite range. Keeping this
   --  scan separate avoids repeating logarithms and arctangents in realtime assertion-policy builds.

   function Rational_Antiderivative_Is_Canonical_Zero (Cache : Rational_Antiderivative) return Boolean;
   --  Return True only for the unique all-zero cache used by a zero-length evaluator.

   function Is_Finite (Value : Dimensionless) return Boolean;
   --  Return True only for an ordinary finite floating-point value. The ordered comparisons used by the implementation
   --  reject both infinities and NaNs without invoking the heavier runtime validity-classification helper.

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
   --  evaluator remains the compact pole/residue cache; this certificate is retained only so derivative bounds can
   --  reuse the interval-Bernstein reconstruction already performed by the tangent-error proof.

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
            Stereographic_Curve_Evaluator.Length_Value > 0.0 * mm
            and then Stereographic_Curve_Evaluator.Length_Value <= Length'Last
            and then Rational_Antiderivative_Is_Well_Formed (Stereographic_Curve_Evaluator.Antiderivative_Cache)),
     Predicate_Failure => "a blend evaluator must contain a coherent zero or positive curve";
   --  Compact, allocation-free curve data retained in planner blocks and used for realtime position evaluation.

   function Finish_Correction_Interval (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Interval;
   --  Enclose the endpoint correction for Axis without directly subtracting two potentially extreme coordinates.

   function Stable_Complex_Log_Ratio
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless;
   --  Evaluate log (((U - A)**2 + B**2) / (A**2 + B**2)) without forming cancellation-prone square sums.

   function Stable_Real_Log_Ratio (Pole, Normalized_Distance : Dimensionless) return Dimensionless;
   --  Evaluate log (abs (U - Pole) / abs Pole) without rounding the argument to zero beside an endpoint pole.

   function Stable_Complex_Angle_Delta
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless;
   --  Evaluate a conjugate-pair argument change using scaled cross and dot products to avoid overflow.

   function Stable_Log_One_Plus (Value : Dimensionless) return Dimensionless
   with Pre => Value > -1.0 and then Is_Finite (Value);
   --  Evaluate log(1 + Value) without losing the small increment when Value is close to zero.

   function Evaluate_Rational_Displacement
     (Cache : Rational_Antiderivative; Normalized_Distance : Dimensionless) return Dimensionless_Axis_Vector
   with
     Pre =>
       Rational_Antiderivative_Is_Well_Formed (Cache)
       and then Normalized_Distance >= 0.0
       and then Normalized_Distance <= 1.0;
   --  Evaluate the dimensionless displacement from distance zero by analytically integrating the cached tangent.

   function Evaluate_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   with
     Pre  =>
       Evaluator.Kind = Positive_Curve_Kind and then Normalized_Distance >= 0.0 and then Normalized_Distance <= 1.0,
     Post =>
       (if Normalized_Distance = 0.0
        then Evaluate_Rational_Point'Result = Evaluator.Start_Point
        elsif Normalized_Distance = 1.0
        then Evaluate_Rational_Point'Result = Evaluator.Finish_Point);
   --  Scale and anchor the cached displacement, then apply the endpoint-flat finish correction. The exact requested
   --  endpoints are returned at normalized distances zero and one.

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

      Certified_Endpoint_Jet_Error : Endpoint_Jet_Error_Bounds := [others => 0.0];
      --  Maximum retained start/finish mismatch for each requested normalized tangent derivative order, or the
      --  saturated unavailable marker when the optional direct endpoint enclosure could not be formed.

      Retained_Tangent_Certificate : Retained_Tangent_Bernstein_Certificate := (others => <>);
      --  Construction-only rational Bernstein data used by V7's direct retained-tangent derivative certificate. The
      --  compact evaluator does not carry this data because realtime point evaluation still uses the pole/residue
      --  form.
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
         (for all Order in Endpoint_Tangent_Derivative_Order =>
            Stereographic_Curve.Certified_Endpoint_Jet_Error (Order) >= 0.0
            and then Is_Finite (Stereographic_Curve.Certified_Endpoint_Jet_Error (Order)))
       and then
         (Stereographic_Curve.Evaluator_Data.Kind = Positive_Curve_Kind
          or else
            (Stereographic_Curve.Certified_Position_Error = 0.0 * mm
             and then Stereographic_Curve.Certified_Tangent_Error = 0.0
             and then
               (for all Order in Endpoint_Tangent_Derivative_Order =>
                  Stereographic_Curve.Certified_Endpoint_Jet_Error (Order) = 0.0))),
     Predicate_Failure =>
       "a stereographic curve must contain nonnegative numerical certificates or saturated endpoint reports";
   --  Full construction result: the compact evaluator, ideal geometry, endpoint jet error enclosures, and both the
   --  legacy and Bernstein certificates used by planner queries. The retained Bernstein form bounds compiled
   --  derivatives directly before falling
   --  back to the pole-based Taylor enclosure.

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
          and then Is_Finite (Closure_Seed.Lambda)
          and then (for all Component in Chart_Component_Index => Is_Finite (Closure_Seed.C0 (Component)))),
     Predicate_Failure => "a valid closure seed must contain finite values and lambda >= 1";
   --  An optional upstream closure solution passed into candidate construction. V7 treats a valid seed as final for
   --  the closure stage: it is neither replaced nor refined, but the resulting curve still has to pass endpoint,
   --  representation, and numerical-safety checks.

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
          and then Is_Finite (Closure_Solution.Lambda)
          and then (for all Component in Chart_Component_Index => Is_Finite (Closure_Solution.C0 (Component)))),
     Predicate_Failure => "a successful closure solution must contain finite values and lambda >= 1";
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

   function Binomial (N, K : Natural) return Dimensionless;
   --  Return the binomial coefficient "N choose K" from Binomial_Table. Requests with K greater than N or N beyond the
   --  stored degree return zero, which simplifies bounded polynomial summations.

   function Closure_Envelope_Bernstein return Raw_Bernstein;
   --  Return the Bernstein coefficients of 256·U⁴·(1 - U)⁴. Its value and first three derivatives vanish at both
   --  endpoints, so scaling it can adjust interior geometry without changing either endpoint tangent jet.

   function Closure_Envelope_Value (U : Dimensionless) return Dimensionless;
   --  Evaluate the endpoint-invisible closure envelope directly at U.

   function Power_Basis (Coefficients : Bernstein_Chart) return Power_Chart;
   --  Convert all three chart components from Bernstein controls to ordinary power coefficients using forward
   --  differences. The returned polynomial represents the same chart in a basis suited to Horner evaluation.

   function Chord_Is_Numerically_In_Endpoint_Plane
     (Request : Blend_Request; Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale) return Boolean;
   --  Check whether the endpoint chord is consistent with the plane spanned by the two endpoint tangents. V7 first
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
       Chord_Length > 0.0 * mm
       and then Is_Finite (Dimensionless (Chord_Length / mm))
       and then Request.Maximum_Position_Error > 0.0 * mm
       and then Is_Finite (Dimensionless (Request.Maximum_Position_Error / mm))
       and then Request.Maximum_Arc_Length >= Chord_Length
       and then Is_Finite (Dimensionless (Request.Maximum_Arc_Length / mm))
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then (for all Axis in Axis_Name => Is_Finite (Chord_Direction (Axis)))
       and then
         (for all Component in Frame_Component_Index =>
            (for all Axis in Axis_Name => Is_Finite (Frame (Component) (Axis))))
       and then
         (for all Order in Start_Chart_Jets'Range =>
            (for all Component in Chart_Component_Index =>
               Is_Finite (Start_Chart_Jets (Order) (Component))
               and then Is_Finite (Finish_Chart_Jets (Order) (Component))))
       and then
         (not Initial_Closure.Valid
          or else Initial_Closure.Lambda <= Dimensionless (Request.Maximum_Arc_Length / Chord_Length));
   --  Attempt complete construction for one frame and its endpoint chart jets. Initial_Closure may provide a
   --  flat-planar solution discovered during warp selection; V7 uses a valid seed without replacing or refining it,
   --  while an absent seed makes this stage solve closure itself.
   --
   --  For flat endpoint jets whose chord is planar within stored-coordinate resolution, V7 removes normal chart modes
   --  proved to be numerical noise before compiling the curve. The ordinary representation and endpoint-correction
   --  certificates still have to accept the canonicalized result.
   --
   --  The result separates failure to close the geometry from failure to compile an accurate realtime representation.
   --  Numerically unsafe arithmetic is reported separately so Create_Blend can retain the most informative failure
   --  seen while trying alternative frames.

   function Build_Rational_Representation
     (Curve                  : in out Stereographic_Curve;
      Power_Coefficients     : Power_Chart;
      Maximum_Position_Error : Length;
      Start_Jet, Finish_Jet  : Endpoint_Tangent_Jet) return Realtime_Compilation_Status
   with
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
   --  Convert the ideal rational tangent into the fixed-capacity partial-fraction evaluator, certify its tangent and
   --  position errors, and populate Curve's realtime and bound data. V7 also retains the outward-rounded common-
   --  denominator Bernstein form produced by the tangent proof so later derivative queries can reuse it directly.
   --  The status explains whether failure was an accuracy limitation or unsafe arithmetic.

   procedure Build_Chart
     (Start_Jets, Finish_Jets : Chart_Jet_Array;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Coefficients            : out Bernstein_Chart;
      D_Lambda_Coefficients   : out Bernstein_Chart)
   with
     Pre =>
       Lambda >= 1.0
       and then Is_Finite (Lambda)
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then (for all Component in Chart_Component_Index => Is_Finite (C0 (Component)))
       and then
         (for all Order in Start_Jets'Range =>
            (for all Component in Chart_Component_Index =>
               Is_Finite (Start_Jets (Order) (Component)) and then Is_Finite (Finish_Jets (Order) (Component))));
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
     Pre =>
       Lambda >= 1.0
       and then Is_Finite (Lambda)
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then (for all Component in Chart_Component_Index => Is_Finite (C0 (Component)))
       and then
         (for all Order in Start_Jets'Range =>
            (for all Component in Chart_Component_Index =>
               Is_Finite (Start_Jets (Order) (Component)) and then Is_Finite (Finish_Jets (Order) (Component))));
   --  Build only the chart coefficients when the caller does not need the derivative with respect to Lambda.

   function Canonicalize_And_Validate_Jet
     (Jet : Endpoint_Tangent_Jet; Chord_Length : Length; Canonical : out Scaled_Tangent_Jet) return Boolean;
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
     (Frame : Frame_Vector_Array; Jet : Scaled_Tangent_Jet; Result : out Chart_Jet_Array) return Boolean;
   --  Express the complete unit-tangent Taylor jet in Frame and apply inverse stereographic projection through third
   --  order. Each chart component is formed as Y(C) = T(C) / (1 + T(0)); Taylor-series division preserves the endpoint
   --  derivatives instead of projecting only the tangent value.
   --
   --  On success, Result contains the projected chart coefficients and the function returns True. If the tangent is
   --  too close to the projection pole, an input is non-finite, or the Taylor arithmetic is unsafe, Result is cleared
   --  to zero and the function returns False.

   function Chart_Value (Coefficients : Bernstein_Chart; U : Dimensionless) return Chart_Vector;
   --  Evaluate the Bernstein chart at U. The implementation chooses its recurrence direction by interval half so it
   --  remains well conditioned near both endpoints.

   function Complete_Frame
     (First           : Position_Scale;
      Start_Tangent   : Position_Scale;
      Finish_Tangent  : Position_Scale;
      Chord_Direction : Position_Scale;
      Frame           : out Frame_Vector_Array) return Boolean;
   --  Extend First into an orthonormal four-dimensional frame using the geometric directions and machine axes as
   --  candidates. Return False if stable Gram-Schmidt construction and its final orthogonality check cannot succeed.

   function Down (Value : Dimensionless) return Dimensionless;
   --  Move a finite value far enough downward to conservatively cover one rounded arithmetic operation.

   function Factorial (N : Natural) return Dimensionless;
   --  Return N factorial as Dimensionless for the small derivative orders used by chart construction.

   function Frame_Coordinate
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Value : Position_Scale) return Dimensionless;
   --  Return the coordinate of Value along one basis vector of Frame. For an orthonormal frame this dot product is the
   --  corresponding component of Value in local frame coordinates.

   function Frame_Vector (Frame : Frame_Vector_Array; Components : Raw_Vector_4) return Position_Scale;
   --  Transform local frame Components back into a vector resolved on the machine axes.

   function Integrate_GL16
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor : Dimensionless; A, B : Dimensionless)
      return Position_Scale
   with
     Pre =>
       A >= 0.0
       and then A <= B
       and then B <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor;
   --  Integrate the ideal unit tangent over normalized physical distance A through B using the symmetric 16-point
   --  Gauss-Legendre rule, returning a dimensionless displacement direction in machine-axis coordinates.

   function Interval_Abs_Max (Value : Interval) return Dimensionless;
   --  Return a conservative upper bound on the absolute value of every member of Value.

   function Interval_Add (Left, Right : Interval) return Interval;
   --  Enclose every sum formed by choosing one value from Left and one from Right.

   function Checked_Interval (Lower, Upper : Dimensionless; Valid : Boolean := True) return Interval;
   --  Construct an interval when its endpoints are finite and ordered; otherwise return the invalid full-range marker.

   function Interval_Exact (Value : Dimensionless) return Interval;
   --  Enclose one already-represented input value without adding uncertainty merely for loading it.

   function Interval_Multiply (Left, Right : Interval) return Interval;
   --  Enclose every product formed by choosing one value from each input interval.

   function Certified_Upper_Square_Root (Value : Dimensionless) return Dimensionless;
   --  Return an outward-proven upper bound on the square root of a finite nonnegative value, or Dimensionless'Last
   --  when the elementary-function proposal cannot be certified by an outward-rounded division comparison.

   function Interval_Negate (Value : Interval) return Interval;
   --  Enclose the additive inverse of every member of Value.

   function Interval_Subtract (Left, Right : Interval) return Interval;
   --  Enclose every difference formed by choosing one value from Left and one from Right.

   function Is_Zero_Projection (Coefficients : Projection_Coefficients) return Boolean;
   --  Return True when every projection coefficient is exactly zero.
   function Raw_Taylor_Multiply (Left, Right : Raw_Taylor) return Raw_Taylor;
   --  Multiply two cubic Taylor series using a Cauchy product and return the coefficients through degree three. Terms
   --  of degree four and above are intentionally discarded because no later operation consumes derivatives above
   --  third order.

   function Raw_Taylor_Reciprocal (Value : Raw_Taylor) return Raw_Taylor;
   --  Compute the truncated Taylor series for 1/Value. This solves the coefficient convolution that makes
   --  Value·Raw_Taylor_Reciprocal (Value) equal to 1 through degree three; it does not take the reciprocal of each
   --  array element independently.
   --
   --  Value contains the Taylor coefficients of the denominator. Its constant coefficient must be safely nonzero.
   --  The result contains the Taylor coefficients of the reciprocal through degree three.
   --
   --  Raises Constraint_Error when the constant coefficient is too close to zero for a stable reciprocal.

   function Safe_Norm (Value : Position_Scale; Success : out Boolean) return Dimensionless;
   --  Compute the Euclidean norm of a dimensionless vector after scaling by its largest component. The scaling avoids
   --  overflow and underflow in the sum of squares. Success is False, and the returned norm is zero, when an input is
   --  non-finite or the calculation cannot be completed safely.

   function Safe_Norm (Value : Position_Offset; Success : out Boolean) return Length;
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
       and then Is_Finite (Maximum_Lambda)
       and then
         (for all Axis in Axis_Name =>
            Is_Finite (Chord_Direction (Axis))
            and then Is_Finite (Start_Tangent (Axis))
            and then Is_Finite (Finish_Tangent (Axis)));
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

   function Stereographic (Y : Chart_Vector) return Raw_Vector_4;
   --  Map a three-dimensional chart point onto the unit sphere in the four-dimensional local tangent frame. This
   --  rational map guarantees a unit tangent for every finite chart point.

   function Stereographic_Jacobian (Y : Chart_Vector) return Raw_Stereo_Jacobian;
   --  Return the analytic derivative of Stereographic with respect to the three chart coordinates.

   function Tangent_At
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor : Dimensionless; U : Dimensionless)
      return Position_Scale
   with
     Pre =>
       U >= 0.0
       and then U <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor;
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

   function Unwarp_Parameter (V, Warp_Factor : Dimensionless) return Dimensionless
   with
     Pre  =>
       V >= 0.0
       and then V <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor,
     Post => Unwarp_Parameter'Result >= 0.0 and then Unwarp_Parameter'Result <= 1.0;
   --  Invert Warp_Parameter, mapping chart coordinate V back to normalized physical distance U.

   function Base_Tangent_Range_Majorants
     (Curve         : Stereographic_Curve;
      Start_V       : Dimensionless;
      End_V         : Dimensionless;
      Highest_Order : Majorant_Order := Maximum_Majorant_Order) return Axis_Majorants;
   --  Bound the machine-axis tangent and its derivatives with respect to unwarped chart coordinate V on the supplied
   --  interval. The calculation propagates Bernstein chart bounds through inverse stereographic projection.

   function Tangent_Range_Majorants
     (Curve          : Stereographic_Curve;
      Start_U        : Dimensionless;
      End_U          : Dimensionless;
      Highest_Order  : Majorant_Order := Maximum_Majorant_Order;
      Base_Majorants : access constant Axis_Majorants := null) return Axis_Majorants;
   --  Convert base chart-coordinate majorants into derivatives with respect to normalized physical distance U by
   --  composing them with the distance warp. A caller may supply reusable base majorants for the same interval.

   procedure Closure_Residual
     (Frame, Closure_Frame : Frame_Vector_Array;
      Start_Jets           : Chart_Jet_Array;
      Finish_Jets          : Chart_Jet_Array;
      Chord_Direction      : Position_Scale;
      Lambda               : Dimensionless;
      C0                   : Raw_Vector_3;
      Warp_Factor          : Dimensionless;
      Residual             : out Raw_Vector_4;
      Jacobian_C           : out Raw_Matrix_4_3;
      D_Residual_D_Lambda  : out Raw_Vector_4;
      Panel_Count          : Positive := 8)
   with
     Pre =>
       Lambda >= 1.0
       and then Is_Finite (Lambda)
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then (for all Component in Chart_Component_Index => Is_Finite (C0 (Component)))
       and then
         (for all Order in Start_Jets'Range =>
            (for all Component in Chart_Component_Index =>
               Is_Finite (Start_Jets (Order) (Component)) and then Is_Finite (Finish_Jets (Order) (Component))));
   --  Evaluate the four-component position-closure residual and its derivatives with respect to the three closure
   --  coefficients and Lambda. Panel_Count controls composite quadrature resolution for the nonlinear solve.

   function Solve_Closure_Fixed
     (Frame                     : Frame_Vector_Array;
      Start_Jets, Finish_Jets   : Chart_Jet_Array;
      Chord_Direction           : Position_Scale;
      Maximum_Lambda, Tolerance : Dimensionless;
      Warp_Factor               : Dimensionless;
      Initial                   : Closure_Solution;
      Panel_Count               : Positive := 1) return Closure_Solution
   with
     Pre =>
       Maximum_Lambda >= 1.0
       and then Is_Finite (Maximum_Lambda)
       and then Tolerance > 0.0
       and then Is_Finite (Tolerance)
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then (not Initial.Success or else Initial.Lambda <= Maximum_Lambda);
   --  Solve the fixed-frame closure equations within the length-ratio and residual limits. Initial supplies the
   --  starting point, while Panel_Count selects the quadrature resolution used for each residual evaluation.

   function Projected_Bound_On_Parameter_Range
     (Curve        : Stereographic_Curve;
      Start_U      : Dimensionless;
      End_U        : Dimensionless;
      Coefficients : Projection_Coefficients)
      return Curvature;
   --  Certify an upper bound on the absolute projected realtime tangent over a normalized-distance interval, including
   --  ideal-to-cache error and the continuous finish correction.

   function Up (Value : Dimensionless) return Dimensionless;
   --  Move a finite value far enough upward to conservatively cover one rounded arithmetic operation.

   --  A degree-seven Hermite chart matches the supplied tangent derivatives through order three. The
   --  endpoint-invisible degree-eight mode
   --
   --     256 * V**4 * (1 - V)**4
   --
   --  then closes the position without changing either endpoint jet. V is a monotone Möbius map of normalized
   --  distance, so asymmetric blends use the complete chart interval without increasing its degree.

   function Ideal_Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position;
   --  Construction-only high-accuracy numerical evaluation of the ideal tangent integral. Realtime code uses
   --  Point_At_Parameter instead.

   procedure Certified_Ideal_Point_At_Parameter
     (Curve     : Stereographic_Curve;
      Parameter : Curve_Parameter;
      Point     : out Position;
      Error     : out Length;
      Success   : out Boolean);
   --  Return the realtime point with a certified radius containing the ideal point. This is intentionally the same
   --  centre used during execution; no second numerical integral needs to be trusted by a caller.

   function Bounds_On_Parameter_Range
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Unit_Speed_Axial_Derivative_Bounds
   with Pre => Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0;
   --  Construction-only derivative bounds for the retained evaluator on the normalized distance interval.

   function Measure_Evaluator_Endpoint_Jet_Error
     (Evaluator            : Stereographic_Curve_Evaluator;
      Requested_Start_Jet  : Endpoint_Tangent_Jet;
      Requested_Finish_Jet : Endpoint_Tangent_Jet;
      Error_Bounds         : out Endpoint_Jet_Error_Bounds) return Boolean;
   --  Outward-enclose retained endpoint tangent-jet errors through normalized derivative order three. False reports
   --  that at least one finite enclosure was unavailable; Error_Bounds is then saturated.

   function Pole_Taylor_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants;
   --  Bound the retained tangent and its first four derivatives directly from the pole/residue evaluator. Each of
   --  64 cells uses a second-order Taylor enclosure with two extra derivative orders. This is the robust fallback when
   --  the retained Bernstein certificate is unavailable, and the preferred path for a point-sized parameter interval
   --  where partial-fraction cancellation is sharper.

   function Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Success : out Boolean) return Axis_Majorants
   with Pre => Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0;
   --  Reconstruct factorial-scaled retained-tangent derivatives from the stored common-denominator Bernstein form.
   --  Exact binomial ratios and outward-rounded interval operations keep coefficient conversions enclosing, while
   --  bounded De Casteljau subdivision tightens the range hull. Success is False when the retained certificate cannot
   --  prove a finite bound, allowing the caller to use Pole_Taylor_Tangent_Range_Majorants instead.
   --
   --  This direct path is exposed privately so child-package tests can prove that a fallback did not hide a failure.

end Prunt.Motion_Planner.Stereographic_Curves;
