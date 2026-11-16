# dimension_check

A Libadalang program that checks the physical dimensions of Ada code the way GNAT's `Dimension_System` and
`Dimension` aspects do, but from `Annotate` aspects, so the code itself needs no GNAT dimension extensions and builds
with any Ada compiler (and with GNATprove, which handles GNAT's dimension aspects badly).

## Annotations

The root floating point type of a dimension system names its base dimensions, in order:

```ada
type Dimensioned_Float is new Long_Float
with Annotate => (Dimension_Check, Dimension_System, "Millimeter", "Second", "Celsius", "Amp", "Gram");
```

Each dimensioned subtype gives the exponent of each base dimension it uses. Omitted dimensions are zero, and a
fractional exponent is written as a string:

```ada
subtype Length is Dimensioned_Float with Annotate => (Dimension_Check, Dimension, "Millimeter", 1);
subtype Velocity is Dimensioned_Float with Annotate => (Dimension_Check, Dimension, "Millimeter", 1, "Second", -1);
subtype Root_Length is Dimensioned_Float with Annotate => (Dimension_Check, Dimension, "Millimeter", "1/2");
subtype Dimensionless is Dimensioned_Float with Annotate => (Dimension_Check, Dimension);
```

A subtype without an annotation has the dimensions of its parent subtype. Unit constants are best declared with a
conversion, `mm : constant Length := Length (1.0);`: a conversion from a literal takes the dimensions of its target,
while `mm : constant Length := 1.0;` gets the literal warning described below. The pragma form
`pragma Annotate (Dimension_Check, Dimension, "Second", 1, Entity => Time);` works too. Other compilers ignore these
annotations, as GNAT ignores annotations for other tools.

`tools/migrate_dimensions.py` rewrites GNAT's aspects into these annotations; see [Migrating](#migrating).

## Exponents

`X ** 2` with a static integer exponent multiplies the dimensions of `X` by 2, as in Ada and GNAT.

GNAT also reads `X ** (1 / 2)` as a square root for a dimensioned operand, where Ada reads it as `X ** 0`. Here a root
is written with a real exponent, `X ** 0.5` or `X ** (1.0 / 3.0)`, which calls the `"**"` of an instance of
`Ada.Numerics.Generic_Elementary_Functions`. Make one visible where it is used, for example next to the root type:

```ada
package Dimensioned_Float_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);
function "**" (Left, Right : Dimensioned_Float'Base) return Dimensioned_Float'Base renames Dimensioned_Float_Math."**";
```

The exponent must be static when the base has dimensions: literals, named numbers and constants with static values,
combined with `+`, `-`, `*`, `/` and integer `**`. It is evaluated exactly, so `(1.0 / 3.0)` is one third, not the
nearest `Long_Float`. An integer division such as `X ** (1 / 2)` is reported as an error, since Ada evaluates it to an
integer.

A function of your own can be given the same rules:

```ada
function Power (Base, Exponent : Dimensioned_Float) return Dimensioned_Float
with Annotate => (Dimension_Check, Power);        --  Base ** Exponent, the exponent static

function Cbrt (X : Dimensioned_Float) return Dimensioned_Float
with Annotate => (Dimension_Check, Root, 3);      --  the dimensions of X divided by 3
```

Functions of any instance of `Ada.Numerics.Generic_Elementary_Functions` follow GNAT: `Sqrt` halves the dimensions of
its argument, `"**"` follows the rule above, and every other function takes dimensionless arguments.

## What is checked

These are GNAT's rules, worked out from `sem_dim.adb` and checked against GNAT 16.2 by the tests:

- `+`, `-`, assignments, return values, aggregate components, actual parameters (also of user-defined operators
  and of calls through access values) and actuals of generic formal objects must have the dimensions of the other
  operand, the target, the result subtype, the component subtype or the formal subtype.
- Comparisons need equal dimensions, except that a numeric literal takes the dimensions of the other operand. So does
  a literal initial value of an object or component, a literal default, a literal range bound and a literal actual of
  a generic formal object. A nonzero literal is then reported as a warning
  (`numeric literal 5.0 is assumed to have dimension [Millimeter]`), which `pragma Warnings (Off, "assumed to be")`
  turns off as it does in GNAT. A negated literal is not a literal.
- In an object declaration or a generic actual, a named number or a conditional expression of literals and named
  numbers takes the dimensions of the object, as in GNAT.
- `*` and `/` combine dimensions, `**` multiplies them, and unary `-`, `+` and `abs` keep them.
- A conversion keeps dimensions: `Length (X)` needs `X` to be a length, unless `X` is of a type outside the system
  (`Length (Long_Float (T))` is the escape hatch). A conversion to the root type, `Dimensioned_Float (X)`, keeps the
  dimensions of `X`.
- A constant of the root type, or of a subtype without dimensions of its own, takes the dimensions of its value:
  `Half_Width : constant Dimensioned_Float := Width / 2.0;` is a length.
- A formal of the root type accepts any dimensions; inside the subprogram it is dimensionless.
- Attributes such as `Length'Max (A, B)`, `Length'Small` and `X'Old` have the dimensions of their prefix.
- The branches of `if` and `case` expressions need equal dimensions.
- Range constraints in subtype, object and component declarations need the dimensions of their subtype.
- Annotations are checked: unknown or repeated dimensions, a subtype that contradicts the dimensions of its parent,
  and annotations on declarations that cannot carry them are errors.

By default dimension_check also makes checks that GNAT does not. `--gnat-compatible` turns them off:

- An expression function's expression must have the dimensions of its result subtype (GNAT does not check it).
- A default expression of a parameter or discriminant must have the dimensions of its subtype (GNAT only warns about
  literals there).
- The operand of a qualified expression `Length'(X)` must be a length (GNAT takes the subtype's dimensions without
  looking at the operand).
- The value arguments of `Length'Max (A, B)`, `Length'Floor (X)` and similar attributes must be lengths.
- The values and bounds of a membership test need the dimensions of the tested value.
- The components of a delta aggregate such as `[P with delta E_Axis => 0.0 * s]` need the dimensions of their
  subtypes (GNAT does not check them).
- An `if` or `case` expression that is the initial value of an object or a generic actual needs the dimensions of
  the object. GNAT gives it the object's subtype and so accepts any dimensions, as in
  `Feedrate : constant Length := (if Homing then Homing_Velocity else 0.0 * mm);`.
- A subtype annotated as dimensionless is not the root type: a dimensionless formal rejects a length, a dimensionless
  constant cannot take the dimensions of its value, and the checks above apply to it. GNAT treats both as having no
  dimensions, so a `Dimensionless` formal accepts any argument.

dimension_check is more permissive than GNAT in two places where GNAT rejects correct code: a `declare` expression
keeps the dimensions of its result (GNAT makes it dimensionless), and a raise expression matches any dimensions.
GNAT 16.2 stops with an internal error on a dimension mismatch in a call through an access-to-subprogram value;
dimension_check reports the mismatch.

## Running it

Build it in this directory with `alr build`, then run it in the Alire environment of the crate that owns the
project file, so that its dependencies resolve. Prunt's pre-build step (`prebuild.adb`) builds it, caching the executable like config_codegen's in
`~/.cache/prunt/dimension_check` (`PRUNT_DIMENSION_CHECK_CACHE` overrides the directory, or disables the cache when set
to "off"), and runs it over `prunt.gpr` whenever Prunt's sources change, so `alr build` fails on a dimension error.
The simulator's pre-build step, which Alire runs after Prunt's, checks `prunt_simulator.gpr` the same way whenever its
sources or Prunt's change. `run_tests.sh` checks `tests.gpr` after building the tests. To run it by hand from the
repository root:

```bash
alr exec -- dimension_check/bin/dimension_check -P prunt.gpr -j 8
```
With no file arguments every unit of the root project is checked; otherwise only the files given. Output lines look like GNAT's:

```
src/foo.adb:12:9: error: dimensions mismatch in assignment: left-hand side has dimension [Millimeter], right-hand side has dimension [Second]
```

The exit status is a failure if there is an error, or a warning with `-W`. `-v` also lists constructs that could not
be checked because Libadalang could not resolve them; without it only their number is printed. `-k` continues past
units that cannot be found, as for sources of a test framework that is not installed. The other options are
Libadalang's (`--help`). Without `-k`, Libadalang's driver exits the program from inside a job
when a source is missing; with several jobs the other jobs can then crash on finalized objects or hang. A missing
source usually means the project is configured for another build: Prunt's tests need the mocked sources that
building the tests selects. Run with `-j1` to get just the error.

Constructs Libadalang cannot resolve are skipped, never reported as errors. Libadalang sometimes picks the wrong
overload of an operator whose operand is a delta aggregate, such as `abs [Offset with delta E_Axis => 0.0 * mm]`;
such calls are skipped too, and the delta aggregate itself is still checked against the type of its base.

## Migrating

From the repository root:

```bash
dimension_check/tools/migrate_dimensions.py $(git ls-files '*.ads' '*.adb')
```

rewrites `Dimension_System` and `Dimension` aspects into annotations and `** (N / M)` exponents into real ones
(`** 0.5`, `** (4.0 / 3.0)`), keeping every line number, and lists each rewrite. It rewrites exponents by text: check
the list for integer divisions that were meant as such. It does not add the real `"**"` that the new exponents need. Where a unit also has a `use` clause for an
instance of `Generic_Elementary_Functions` on the same type, both `"**"` functions are visible and a real exponent is
ambiguous; qualify or rename the functions the unit needs from the instance instead of using it.

The rewritten exponents compute the same values with Prunt's GNAT fork, which evaluates `X ** (1 / 2)` with the
`"**"` of `Generic_Elementary_Functions` at the precision of the operand's type (`Long_Float` for a `Long_Float`
operand), with the exponent rounded to that type, as the migrated `X ** 0.5` does. Upstream GNAT instead evaluates it
in `Long_Long_Float` (80-bit on x86-64), so with upstream GNAT results can differ in the last bit.

## Validation

On 2026-11-15 the checker was run over a migrated copy of all of Prunt (`src`, generated sources, tests and the
simulator) with no findings in either mode, and the units of `prunt.gpr` in that copy passed GNAT's semantic checks
(`-gnatc`) once `prunt-input_shapers-basic_shapers.adb` qualified its calls of `Exp` instead of using its instance. Each of 1,657
mutations of Prunt's sources (a unit changed, a subtype swapped, an exponent changed) was applied to both the
original and the migrated copy, and GNAT's verdict on the original compared with dimension_check `--gnat-compatible`
on the migrated copy: they agree on all but 9 of the 1,545 mutations that give no other compile error. Seven of those
are the same error located on another line of a compound statement, and two are inside the
`abs [... with delta ...]` call that Libadalang misresolves. The comparison found the two GNAT gaps that are now the
delta aggregate and conditional initial value checks.

## Tests

In this directory, after `alr build` (`alr exec` puts GNAT on the path for the parity tests):

```bash
alr exec -- tests/run_tests.py -v
```

`tests/cases` holds sources that mark the expected findings with `--! error:` comments and are checked in both
modes. `tests/gnat` holds sources written with GNAT's aspects: GNAT compiles them, and the statements where it reports
dimension errors and literal warnings must be exactly those where dimension_check `--gnat-compatible` reports them on
copies rewritten by the migration script. Both compare by statement rather than by line, so the sources can be
formatted with `format_ada.py` like the rest of the repository.

## Licence

dimension_check implements GNAT's dimensionality checking and is derived from GNAT, in particular from
`gcc/ada/sem_dim.adb` (Copyright (C) 2011-2025, Free Software Foundation, Inc.), whose rules it reproduces. Like GNAT,
it is free software under the GNU General Public License, version 3 or (at your option) any later version; see
[COPYING](COPYING). This covers everything in this directory. The rest of Prunt stays under the MIT licence of the
top-level `COPYING`: Prunt's build only runs the checker, and none of it is linked into Prunt.
