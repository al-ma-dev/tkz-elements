## [2026/10/11 — version 6.00c]

   ### Added
   - **conic**
     - `EL_from_equation(t)` or `EL_from_equation(A, B, C, D, E, F)`: ellipse
       defined by its equation Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0. Like
       `EL_five_points`, it returns the arguments of `conic(...)`:
       `CO.EL = conic(EL_from_equation(1, 0, 4, 0, 0, -4))`.
     - `EL_conjugate(c, m, n)`: ellipse with center `c` and conjugate
       semi-diameters `[c, m]` and `[c, n]` (Rytz construction). It returns the arguments of `conic(...)`.
   - **line**
     - `affinity(L, k, ...)` accepts objects as well as points: a line, a triangle, a quadrilateral and a parallelogram keep their class; a square and a rectangle become parallelograms; a circle and an ellipse become an ellipse (a circle if the axes are equal). A ratio 0 with an object, or an unsupported object (hyperbola, parabola, …), raises an explicit error.
   - **occs**
     - `point(u, v)`: the point with coordinates (u, v) in the system (inverse of `coordinates`).
   - **triangle**
     - `angle(i)`: the angle object (class `angle`) at the vertex i = 1, 2, 3
       (numbering of `get(i)`), or at a vertex given as a point.
   - **square**, **rectangle**, **quadrilateral**, **parallelogram**
     - `get(i)` returns the i-th vertex (1 to 4), as for `line`, `circle` and
       `triangle`; `get()` still returns the four vertices.

   ### Fixed
   - **main**
     - `tkz_elements_tkz.lua` was loaded twice. The reserved tables are now listed once, in `tkz.tables`, used by `init_elements()` and by the  documentation.
   - **tkz-elements.sty**
     - Transfer macros (`\tkzGetNodes`, `\tkzUseLua`, `\tkzPathCount`, …) are now defined whatever the loading order: previously they were undefined when  `tkz-elements` was loaded before `tkz-euclide`, or with plain TikZ.
       An old `tkz-euclide` (< 2024/04/19) loaded first still keeps its own macros.
     - The version of `tkz-euclide` is also checked when it is loaded after `tkz-elements` (at `\begin{document}`).
     - Warning for an old `tkz-euclide`: the message gave the version number
       5.10c with the date 2024/04/19 (5.10c is dated 2024/04/27); it now gives
       the minimum date and recommends 5.13c.
     - `\pgfutil@ifnextchar` replaced by the kernel `\@ifnextchar`.
     - Without LuaLaTeX, loading now stops after a single error (`\endinput`)
       instead of a cascade of errors; removed the doubled period in the message.
   - **point**
     - `rotation`, `homothety` and `symmetry` now rebuild an object of the same
       class: a rotated `rectangle`, `quadrilateral` or `parallelogram` was
       returned as a `square`, and the symmetric of any object other than a
       point, line or circle was returned as a `triangle`. With several
       arguments, each one may now be any supported object (not only points).
       An unsupported object (e.g. a conic) raises an explicit error.
     - `__eq` now uses the same criterion as `identity` (Euclidean distance
       less than `tkz.epsilon`), as announced in 5.03c; comparing a point with
       a non-point returns `false`.
     - Errors (invalid point, division by zero, normalization of a null vector,
       `orthogonal(d)` of a null vector, conversion of a non-numeric value) now
       stop the Lua chunk with `error()` and report the line of the user code,
       instead of `tex.error()` followed by `nil` or `nan` values.
     - `math.round` (not standard Lua, provided by lualibs) replaced by a local
       function.
     - `tostring`: no more `-0.00`; integer imaginary parts are written without
       decimals (`1+2i` instead of `1+2.00i`).
     - New method `z:tonumber()`. Lua has no `__tonumber` metamethod, so
       `tonumber(z)` always returned `nil`; `point.__tonumber` is kept as an alias.
     - Internal `class()`: inheritance from a base class was lost (the second
       `setmetatable` overwrote `__index`).
   - **maths**
     - `tkz.angle_between_vectors` now returns an angle in (-π, π], as
       documented (it returned e.g. 190° instead of -170°): it uses
       `math.atan(y, x)`. A null vector raises an error instead of returning `nan`.
     - `get_angle_`: `error()` instead of `tex.error()` (the computation went on
       and produced a second, misleading division-by-zero error).
     - `tkz_display_` (used by `tkz.display`) no longer depends on `math.round`
       (lualibs) and no longer prints `-0.00`. `tostring(point)` now uses the
       same formatter: a single display function for numbers and complex numbers.
     - `tkz.round` / `tkz_round_`: symmetric rounding (`-2.5` gives `-3`), and an
       integral result is returned as an integer (`3`, not `3.0`).
   - **points**
     - `barycenter_`: `error()` when the sum of the weights is (close to) zero,
       instead of `tex.error()` followed by a division by zero.
     - `random_point_(lower, upper)`: real coordinates; non-integer bounds were
       rejected by `math.random` and only integer coordinates were produced.
   - **tkz**
     - `tkz.GetNodes()`: a point with undefined coordinates (NaN) now produces an
       explicit warning naming the point and is placed at (0,0), instead of a
       `Missing number` / `Illegal unit` TeX error.
       `tkz.register_coord_warning` accepts an optional `kind` argument.
     - `tkz.angle_between_vectors`: the error for a null vector now reports the
       line of the user code.

   - **line**
     - `harmonic("int", pt)` and `harmonic("ext", pt)` returned `nil` (the short
       mode names were converted in the wrong branch).
     - The constructor checks that its two arguments are points.
     - Errors stop with a clear message at the line of the user code, instead of
       `tex.error` followed by `nil` or `nan`: `report` (degenerate line),
       `harmonic` (invalid mode), `s_a` and `a_s` (no solution, degenerate
       triangle), `abscissa_`.

   - **circle**
     - `common_tangent` failed for two circles of equal radii ("the sum of the
       weights is zero"), a regression of 5.10c: the external center of
       similitude, at infinity, was computed before the equal radii case.
     - `external_similitude` and `similitude(C, "external")` with equal radii:
       explicit error (the center of similitude is at infinity).
     - `common_tangent(C, mode)`: the mode `"both"` (documented but not
       implemented) returns all the common tangents, externals first: 4 for
       disjoint circles, 3 for externally tangent circles, 2 for secant circles,
       1 for internally tangent circles. When no tangent of the requested family
       exists (one circle inside the other, internal tangents of secant circles,
       …), an explicit error is raised instead of returning lines with `nil`
       points; an unknown mode raises an error. For internally tangent circles,
       the external mode returns the tangent at the contact point, as documented.
     - Errors now stop with a clear message at the line of the user code, instead
       of `tex.error` followed by `nil`: `orthogonal_from` (point inside the
       circle; the message was lost), `orthogonal_through` (collinear points;
       the case of inverse points returned `nil` silently), `point(t, P)` (P not on
       the circle), `inversion` at the center, `midcircle`, `CLL` (choice not in
       1..4), `position` (unsupported object).
     - `position` was defined twice.
     - `orthogonal_through(pta, ptb)`: when the two points are inverse with
       respect to the circle, an error was raised although the documentation
       announced a solution. Every circle through two inverse points is
       orthogonal: the circle with diameter [pta, ptb] is now returned, or the
       circle through a third point given as optional argument
       (`orthogonal_through(pta, ptb, X)`). Two equal points raise a clear
       error. The old call `orthogonal_through(pta, ptb, EPS)` still works.
     - Removed the internal function `orthogonal_circle_through_`, never used,
       whose case "point on the circle" gave a tangent circle instead of an
       orthogonal one.

   - **matrix**
     - `inverse()` (and `M^-n`) always returned the identity matrix:
       `inv_matrix` ignored the result of `gauss_jordan()`, which works on a copy.
       `is_orthogonal()`, which relies on the inverse, is fixed as well.

   - **line**
     - `translation` and `reflection` now work like the transformations of
       `point`: each argument may be a point or any supported object (line,
       circle, triangle, square, rectangle, quadrilateral, parallelogram), and
       an object of the same class is returned. Previously, several objects
       other than points raised an error, and squares, rectangles,
       quadrilaterals and parallelograms were not supported. An unsupported
       object (e.g. a conic) raises an explicit error.
     - `projection` and `projection_ll` accept only points (an object would
       not keep its class): any other argument now raises an explicit error
       at the line of the user code, instead of a Lua error inside the library.
   - **circle**
     - `inversion` with several arguments now accepts lines and circles as
       well as points (previously only points); any other object raises an
       explicit error instead of being treated as a circle.

   - **angle**
     - New attribute `type` (`"angle"`), as for the other classes. The
       constructor checks its arguments (three points, distinct from the vertex)
       and reports errors at the line of the user code.
   - **occs**
     - The constructor checks its arguments: a degenerate line gave `nan` axes
       without error, a point instead of a line gave a Lua error inside the
       library. `coordinates` checks its argument and no longer uses `tex.error`.
   - **path**
     - `get()` without argument returns all the points (it raised an error);
       `get(i)` is unchanged.
     - `p1 + p2`: the common point is now recognized whatever its writing
       (`(1,0)` and `(1.00000,0.00000)`), and is no longer duplicated.
     - `concat_rawpair`: spaces around the coordinates are removed; a path of
       pairs (built with `add_pair`) raises an explicit error.
     - `get_point`, `get_number_path`, `iter`: errors report the line of the
       user code; the points are read by `utils.parse_point`.
     - `add_point` and `add_pair_to_path` store the coordinates with 10
       decimals by default (previously 5). The points returned in paths
       (`CPP`, `CCP`, ..., read back by `tkz.nodes_from_paths`) lost their
       precision: a tangency was no longer detected by `intersection`
       (tolerance `tkz.epsilon` = 1e-10) and the result was `nil`.
   - **triangle**
     - Errors stop with a clear message at the line of the user code, instead of
       `tex.error` followed by `nil`: constructor (degenerate triangle),
       `kimberling` (number not available: the message itself raised a Lua
       error), `parameter`, `excenter`, `poncelet_point`, `orthopole`,
       `apollonius_points`, `apollonius_circle`, `euler_line` (equilateral
       triangle), `thebault_circles`, `kiepert_parabola`, `macbeath_inellipse`,
       `orthic_inellipse`, `path`, `LLL`/`c_lll`, `cevian_point`,
       `ellipse_inscribed`.
     - Methods taking a vertex (`get_angle`, `symmedian_line`, …): a point with the
       same coordinates as a vertex is now accepted (previously only the stored
       vertex object); an invalid argument raises a clear error instead of
       `tex.error` followed by a Lua error.
     - `three_tangent_circles` failed (`circle:new` received `nil`): it still
       called `projection(pt)`, renamed `projections(pt)` in 5.04c, while
       `projection()` now applies to the vertices. It now uses `projections`.
     - `kiepert_parabola` assigned the global variables `L` and `F`: after a
       call, the table `L` of the lines was replaced by the Euler line.
       They are now local.
   - **utils**
     - Errors (`parse_point`, `format_number`, `format_coord`, `format_point`,
       `almost_equal`, `table_len_contiguous`) now stop the Lua chunk with
       `error()` and a clear message, instead of `tex.error()` followed by `nil`
       values. An invalid string given to `path:add_point` is reported at the
       line of the user code.
     - `format_number`, `format_coord`, `format_point`: the number of decimals
       must be a non-negative integer (a value like 2.5 caused a Lua error);
       no more `-0.00`.

   - **conic**
     - `intersection(L, P)` and `intersection(L, H)` (line and parabola or
       hyperbola) crashed when there was no common point; they now return
       nothing, as for an ellipse. A line parallel to the axis of a parabola, or
       to an asymptote of a hyperbola, now gives its single intersection point
       (instead of `nan` values), and an asymptote gives none.
     - `position(pt)`: a point between the branches of a hyperbola crashed; it
       now gives `"OUT"`. The center of an ellipse was reported `"OUT"`; it is
       now `"IN"`.
     - `tangent_from(pt)` from a point inside the conic raises a clear error
       instead of an arithmetic error.
     - The constructor checks its arguments: the focus must be a point, the
       directrix a line, the eccentricity a positive number, and the focus must
       not lie on the directrix.
     - `orthoptic()` raises an error for a hyperbola with e >= sqrt(2) (it
       returned `nil`).
     - `antipode` (parabola), `asymptotes` (not a hyperbola), `path`,
       `PA_focus`, `EL_five_points` and `ellipse_axes_angle`: `error()` instead
       of `tex.error()`; the message of `EL_five_points` gave a wrong function
       name.
     - `search_ellipse` no longer creates the global variables `Names` and
       `points`.
   - **intersection**
     - The option `{eps = ...}` (the documented form) was ignored: only `EPS`
       was read. Both are now accepted.
     - An unsupported pair of objects (circle and conic, two conics, a line and
       a triangle, ...) raises a clear error, instead of a Lua error or of no
       result without any message. An invalid third argument raises an error
       (it used `tex.error` and the computation went on).
     - Line and ellipse: a degenerate line (two identical points) gave `nan`
       points; it now gives no intersection, as with a circle.
     - Relative tolerances: the parallelism test of `intersection_ll_` and the
       tangency test of `intersection_le` no longer depend on the lengths of
       the segments that define the lines.
     - The tolerance is also passed to the line/parabola and line/hyperbola
       intersections.
     - Tangent circles: `intersection_cc_` detected the tangency on the
       half-chord h = sqrt(h2), which amplifies rounding errors (h2 ~ 1e-15
       gives h ~ 3e-8); the point of contact was wrong by about 1e-7. The
       tangency is now tested on the distances, as in `intersection_lc_`, and
       the exact point of contact is returned. This fixes
       `triangle:feuerbach_point()`, `triangle:feuerbach()`,
       `intersection(C1, C2)` for tangent circles, `circle:tangent_from(pt)`
       with `pt` on the circle and the common tangent of two tangent circles;
       the center given by `triangle:feuerbach_apollonius_k181()` was wrong by
       about 5e-6.
   - **matrix**
     - `matrix.htm(phi, tx, ty, sx, sy)`: the scale factors were applied to the
       diagonal only, so a rotation combined with a scaling was not a
       similarity; the matrix is now T·R·S (scaling, then rotation, then
       translation). Results are unchanged when `sx = sy = 1`.
     - `htm_apply` crashed with a circle, a square, a rectangle, a
       quadrilateral or a parallelogram, and accepted only points when several
       arguments were given. Each argument may now be a point or an object; a
       similarity keeps the class of the objects, another affine map changes a
       circle or an ellipse into an ellipse and a square or a rectangle into a
       parallelogram (as `line:affinity`). An unsupported object, a matrix that
       is not 3x3 with last row (0, 0, 1), or a degenerate map with an object
       raise an explicit error.
     - `is_orthogonal` returned `false` for rotation matrices (exact comparison
       of floating numbers): it now tests AᵀA = I with the tolerance
       `tkz.epsilon`. `is_diagonal` and `==` also use this tolerance.
     - `diagonalize`: a matrix other than 2x2 gave a wrong result without
       message; a null entry at (1,2) gave `inf`/`nan`; the matrix P is now also
       returned for complex eigenvalues; a double eigenvalue raises an error
       (not diagonalizable).
     - The determinant is computed on first access to `det` (it was computed
       for every matrix, by cofactor expansion): explicit formulas up to 3x3,
       Gaussian elimination beyond. An integer matrix up to 3x3 now has an
       integer determinant (`-10` instead of `-10.0`).
     - `inverse`, `inv_matrix` and `A^-1` of a singular matrix: `error()`
       instead of a message written in the document and a `nil` result.
     - Clear errors instead of `nil` or Lua errors: sum, difference or product
       of matrices of incompatible sizes, rows of different lengths, non-integer
       exponent, power of a non-square matrix, `matrix.square` with a wrong
       number of values, `adjugate` beyond 3x3, `homogenization` of a matrix
       that is not 2x1, `augment_right` and `submatrix` with wrong arguments.
     - `print_array` used a global variable `j`.
   - **triangle**
     - `in_out(pt, true)` raised a Lua error: the second argument was passed as
       the tolerance. `true` now excludes the boundary.
   - **vector**
     - The constructor checks its arguments: `vector(1, 2)` gave a Lua error and
       `vector(A)` an error about matrices; both now give a clear error.
     - `normalize` of a null vector: `error()` instead of `tex.error()` (the
       computation went on: `orthogonal(side, length)` of a null vector returned
       a vector in an arbitrary direction).
     - `V + P`, `V - P`, `^` and `..` with a non-vector, and `__mul` with
       invalid operands: clear errors reported at the line of the user code.
   - **conic**, **matrix**: the constructor errors are reported at the line of
     the user code (the level of `error()` did not take into account the tail
     call in `__call`).
   - **square**, **rectangle**, **quadrilateral**, **parallelogram**
     - The constructors check that four points are given (a wrong argument gave
       an error about lines or points).
     - `parallelogram`: four points whose diagonals do not have the same
       midpoint raise an error (`tex.error` was followed by the creation of the
       object).
     - `quadrilateral:area()`: the area of a non-convex quadrilateral is now
       computed (it raised an error and returned `nil`); only a crossed
       quadrilateral raises an error.
     - `quadrilateral:is_cyclic()` tests whether the four vertices are
       concyclic and returns `false` for a non-convex quadrilateral (it raised
       an error and returned `nil`).
     - `quadrilateral:poncelet_point()` accepts any quadrilateral (the Poncelet
       point is defined for four points); an error is raised only when three
       vertices are collinear.
   - **regular_polygon**
     - The constructor checks its arguments: two points (center and first
       vertex, distinct) and an integer number of sides >= 3 (`nb < 3` used
       `tex.error` and returned `nil`; a missing or non-integer number gave a
       Lua error or a wrong polygon).
     - `get(i)` with an invalid index raises an error (`tex.error`, or nothing
       for `i <= 0`); `get()` now returns all the vertices, as for the other
       classes (the table is still available as the attribute `vertices`).
   - **list_point**
     - `new`, `add`, `extend` and `map` check that they receive points (a
       non-point gave a Lua error later, e.g. in `bbox()`).
     - `get()` without argument returns all the points, as for the other
       classes (it returned `nil`).
     - New attribute `type = "list_point"`, as for the other objects.
     - `to_tikz_path()` (and `point:to_tikz()`, moved to `point.lua`) write the
       coordinates with fixed decimals, as the other path outputs, instead of
       raw numbers such as `1e-17`.
   - **fct**, **pfct**
     - `pfct:scale()` and `pfct:map()` crashed (they called an undefined
       method `copy`).
     - An invalid expression raises an error at the line of the user code,
       when the object is created (`tex.error` was followed by a function equal
       to 0 for `fct`, to `nan` for `pfct`).
     - `fct:eval(x)` returns `nan` or an infinite value when f is not defined
       at x, as documented (it returned 0, so that `point(x)` gave a wrong
       point); `fct:point`, `tangent` and `half_tangents` raise an error when f
       is not defined at the point.
     - `pfct`: a coordinate that is not a number (e.g. `nil`) is treated as
       undefined (it gave a Lua error); `point(t)` raises an error when the
       curve is not defined at t and evaluates the curve once.
     - `pfct:file()` skips infinite values, writes fixed decimals and checks
       its arguments; `set_map` and `map` errors are reported at the line of
       the user code.
   - **tkz**
     - `tkz.poncelet_point` called a nonexistent function (`tex.xs`) when the
       construction failed; three collinear points gave an error about
       barycenters. `tkz.poncelet_point`, `quadrilateral:poncelet_point` and
       `triangle:poncelet_point` now share the same computation and raise a
       clear error.
     - `tkz.trisector` with an indirect angle: `error()` instead of
       `tex.error` followed by the word "stop" written in the document.
     - `tkz.solve_linear_system` with more equations than unknowns returned as
       many rows as equations; it now returns one row per unknown.
     - `tkz.solve`: a wrong number of parameters or a zero leading coefficient
       raises a clear error (`tex.error`, or an infinite value); `tkz.cramer`
       and `tkz.range`: clear errors.
   - **circle**
     - `CPP(a, b)`: when the line (ab) is tangent to the circle and the center
       is on the perpendicular bisector of [ab], the method raised an error
       (circle through three collinear points); the degenerate solution is now
       skipped and the valid circle is returned.
     - `midcircle(C)`: two circles with the same radius raised an obscure error
       ("barycenter: the sum of the weights is zero"): the external center of
       similitude is at infinity. The midcircle is then the perpendicular
       bisector of the centers, returned as a `line` (with, for two secant
       circles, the circle centered at the midpoint of the centers). Two
       concentric circles gave `nil`: the midcircle is the concentric circle of
       radius sqrt(r1 r2). Two identical circles raise a clear error.
   ### Deprecated
   - `get_points(obj)`: still available but not recommended; use `obj:get()`.
     A warning is written once in the terminal and the log. An unsupported
     object now raises an error instead of returning nothing.

   ### Removed
   - Undocumented methods, made redundant by the methods above, which accept
     several objects: `point:set_rotation`, `point:set_symmetry`,
     `point:rotation_pt`, `line:set_translation`, `line:translation_pt`,
     `line:translation_L`, `line:translation_T`, `line:translation_C`,
     `line:set_reflection`, `line:symmetry_axial_L`, `line:symmetry_axial_T`,
     `line:symmetry_axial_C`, `line:set_projection`, `line:set_projection_ll`,
     `line:set_affinity`, `circle:set_inversion`, `path:get_rawpair_list`,
     `circle:external_tangent`, `circle:internal_tangent` (replaced by
     `common_tangent`), `circle:are_circles_tangents`, `line:circle_swap`
     (use `circle("swap")`).
   - Undocumented matrix methods, now internal: `swap_rows`, `k_mul_row`,
     `add_k_mul_row`, `swap_rows_inplace`, `k_mul_row_inplace`,
     `add_k_mul_row_inplace`, `left_block`, `right_block`, `k_mul` (use `k * A`).

   ### Documentation
     - conic: hyperbola construction example, `LFq` (undefined) instead of
       `LFa` in `intersection(LC, LFa)` (Lua error "attempt to index a nil
       value", now "unsupported objects (line, nil)").
     - fct, pfct: the constructor sections said that the `math.` prefix was
       required (it must not be used); constructor example (`math.exp`,
       nonexistent `value`) corrected; `compile()` described as it is; `fct`:
       `tangent` and `half_tangents` added to the table; `pfct`: `eval`, `at`,
       `set_scale`, `scale`, `set_map`, `map`, `file`, `jobfile` documented.
     - list_point: the attributes `n` and `items` did not exist (replaced by
       `type`, `len()` and `get(i)`); `get()`; argument checks; new section on
       `as_path()`; the `unpack` example no longer replaces the table `LP`.
     - regular_polygon: the table `RP` (the doc used `P`); `apothem` is a length
       (the doc described the foot); attributes `nb`, `apothem_foot`,
       `perimeter`, `area` added; new section on `get(<i>)`; `name` described.
     - square, rectangle, parallelogram, quadrilateral: attribute tables
       corrected (`da` instead of `ad`, examples of `ac`, `side`,
       `apothem_foot`; side lengths of a quadrilateral described as
       "barycenter"); missing attributes of the square; the doc announced a
       shape test for squares and rectangles, which does not exist; the
       parallelogram constructor takes four points (the doc gave three);
       `is_convex`, `is_cyclic`, `poncelet_point` and the new section `area`;
       `rectangle:diagonal` builds a rectangle with sides parallel to the axes.
     - vector: table of methods reorganized (`dot`, `cross`, `angle_to` return
       numbers); new section on `get`, `add`, `scale`, `dot`, `cross`,
       `angle_to` and `rotate`, with an example.
     - matrix: `matrix.htm` (complete form with the scale factors),
       `htm_apply` (accepted objects and their class), `is_orthogonal`,
       `is_diagonal`, `diagonalize`, `inverse` (errors); the `is_orthogonal`
       example uses `transpose()` and `inverse()`; `get_htm_point` is listed
       as a function.
     - intersection: no intersection gives `nil` (the doc said `false`; the
       line-line example tested `x == false`); option `eps` (alias `EPS`);
       order of the points for a parabola or a hyperbola; single intersection
       point of a line parallel to the axis or to an asymptote.
     - triangle: new sections on `check_isosceles`, `parameter`,
       `brocard_midpoint`, `soddy_points`, `conway_center`, `the_apollonius_center`,
       `trisector`, `soddy_line`, `thebault_circles`, `the_apollonius_circle`,
       `morley`, `napoleon` (previously undocumented).
     - line: new section on `equidistant_with(L, <choice>)`.
     - circle: `common_tangent`, description of the results and of the errors.
     - triangle: new section on `angle(i)`; `get_angle`: accepted arguments
       corrected; example of `alpha_`, `beta_`, `gamma_` corrected.
     - angle: `type` and `norm` added to the table of attributes; interval of  `norm` corrected ([0, 2pi)).
     - occs: table of methods corrected (constructor name and reference); new
       section on `point(u, v)`.
     - utils: new sections on `utils.clamp`, `utils.sign` and `utils.w`;
       default tolerance of `almost_equal` corrected (`tkz.epsilon`).
     - New sections on `get(<i>)` for `square`, `rectangle`, `quadrilateral` and `parallelogram`.
     - New section on `EL_conjugate`; `line:affinity`: accepted objects and a
       new example (image of a circle).
     - `line:translation` and `line:reflection`: list of accepted objects updated; several objects can be given.
     - New section on `EL_from_equation`, with an example; added to the
       table of conic functions.
     - Method names aligned with the code: `common_tangent`, `random`,
       `orthoptic`, `affinity`, `lemoine_inellipse`, `mandart_inellipse`,
       `nagel_point`, `mittenpunkt_point`, `macbeath_point`, `to_tikz_path`,
       `regular_polygon:incircle`; signature of `feuerbach_apollonius(EPS)`
       and `feuerbach_apollonius_k181(EPS)`.
     - Added documentation of `medial_points()`, `orthic_points()`,
       `tangential_points()`; `EL_five_points` added to the conic functions table.
     - Index: merged duplicate groups (`regular`/`regular_polygon`,
       `math`/`conic`, `misc`/`tkz`, `package`/`tkz-elements`, `TikZ`/`tikz`).
     - README: corrected the location of the manual and its sources.
     - Point metamethods table: `tonumber(z.a)` replaced by `z.a:tonumber()`.
     - Removed the duplicated 5.04c entry in this changelog.


## [2026/09/30 — version 5.06c]

### Changed
- **triangle**
  - Changed the order of returned points for `reflections` and `projections`:
    (1 → pa, 2 → pb, 3 → pc).
  - Added methods returning directly the vertices of special triangles:
    - `orthic_points()`
    - `medial_points()`
    - `tangential_points()`
- **circle**
  - Modification of the `point` method.  Added an optional point parameter to specify the reference point on the circle.

### Documentation
  - Reorganized the method descriptions in the **point** class, especially for utility   methods and the `rotation` method.
  - Fixed an incorrect example of the `PPP` method in the **point** class.
  - Section 16.4.1 (Parabola construction): corrected `bisector of angle HMF` (instead   of `angle HFT`).
  - Section 7.3 (Use of Trilinear Coordinates): corrected `projection` → `projections`   in the code.
    Thanks to :SwiftWu for these remarks.
  - Added documentation for the `print` method.

## [2026/03/14 — version 5.05c]

### Added
- **triangle**
   - methods `isotomic`, `complement`, `anticomplement`
   - function triangle:ellipse_inscribed({brianchon/center/focus})
   - function triangle:cevian_point(p, q, eps)

- **conics**
   - function EL_five_points(z1,z2,z3,z4,z5)

### Documentation
- **conic**
   - documentation clarification about constructor usage and helper functions


## [2026/03/11 — version 5.04c]

   ### Added

   - **triangle**
     - Added new attributes: `ma`, `mb`, `mc`, `ha`, `hb`, `hc`, `ia`, `ib`, `ic`
       - `ma`, `mb`, `mc`: midpoints of the sides
       - `ha`, `hb`, `hc`: feet of the altitudes
       - `ia`, `ib`, `ic`: feet of the internal angle bisectors

     - Added method: `triangle:reflections(pt)`

   ### Changed

   - **triangle**
   - Renamed method: `projection(pt)` -> `projections(pt)`

   ### Notes
   - **triangle**
      - `projections(pt)` and `reflections(pt)` apply to an arbitrary point.
      - `projection()` and `reflection()` apply to the triangle vertices.



## [2026/03/04 — version 5.03c]

### Changed

- **path**
  - `add_point` now accepts either a `point` object or a string
    representing coordinates (e.g. `"(2,3)"`, `"(1e-5,0)"`).
  - String arguments are automatically parsed using
    `utils.parse_point`.
  - Improves API flexibility while preserving internal
    TikZ-formatted storage.

- **utils**
  - Clarified and documented support for scientific notation
    in `parse_point` (e.g. `1e-5`, `2E3`).
  - Improved documentation of `format_number`,
    `checknumber`, and `to_decimal_string`.
  - Reinforced consistent fixed-decimal formatting
    for TikZ output.

- **tkz-elements.sty**
  - Removed the `tkzelements` environment.

- **point**
  - Unified approximate equality and geometric identity.
  - `__eq` now uses the same Euclidean proximity criterion as `identity`.
  - Removed coordinate-wise comparison in favor of a rotation-invariant
    distance test.
  - Centralized comparison logic for improved numerical consistency.

- **triangle**
  - Added `radical_circle`: the circle orthogonal to the three excentral circles.
  - Added `the_apollonius_center` and `the_apollonius_circle`.
  - `triangle.feuerbach` is now defined as `triangle.euler_circle()`.

### Documentation

- Improved documentation of `\tkzDrawCoordinates`.
- Explained its direct mapping to TikZ `plot coordinates`.
- Clarified that a Lua `path` is a discrete computational object
  rendered as an explicit polygonal chain.
- Emphasized the design principle:

  *Lua computes — TikZ draws.*

- Clarified terminology:
  - **mediator** refers to the **perpendicular bisector** of a segment.


## [2026/02/20 — version 5.02c]

### Added

#### **path**
- Added method `scale` (anisotropic scaling).

#### **fct**
- Added methods `half_tangents` and `tangent`.
- Tangents now return **unit direction points** (instead of `line` objects).
- Fully compatible with `set_scale`.
- Unified implementation based on coordinate generation.

---

### Changed

#### **path**
- In `homothety`, the center argument is now optional.

#### **fct** and **pfct**
- Removed graph-related drawing macros:
  `\tkzDrawPointOnGraph`, `\tkzDrawPointsOnGraph`,
  `\tkzDrawPointOnParamGraph`, and `\tkzDrawPointsOnParamGraph`.

  These macros were incompatible with `set_scale` and conflicted
  with the package design principle: computations must be
  performed in Lua, while drawing is delegated to TikZ or
  `\tkzDrawPoints`.

#### **tkz-elements.sty**
- Cleanup and internal corrections.
- Simplified the Lua–TikZ bridge layer.

## [2026/02/17 — version 5.00c]

### Docs
- Major Documentation Restructuring
- Global Reorganization
     The documentation is now structured into **four coherent parts**:

     1.  **Geometry Core**
     2.  **Algebra and Tools**
     3.  **Lua and Integration**
     4.  **Mathematical and Computational Foundations**

     This restructuring improves conceptual clarity and clearly separates
     geometric theory from implementation details.

- New Sections
     -   Added subsection **"Special Characters and Catcodes"**
     -   Added new section:
     -   **Computational Model and Geometric Engine**

### Added
- **line**
      -   Added method `where_on_line`.
-   New Classes
      -   Added class `fct` (function)
      -   Added class `pfct` (parametric function)
      - Standard tables introduced:
             -   `F` → functions
             -   `PF` → parametric functions
      -   Added class `list_point`

### Changed
-  Unified Geometric Position Model
     - A major harmonization of geometric relation methods has been introduced.
- Standardized Return Values

   All geometric position methods now consistently return:
   -   `"ON"`, `"IN"`, `"OUT"`
   -   or relation-specific classifications (e.g. `"TANGENT"`,
       `"DISJOINT"`, etc.)
   Boolean wrappers remain available for backward compatibility.

- **line** New & Updated Methods
     -   `line:position(pt)`\ → Returns `"ON"` or `"OUT"`.
     -   `line:in_out(pt)`\  → Boolean wrapper of `position`.
     -   `line:path(n)`\  → Parameter `n` is now documented.

- **circle**
    -  `circle:position(obj)` now accepts **generic objects**:
    -   Relation names are now consistently uppercase:
        - `point`      `IN`, `ON`, `OUT`
        - `line`       `DISJOINT`, `TANGENT`, `INTERSECT`
        -  `circle`     Extended circle--circle classification

- **Triangle**
    -  `on_triangle`
    -   New Attributes
         -   `alpha_` , `beta_`,  `gamma_`,  `orientation`,  `cross`

- **angle**  Major development and stabilization.
      -   `value` and `deg` are now **attributes**\ (no longer methods).

- **Path System**
      -   `path:count()` is now documented.
      -   `tkzSetCountFromPath` is now documented.
      -   Improved integration between `list_point` and `path`.

###  New Macros (Graph Support)

- For `fct` and `pfct`:

     -   `\tkzDrawPointOnGraph`
     -   `\tkzDrawPointsOnGraph`
     -    `\tkzDrawPointOnParamGraph`
     -    `\tkzDrawPointsOnParamGraph`

------------------------------------------------------------------------

## [2026/01/25 — version 4.50c]
### Added
- **angle**
  - New class `angle` (experimental):
     provides a dedicated object for oriented and non-oriented angles.
     This class is experimental and not yet fully integrated into all
     parts of the package.
- **list_point**
  - New class `list_point` (experimental):
    container class for ordered collections of points.
    This class is experimental and not yet fully integrated into the
     whole tkz-elements ecosystem.
- **quadrilateral**
   - Added is_convex and corrected area computation in quadrilateral.
- **conic**
   - Added method conic:get(i).
- **point**
   - Added new midpoint helper functions in point:
      - midpoints_chain_(...)
      - midpoints_cycle_(...)
- **matrix**
    -  Added matrix:gauss_jordan and matrix:rank (documentation pending).

### Removed
- Removed alternative and ambiguous definitions:
    - circle:radius(center, radius)
- Deprecated circle constructors in favor of explicit factories:
    - from_radius
    - from_diameter
- Removed obsolete triangle area computation based on circular formulas.

### Fixed
- Fixed a bug in `conic:common_tangent` (`E2 = CO.E2`).
- Fixed incorrect area formula in `circle:area`
  (previously `0.5 * tkz.tau * radius^2`).
- Fixed several bugs in the `circle`, `line`, and `point` classes.
- Fixed a bug in `regular_polygon:side`.
- Fixed `get_points` for ellipses (method no longer applicable).
- Fixed `line_position_(c, r, a, b, EPS)` behavior.
- Fixed issues related to orthogonal constructions in `tkz` (orthopole).
- Improved tkz.parabola.
### Changed
- Replaced `matrix:identity` by `matrix.identity`.
- Rewritten `triangle:get_angle`.
- Changed `orthogonal_circle_through_` implementation.
- Renamed projection-based constructions:
- `proj` → `apothem_foot` in `regular_polygon` and `square`.
- Defined `apothem` as an alias for `inradius`.
- Changed `square:side` into `square.from_side`.
- Changed `square:rotation` into `square.by_rotation`.
- Updated square construction from a line
- `iscyclic` now is `is_cyclic` in quadrilateral.

### Todo
- Clarify and extend common tangent computations for conics.

## [2026/01/22 — version 4.45c]

### Changed
- **tkz-elements.sty**
  - Replaced `luatexluaescapestring` with `luaescapestring`.

- **line**
  -  Simplification of the method `LLL`

### Documentation
- Corrected the example related to the radical center.
- Improved the documentation of the `radical center` and the `radical circle`.
- Added clarifications concerning the `mini` option of `tkz-euclide`, in particular its interaction with
  the `\tkzCompass` macro and the `show` option.
- Removed several sections (`examples`, `theorems`, and `apollonius`) in order to streamline the documentation.
  These materials are now gathered in a separate document entitled *Euclidean Geometry*, available on
  my website: `https://altermundus.fr`.
- Correction of Archimedes Spiral example.

## [2025/12/09 — version 4.44c]

### Added
- **triangle**
  - New methods: `feuerbach_apollonius_k181` and `feuerbach_apollonius`.
  - Kimberling point **#181** added (Apollonius point associated with the Feuerbach configuration).

- **circle**
  - Addition of several special-case handlers inside the `CCC` method.

- **vector**
  - Two new attributes: `dx` and `dy`.
  - New methods: `is_zero`, `is_parallel`, `is_orthogonal`, `dot`, `cross`,
    `angle_to`, `rotate`, `orthogonal`, and `get`.

- **tkz**
  - New constants:
    - `tkz.maxdimen_pt = 16383.99999`
    - `tkz.max_coord_cm = tkz.maxdimen_pt * tkz.pt`  (≈ 575.8 cm)
    - `tkz.max_coord_safe = 0.9 * tkz.max_coord_cm`  (≈ 518 cm)

### Changed
- **tkz-elements.sty**
  - Improved handling of `\tkzGetNodes`: only names of points ending in *p* or *pp* are transformed; all others are preserved.
    The macro now calls the Lua function `tkz.GetNodes`.
  - Update of the `vector` class implementation.
  - Update of `tkz.GetNodes`: coordinate checking has been added; when a coordinate exceeds the safe limit (≈ 518 cm), the
    corresponding node is placed at `(0,0)` and a warning is written to the `.log` file.

### Docs
- Restoration of the *Overview* and *Contents* sections.
- Additions and refinements in the documentation of the `vector` class.
- Correction of the example on **Soddy circles**.


## [2025/11/24 version 4.42c]

### Added
- **angle**
  - New class `angle` (not yet connected to the others).
    The identifier `"A"` is used internally to store objects of type `angle`.

- **circle**
  - Addition of `CCL`.
  - New method `similitude` with optional argument `"internal"` or `"external"`.

- **tkz**
  - New angle-related functions, all accepting an optional EPS argument to adjust precision:
    `tkz.inner_angle`, `tkz.angle_between_vectors`, `tkz.get_angle_normalize`.
  - New comparison helper: `tkz.approx` (optional EPS argument).

- **line**
  - Aliases `perpendicular_bisector` and `bisector` added for `mediator`.
  - New method `harmonic` with optional argument `"internal"`, `"external"`, or `"both"`.

- **triangle**
  - `isodynamic_points` (alias `neuberg_points`).
  - `three_apollonius_circles`.
  - `apollonius_circle`.
  - `apollonius_points`.
  - `apollonius_circles`.

### Changed
- **matrix**
  - Corrections to `print_matrix` and `matrix:print`.
  - Correction of method `htm`.

- **intersection**
  - Rewriting of algorithms with introduction of an EPS parameter to adjust precision.

- **circle**
  - The following functions now accept an optional EPS parameter:
    `on_circle`, `in_out`, `in_disk`, `in_out_disk`,
    `in_disk_strict`, `out_disk_strict`, `in_out_disk_strict`,
    `line_position`, `pole`, `is_tangent`, `is_secant`, `circles_position`,
    `orthogonal_through`, `midcircle`, `common_tangent`.
  - Improved `CLL` for the case of parallel lines.

- **line**
  - Improved `LLL`.

### Docs
- Added Gothic example for CLP.
- Added example with parallel lines for CLL.
- Rewriting of `search_circle.lua`.
- Apollonius section: new CLP examples.
- Lua lesson: using `string.char(37)`.

## [2025/11/13 version 4.40c]

### Added
- **points**:
   - `length(p)`:  `z.A:length(z.B)` equiv. `tkz.length(z.A, z.B)`
-**line**:
   - improvement of `orthogonal_at` and `collinear_at`
- **circle**:
  - `from_radius` equiv. `through` and `from_diameter` equiv. `diameter`
  - modification of `common_tangent`
- **triangle**:
  - `trilinear_to_d`
- **tkz**:
  - `tkz.range`, `tkz.deg`, `tkz.rad` and `tkz.pt`
  - `tkz.is_NaN`, `tkz.is_Inf`, `tkz.notDef`
  - `tkz.fsolve(f, a, b, n [, opts])`
  - `tkz.derivative(f, x0 [, accuracy])`
- **conic**
  - adding `common_tangent`
### Changed
- **conic**:
   -  modification of `path` for an ellipse: new options with "swap" and "ccw" or "cw"
### Docs
   - `\tkzDrawCoordinates[smooth]` instead of `\tkzDrawCoordinatessmooth` in class "conic"
   - adding examples for CCL

## [2025/11/08 version 4.35c]

### Added

- **point**:
   - `shift_collinear_to(B, dist)`,
   `PPP` (circle through three points)
- **line**:
   - `collinear_at_distance(d)`
   - `LLL` (circle tangent to three lines)
- **circle**:
   - `line_position(L)`,
   - `lines_position(L1, L2, mode)`,
   - `is_tangent(L)`, `is_secant(L)`, `is_disjoint(L)`
   - `inversion_neg` (inversion then symmetry)
   - `tangent_parallel`
   - `CLL` or `c_c_ll`
   - add mode in common_tangent by default internal external or both
   - `are_circles_tangents(C)`
   - `CCL` and `CCC` and `CCC_gergonne`
- **path**
   - `get(i)` to extract point of path
- **triangle**
   - modification class triangle: confused test points, collinearity test
   - remove function triangle:c_ll_p
- **tkz**
   - `tkz.approx(x, y)`   comparison of two real numbers
   - `tkz.nodes_from_paths`

 ### Changed
  - All methods relating to contact problems have been improved and supplemented in order to deal with specific cases.
  - I added the following nomenclature for contact issues: `PPP`(new), `LLL`(new), `LPP`, `LLP`, `CPP`, `CCP`, `CLP`,  `CLL`(new), `CCL`(new) and `CCC`(new).
  - `c_lc_p` has two aliases 'c_cl_p' and `CLP`
  - `c_cc_p` has an alias `CCP`
  - colinear is now collinear !
  - tkz-elements-demo_5.lua has been adapted
  - midcircle (amelioration): midcircle between two circles or between a circle and a line.
### Docs
   - typo: correction collinear instead of colinear
   - Added descriptions and examples of new methods, plus a few corrections.


## [2025/10/04 version 4.30c]
### Added
- **circle**:
  - `in_disk` same as `in_out_disk`
  - `on_circle` same as `in_out`
  - `in_disk_strict`
  - `out_disk_strict`
- **line**
  - `side_line` This method assigns a value of -1, 0, or 1 to a given point depending on its position in the plane relative to the line.
  - `on_line` = `in_out_line`
  -   `in_out_segment` = `on_segment`
### Changed
- **circle**
  - `common_tangent` Complete rewriting of the procedure, which now takes into account the position of circles and, above all, adds an option to choose between external tangents and internal tangents when they exist.
  - `c_cc_p` Complete rewriting of the procedure. You can use an "external" or "internal" option to determine whether solution circles exist. The method takes into account the relative position of the circles.
- **line**
  - `c_ll_p(L, p)` now has a line and a point as its argument.

### Docs
- Correction of typography.
- Adjusting documentation for new methods.
- Rewriting of numerous method presentations.

## [2025/09/24 version 4.25c]
### Added
- **path**:
  - `get_point`
  - `iter()`.
- **tkz-elements.sty**:
  - `\tkzGetPointFromPath(#1,#2){#3}`
  - `\tkzPathCount(#1){#2}`
  - `\tkzDrawFromPointToPath`.
- **tkz**: Function `tkz.parabola(a, b, c)` to obtain the quadratic form whose graph passes through the three points `a`, `b`, and `c` (under certain conditions).

### Changed
- **Source structure**: Removed version numbers and dates from each `tkz_elements_xxx.lua` file.
  The official version and release date are now maintained **only** in:
  - `tkz-elements.sty` (for LaTeX users),
  - `README.md`,
  - `CHANGELOG.md`,
  - `doc/tkz-elements.pdf`.
- **occs**: The `coordinates` method now returns numbers instead of strings (removed `checknumber_`).
- **path**: Improved `get_number_path` method.


### Docs
- Renamed several examples: the old names were more explicit.
- After **Overview**, added three navigation tables (attributes, methods, metamethods) for each class. All elements should now be listed, with links to their definitions and, where relevant, to examples.
- Rewrote the section on **path**.
- Added in **Theorems**: Circle–Point Midpoint Theorem.
- Added documentation for the new macros of **tkz-elements.sty**.
- Added in **Examples**: Construction of the midcircle of two disjoint circles.


## [4.21c] — 2025-09-21
### Docs
- Changed the names of the examples.
- The examples now use external Lua files.
- Corrected the documentation.
- Cleaned up the files: unused labels have been removed, as well as **overfull box**.

## [4.20c] — 2025-09-17
### Added
- **triangle**: `thebault/c_c` (Thébault’s problem III), `poncelet_point`, `orthopole(L)`,
  `mixtilinear_incircle`, `three_tangent_circles`, `morley`, `soddy`, `napoleon`.
- **quadrilateral**: support for orthopole via helper function `tkz.orthopole(a, b, c, l)`.

### Changed
- **circle**: improved `common_tangent`, `orthogonal_through`, `orthogonal_from`, `c_cc_p`.
- **line**: improved `distance`.
- **triangle**: rewrites of `symmedian_line`, `altitude`, `bisector`, `bisector_ext`,
  `mediator`, `ex_circle`, `reflection`.

### Fixed
- **circle**: `common_tangent` correctness issues.

### Docs
- New subsection **Best practices** in **Writing Convention, best practices and common mistakes**.
- New examples for `thebault/c_c` in **class triangle** and **theorems** (Thébault’s problem III).
- New examples for `poncelet_point` (triangle and quadrilateral).
- Examples for **orthopole** (triangle + examples section).
- Various corrections.

## [4.15c] — 2025‑06‑15
### Added
- `tkz.trisector` to divide a sector into three equal parts (returns two points).
- Triangle method: `mediator` (perpendicular bisector of the side opposite the chosen vertex).
- Utility: `tkz.is_direct` to test whether ∠ABC is oriented positively.

### Changed
- Triangle methods now accept the vertex **itself** in place of a permutation index (e.g. `bisector`, `altitude`, `ex-circle`), improving clarity and consistency.
- Renamed `tkz_dc` → `tkz.dc`; removed remaining occurrences of the old name.

### Docs
- Updated manual; new example added (**Morley triangle**).

## [4.10c]
### Added
- Modules `utils` and `tkz` (formatting, rounding, comparisons, table ops; helpers like `tkz.solve`, `tkz.midpoint`, `tkz.length`, angle tools, barycenter, etc.).
- `path` class interop with conics and triangles; TikZ macros `\tkzDrawPointsFromPath`, `\tkzGetPointsFromPath`, `\tkzDrawCirclesFromPaths`, `\tkzDrawSegmentsFromPaths`.
- Triangle: `poncelet_point`, `orthopole`.
- Macro aliasing: `\tkzPN` = `\tkzPrintNumber`, `\tkzDrawPath` = `\tkzDrawCoordinates`.
- `\tkzEraseLuaObj` to delete a Lua object.

### Changed
- Circle method `orthogonal_through` rewritten and corrected.

### Docs
- New examples: Archimedean spiral, Poncelet point, Orthopole, Path examples.

## [4.00c]
### Added
- New `path` class; short syntax for constructors (omit `.new`).
- Metapost “mini” macro; options `known` and `near` for intersections; trilinear/barycentric rewrites; line methods `orthogonal_at(pt,k)` and `collinear_at(pt,k)`.

### Changed
- Removed scaling within the Lua section (prefer TikZ‑level scaling).
- Various fixes: triangle `point` method; rectangle `get_lengths`; Gauss pivot improvements.

### Docs
- Major rewrite; new sections (**Short contents, Getting started, Class path, LuaLaTeX for beginners, Global variables and constants, Various functions, Module utils, Metapost**).
