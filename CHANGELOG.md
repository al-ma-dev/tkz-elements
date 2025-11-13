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
   - modification class triangle : confused test points, collinearity test
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
  - `on_segment` = `in_out_segment`
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
