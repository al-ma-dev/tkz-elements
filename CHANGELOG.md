# CHANGELOG

All notable changes to **tkz-elements** will be documented in this file.
This format follows the spirit of <https://keepachangelog.com/> (categories **Added / Changed / Fixed / Removed / Deprecated / Docs / Security**).
Versions here reflect the project’s scheme (e.g. `4.21c`).

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
- Metapost “mini” macro; options `known` and `near` for intersections; trilinear/barycentric rewrites; line methods `orthogonal_at(pt,k)` and `colinear_at(pt,k)`.

### Changed
- Removed scaling within the Lua section (prefer TikZ‑level scaling).
- Various fixes: triangle `point` method; rectangle `get_lengths`; Gauss pivot improvements.

### Docs
- Major rewrite; new sections (**Short contents, Getting started, Class path, LuaLaTeX for beginners, Global variables and constants, Various functions, Module utils, Metapost**).
