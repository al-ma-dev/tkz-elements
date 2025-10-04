# tkz-elements — Euclidean geometry with LuaLaTeX

_Current release: [2025/10/04 version 4.30c]_

`tkz-elements` is a Lua library that performs the computations needed to define the objects of an Euclidean geometry figure (points, lines, circles, conics, triangles, paths, …). All numerical work happens in **Lua**; the drawing is left to **tkz‑euclide** (recommended) or **TikZ**.

---

## Features
- **Object-oriented design**: classes `point`, `line`, `circle`, `conic`, `triangle`, `path`, …
- **Rich geometry toolbox**: bisectors, altitudes, tangents, radical axis/center, Poncelet point, orthopole, Kimberling points, mixtilinear circle, Morley/Soddy/Napoleon constructions, and more
- **Utilities module**: helpers for formatting numbers/points, solving equations, trigonometry/angles, barycentric & trilinear coordinates, comparisons, rounding, etc.
- **Smooth Lua → TikZ transfer**: move computed objects from Lua to TikZ/`tkz-euclide` with simple macros

## Requirements
- **LuaLaTeX** (Lua 5.3)
- **tkz‑euclide** (or **TikZ**)
- A recent TeX distribution (TeX Live / MiKTeX / MacTeX)

> Tip: If you use `xcolor`, load it **before** `tkz-euclide`.

## Installation
`tkz-elements` is available via TeX Live and MiKTeX package managers.

For a manual install, place files in your TDS tree (or `TEXMFHOME`) following the usual directory structure of LaTeX packages.

## Quick start
```tex
% !TEX TS-program = lualatex
\documentclass{standalone}
\usepackage[mini]{tkz-euclide}
\usepackage{tkz-elements}
\begin{document}
\directlua{
  init_elements()
  -- Create geometry in Lua
  z.A = point(0,0)
  z.B = point(2,0)
  z.C = point(1,1.2)
}
\begin{tikzpicture}
  % Transfer Lua points to TikZ nodes A, B, C
  \tkzGetNodes
  % Draw with tkz-euclide / TikZ
  \tkzDrawPolygon(A,B,C)
  \tkzDrawPoints(A,B,C)
  \tkzLabelPoints(A,B,C)
\end{tikzpicture}
\end{document}
```
> Alternative: the `tkzelements` environment (requires `luacode`).

## Documentation & examples
- Project page and examples (including **Golden Arbelos**): <http://altermundus.fr>
- The full manual ships with the distribution (see `doc/`).
  If you build from sources, the manual is produced at:
  `doc/src/build/TKZdoc-elements-main.pdf`

---

## GitHub (source, issues, roadmap)
Development happens on GitHub:
- **Repository**: <https://github.com/al-ma-dev/tkz-elements>
- **Issues**: <https://github.com/al-ma-dev/tkz-elements/issues>
- **Changelog**: see `CHANGELOG.md`
- **Contributing guide**: `CONTRIBUTING.md` (PRs welcome)
- **License**: LPPL 1.3c

### Report bugs / request features
Please open a GitHub issue and include:
- a **MWE** (Minimal Working Example) in LaTeX;
- your **TeX distribution** (TeX Live/MacTeX/MiKTeX), LuaLaTeX version, OS;
- relevant **log excerpts** (with `-file-line-error`).

### Versioning & releases
- Versions follow the scheme `4.xx c` (e.g., `4.25c`) and are tagged on GitHub.
- The PDF manual ships in the repository and on CTAN.

The official version number and release date of **tkz-elements** are recorded in:

- `tkz-elements.sty` (LaTeX package file, visible in the log),
- `README.md` (this file),
- `CHANGELOG.md` (detailed history),
- `doc/tkz-elements.pdf` (documentation front page).

Lua source files (`tkz_elements_xxx.lua`) no longer carry their own version/date headers.

---

## License
This work may be modified and distributed under the terms of the
[LaTeX Project Public License](https://www.latex-project.org/lppl/), version 1.3c or later.

## Changelog
Detailed changes are tracked in **CHANGELOG.md**.

## Author
Alain Matthes — al (dot) ma (at) mac (dot) com
