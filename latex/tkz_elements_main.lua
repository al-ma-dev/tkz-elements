-- File: tkz_elements_main.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- Classes (files tkz_elements_<name>.lua)
local modules = {
	"angle",
	"circle",
	"conic",
	"line",
	"matrix",
	"occs",
	"parallelogram",
	"path",
	"point",
	"list_point",
	"quadrilateral",
	"rectangle",
	"regular_polygon",
	"square",
	"tkz",
	"triangle",
	"utils",
	"vector",
	"function",
	"param_function",
}

-- Internal functions (files tkz_elements_functions_<name>.lua)
local functions = {
	"maths",
	"intersections",
	"points",
	"lines",
	"circles",
	"triangles",
	"regular",
	"matrices",
	"conics",
	"vectors",
	"square",
	"parallelogram",
}

for _, name in ipairs(modules) do
	require("tkz_elements_" .. name)
end
for _, name in ipairs(functions) do
	require("tkz_elements_functions_" .. name)
end

-- Tables reserved for storing objects: { table, class }.
-- Used by init_elements() and by the documentation (list of reserved classes).
tkz.tables = {
	{ "A",  "angle" },
	{ "C",  "circle" },
	{ "CO", "conic" },
	{ "F",  "fct" },
	{ "PF", "pfct" },
	{ "L",  "line" },
	{ "LP", "list_point" },
	{ "M",  "matrix" },
	{ "O",  "occs" },
	{ "P",  "parallelogram" },
	{ "PA", "path" },
	{ "z",  "point" },
	{ "Q",  "quadrilateral" },
	{ "R",  "rectangle" },
	{ "RP", "regular_polygon" },
	{ "S",  "square" },
	{ "T",  "triangle" },
	{ "V",  "vector" },
}

-- Initialize elements: clear the reserved tables
function init_elements()
	for _, t in ipairs(tkz.tables) do
		_G[t[1]] = {}
	end
end
