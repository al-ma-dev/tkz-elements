-- File: tkz_elements_main.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

tkz_reserved_names = {} -- liste des noms à ne pas utiliser

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
	"quadrilateral",
	"rectangle",
	"regular_polygon",
	"square",
	"tkz",
	"triangle",
	"utils",
	"vector",
}

for _, name in ipairs(modules) do
	local modname = "tkz_elements_" .. name
	require(modname)
	-- Registers the name if it corresponds to a constructor (not utils/constants)
	-- if not ( name == "constants") then
	table.insert(tkz_reserved_names, name)
	-- end
end
require("tkz_elements_functions_maths.lua")
require("tkz_elements_functions_intersections.lua")
require("tkz_elements_functions_points.lua")
require("tkz_elements_functions_lines.lua")
require("tkz_elements_functions_circles.lua")
require("tkz_elements_functions_triangles.lua")
require("tkz_elements_functions_regular.lua")
require("tkz_elements_functions_matrices.lua")
require("tkz_elements_functions_conics.lua")
require("tkz_elements_functions_vectors.lua")
require("tkz_elements_tkz.lua")

-- Initialize elements

function init_elements()
	local tables_to_clear = {
		"A",
		"C",
		"CO",
		"L",
		"M",
		"O",
		"P",
		"PA",
		"Q",
		"R",
		"RP",
		"S",
		"T",
		"V",
		"z",
	}
	for _, name in ipairs(tables_to_clear) do
		_G[name] = {}
	end
end
