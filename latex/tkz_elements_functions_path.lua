-- File: tkz_elements_functions_misc.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
-- ----------------------------------------------------------------
--
-- ----------------------------------------------------------------
--  Utilitaire : extrait (x, y) de "(x,y)"

function path_from_pairs(p1, p2, decimals)
	decimals = decimals or 5
	assert(#p1 == #p2, "Paths of different lengths")
	local result = path:new()
	for i = 1, #p1 do
		local x1, y1 = utils.parse_point(p1[i])
		local x2, y2 = utils.parse_point(p2[i])
		local z1 = { re = x1, im = y1 }
		local z2 = { re = x2, im = y2 }
		result:add_pair_to_path(z1, z2, decimals)
	end
	return result
end

function stock_paths(...)
	local args = { ... }
	local n = select("#", ...)
	local decimals = 5
	local paths = {}

	-- Check if last argument is a number (decimals)
	if type(args[n]) == "number" then
		decimals = args[n]
		for i = 1, n - 1 do
			paths[i] = args[i]
		end
	else
		for i = 1, n do
			paths[i] = args[i]
		end
	end

	local len = #paths[1]
	for i = 2, #paths do
		assert(#paths[i] == len, "Paths have different lengths")
	end

	local result = path:new()
	for i = 1, len do
		local parts = {}
		for _, p in ipairs(paths) do
			local x, y = utils.parse_point(p[i])
			table.insert(parts, string.format("%." .. decimals .. "f", x))
			table.insert(parts, string.format("%." .. decimals .. "f", y))
		end
		table.insert(result, table.concat(parts, "/"))
	end

	return result
end
