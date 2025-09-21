-- File: tkz_elements_functions_points.lua
-- Version: 4.21c   Date: 2025/09/21
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

function id()
	for i, k in pairs(z) do
		if _G[i] == k then
		else
			_G[i] = k
		end
	end
end

function polar_(radius, phi)
	return point(radius * math.cos(phi), radius * math.sin(phi))
end

function polar(radius, phi)
	return polar_(radius, phi)
end

function polar_deg(radius, phi)
	return polar_(radius, phi * math.pi / 180)
end

function barycenter_(...)
	local cp = table.pack(...)
	local sum = 0
	local weight = 0
	for i = 1, cp.n do
		sum = sum + cp[i][1] * cp[i][2]
		weight = weight + cp[i][2]
	end
	return sum / weight
end

function rotation_(c, a, pt)
	local z = point(math.cos(a), math.sin(a))
	return z * (pt - c) + c
end

-- Define the set_rotation_ function
function set_rotation_(c, angle, ...)
	local tp = table.pack(...)
	local t = {}
	for i = 1, tp.n do
		table.insert(t, rotation_(c, angle, tp[i]))
	end
	return table.unpack(t)
end

function symmetry_(c, pt)
	return 2 * c - pt
end

function set_symmetry_(c, ...)
	local tp = table.pack(...)
	local t = {}
	for i = 1, tp.n do
		table.insert(t, symmetry_(c, tp[i]))
	end
	return table.unpack(t)
end

function homothety_(c, t, p)
	return c + t * (p - c)
end

function set_homothety_(c, coeff, ...)
	local tp = table.pack(...)
	local t = {}
	for i = 1, tp.n do
		table.insert(t, homothety_(c, coeff, tp[i]))
	end
	return table.unpack(t)
end

function translation_(a, p)
	return a + p
end

function set_translation_(u, ...)
	local tp = table.pack(...)
	local t = {}
	for i = 1, tp.n do
		table.insert(t, (u + tp[i]))
	end
	return table.unpack(t)
end

function random_point_(lower, upper)
	math.randomseed(tonumber(tostring(os.time()):reverse():sub(1, 6)))
	x = math.random(lower, upper)
	y = math.random(lower, upper)
	return point(x, y)
end

function midpoints_(...)
	local arg = table.pack(...)
	local n = arg.n
	local t = {}
	for i = 1, n - 1 do
		table.insert(t, (arg[i] + arg[i + 1]) / 2)
	end
	table.insert(t, (arg[n] + arg[1]) / 2)
	return table.unpack(t)
end

function midpoint_(a, b)
	return (a + b) / 2
end

function get_points(obj)
	-- Map of object types to their respective point keys
	local point_map = {
		line = { "pa", "pb" }, -- Line has two points
		triangle = { "pa", "pb", "pc" }, -- Triangle has three points
		circle = { "center", "through" }, -- Circle has center and a point through its circumference
		ellipse = { "pc", "pa", "pb" }, -- Ellipse has three key points
		square = { "pa", "pb", "pc", "pd" }, -- Square has four vertices
		rectangle = { "pa", "pb", "pc", "pd" }, -- Rectangle has four vertices
		quadrilateral = { "pa", "pb", "pc", "pd" }, -- Quadrilateral has four vertices
		parallelogram = { "pa", "pb", "pc", "pd" }, -- Parallelogram has four vertices
	}

	-- Check if the object's type is recognized
	if point_map[obj.type] then
		local points = {}
		-- Iterate over the keys for the given type and extract the corresponding values
		for _, key in ipairs(point_map[obj.type]) do
			table.insert(points, obj[key])
		end
		-- Return all points as multiple return values
		return table.unpack(points)
	end
end
