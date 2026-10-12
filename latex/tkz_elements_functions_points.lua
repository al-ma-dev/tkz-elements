-- File: tkz_elements_functions_points.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes


function is_point_(p)
	return type(p) == "table" and p.type == "point"
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

-- barycenter_({A, a}, {B, b}, ...) : barycenter of the weighted points
function barycenter_(...)
	local cp = table.pack(...)
	local sum = 0
	local weight = 0
	for i = 1, cp.n do
		sum = sum + cp[i][1] * cp[i][2]
		weight = weight + cp[i][2]
	end
	if math.abs(weight) < tkz.epsilon then
		error("tkz-elements: barycenter: the sum of the weights is zero", 2)
	end
	return sum / weight
end

-- ----------------------------------------------------------------
-- Transformations of objects
-- ----------------------------------------------------------------
-- An object is rebuilt from the images of its defining points.
-- To support a new class, add a line to this table.
local shapes = {
	line          = { "pa", "pb" },
	circle        = { "center", "through" },
	triangle      = { "pa", "pb", "pc" },
	square        = { "pa", "pb", "pc", "pd" },
	rectangle     = { "pa", "pb", "pc", "pd" },
	quadrilateral = { "pa", "pb", "pc", "pd" },
	parallelogram = { "pa", "pb", "pc", "pd" },
}

-- Apply f (point -> point) to an object and rebuild an object of the
-- same class. Errors are reported at the line of the user code
-- (user -> method -> map_objects_ -> map_object).
local function map_object(obj, f, name)
	if type(obj) ~= "table" then
		error("tkz-elements: " .. name .. ": expected a geometric object, got a " .. type(obj), 4)
	end
	if obj.type == "point" then
		return f(obj)
	end
	local keys = shapes[obj.type]
	if not keys then
		error("tkz-elements: " .. name .. ": unsupported object of type '" .. tostring(obj.type) .. "'", 4)
	end
	local pts = {}
	for i, k in ipairs(keys) do
		pts[i] = f(obj[k])
	end
	return _G[obj.type]:new(table.unpack(pts))
end

-- map_objects_(f, name, ...) : apply f to every argument (points or
-- objects); return as many results as arguments.
-- name is used in error messages, e.g. "point:rotation".
function map_objects_(f, name, ...)
	local n = select("#", ...)
	local results = {}
	for i = 1, n do
		results[i] = map_object(select(i, ...), f, name)
	end
	return table.unpack(results, 1, n)
end

-- ----------------------------------------------------------------
-- Affine maps of objects (line:affinity, matrix:htm_apply)
-- ----------------------------------------------------------------
-- Class of the image of an object by an affine map that is not a
-- similarity: a square or a rectangle becomes a parallelogram.
local affine_classes = {
	line          = { "line",          { "pa", "pb" } },
	triangle      = { "triangle",      { "pa", "pb", "pc" } },
	quadrilateral = { "quadrilateral", { "pa", "pb", "pc", "pd" } },
	parallelogram = { "parallelogram", { "pa", "pb", "pc", "pd" } },
	square        = { "parallelogram", { "pa", "pb", "pc", "pd" } },
	rectangle     = { "parallelogram", { "pa", "pb", "pc", "pd" } },
}

-- image of an ellipse of center c and conjugate semi-diameters [c,m], [c,n]
local function affine_ellipse(f, c, m, n)
	local c1, m1, n1 = f(c), f(m), f(n)
	local a, b, angle = conjugate_axes_(c1, m1, n1)
	if math.abs(a - b) <= tkz.epsilon then
		return circle:new(c1, c1 + polar_(a, angle))
	end
	return conic(EL_radii(c1, a, b, angle))
end

-- affine_objects_(f, name, similar, degenerate, ...)
-- f          : affine map point -> point
-- similar    : true if f is a similarity (the objects keep their class)
-- degenerate : true if f is not invertible (only points are accepted)
-- Each argument may be a point or an object; a circle and an ellipse
-- become an ellipse (or a circle). Returns the table of the images and
-- their number. Errors are reported at the line of the user code
-- (user -> method -> affine_objects_).
function affine_objects_(f, name, similar, degenerate, ...)
	local n = select("#", ...)
	local t = {}
	for i = 1, n do
		local obj = select(i, ...)
		local ty = type(obj) == "table" and obj.type or type(obj)
		if ty == "point" then
			t[i] = f(obj)
		else
			local tname = (ty == "conic") and obj.subtype or ty
			if degenerate then
				error("tkz-elements: " .. name .. ": degenerate transformation, the image of a "
					.. tostring(tname) .. " is degenerate", 3)
			end
			local cl = affine_classes[ty]
			if similar and shapes[ty] then
				t[i] = map_object(obj, f, name)
			elseif cl then
				local pts = {}
				for j, key in ipairs(cl[2]) do
					pts[j] = f(obj[key])
				end
				t[i] = _G[cl[1]]:new(table.unpack(pts))
			elseif ty == "circle" then
				local c = obj.center
				t[i] = affine_ellipse(f, c, obj.through, rotation_(c, math.pi / 2, obj.through))
			elseif ty == "conic" and obj.subtype == "ellipse" then
				t[i] = affine_ellipse(f, obj.center, obj.vertex, obj.covertex)
			else
				error("tkz-elements: " .. name .. ": unsupported object of type '"
					.. tostring(tname) .. "'", 3)
			end
		end
	end
	return t, n
end

function rotation_(c, a, pt)
	local r = point(math.cos(a), math.sin(a))
	return r * (pt - c) + c
end

function symmetry_(c, pt)
	return 2 * c - pt
end

function homothety_(c, t, p)
	return c + t * (p - c)
end

function translation_(u, p)
	return u + p
end

-- Random point whose coordinates are real numbers in [lower, upper).
-- (math.random(lower, upper) only accepts integers and only returns
-- integers.)
function random_point_(lower, upper)
	if not tkz._seeded then
		math.randomseed(os.time())
		tkz._seeded = true
	end
	local x = lower + (upper - lower) * math.random()
	local y = lower + (upper - lower) * math.random()
	return point(x, y)
end

-- midpoints_chain_(...)
-- midpoints_cycle_(...)
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

function midpoints_chain_(...)
	local arg = table.pack(...)
	local n = arg.n
	local t = {}

	if n < 2 then
		return nil
	end

	for i = 1, n - 1 do
		t[#t + 1] = (arg[i] + arg[i + 1]) / 2
	end

	return table.unpack(t)
end

function midpoints_cycle_(...)
	local arg = table.pack(...)
	local n = arg.n
	local t = {}

	if n < 2 then
		return nil
	end

	for i = 1, n - 1 do
		t[#t + 1] = (arg[i] + arg[i + 1]) / 2
	end

	-- fermeture du cycle
	t[#t + 1] = (arg[n] + arg[1]) / 2

	return table.unpack(t)
end


-- get_points(obj) : kept for compatibility, not recommended.
-- Use the method get instead: z.A, z.B = L.AB:get() ; z.A = L.AB:get(1)
-- A warning is written once (terminal and log).
local get_points_warned = false
function get_points(obj)
	if not get_points_warned then
		get_points_warned = true
		texio.write_nl("term and log",
			"[tkz-elements] Warning: get_points(obj) is deprecated, use obj:get() instead.")
	end
	-- Map of object types to their respective point keys
	local point_map = {
		line = { "pa", "pb" }, -- Line has two points
		triangle = { "pa", "pb", "pc" }, -- Triangle has three points
		circle = { "center", "through" }, -- Circle has center and a point through its circumference
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
	error("tkz-elements: get_points: unsupported object of type '"
		.. tostring(type(obj) == "table" and obj.type or type(obj)) .. "'", 2)
end


