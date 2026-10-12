-- File: tkz_elements_quadrilateral.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

quadrilateral = {}
quadrilateral.__index = quadrilateral
function quadrilateral:new(pa, pb, pc, pd)
	if not (is_point_(pa) and is_point_(pb) and is_point_(pc) and is_point_(pd)) then
		-- level 2: __call is a tail call (return cls:new(...))
		error("tkz-elements: quadrilateral: four points are expected", 2)
	end
	local type = "quadrilateral"
	local a = point.abs(pb - pa)
	local b = point.abs(pc - pb)
	local c = point.abs(pd - pc)
	local d = point.abs(pa - pd)
	local ab = line:new(pa, pb)
	local bc = line:new(pb, pc)
	local cd = line:new(pc, pd)
	local da = line:new(pd, pa)
	local ac = line:new(pa, pc)
	local bd = line:new(pb, pd)
	local center = intersection_ll_(pa, pc, pb, pd)
	local g = barycenter_({ pa, 1 }, { pb, 1 }, { pc, 1 }, { pd, 1 })
	local o = {
		pa = pa,
		pb = pb,
		pc = pc,
		pd = pd,
		a = a,
		b = b,
		c = c,
		d = d,
		ab = ab,
		bc = bc,
		cd = cd,
		da = da,
		ac = ac,
		bd = bd,
		center = center,
		g = g,
		type = type,
	}
	setmetatable(o, self)
	return o
end

setmetatable(quadrilateral, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})
-----------------------
-- get() returns the four vertices ; get(i) returns the i-th vertex (1 to 4)
function quadrilateral:get(i)
	if i == nil then
		return self.pa, self.pb, self.pc, self.pd
	end
	return ({ self.pa, self.pb, self.pc, self.pd })[i]
end

-- true if the four vertices lie on a same circle
function quadrilateral:is_cyclic(EPS)
	EPS = EPS or tkz.epsilon
	local A, B, C, D = self.pa, self.pb, self.pc, self.pd
	if tkz.aligned(A, B, C) then
		return false
	end
	local o = circum_center_(A, B, C)
	local r = point.abs(A - o)
	return math.abs(point.abs(D - o) - r) <= EPS * math.max(r, 1)
end

-- common point of the nine-point circles of ABC, BCD, CDA and DAB
function quadrilateral:poncelet_point()
	local p, err = poncelet_point_(self.pa, self.pb, self.pc, self.pd)
	if not p then
		error("tkz-elements: quadrilateral:poncelet_point: " .. err, 2)
	end
	return p
end

function quadrilateral:is_convex(EPS)
	EPS = EPS or tkz.epsilon

	local A, B, C, D = self.pa, self.pb, self.pc, self.pd

	local function cross(u, v)
		return (point.conj(u) * v).im
	end

	local z1 = cross(B - A, C - B)
	local z2 = cross(C - B, D - C)
	local z3 = cross(D - C, A - D)
	local z4 = cross(A - D, B - A)

	-- collinear consecutive vertices: degenerate
	if math.abs(z1) < EPS or math.abs(z2) < EPS
		 or math.abs(z3) < EPS or math.abs(z4) < EPS then
		return false
	end

	local s1 = z1 > 0
	return (z2 > 0) == s1
		 and (z3 > 0) == s1
		 and (z4 > 0) == s1
end

-- true if the sides [AB] and [CD], or [BC] and [DA], cross each other
function quadrilateral:is_crossed_()
	local A, B, C, D = self.pa, self.pb, self.pc, self.pd
	local function cross(u, v)
		return (point.conj(u) * v).im
	end
	local function segments_cross(p1, p2, p3, p4)
		local d1 = cross(p4 - p3, p1 - p3)
		local d2 = cross(p4 - p3, p2 - p3)
		local d3 = cross(p2 - p1, p3 - p1)
		local d4 = cross(p2 - p1, p4 - p1)
		return d1 * d2 < 0 and d3 * d4 < 0
	end
	return segments_cross(A, B, C, D) or segments_cross(B, C, D, A)
end

-- area of a non-crossed quadrilateral (convex or not)
function quadrilateral:area()
	if self:is_crossed_() then
		error("tkz-elements: quadrilateral:area: undefined for a crossed quadrilateral", 2)
	end

	local A, B, C, D = self.pa, self.pb, self.pc, self.pd

	local function cross(u, v)
		return (point.conj(u) * v).im
	end

	local s =
			cross(A, B)
		+ cross(B, C)
		+ cross(C, D)
		+ cross(D, A)

	return 0.5 * math.abs(s)
end


return quadrilateral
