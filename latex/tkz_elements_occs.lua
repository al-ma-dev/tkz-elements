-- File: tkz_elements_occs.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

occs = {}
occs.__index = occs
-- occs(L, zO): orthonormal system with origin zO; the ordinate axis has
-- the direction of the line L, the abscissa axis is obtained by a rotation
-- of -pi/2 (direct system).
function occs:new(L, zO)
	if type(L) ~= "table" or L.type ~= "line" then
		error("tkz-elements: occs: the first argument must be a line", 2)
	end
	if point.abs(L.pb - L.pa) <= tkz.epsilon then
		error("tkz-elements: occs: the line is degenerate (its two points are identical)", 2)
	end
	if type(zO) ~= "table" or zO.type ~= "point" then
		error("tkz-elements: occs: the second argument (origin) must be a point", 2)
	end
	local type = "occs"
	local origin = zO
	local y = report_(L.pa, L.pb, 1, zO)
	local x = rotation_(zO, -math.pi / 2, y)
	local abscissa = line:new(zO, x)
	local ordinate = line:new(zO, y)
	local o = { origin = origin, x = x, y = y, abscissa = abscissa, ordinate = ordinate, type = type }
	setmetatable(o, self)
	return o
end

setmetatable(occs, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})
-----------------------
-- coordinates(pt): coordinates (u, v) of the point pt in the system
function occs:coordinates(pt)
	if type(pt) ~= "table" or pt.type ~= "point" then
		error("tkz-elements: occs:coordinates: a point is expected", 2)
	end
	local O = self.origin
	local d = pt - O
	-- unit vectors of the axes (length 1 by construction)
	local ux = self.x - O
	local uy = self.y - O
	return d .. ux, d .. uy
end

-- point(u, v): the point with coordinates (u, v) in the system
-- (inverse of coordinates)
function occs:point(u, v)
	if type(u) ~= "number" or type(v) ~= "number" then
		error("tkz-elements: occs:point: two numbers are expected", 2)
	end
	local O = self.origin
	return O + u * (self.x - O) + v * (self.y - O)
end


return occs
