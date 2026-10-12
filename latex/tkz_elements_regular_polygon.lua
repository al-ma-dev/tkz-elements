-- File: tkz_elements_regular_polygon.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes


regular_polygon = {}
regular_polygon.__index = regular_polygon
-- za: center, zb: first vertex, nb: number of sides
function regular_polygon:new(za, zb, nb)
	-- level 2: __call is a tail call (return cls:new(...))
	if not (is_point_(za) and is_point_(zb)) then
		error("tkz-elements: regular_polygon: a center and a vertex (two points) are expected", 2)
	end
	if type(nb) ~= "number" or nb ~= math.floor(nb) or nb < 3 then
		error("tkz-elements: regular_polygon: the number of sides must be an integer >= 3, got "
			.. tostring(nb), 2)
	end
	if point.abs(zb - za) <= tkz.epsilon then
		error("tkz-elements: regular_polygon: the vertex must be distinct from the center", 2)
	end
	local type = "regular_polygon"
	local vertices = regular_(za, zb, nb)
	local center = za
	local through = zb
	local angle = tkz.tau / nb
	local circumradius = point.abs(zb - za)
	local circ = circle:new(za, zb)
	local inradius = circumradius * math.cos(math.pi / nb)
	local side = 2 * circumradius * math.sin(math.pi / nb)
	local apothem_foot = projection_(vertices[1], vertices[2], za)
	local perimeter = nb * side
	local area = (perimeter * inradius) / 2
	local regular = {
		type = type,
		center = center,
		through = through,
		circumradius = circumradius,
		inradius = inradius,
		apothem = inradius,
		vertices = vertices,
		circle = circ,
		nb = nb,
		angle = angle,
		side = side,
		apothem_foot = apothem_foot,
		perimeter = perimeter,
		area = area,
	}
	setmetatable(regular, self)
	return regular
end

setmetatable(regular_polygon, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})
-- get() returns all the vertices; get(i) returns the i-th vertex (1 to nb)
function regular_polygon:get(i)
	if i == nil then
		return table.unpack(self.vertices, 1, self.nb)
	end
	if type(i) ~= "number" or i ~= math.floor(i) or i < 1 or i > self.nb then
		error("tkz-elements: regular_polygon:get: the index must be an integer between 1 and "
			.. self.nb .. ", got " .. tostring(i), 2)
	end
	return self.vertices[i]
end

function regular_polygon:incircle()
	return circle:new(self.center, projection_(self.vertices[1], self.vertices[2], self.center))
end

function regular_polygon:name(nm)
	for k, v in ipairs(self.vertices) do
		z[nm .. k] = v
	end
end

return regular_polygon
