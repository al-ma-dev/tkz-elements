-- File: tkz_elements_regular.lua
-- Version: 4.21c   Date: 2025/09/21
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes


regular_polygon = {}
regular_polygon.__index = regular_polygon
function regular_polygon:new(za, zb, nb) -- za = center zb a vertex
	local type = "regular_polygon"
	local vertices = regular_(za, zb, nb)
	local center = za
	local through = zb
	local angle = 2 * math.pi / nb
	local circumradius = point.abs(zb - za)
	local circle = circle:new(za, zb)
	local inradius = circumradius * math.cos(math.pi / nb)
	local side = circumradius * math.sin(math.pi / nb)
	local proj = projection_(vertices[1], vertices[2], za)
	local perimeter = nb * side
	local area = (perimeter * inradius) / 2
	local regular = {
		type = type,
		center = center,
		through = through,
		circumradius = circumradius,
		inradius = inradius,
		vertices = vertices,
		circle = circle,
		nb = nb,
		angle = angle,
		side = side,
		proj = proj,
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
function regular_polygon:get(i)
	if i == nil then
		return self.vertices -- retourne toute la matrice
	else
		if i <= self.nb then
			return self.vertices[i]
		else
			tex.error("Bad argument")
		end
	end
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
