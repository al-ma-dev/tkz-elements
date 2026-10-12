-- File: tkz_elements_square.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           Squares
---------------------------------------------------------------------------

square = {}
square.__index = square
function square:new(pa, pb, pc, pd)
	if not (is_point_(pa) and is_point_(pb) and is_point_(pc) and is_point_(pd)) then
		-- level 2: __call is a tail call (return cls:new(...))
		error("tkz-elements: square: four points are expected", 2)
	end
	local type = "square"
	local side = point.abs(pb - pa)
	local center = midpoint_(pa, pc)
	local circumradius = point.abs(center - pa)
	local inradius = side / 2
	local diagonal = math.sqrt(2) * side
  local apothem_foot = midpoint_(pa, pb)
	local ab = line:new(pa, pb)
	local bc = line:new(pb, pc)
	local cd = line:new(pc, pd)
	local da = line:new(pd, pa)
	local bd = line:new(pb, pd)
	local ac = line:new(pa, pc)
	local area = side * side
	local perimeter = 4 * side
	local o = {
		pa = pa,
		pb = pb,
		pc = pc,
		pd = pd,
		side = side,
		center = center,
		circumradius = circumradius,
		inradius = inradius,
		apothem =  inradius,
		diagonal = diagonal,
		apothem_foot = apothem_foot,
		ab = ab,
		ac = ac,
		bc = bc,
		da = da,
		cd = cd,
		bd = bd,
		type = type,
		area = area,
		perimeter = perimeter,
	}
	setmetatable(o, self)
	return o
end

setmetatable(square, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

-- get() returns the four vertices ; get(i) returns the i-th vertex (1 to 4)
function square:get(i)
	if i == nil then
		return self.pa, self.pb, self.pc, self.pd
	end
	return ({ self.pa, self.pb, self.pc, self.pd })[i]
end


return square
