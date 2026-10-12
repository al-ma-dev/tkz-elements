-- File: tkz_elements_parallelogram.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           Parallelogram
---------------------------------------------------------------------------

parallelogram = {}
parallelogram.__index = parallelogram
function parallelogram:new(pa, pb, pc, pd)
	if not (is_point_(pa) and is_point_(pb) and is_point_(pc) and is_point_(pd)) then
		-- level 2: __call is a tail call (return cls:new(...))
		error("tkz-elements: parallelogram: four points are expected", 2)
	end
	local zi = midpoint_(pa, pc)
	local zj = midpoint_(pb, pd)
	if point.abs(zj - zi) >= tkz.epsilon then
		error("tkz-elements: parallelogram: the diagonals do not have the same midpoint", 2)
	end
	local type = "parallelogram"
	local center = zi
	local ab = line:new(pa, pb)
	local bc = line:new(pb, pc)
	local cd = line:new(pc, pd)
	local da = line:new(pd, pa)
	local ac = line:new(pa, pc)
	local bd = line:new(pb, pd)
	local o = {
		pa = pa,
		pb = pb,
		pc = pc,
		pd = pd,
		ab = ab,
		ac = ac,
		bc = bc,
		da = da,
		cd = cd,
		bd = bd,
		center = center,
		type = type,
	}
	setmetatable(o, self)
	return o
end

setmetatable(parallelogram, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

-- get() returns the four vertices ; get(i) returns the i-th vertex (1 to 4)
function parallelogram:get(i)
	if i == nil then
		return self.pa, self.pb, self.pc, self.pd
	end
	return ({ self.pa, self.pb, self.pc, self.pd })[i]
end

return parallelogram
