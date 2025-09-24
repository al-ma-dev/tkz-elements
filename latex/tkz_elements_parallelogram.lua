-- File: tkz_elements_parallelogram.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           Parallelogram
---------------------------------------------------------------------------

parallelogram = {}
parallelogram.__index = parallelogram
function parallelogram:new(za, zb, zc, zd)
	local d
	local zi = midpoint_(za, zc)
	local zj = midpoint_(zb, zd)
	if point.abs(zj - zi) < tkz.epsilon then
	else
		tex.error("it's not a parallelogram")
	end
	local type = "parallelogram"
	local center = midpoint_(za, zc)
	local ab = line:new(za, zb)
	local bc = line:new(zb, zc)
	local cd = line:new(zc, zd)
	local da = line:new(zd, za)
	local ac = line:new(za, zc)
	local bd = line:new(zb, zd)
	local o = {
		pa = za,
		pb = zb,
		pc = zc,
		pd = zd,
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

function parallelogram:get()
	return self.pa, self.pb, self.pc, self.pd
end

function parallelogram:fourth(za, zb, zc)
	local zd = zc + (za - zb)
	return parallelogram:new(za, zb, zc, zd)
end

return parallelogram
