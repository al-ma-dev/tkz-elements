-- File: tkz_elements_rectangle.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           rectangle
---------------------------------------------------------------------------

rectangle = {}
rectangle.__index = rectangle
function rectangle:new(za, zb, zc, zd)
	local d
	local zi = midpoint_(za, zc)
	local zj = midpoint_(zb, zd)
	local type = "rectangle"
	local center = midpoint_(za, zc)
	local circumradius = point.abs(center - za)
	local ab = line:new(za, zb)
	local bc = line:new(zb, zc)
	local cd = line:new(zc, zd)
	local da = line:new(zd, za)
	local ac = line:new(za, zc)
	local bd = line:new(zb, zd)
	local length = point.mod(zb - za)
	local width = point.mod(zd - za)
	local diagonal = point.abs(zc - za)
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
		diagonal = diagonal,
		circumradius = circumradius,
		center = center,
		length = length,
		width = width,
		type = type,
	}
	setmetatable(o, self)
	return o
end

setmetatable(rectangle, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

function rectangle:get()
	return self.pa, self.pb, self.pc, self.pd
end

function rectangle:angle(zi, za, an)
	local zb, zc, zd
	local zc = symmetry_(zi, za)
	local zb = rotation_(zi, -an, zc)
	local zd = symmetry_(zi, zb)
	return rectangle:new(za, zb, zc, zd)
end

function rectangle:gold(za, zb, swap)
	local zc, zd
	local a = point.arg(zb - za)
	local d = point.abs(zb - za) * tkz.invphi
	swap = (swap == "swap")
	if swap then
		zc = zb:south(d)
		zc = rotation_(zb, a, zc)
		zd = za:south(d)
		zd = rotation_(za, a, zd)
		return rectangle:new(za, zb, zc, zd)
	else
		zc = zb:north(d)
		zc = rotation_(zb, a, zc)
		zd = za:north(d)
		zd = rotation_(za, a, zd)
		return rectangle:new(za, zb, zc, zd)
	end
end

function rectangle:diagonal(za, zc, swap)
	local zb, zd
	swap = (swap == "swap")
	if swap then
		zd = point(zc.re, za.im)
		zb = point(za.re, zc.im)
		return rectangle:new(za, zb, zc, zd)
	else
		zb = point(zc.re, za.im)
		zd = point(za.re, zc.im)
		return rectangle:new(za, zb, zc, zd)
	end
end

function rectangle:side(za, zb, d, swap)
	local a, zc, zd
	a = point.arg(zb - za)
	swap = (swap == "swap")
	if swap then
		zc = zb:south(d)
		zc = rotation_(zb, a, zc)
		zd = za:south(d)
		zd = rotation_(za, a, zd)
		return rectangle:new(za, zb, zc, zd)
	else
		zc = zb:north(d)
		zc = rotation_(zb, a, zc)
		zd = za:north(d)
		zd = rotation_(za, a, zd)
		return rectangle:new(za, zb, zc, zd)
	end
end

-------------------
function rectangle:get_lengths()
	local a = self.pa
	local b = self.pb
	local c = self.pc
	local d = self.pd
	return point.mod(b - a), point.mod(c - b)
end

return rectangle
