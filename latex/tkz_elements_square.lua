-- File: tkz_elements_square.lua
-- Version: 4.21c   Date: 2025/09/21
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           Squares
---------------------------------------------------------------------------

square = {}
square.__index = square
function square:new(za, zb, zc, zd)
	local d
	local zi = midpoint_(za, zc)
	local zj = midpoint_(zb, zd)
	local type = "square"
	local side = point.abs(zb - za)
	local pc = rotation_(zb, -math.pi / 2, za)
	local pd = rotation_(za, math.pi / 2, zb)
	local center = midpoint_(za, zc)
	local circumradius = point.abs(center - za)
	local inradius = circumradius * math.cos(math.pi / 4)
	local diagonal = math.sqrt(2) * side
	local proj = projection_(za, zb, center)
	local ab = line:new(za, zb)
	local bc = line:new(zb, zc)
	local cd = line:new(zc, zd)
	local da = line:new(zd, za)
	local bd = line:new(zb, zd)
	local ac = line:new(za, zc)
	local area = side * side
	local perimeter = 4 * side
	local o = {
		pa = za,
		pb = zb,
		pc = zc,
		pd = zd,
		side = side,
		center = center,
		circumradius = circumradius,
		inradius = inradius,
		diagonal = diagonal,
		proj = proj,
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

function square:get()
	return self.pa, self.pb, self.pc, self.pd
end

function square:rotation(zi, za)
	local zb = rotation_(zi, math.pi / 2, za)
	local zc = rotation_(zi, math.pi / 2, zb)
	local zd = rotation_(zi, math.pi / 2, zc)
	return square:new(za, zb, zc, zd)
end

function square:side(za, zb, swap)
	swap = (swap == "swap")
	if swap then
		-- Rotate zb clockwise around za to get zc
		local zc = rotation_(zb, math.pi / 2, za)
		-- Rotate za counterclockwise around zb to get zd
		local zd = rotation_(za, -math.pi / 2, zb)
		return square:new(za, zb, zc, zd)
	else
		-- Rotate zb counterclockwise around za to get zc
		local zc = rotation_(zb, -math.pi / 2, za)
		-- Rotate za clockwise around zb to get zd
		local zd = rotation_(za, math.pi / 2, zb)
		return square:new(za, zb, zc, zd)
	end
end

return square
