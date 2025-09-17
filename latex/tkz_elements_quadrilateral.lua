-- File: tkz_elements_quadrilateral.lua
-- Version: 4.20c   Date: 2025/09/17
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

quadrilateral = {}
quadrilateral.__index = quadrilateral
function quadrilateral:new(za, zb, zc, zd)
	local type = "quadrilateral"
	local a = point.abs(zb - za)
	local b = point.abs(zc - zb)
	local c = point.abs(zd - zc)
	local d = point.abs(za - zd)
	local ab = line(za, zb)
	local bc = line(zb, zc)
	local cd = line(zc, zd)
	local da = line(zd, za)
	local ac = line(za, zc)
	local bd = line(zb, zd)
	local i = intersection_ll_(za, zc, zb, zd)
	local g = barycenter_({ za, 1 }, { zb, 1 }, { zc, 1 }, { zd, 1 })
	local o = {
		pa = za,
		pb = zb,
		pc = zc,
		pd = zd,
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
		i = i,
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
function quadrilateral:get()
	return self.pa, self.pb, self.pc, self.pd
end

function quadrilateral:iscyclic()
	local d
	local alpha = point.arg((self.pd - self.pa) / (self.pb - self.pa))
	local beta = point.arg((self.pb - self.pc) / (self.pd - self.pc))
	if math.abs(alpha + beta - math.pi) < tkz.epsilon then
		return true
	else
		return false
	end
end

function quadrilateral:poncelet_point()
	local A, B, C, D = self.pa, self.pb, self.pc, self.pd
	local e1 = euler_center_(A, B, C)
	local e2 = euler_center_(A, C, D)
	local m = midpoint_(A, C)
	local x, y = intersection_cc_(e1, m, e2, m)

	if not x or not y then
		tex.error("Poncelet construction failed: no intersection.")
	end

	if x == m then
		return y
	else
		return x
	end
end

return quadrilateral
