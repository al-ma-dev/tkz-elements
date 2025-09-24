-- File: tkz_elements_occs.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

occs = {}
occs.__index = occs
function occs:new(L, zO)
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
function occs:coordinates(pt) -- S,U,V orthonormé
	local xs = self.origin.re
	local ys = self.origin.im
	local x = pt.re
	local y = pt.im
	local xsu = (self.x - self.origin).re
	local ysu = (self.x - self.origin).im
	local xsv = (self.y - self.origin).re
	local ysv = (self.y - self.origin).im
	local xxs = x - xs
	local yys = y - ys
	return (xsu * xxs + ysu * yys), (xsv * xxs + ysv * yys)
end

return occs
