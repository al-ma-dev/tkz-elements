-- File: tkz_elements_vectors.lua
-- Version: 4.21c   Date: 2025/09/21
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- ----------------------------------------------------------------------------
vector = {}
vector.__index = vector
-- Constructor for creating a new vector
function vector:new(za, zb)
	local type = "vector"
	local slope = angle_normalize_(point.arg(zb - za))
	local norm = point.mod(zb - za)
	local mtx = matrix:new({ { za }, { zb } })
	local z = zb - za
	local vect = { tail = za, head = zb, z = z, norm = norm, mtx = mtx, slope = slope, type = type }
	setmetatable(vect, self)
	return vect
end

setmetatable(vector, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

function vector.__add(v1, v2)
	return v1:add(v2)
end

function vector.__sub(v1, v2)
	return v1:add(v2:scale(-1))
end

function vector.__unm(v)
	return v:scale(-1)
end

function vector.__mul(r, v)
	return v:scale(r)
end

function vector.__pow(v1, v2)
	local z
	z = point.conj(v1.head - v1.tail) * (v2.head - v2.tail)
	return z.im
end

function vector.__concat(v1, v2)
	local z
	z = (v1.head - v1.tail) * point.conj((v2.head - v2.tail))
	return z.re
end

function vector:get()
	return self.pa, self.pb
end

-- Normalize the vector (unit vector)
function vector:normalize()
	local z = self.head - self.tail
	local d = point.abs(z)
	local nz = point(z.re / d, z.im / d)
	return vector:new(self.tail, nz + self.tail)
end

function vector:add(ve)
	return vector:new(self.tail, self.head + ve.head - ve.tail)
end
-- Create an orthogonal vector
function vector:orthogonal(d)
	if d == nil then
		return vector:new(self.tail, rotation_(self.tail, math.pi / 2, self.head))
	else
		local z = self.tail + point(d * math.cos(self.slope), d * math.sin(self.slope))
		return vector:new(self.tail, rotation_(self.tail, math.pi / 2, z))
	end
end

--Scale the vector by a scalar
function vector:scale(d)
	local z = scale_(self, d)
	return vector:new(self.tail, z)
end

function vector:at(zc)
	return vector:new(zc, zc + self.head - self.tail)
end

return vector
