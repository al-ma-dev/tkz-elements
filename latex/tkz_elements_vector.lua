-- File: tkz_elements_vectors.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- ----------------------------------------------------------------------------
vector = {}
vector.__index = vector
-- Constructor for creating a new vector
function vector:new(za, zb)
	local type = "vector"
	local mtx = matrix:new({ { za }, { zb } })
  local z = zb - za
	local dx = z.re
	local dy = z.im
	local norm = point.mod(z)
	local slope = angle_normalize_(point.arg(z))
local vect = {
			type  = "vector",
			tail  = za,
			head  = zb,
			z     = z,
			dx    = dx,
			dy    = dy,
			norm  = norm,
			slope = slope,
			mtx   = mtx,
	}

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

-- function vector.__mul(r, v)
	-- return v:scale(r)
-- end

function vector.__mul(a, b)
	local ta, tb = type(a), type(b)

	if ta == "number" and getmetatable(b) == vector then
		return b:scale(a)
	elseif tb == "number" and getmetatable(a) == vector then
		return a:scale(b)
	else
		error("vector.__mul: expected scalar * vector or vector * scalar")
	end
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
	return self.tail, self.head
end

function vector:is_zero(EPS)
	EPS = EPS or tkz.epsilon
	return self.norm <= EPS
end

function vector:is_parallel(v, EPS)
	EPS = EPS or tkz.epsilon
	if self:is_zero(EPS) or v:is_zero(EPS) then
		return false
	end
	return math.abs(self:cross(v)) <= EPS * self.norm * v.norm
end

function vector:is_orthogonal(v, EPS)
	EPS = EPS or tkz.epsilon
	if self:is_zero(EPS) or v:is_zero(EPS) then
		return false
	end
	return math.abs(self:dot(v)) <= EPS * self.norm * v.norm
end


-- Normalize the vector (unit vector)
-- function vector:normalize()
	-- local z = self.head - self.tail
	-- local d = point.abs(z)
	-- local nz = point(z.re / d, z.im / d)
	-- return vector:new(self.tail, nz + self.tail)
-- end

function vector:dot(v)
	local z = point.conj(self.z) * v.z
	return z.re
end

function vector:cross(v)
	local z = point.conj(self.z) * v.z
	return z.im
end



function vector:normalize()
	if self.norm == 0 then
		tex.error("Cannot normalize the zero vector.")
		return self
	end
	local u = point(self.z.re / self.norm, self.z.im / self.norm)
	return vector:new(self.tail, self.tail + u)
end


function vector:add(ve)
	return vector:new(self.tail, self.head + ve.head - ve.tail)
end



-- --Scale the vector by a scalar
-- function vector:scale(d)
	-- local z = scale_(self, d)
	-- return vector:new(self.tail, z)
-- end

function vector:scale(d)
	local new_head = scaled_head_(self, d)
	return vector:new(self.tail, new_head)
end


function vector:at(zc)
	return vector:new(zc, zc + self.head - self.tail)
end

function vector:angle_to(v)
	-- angle entre self et v (orienté)
	return angle_normalize_(v.slope - self.slope)
end

function vector:rotate(theta)
	return vector:new(self.tail, rotation_(self.tail, theta, self.head))
end

-- side = "ccw" (par défaut) ou "cw"
-- length optionnel pour imposer la norme du vecteur orthogonal
function vector:orthogonal(side, length)
	local theta = (side == "cw") and -math.pi / 2 or math.pi / 2

	if length then
		-- on part du vecteur normalisé, on impose sa norme, puis on tourne
		local u = self:normalize()
		local tip = self.tail + point(length * math.cos(u.slope), length * math.sin(u.slope))
		return vector:new(self.tail, rotation_(self.tail, theta, tip))
	else
		return self:rotate(theta)
	end
end

return vector
