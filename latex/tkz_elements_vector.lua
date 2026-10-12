-- File: tkz_elements_vector.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- ----------------------------------------------------------------------------
vector = {}
vector.__index = vector

local function is_point(p)
	return type(p) == "table" and p.type == "point"
end

local function is_vector(v)
	return getmetatable(v) == vector
end

-- Constructor: vector from the tail za to the head zb
function vector:new(za, zb)
	if not (is_point(za) and is_point(zb)) then
		-- level 2: __call is a tail call (return cls:new(...))
		error("tkz-elements: vector: two points are expected", 2)
	end
	local z = zb - za
	local norm = point.mod(z)
	local slope = 0
	if norm > tkz.epsilon then
		slope = angle_normalize_(point.arg(z))
	end
	local vect = {
		type  = "vector",
		tail  = za,
		head  = zb,
		z     = z,
		dx    = z.re,
		dy    = z.im,
		norm  = norm,
		slope = slope,
		mtx   = matrix:new({ { za }, { zb } }),
	}
	setmetatable(vect, self)
	return vect
end

setmetatable(vector, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

-- ----------------------------------------------------------------------------
-- metamethods
-- ----------------------------------------------------------------------------
local function check_vectors(v1, v2, op)
	if not (is_vector(v1) and is_vector(v2)) then
		error("tkz-elements: vector: " .. op .. " of two vectors expected", 3)
	end
end

function vector.__add(v1, v2)
	check_vectors(v1, v2, "sum")
	return v1:add(v2)
end

function vector.__sub(v1, v2)
	check_vectors(v1, v2, "difference")
	return v1:add(v2:scale(-1))
end

function vector.__unm(v)
	return v:scale(-1)
end

function vector.__mul(a, b)
	if type(a) == "number" and is_vector(b) then
		return b:scale(a)
	elseif type(b) == "number" and is_vector(a) then
		return a:scale(b)
	else
		error("tkz-elements: vector: scalar * vector or vector * scalar expected", 2)
	end
end

-- v1 ^ v2 : cross product (determinant)
function vector.__pow(v1, v2)
	check_vectors(v1, v2, "^")
	return v1:cross(v2)
end

-- v1 .. v2 : dot product
function vector.__concat(v1, v2)
	check_vectors(v1, v2, "..")
	return v1:dot(v2)
end

-- ----------------------------------------------------------------------------
-- methods
-- ----------------------------------------------------------------------------
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

function vector:dot(v)
	local z = point.conj(self.z) * v.z
	return z.re
end

function vector:cross(v)
	local z = point.conj(self.z) * v.z
	return z.im
end

-- unit vector with the same tail
function vector:normalize()
	if self.norm <= tkz.epsilon then
		error("tkz-elements: vector:normalize: cannot normalize a null vector", 2)
	end
	local u = point(self.z.re / self.norm, self.z.im / self.norm)
	return vector:new(self.tail, self.tail + u)
end

function vector:add(ve)
	return vector:new(self.tail, self.head + ve.head - ve.tail)
end

function vector:scale(d)
	return vector:new(self.tail, scaled_head_(self, d))
end

function vector:at(zc)
	return vector:new(zc, zc + self.head - self.tail)
end

-- oriented angle from self to v
function vector:angle_to(v)
	return angle_normalize_(v.slope - self.slope)
end

function vector:rotate(theta)
	return vector:new(self.tail, rotation_(self.tail, theta, self.head))
end

-- side   = "ccw" (default) or "cw"
-- length : optional norm of the orthogonal vector
function vector:orthogonal(side, length)
	local theta = (side == "cw") and -math.pi / 2 or math.pi / 2
	if length then
		if self.norm <= tkz.epsilon then
			error("tkz-elements: vector:orthogonal: null vector", 2)
		end
		-- unit vector scaled to the given length, then rotated
		local tip = self.tail + point(length * math.cos(self.slope), length * math.sin(self.slope))
		return vector:new(self.tail, rotation_(self.tail, theta, tip))
	else
		return self:rotate(theta)
	end
end

return vector
