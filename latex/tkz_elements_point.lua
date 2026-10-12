-- File: tkz_elements_point.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- local definition of class

local function class(init, base)
	local cls = {}
	cls.__index = cls

	if base then
		cls._base = base
	end

	-- One single metatable: __index (inheritance) and __call (constructor).
	-- Two successive setmetatable calls would discard the inheritance.
	setmetatable(cls, {
		__index = base,
		__call = function(class_tbl, ...)
			local self = setmetatable({}, class_tbl)
			if init then
				init(self, ...)
			elseif base and base.init then
				base.init(self, ...)
			end
			return self
		end,
	})

	cls.init = init

	function cls:is_a(klass)
		local m = getmetatable(self)
		while m do
			if m == klass then
				return true
			end
			m = m._base
		end
		return false
	end

	return cls
end

-- -------------------------------------------------------------------
-- helpers
-- -------------------------------------------------------------------
-- error() stops the current Lua chunk and reports the line of the
-- caller (level), whereas tex.error() lets the computation go on with
-- invalid values (nil, nan) that fail later with obscure messages.
local function point_error(msg, level)
	error("tkz-elements (point): " .. msg, (level or 1) + 1)
end

point = class(function(p, re, im)
	-- case 1: standard call point(x, y)
	if type(re) == "number" and type(im) == "number" then
		p.re = re
		p.im = im

	-- case 2: internal call point({re=..., im=...})
	elseif type(re) == "table" and im == nil and type(re.re) == "number" and type(re.im) == "number" then
		p.re = re.re
		p.im = re.im

	-- invalid case
	else
		-- level 3: init -> __call -> user code
		point_error(
			"invalid point: expected two numbers or a table {re=..., im=...}, got ("
				.. type(re) .. " " .. tostring(re) .. ", "
				.. type(im) .. " " .. tostring(im) .. ")",
			3
		)
	end

	p.type = "point"
	p.argument = math.atan(p.im, p.re)
	p.modulus = math.sqrt(p.re * p.re + p.im * p.im)
	p.mtx = matrix:new({ { p.re }, { p.im } })
end)

local sqrt = math.sqrt
local cos = math.cos
local sin = math.sin
local exp = math.exp
local atan = math.atan
local min = math.min
local max = math.max
local abs = math.abs


local function is_point(obj)
	if type(obj) ~= "table" then
		return false
	end
	local mt = getmetatable(obj)
	while mt do
		if mt == point then
			return true
		end
		mt = mt._base
	end
	return false
end

local function topoint(z, level)
	if is_point(z) then
		return z
	elseif type(z) == "number" then
		return point(z, 0)
	elseif type(z) == "table" and type(z.re) == "number" and type(z.im) == "number" then
		return point(z) -- It is managed in the constructor.
	else
		-- level: 1 topoint, 2 its caller, 3 the user code
		point_error("cannot convert a " .. type(z) .. " (" .. tostring(z) .. ") to a point", level or 3)
	end
end


local function check(z1, z2)
	-- one more level: topoint <- check <- metamethod <- user code
	return topoint(z1, 4), topoint(z2, 4)
end
-- -------------------------------------------------------------------
-- metamethods
-- -------------------------------------------------------------------
-- redefine arithmetic operators!
function point.__add(z1, z2)
	local c1, c2 = check(z1, z2)
	return point(c1.re + c2.re, c1.im + c2.im)
end

function point.__sub(z1, z2)
	local c1, c2 = check(z1, z2)
	return point(c1.re - c2.re, c1.im - c2.im)
end

function point.__unm(z)
	local z = topoint(z)
	return point(-z.re, -z.im)
end

function point.__mul(z1, z2)
	local c1, c2 = check(z1, z2)
	return point(c1.re * c2.re - c1.im * c2.im, c1.im * c2.re + c1.re * c2.im)
end

-- dot product is '..'  (a+ib) (c-id) = ac+bd + i(bc-ad)
function point.__concat(z1, z2)
	local c1, c2 = check(z1, z2)
	local z = c1 * point.conj(c2)
	return z.re
end

-- determinant  is '^'   (a-ib) (c+id) = ac+bd + i(ad - bc)
function point.__pow(z1, z2)
	local c1, c2 = check(z1, z2)
	local z = point.conj(c1) * c2
	return z.im
end

function point.__div(z1, z2)
	local c1, c2 = check(z1, z2)
	local den = c2.re * c2.re + c2.im * c2.im
	if den == 0 then
		point_error("division by zero", 2)
	end
	return point((c1.re * c2.re + c1.im * c2.im) / den, (c1.im * c2.re - c1.re * c2.im) / den)
end

-- Display of a point: the formatter is tkz_display_ (functions_maths),
-- shared with tkz.display().
function point.__tostring(z)
	return tkz_display_(z)
end

-- Lua has no __tonumber metamethod: tonumber(z) always returns nil.
-- Use z:tonumber() instead.
function point.tonumber(z)
	if math.abs(z.im) < tkz.epsilon then
		return z.re
	else
		return nil
	end
end
point.__tonumber = point.tonumber -- kept for compatibility

-- Two points are equal when their distance is less than tkz.epsilon.
-- The same criterion is used by == and by identity().
function point:identity(pt)
	return point.abs(self - pt) < tkz.epsilon
end

function point.__eq(z1, z2)
	if not (is_point(z1) and is_point(z2)) then
		return false
	end
	return z1:identity(z2)
end

-- -------------------------------------------------------------------
local function pyth(a, b)
	if a == 0 and b == 0 then
		return 0
	end
	a, b = abs(a), abs(b)
	a, b = max(a, b), min(a, b)
	return a * sqrt(1 + (b / a) ^ 2)
end

function point.conj(z)
	local cx = topoint(z)
	return point(cx.re, -cx.im)
end

function point.mod(z)
	local cx = topoint(z)
	return pyth(cx.re, cx.im)
end

function point.abs(z)
	local cx = topoint(z)
	local function sqr(x)
		return x * x
	end
	return sqrt(sqr(cx.re) + sqr(cx.im))
end

function point.norm(z)
	local cx = topoint(z)
	local function sqr(x)
		return x * x
	end
	return (sqr(cx.re) + sqr(cx.im))
end

function point.power(z, n)
	if type(z) == "number" then
		return z ^ n
	else
		local m = z.modulus ^ n
		local a = angle_normalize_(z.argument * n)
		return polar_(m, a)
	end
end

function point.arg(z)
	local cx = topoint(z)
	return math.atan(cx.im, cx.re)
end

function point.get(z)
	return z.re, z.im
end

function point.sqrt(z)
	local cx = topoint(z)
	local len = math.sqrt(cx.re ^ 2 + cx.im ^ 2)
	local sign = (cx.im < 0 and -1) or 1
	return point(math.sqrt((cx.re + len) / 2), sign * math.sqrt((len - cx.re) / 2))
end

-- ============= methods =========--
-- the result is a point
function point:new(a, b)
	return point(a, b)
end

function point:polar(radius, phi)
	return point:new(radius * math.cos(phi), radius * math.sin(phi))
end

function point:polar_deg(radius, phi)
	return polar_(radius, phi * math.pi / 180)
end

function point:north(d)
	local d = d or 1
	return self + polar_(d, math.pi / 2)
end

function point:south(d)
	local d = d or 1
	return self + polar_(d, 3 * math.pi / 2)
end

function point:east(d)
	local d = d or 1
	return self + polar_(d, 0)
end

function point:west(d)
	local d = d or 1
	return self + polar_(d, math.pi)
end

function point:length(p)
	return length_(self, p)
end

function point:normalize()
	local d = point.abs(self)
	if d < tkz.epsilon then
		point_error("normalize: null vector (point too close to the origin)", 2)
	end
	return point(self.re / d, self.im / d)
end

function point:normalize_from(p)
	local u = (self - p)
	local d = point.abs(u)
	if d < tkz.epsilon then
		point_error("normalize_from: the two points are identical", 2)
	end
	return point(u.re / d, u.im / d) + p
end

-- function point:identity(pt)
-- 	return point.abs(self - pt) < tkz.epsilon
-- end

function point:orthogonal(d)
	local m
	if d == nil then
		-- If no scaling factor d is provided, return the point rotated 90 degrees counterclockwise
		return point(-self.im, self.re)
	else
		m = point.mod(self) -- Get the modulus (magnitude) of the current point
		if m < tkz.epsilon then
			point_error("orthogonal: null vector, the direction is undefined", 2)
		end
		return point(-self.im * d / m, self.re * d / m) -- Return the scaled orthogonal point
	end
end

function point:at(z)
	return point(self.re + z.re, self.im + z.im)
end

function point:print()
	tex.print(tostring(self))
end

-- Shift the current point A orthogonally to the direction (AB)
-- by a distance "dist"
function point:shift_orthogonal_to(B, dist)
	local v = B - self             -- vector AB
	local n = v:orthogonal(dist)   -- normal vector of norm "dist"
	return n:at(self)              -- shifts A
end

-- Shift the current point A along the direction (AB)
-- by a distance "dist"
function point:shift_collinear_to(B, dist)
	local v = (B - self):normalize()
	return self + v*dist
end

function point:scale(kx, ky)
	ky = ky or kx
	return point(kx * self.re, ky * self.im)
end
-- =====================
-- the result is a circle
-- =====================
-- Circle through three points: self, a, b
-- (circumcircle). Returns:
--   PA_center : path of the centers
--   PA_through: path of the "through" points
--   n         : number of circles found (0 if degenerate, 1 otherwise)
function point:PPP(a, b)
	-- output paths (always created, even if n = 0)
	local PA_center = path()
	local PA_through = path()

	-- degenerate case: collinear points or two identical points
	if tkz.aligned(self, a, b)
		 or self:identity(a) or self:identity(b) or a:identity(b) then
		return PA_center, PA_through, 0
	end

	-- center of the circumcircle
	local o = circum_center_(self, a, b)

	-- "self" is used as the through point (any of the three would do)
	PA_center:add_point(o)
	PA_through:add_point(self)

	return PA_center, PA_through, 1
end

-- ----------------------------------------------------------------
-- transformations
-- ----------------------------------------------------------------
-- Central symmetry with center self.
-- z.B = z.O:symmetry(z.A) ; L.new = z.O:symmetry(L.AB)
-- z.B, L.new, T.new = z.O:symmetry(z.A, L.AB, T.ABC)
function point:symmetry(...)
	return map_objects_(function(p) return symmetry_(self, p) end, "point:symmetry", ...)
end

-- Rotation with center self (angle in radians).
function point:rotation(angle, ...)
	return map_objects_(function(p) return rotation_(self, angle, p) end, "point:rotation", ...)
end

-- Homothety with center self and ratio coeff.
function point:homothety(coeff, ...)
	return map_objects_(function(p) return homothety_(self, coeff, p) end, "point:homothety", ...)
end

-- "(x,y)" with fixed decimals, for TikZ
function point:to_tikz()
	return "(" .. checknumber_(self.re) .. "," .. checknumber_(self.im) .. ")"
end
