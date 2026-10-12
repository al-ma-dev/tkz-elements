-- File: tkz_elements_functions_maths.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

---------------------------------------------------------------------------
---- numbers --------------------------------------------------------------
---------------------------------------------------------------------------

function near_integer_(x)
	return is_zero_(x - tkz_round_(x))
end


function residue_(x)
	return x % 1
end

function is_zero_(x)
	return math.abs(x) < tkz.epsilon
end

function set_zero_(x)
	return is_zero_(x) and 0 or x
end

function checknumber_(x)
	if type(x) == "number" then
		return string.format("%.12f", x)
	elseif type(x) == "string" and tonumber(x) then
		return string.format("%.12f", tonumber(x))
	else
		return x -- table or invalid value: returned unchanged
	end
end

-- Round to idp decimals (default 0).
-- Rounding is symmetric: tkz_round_(-2.5) == -3, tkz_round_(2.5) == 3.
-- An integral result is returned as a Lua integer (3, not 3.0), so that
-- tostring() and \tkzUseLua print "3".
-- Note: 1.005 is stored as 1.00499999... in binary, so
-- tkz_round_(1.005, 2) gives 1.0, as in every language using doubles.
function tkz_round_(num, idp)
	idp = idp or 0
	local mult = 10 ^ idp
	local r = math.floor(math.abs(num) * mult + 0.5) / mult
	if num < 0 then
		r = -r
	end
	return math.tointeger(r) or r
end

---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

--[[
  Computes the 2D cross product (signed area) of the triangle
  formed by (x1,y1), (x2,y2), and (x3,y3).

  Returns:
    > 0 if counter-clockwise (left turn),
    < 0 if clockwise (right turn),
    = 0 if colinear.

  Equivalent to the imaginary part of the complex wedge product:
    (z2 - z1) ^ (z3 - z1)
  where zi = xi + i*yi
--]]
function oriented_area2_(x1, y1, x2, y2, x3, y3)
	return (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)
end


function det3pt_(z1, z2, z3)
	return oriented_area2_(z1.re, z1.im, z2.re, z2.im, z3.re, z3.im)
end
-- det3pt_ (orient2d): determinant of three points

function is_linear_(z1, z2, z3)
	return math.abs(det3pt_(z1, z2, z3)) < tkz.epsilon
end

-- Parabola y = Ax^2 + Bx + C through three points (nil if two abscissas are equal)
function parabola_(xa, ya, xb, yb, xc, yc)
	local D = (xa - xb) * (xa - xc) * (xb - xc)
	if math.abs(D) < tkz.epsilon then
		return nil
	end

	local A = (xc * (yb - ya) + xb * (ya - yc) + xa * (yc - yb)) / D
	local B = (xc * xc * (ya - yb) + xb * xb * (yc - ya) + xa * xa * (yb - yc)) / D
	local C = (xb * xc * (xb - xc) * ya + xc * xa * (xc - xa) * yb + xa * xb * (xa - xb) * yc) / D
	return A, B, C
end

---------------------------------------------------------------------------
---- angles -------------------------------------
---------------------------------------------------------------------------

-- Oriented angle BAC at A, in (-pi, pi]
function get_angle_(a, b, c)
	if point.abs(b - a) < tkz.epsilon or point.abs(c - a) < tkz.epsilon then
		error("tkz-elements: get_angle_: the angle is undefined (two points are identical)", 2)
	end
	-- vectors AB = b - a, AC = c - a
	-- arg(AC / AB) = angle that turns AB onto AC
	return point.arg((c - a) / (b - a))
end


function inner_angle_(a, b, c)
	return math.abs(get_angle_(a, b, c))
end


function angle_normalize_(a)
	return a % tkz.tau  -- in [0, 2pi)
end


function get_angle_normalize_(a, b, c)
	return angle_normalize_(get_angle_(a, b, c))
end



-- Oriented angle from vector AB to vector CD, in (-π, π].
function tkz_angle_between_vectors_(a, b, c, d)
	local zab = b - a
	local zcd = d - c
	if point.abs(zab) < tkz.epsilon or point.abs(zcd) < tkz.epsilon then
		error("tkz-elements: angle_between_vectors: null vector, the angle is undefined", 3)
	end
	-- y = det(AB, CD), x = AB . CD
	local y = zab.re * zcd.im - zab.im * zcd.re
	local x = zab.re * zcd.re + zab.im * zcd.im
	return math.atan(y, x) -- atan2 (Lua 5.3): result in (-π, π]
end


---------------------------------------------------------------------------
-------------------------- end angles --------------------------
---------------------------------------------------------------------------

---------------------------------------------------------------------------
----------------- display  ---------------------------------------
-----------------------------------------------------
-- Format a real number:
--   * an integer (up to tkz.epsilon) is written without decimals;
--   * other values are written with tkz.dc decimals;
--   * a value that rounds to zero is written "0" (never "-0.00").
local function format_real_(r)
	if near_integer_(r) then
		return string.format("%d", tkz_round_(r))
	end
	local str = string.format("%." .. tkz.dc .. "f", r)
	if tonumber(str) == 0 then
		return "0"
	end
	return str
end

-- Display a number or a point (complex number): 2, -1.50, i, -2i, 1+2i,
-- 1.50-0.25i. Used by tostring(point) and tkz.display().
-- The second argument (format) is accepted for compatibility and ignored.
function tkz_display_(z, _)
	if type(z) == "number" then
		return format_real_(z)
	end

	local re_str = format_real_(z.re)
	local im_str = format_real_(z.im)

	if im_str == "0" then
		return re_str
	end

	local im_part
	if im_str == "1" then
		im_part = "i"
	elseif im_str == "-1" then
		im_part = "-i"
	else
		im_part = im_str .. "i"
	end

	if re_str == "0" then
		return im_part
	elseif z.im > 0 then
		return re_str .. "+" .. im_part
	else
		return re_str .. im_part
	end
end
