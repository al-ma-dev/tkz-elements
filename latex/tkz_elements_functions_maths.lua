-- File: tkz_elements_functions_maths.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

---------------------------------------------------------------------------
---- numbers --------------------------------------------------------------
---------------------------------------------------------------------------

function is_integer_(x)
	return type(x) == "number"
		and x == x -- exclude NaN
		and x < math.huge -- exclude +inf
		and -x < math.huge -- exclude -inf
		and is_zero_(x - tkz_round_(x))
end

function near_integer_(x)
	return is_zero_(x % 1)
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
		return x -- table ou invalide
	end
end

function tkz_round_(num, idp)
	idp = idp or 0
	local mult = 10 ^ idp
	return math.floor(num * mult + 0.5) / mult
end

---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

function dot_product_(z1, z2, z3)
	return (z2 - z1) .. (z3 - z1)
end

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
function orient2d_(x1, y1, x2, y2, x3, y3)
	return (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)
end

function det3pt_(z1, z2, z3)
	return orient2d_(z1.re, z1.im, z2.re, z2.im, z3.re, z3.im)
end
--orient2d or det3pt determinant de 3 pts

function islinear_(z1, z2, z3)
	return math.abs(det3pt_(z1, z2, z3)) < tkz.epsilon
end
is_linear_ = islinear_

function isortho_(z1, z2, z3)
	return math.abs((z2 - z1) .. (z3 - z1)) < tkz.epsilon
end

function parabola_(xa, ya, xb, yb, xc, yc) -- added
	local D = (xa - xb) * (xa - xc) * (xb - xc)
	local A = (xc * (yb - ya) + xb * (ya - yc) + xa * (yc - yb)) / D
	local B = (xc * xc * (ya - yb) + xb * xb * (yc - ya) + xa * xa * (yb - yc)) / D
	local C = (xb * xc * (xb - xc) * ya + xc * xa * (xc - xa) * yb + xa * xb * (xa - xb) * yc) / D
	return A, B, C
end

---------------------------------------------------------------------------
---- angles -------------------------------------
---------------------------------------------------------------------------

function get_angle_(a, b, c)
	if (b == a) or (c == a) then
		tex.error("Points confused in get_angle_")
	end
	return point.arg((c - a) / (b - a))
end

function get_angle_normalize_(a, b, c)
	return angle_normalize_(get_angle_(a, b, c))
end

function angle_normalize_(a)
	while a < 0 do
		a = a + 2 * math.pi
	end
	while a >= 2 * math.pi do
		a = a - 2 * math.pi
	end
	return a
end

function tkz_angle_between_vectors_(a, b, c, d)
	-- Vector calculation
	local zab = b - a
	local zcd = d - c

	-- Angle between vectors using ratio argument
	local theta = math.atan(-zab.im * zcd.re + zab.re * zcd.im, zab.re * zcd.re + zab.im * zcd.im)

	return theta -- Angle in radians
end
---------------------------------------------------------------------------
-------------------------- end angles --------------------------
---------------------------------------------------------------------------

---------------------------------------------------------------------------
----------------- display  ---------------------------------------
-----------------------------------------------------
-- Optionnel : si utils.sign n’est pas fiable, utilise ceci
local function sign(x)
	if x > 0 then
		return "+"
	end
	if x < 0 then
		return "-"
	end
	return ""
end

function tkz_display_(z)
	local function display_real(r)
		if r == nil then
			return ""
		end
		local fmt
		if near_integer_(r) then
			r = math.round(r)
			fmt = "%.0f"
		else
			fmt = string.format("%%.%df", tkz.dc)
		end
		return string.format(fmt, r)
	end

	local function display_imag(r, force_sign)
		local sgn = sign(r)
		if not force_sign and sgn == "+" then
			sgn = "" -- supprime le + quand il n’est pas nécessaire
		end
		r = math.abs(r)
		local part
		if math.abs(r - 1) < tkz.epsilon then
			part = "" -- afficher juste "i" ou "-i"
		elseif near_integer_(r) then
			part = tostring(math.round(r))
		else
			part = display_real(r)
		end
		return sgn, part
	end

	if type(z) == "number" then
		return display_real(z)
	end

	local re, im = z.re, z.im
	local is_re_zero = is_zero_(re)
	local is_im_zero = is_zero_(im)

	if is_re_zero and is_im_zero then
		return "0"
	end

	if is_im_zero then
		return display_real(re)
	end

	if is_re_zero then
		local sgn, part = display_imag(im, false)
		return sgn .. part .. "i"
	end

	-- Cas général : partie réelle et partie imaginaire
	local str_re = display_real(re)
	local sgn_im, part_im = display_imag(im, true)
	return str_re .. sgn_im .. part_im .. "i"
end

---   end display  ---------------------------------------
---------------------------------------------------
