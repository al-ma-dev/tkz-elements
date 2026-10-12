-- File: tkz_elements_functions_lines.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

function length_(a, b)
	return point.abs(a - b)
end

function distance_(a, b, pt)
	return point.mod(projection_(a, b, pt) - pt)
end

function line_in_out_(a, b, pt)
	return math.abs((b - a) ^ (pt - a)) <= tkz.epsilon
end

-- --- core predicate (EPS-aware) -----------------------------------
function line_on_(a, b, pt, EPS)
	EPS = EPS or tkz.epsilon
	return math.abs((b - a) ^ (pt - a)) <= EPS
end

function on_segment_(a, b, pt, EPS)
	EPS = EPS or tkz.epsilon
	return point.mod(pt - a) + point.mod(pt - b) - point.mod(b - a) <= EPS
end
-- Parameter of orthogonal projection of pt on the oriented line pa->pb
-- t < 0 : before pa ; 0..1 : between ; t > 1 : after pb
function line:abscissa_(pt, EPS)
	EPS = EPS or tkz.epsilon
	local a, b = self.pa, self.pb
	local u = b - a
	local uu = (u .. u) -- dot product
	if uu <= EPS*EPS then
		error("tkz-elements: abscissa: the line is degenerate (its two points are identical)", 2)
	end
	local t = ((pt - a) .. u) / uu
	return t
end

function is_parallel_(pa, pb, pc, pd)
	local det = (pb - pa) ^ (pd -pc)
	return math.abs(det) < tkz.epsilon
end

function is_orthogonal_(pa, pb, pc, pd)
	local dot = (pb - pa) .. (pd - pc)
	return math.abs(dot) < tkz.epsilon
end

function is_equidistant_(a, b, p)
	return math.abs(point.mod(a - p) - point.mod(b - p)) < tkz.epsilon
end

---------------------------------------------------------------------------
--                 Lines
---------------------------------------------------------------------------
function normalize_(a, b)
	return a + (b - a) / point.mod(b - a)
end

function ortho_from_(p, a, b)
	return p + (b - a) * point(0, 1)
end

function ll_from_(p, a, b)
	return p + b - a
end

function slope_(a, b)
	return angle_normalize_(point.arg(b - a))
end

function gold_segment_(a, b)
	return a + (b - a) * tkz.invphi
end

function online_(a, b, t)
	return barycenter_({ a, (1 - t) }, { b, t })
end

function mediator_(a, b)
	local m = midpoint_(a, b)
	return m, rotation_(m, math.pi / 2, b)
end

-- triangle specific
function equilateral_tr_(a, b)
	return rotation_(a, math.pi / 3, b)
end

function isosceles_right_tr_(a, b)
	local pt = rotation_(a, math.pi / 4, b)
	return a + (pt - a) * math.sin(math.pi / 4)
end

function gold_tr_(a, b)
	local pt = rotation_(a, math.pi / 2, b)
	return a + (pt - a) * tkz.invphi
end

function euclide_tr_(a, b)
	return rotation_(a, math.pi / 5, b)
end

function golden_tr_(a, b)
	local pt = rotation_(a, tkz.tau / 5, b)
	return a + (pt - a) * tkz.phi
end

function div_harmonic_int_(a, b, n)
	local k = point.abs(a - n) / point.abs(b - n)
	return barycenter_({ a, 1 }, { b, k })
end

function div_harmonic_ext_(a, b, n)
	local k = point.abs(a - n) / point.abs(b - n)
	return barycenter_({ a, 1 }, { b, -k })
end

function div_harmonic_both_(a, b, k)
	return barycenter_({ a, 1 }, { b, k }), barycenter_({ a, 1 }, { b, -k })
end

function golden_ratio_(a, b)
	return a + (b - a) * tkz.invphi
end

function projection_(pa, pb, pt)
	if is_linear_(pa, pb, pt) then
		return pt
	else
		local v = pb - pa
		local z = ((pt - pa) .. v) / (v .. v) -- .. dot product
		return pa + z * v
	end
end

function projection_ll(Dt1, Dt2, pt)
	return projection_ll_(Dt1.pa, Dt1.pb, Dt2.pa, Dt2.pb, pt)
end

function projection_ll_(pa, pb, pc, pd, pt)
	if is_linear_(pa, pb, pt) then
		return pt
	else
		local m = ll_from_(pt, pc, pd)
		return intersection_ll_(pt, m, pa, pb)
	end
end

function affinity_(pa, pb, pc, pd, k, pt)
	local p = projection_ll_(pa, pb, pc, pd, pt)
	return homothety_(p, k, pt)
end

function symmetry_axial_(pa, pb, pt)
	local p = projection_(pa, pb, pt)
	return symmetry_(p, pt)
end

reflection_ = symmetry_axial_

function square_(a, b)
	return rotation_(b, -math.pi / 2, a), rotation_(a, math.pi / 2, b)
end

function in_segment_(a, b, pt)
	return point.mod(pt - a) + point.mod(pt - b) - point.mod(b - a) <= tkz.epsilon
end

function report_(za, zb, d, pt)
	local len = point.mod(zb - za)
	local t = d / len
	local result = barycenter_({ za, 1 - t }, { zb, t })

	if pt then
		return result + pt - za
	else
		return result
	end
end

function collinear_at_(za, zb, pt, k)
	if k then
		return pt + k * (zb - za)
	else
		return pt + (zb - za)
	end
end

function orthogonal_at_(za, zb, pt, k)
	if k then
		return pt + k * (zb - za) * point(0, 1)
	else
		return pt + (zb - za) * point(0, 1)
	end
end


function collinear_at_distance_(za, zb, d)
	 local len = point.mod(zb - za)
	 local x = orthogonal_at_(za, zb, za, d / len)
	 return x, collinear_at_(za, zb, x, 1)
end


-- Lines equidistant from two lines L1 and L2
-- Parallel lines: returns the "middle" parallel (unique)
-- Secant lines: returns the two bisectors
-- Optional parameter 'choice':
--   * nil    -> 1 line (parallel lines) or 2 lines (secant lines)
--   * 1 or 2 -> only the bisector #1 (u,v) or #2 (up,vp)
function lines_equidistant_(L1, L2, choice)
	local a, b = L1:get()
	local c, d = L2:get()

	-- 1) Parallel lines -> "middle" line
	if is_parallel_(a, b, c, d) then
		-- Distance between the parallel lines (AB) and (CD)
		local D = distance_(a, b, c)
		-- Sign, towards L2
		local s = (L1:side_line(c) >= 0) and 1 or -1
		-- The line halfway (unique)
		local mid = L1:collinear_at_distance(s * D / 2)
		return mid
	end

	-- 2) Secant lines -> two bisectors through the intersection
	local i = intersection_ll_(a, b, c, d)

	-- Unit points from i on each line, in both directions
	-- NB: report_(pa, pb, +-1, i) -> point on (AB) at distance 1 from i
	local u  = report_(a, b,  1, i)
	local up = report_(a, b, -1, i)
	local v  = report_(c, d,  1, i)
	local vp = report_(c, d, -1, i)

	-- The bisectors are the perpendicular bisectors of [u v] and [v up]
	local p1, q1 = mediator_(u,  v)   -- bisector #1
	local p2, q2 = mediator_(v, up)  -- bisector #2

	local B1 = line:new(p1, q1)
	local B2 = line:new(p2, q2)

	if choice == 1 then return B1 end
	if choice == 2 then return B2 end
	return B1, B2
end

