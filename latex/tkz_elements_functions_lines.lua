-- File: tkz_elements_functions_lines.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

function length_(a, b)
	return point.abs(a - b)
end

function distance_(a, b, pt)
	return point.mod(projection_(a, b, pt) - pt)
end

function line_in_out_(a, b, pt)
	return math.abs((pt - a) ^ (pt - b)) <= tkz.epsilon
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

function midpoint_(z1, z2)
	return (z1 + z2) / 2
end
-- triangle specific
function equilateral_tr_(a, b)
	return rotation_(a, math.pi / 3, b)
end

function isosceles_right_tr(a, b)
	local pt = rotation_(a, math.pi / 4, b)
	return a + (pt - a) * math.sin(math.pi / 4)
end

function gold_tr(a, b)
	local pt = rotation_(a, math.pi / 2, b)
	return a + (pt - a) * tkz.invphi
end

function euclide_tr(a, b)
	return rotation_(a, math.pi / 5, b)
end

function golden_tr(a, b)
	local pt = rotation_(a, 2 * math.pi / 5, b)
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
-- projection
function projection(Dt, pt)
	return projection_(Dt.pa, Dt.pb, pt)
end

function projection_(pa, pb, pt)
	if islinear_(pa, pb, pt) then
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
	if islinear_(pa, pb, pt) then
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

function set_symmetry_axial_(u, v, ...)
	local t = {}
	for _, value in ipairs({ ... }) do
		table.insert(t, symmetry_axial_(u, v, value))
	end
	return table.unpack(t)
end

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

function colinear_at_(za, zb, pt, k)
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

function bisector(a, b, c)
	local i = in_center_(a, b, c)
	return line:new(a, intersection_ll_(a, i, b, c))
end

function altitude(a, b, c)
	local o, p
	-- Get the orthocenter (which is the point of concurrency of the altitudes)
	o = ortho_center_(a, b, c)

	-- Get the perpendicular projection of point 'a' onto the line defined by 'b' and 'c'
	p = projection_(b, c, a)

	-- Return the altitude, which is the line from point 'a' to the point 'p'
	return line:new(a, p)
end

function bisector_ext(a, b, c) -- n=1 swap n=2 swap 2
	local i, p
	-- Get the incenter of the triangle
	i = in_center_(a, b, c)

	-- Rotate the incenter by 90 degrees around point 'a' to compute the external bisector
	p = rotation_(a, math.pi / 2, i)

	-- Return the external bisector as the line passing through points 'a' and 'p'
	return line:new(a, p)
end
-- orthonormal cartesian coordinate system
-- function occs_(p,za,zb)
--   local x = report_(za,zb,1,p)
--   local y = ortho_from_(p,p,x)
--   return x,y
-- end
