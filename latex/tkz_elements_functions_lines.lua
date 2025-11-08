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

-- Lignes équidistantes de deux droites L1 et L2
-- Cas parallèles : retourne la parallèle "milieu" (unique)
-- Cas sécantes  : retourne les deux bissectrices (interne, externe)
-- Paramètre optionnel 'choice' :
--   * nil     → retourne soit 1 ligne (parallèles) soit 2 lignes (sécantes)
--   * 1 ou 2  → ne retourner que la bissectrice #1 (u,v) ou #2 (up,vp)
function lines_equidistant_(L1, L2, choice)
	local a, b = L1:get()
	local c, d = L2:get()

	-- 1) Parallèles → ligne "milieu"
	if is_parallel_(a, b, c, d) then
		-- Distance entre deux parallèles (AB) et (CD)
		local D = distance_(a, b, c)
		-- Signe pour aller du côté de L2
		local s = (L1:side_line(c) >= 0) and 1 or -1
		-- La droite à mi-distance (unique)
		local mid = L1:collinear_at_distance(s * D / 2)
		return mid
	end

	-- 2) Sécantes → deux bissectrices passant par l'intersection
	local i = intersection_ll_(a, b, c, d)

	-- Points unité depuis i sur chaque droite, dans les deux orientations
	-- NB : report_(pa, pb, ±1, i) → point sur (AB) à distance unitaire depuis i
	local u  = report_(a, b,  1, i)
	local up = report_(a, b, -1, i)
	local v  = report_(c, d,  1, i)
	local vp = report_(c, d, -1, i)

	-- Les bissectrices sont les médiatrices de [u v] et [up vp]
	local p1, q1 = mediator_(u,  v)   -- bissectrice #1 (interne)
	local p2, q2 = mediator_(v, up)  -- bissectrice #2 (externe)

	local B1 = line:new(p1, q1)
	local B2 = line:new(p2, q2)

	if choice == 1 then return B1 end
	if choice == 2 then return B2 end
	return B1, B2
end

