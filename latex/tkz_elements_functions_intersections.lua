-- File: tkz_elements_intersections.lua
-- Version: 4.20c   Date: 2025/09/17
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-------------------------------------------------------------------------
-- intersection of lines
-------------------------------------------------------------------------
function intersection_ll(la, lb)
	return intersection_ll_(la.pa, la.pb, lb.pa, lb.pb)
end
---------------------------------------------------------------------------
-- intersection of a line and a circle
---------------------------------------------------------------------------
function intersection_lc(D, C)
	return intersection_lc_(D.pa, D.pb, C.center, C.through)
end -- function
---------------------------------------------------------------------------
-- intersection of two circles
---------------------------------------------------------------------------
function intersection_cc(Ca, Cb)
	return intersection_cc_(Ca.center, Ca.through, Cb.center, Cb.through)
end -- function

--  line ellipse
function intersection_le(L, E)
	local a, b, c, d, t1, t2, z1, z2, A, B, Bx, By, Ax, Ay, Rx, Ry, sd
	A = (L.pa - E.center) * (point(math.cos(E.slope), -math.sin(E.slope)))
	B = (L.pb - E.center) * (point(math.cos(E.slope), -math.sin(E.slope)))
	Rx = E.Rx
	Ry = E.Ry
	Ax = A.re
	Ay = A.im
	Bx = B.re
	By = B.im
	a = Rx ^ 2 * (By - Ay) ^ 2 + Ry ^ 2 * (Bx - Ax) ^ 2
	b = 2 * Rx ^ 2 * Ay * (By - Ay) + 2 * Ry ^ 2 * Ax * (Bx - Ax)
	c = Rx ^ 2 * Ay ^ 2 + Ry ^ 2 * Ax ^ 2 - Rx ^ 2 * Ry ^ 2
	d = b ^ 2 - 4 * a * c

	if d > 0 then
		sd = math.sqrt(d)
		t1 = (-b + sd) / (2 * a)
		t2 = (-b - sd) / (2 * a)
		z1 = point(Ax + (Bx - Ax) * t1, Ay + (By - Ay) * t1)
		z2 = point(Ax + (Bx - Ax) * t2, Ay + (By - Ay) * t2)
		if angle_normalize_(point.arg(z1)) < angle_normalize_(point.arg(z2)) then
			return z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center,
				z2 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center
		else
			return z2 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center,
				z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center
		end -- if
	elseif math.abs(d) < tkz.epsilon then
		t1 = -b / (2 * a)
		z1 = point(Ax + (Bx - Ax) * t1, Ay + (By - Ay) * t1)
		return z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center,
			z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center
	else
		return false, false
	end
end

function intersection_ll_(a, b, c, d)
	local x1, y1, x2, y2, x3, y3, x4, y4
	local DN, NX, NY

	x1, y1 = a.re, a.im
	x2, y2 = b.re, b.im
	x3, y3 = c.re, c.im
	x4, y4 = d.re, d.im

	DN = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

	if math.abs(DN) < tkz.epsilon then
		return false
	end

	NX = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
	NY = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)

	return point(NX / DN, NY / DN)
end

function intersection_lc_(pa, pb, c, p)
	local zh, dh, arg_ab, phi, c1, c2, r, test

	r = point.mod(c - p)
	zh = projection_(pa, pb, c)
	dh = point.abs(c - zh)
	arg_ab = point.arg(pa - pb)

	if dh < tkz.epsilon then
		-- Le centre du cercle est sur la droite
		return c + polar_(r, math.pi + arg_ab), c + polar_(r, arg_ab)
	elseif math.abs(r - dh) < tkz.epsilon then
		-- La droite est tangente au cercle
		return zh, zh
	elseif dh > r then
		-- Aucune intersection
		return false, false
	else
		-- Il y a une intersection, calcul de l'angle
		phi = math.asin(dh / r)
		test = (pa - pb) * point.conj(c - zh)
		if test.im < 0 then
			phi = math.pi + phi
		end

		c1 = angle_normalize_(arg_ab + phi)
		c2 = angle_normalize_(math.pi + arg_ab - phi)

		-- Retourner les deux points d'intersection
		if c2 < c1 then
			return c + polar_(r, c2), c + polar_(r, c1)
		else
			return c + polar_(r, c1), c + polar_(r, c2)
		end
	end
end

function intersection_cc_(ca, pa, cb, pb)
	local d, cosphi, phi, ra, rb, c1, c2, epsilon

	-- Précision pour arrondir les résultats
	epsilon = 12
	-- Distance entre les centres des cercles
	d = point.abs(ca - cb)
	-- Rayons des cercles
	ra = point.abs(ca - pa)
	rb = point.abs(cb - pb)

	-- Calcul du cosinus de l'angle phi entre les centres et les points sur les cercles
	cosphi = tkz_round_((ra * ra + d * d - rb * rb) / (2 * ra * d), epsilon)

	-- Calcul de l'angle phi
	phi = tkz_round_(math.acos(cosphi), epsilon)

	-- Si phi est invalide (par exemple, cosphi > 1 ou < -1), aucune intersection
	if not phi then
		return false, false
	elseif math.abs(phi) < tkz.epsilon then
		-- Les cercles sont tangents l'un à l'autre, retourne le même point pour les deux intersections
		return ca + polar_(ra, point.arg(cb - ca)), ca + polar_(ra, point.arg(cb - ca))
	else
		-- Calcul des angles des points d'intersection
		c1 = angle_normalize_(phi + point.arg(cb - ca))
		c2 = angle_normalize_(-phi + point.arg(cb - ca))

		-- Retourner les points d'intersection dans l'ordre croissant des angles
		if c1 < c2 then
			return ca + polar_(ra, c1), ca + polar_(ra, c2)
		else
			return ca + polar_(ra, c2), ca + polar_(ra, c1)
		end
	end
end

function intersection(X, Y, opts)
	local t = {} -- Table pour stocker les points d'intersection

	-- Intersection cercle-cercle, ligne-cercle, ligne-ligne, conique-ligne...
	local function add(z1, z2)
		if z1 then
			table.insert(t, z1)
		end
		if z2 then
			table.insert(t, z2)
		end
	end

	-- Cas principaux
	if X.type == "circle" then
		if Y.type == "circle" then
			add(intersection_cc(X, Y))
		else
			add(intersection_lc(Y, X))
		end
	elseif X.type == "line" then
		if Y.type == "circle" then
			add(intersection_lc(X, Y))
		elseif Y.type == "line" then
			add(intersection_ll(X, Y))
		elseif Y.type == "conic" then
			if Y.subtype == "parabola" then
				add(Y:inter_Pa_line(X.pa, X.pb))
			elseif Y.subtype == "hyperbola" then
				add(Y:inter_Hy_line(X.pa, X.pb))
			elseif Y.subtype == "ellipse" then
				add(intersection_le(X, Y))
			end
		end
	elseif X.type == "conic" then
		if X.subtype == "parabola" then
			add(X:inter_Pa_line(Y.pa, Y.pb))
		elseif X.subtype == "hyperbola" then
			add(X:inter_Hy_line(Y.pa, Y.pb))
		elseif X.subtype == "ellipse" then
			add(intersection_le(Y, X))
		end
	end

	-- Réorganisation selon l'option
	if opts then
		if opts.known and #t == 2 then
			local z1, z2 = t[1], t[2]
			if z1 == opts.known then
				t = { z2, z1 }
			elseif z2 == opts.known then
				t = { z1, z2 }
			end
		elseif opts.near and #t == 2 then
			local z1, z2 = t[1], t[2]
			local d1 = (z1 - opts.near):abs()
			local d2 = (z2 - opts.near):abs()
			if d2 < d1 then
				t = { z2, z1 }
			end
		end
	end

	return table.unpack(t)
end
