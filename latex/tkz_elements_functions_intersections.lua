-- File: tkz_elements_intersections.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-------------------------------------------------------------------------
-- intersection of lines
-------------------------------------------------------------------------
function intersection_ll(la, lb ,EPS)
	return intersection_ll_(la.pa, la.pb, lb.pa, lb.pb, EPS)
end
---------------------------------------------------------------------------
-- intersection of a line and a circle
---------------------------------------------------------------------------
function intersection_lc(D, C, EPS)
	return intersection_lc_(D.pa, D.pb, C.center, C.through, EPS)
end -- function
---------------------------------------------------------------------------
-- intersection of two circles
---------------------------------------------------------------------------
function intersection_cc(Ca, Cb, EPS)
	return intersection_cc_(Ca.center, Ca.through, Cb.center, Cb.through, EPS)
end -- function

--  line ellipse
function intersection_le(L, E, EPS)
	 EPS = EPS or tkz.epsilon
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
	elseif math.abs(d) < EPS then
		t1 = -b / (2 * a)
		z1 = point(Ax + (Bx - Ax) * t1, Ay + (By - Ay) * t1)
		return z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center,
			z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center
	else
		return false, false
	end
end

function intersection_ll_(a, b, c, d, EPS)
	local x1, y1 = a.re, a.im
	local x2, y2 = b.re, b.im
	local x3, y3 = c.re, c.im
	local x4, y4 = d.re, d.im

	 EPS = EPS or tkz.epsilon

	-- cas dégénérés : une "droite" réduite à un point
	if (x1 == x2 and y1 == y2) or (x3 == x4 and y3 == y4) then
		return false
	end

	local DN = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

	-- droites parallèles ou quasi-parallèles
	if math.abs(DN) < EPS then
		return false
	end

	local NX = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
	local NY = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)

	return point(NX / DN, NY / DN)
end




function intersection_lc_(pa, pb, c, p, EPS)
	-- pa, pb : deux points définissant la droite
	-- c      : centre du cercle
	-- p      : point sur le cercle (pour le rayon)
	-- Retour :
	--   - false, false si pas d’intersection
	--   - P, P si tangence
	--   - P1, P2 si sécante (ordonnées par angle autour de c)
   EPS = EPS or tkz.epsilon


	-- Rayon du cercle
	local r = point.abs(c - p)

	-- Vecteur directeur de la droite (pa -> pb)
	local ab = pb - pa
	local abx, aby = ab.re, ab.im
	local lab2 = abx*abx + aby*aby

	-- Droite dégénérée : pa == pb
	if lab2 < EPS * EPS then
		return false, false
	end

	local lab = math.sqrt(lab2)
	local ex, ey = abx / lab, aby / lab  -- vecteur unitaire sur la droite

	-- Projection du centre c sur la droite (pa,pb)
	local zh = projection_(pa, pb, c)
	local dh = point.abs(c - zh)        -- distance centre -> droite

	-- Pas d'intersection : la droite est trop loin du cercle
	if dh > r + EPS then
		return false, false
	end

	-- Tangence : un seul point (renvoyé deux fois)
	if math.abs(dh - r) <= EPS then
		return zh, zh
	end

	-- Cas général : deux intersections.
	-- Longueur le long de la droite à partir du pied de la perpendiculaire.
	local h2 = r*r - dh*dh
	local R2 = math.max(r*r, lab2)

	if h2 < -EPS * R2 then
		-- numériquement négatif => pas de solution réelle
		return false, false
	end
	if h2 < 0 then
		h2 = 0
	end

	local h = math.sqrt(h2)

	local xh, yh = zh.re, zh.im
	local x1 = xh + h * ex
	local y1 = yh + h * ey
	local x2 = xh - h * ex
	local y2 = yh - h * ey

	local z1 = point(x1, y1)
	local z2 = point(x2, y2)

	-- Tri par angle autour du centre c, comme pour intersection_cc_
	local a1 = angle_normalize_(point.arg(z1 - c))
	local a2 = angle_normalize_(point.arg(z2 - c))

	if a1 <= a2 then
		return z1, z2
	else
		return z2, z1
	end
end

function intersection_cc_(ca, pa, cb, pb, EPS)
	-- ca, pa, cb, pb : points (type point/complex)
	-- Retour :
	--   - false, false si pas d'intersection
	--   - P, P si tangence
	--   - P1, P2 si sécante (ordre croissant en angle autour de ca)
   EPS = EPS or tkz.epsilon
	local ra = point.abs(ca - pa)
	local rb = point.abs(cb - pb)

	-- vecteur entre centres
	local dc = cb - ca
	local dx, dy = dc.re, dc.im
	local d2 = dx*dx + dy*dy
	local d  = math.sqrt(d2)

	-- centres (quasi) confondus
	if d < EPS then
		-- mêmes rayons -> cercles confondus : géométriquement infinité de solutions
		-- pour l’instant on renvoie "pas d’intersection" (comportement ancien : false,false)
		if math.abs(ra - rb) < EPS then
			return false, false
		else
			return false, false
		end
	end

	-- positions relatives simples : trop loin ou l'un dans l'autre sans contact
	-- (ces tests ne sont pas obligatoires mais évitent des racines négatives parasites)
	if d > ra + rb + EPS then
		-- trop éloignés
		return false, false
	end
	if d < math.abs(ra - rb) - EPS then
		-- l’un est strictement à l’intérieur de l’autre, sans tangence
		return false, false
	end

	-- projection du point d’intersection "médian" sur la droite des centres
	local a = (ra*ra - rb*rb + d2) / (2*d)
	local h2 = ra*ra - a*a

	-- tolérance relative sur h2
	local R2 = math.max(ra*ra, rb*rb, d2)
	if h2 < -EPS * R2 then
		-- numériquement négatif -> pas de solution réelle
		return false, false
	end

	if h2 < 0 then
		-- très léger négatif dû aux erreurs de flottants
		h2 = 0
	end

	-- point "milieu" sur la droite (ca -> cb)
	local ux, uy = dx / d, dy / d      -- vecteur unitaire ca->cb
	local mx = ca.re + a * ux
	local my = ca.im + a * uy

	local h = math.sqrt(h2)

	-- si h ~ 0 : tangence
	if h < EPS * d then
		local P = point(mx, my)
		return P, P
	end

	-- vecteur unitaire perpendiculaire
	local px, py = -uy, ux

	local x1 = mx + h * px
	local y1 = my + h * py
	local x2 = mx - h * px
	local y2 = my - h * py

	local z1 = point(x1, y1)
	local z2 = point(x2, y2)

	-- ordre croissant en angle autour de ca (comme ton ancienne version)
	local c1 = angle_normalize_(point.arg(z1 - ca))
	local c2 = angle_normalize_(point.arg(z2 - ca))

	if c1 <= c2 then
		return z1, z2
	else
		return z2, z1
	end
end


function intersection(X, Y, opts)
-- Normalisation des arguments
local EPS

if type(opts) == "number" then
	-- Ancien style : intersection(X, Y, EPS)
	EPS  = opts
	opts = nil
elseif type(opts) == "table" then
	-- Nouveau style : opts.eps (ou opts.EPS)
	EPS = opts.EPS or tkz.epsilon
elseif opts == nil then
	EPS = tkz.epsilon
else
	tex.error("intersection: invalid third argument (expected table or number).")
	return
end

opts = opts or {}

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
			add(intersection_cc(X, Y, EPS))
		else
			add(intersection_lc(Y, X, EPS))
		end
	elseif X.type == "line" then
		if Y.type == "circle" then
			add(intersection_lc(X, Y, EPS))
		elseif Y.type == "line" then
			add(intersection_ll(X, Y, EPS))
		elseif Y.type == "conic" then
			if Y.subtype == "parabola" then
				add(Y:inter_Pa_line(X.pa, X.pb))
			elseif Y.subtype == "hyperbola" then
				add(Y:inter_Hy_line(X.pa, X.pb))
			elseif Y.subtype == "ellipse" then
				add(intersection_le(X, Y, EPS))
			end
		end
	elseif X.type == "conic" then
		if X.subtype == "parabola" then
			add(X:inter_Pa_line(Y.pa, Y.pb))
		elseif X.subtype == "hyperbola" then
			add(X:inter_Hy_line(Y.pa, Y.pb))
		elseif X.subtype == "ellipse" then
			add(intersection_le(Y, X, EPS))
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
