-- File: tkz_elements_conic.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

---------------------------------------------------------------------------
--                           conics
---------------------------------------------------------------------------
-----------------------------
----- Conic parameters
-----------------------------

-- foci, directrix and eccentricity are the main parameters and are used to define all conics.

-------------------- General parameters common to all conics -------------
-- (e) is  eccentricity  e = 1 for parabola; e = p / h

-- (h) distance between the focus and the directrix (sometime called f) ;  h = p / e ellipse and hyperbola and h = p for parabola

-- (p) semi-latus rectum (The latus rectum is the chord parallel to the directrix and passing through a focus) It's also called, the focal parameter (distance from a focus to the corresponding directrix.)

-- vertex We can define the distance between the vertex and the directrix or the distance between the vertex and the focus p / (e + 1) so p / 2 for parabola;   (e * h) / (e + 1) = h * e / (e + 1)

-- major_axis : The principal axis is the line joining the foci of an ellipse or hyperbola, and its midpoint is the curve's center. A parabola has no center, in this case, the major_axis is the line joining the focus and the vertex.

-- parabola has no center and one focus. c/a for hyperbola and ellipse
-- (c) is the distance between the center and a focus. (line eccentricity)

-- for ellipse
-- a semi_major_awis distance between center and vertex
-- b semi_mminor_axis distance between center and covertex
-- c = math.sqrt(a^2 - b^2) and p = b^2 / a
-- h = b^2 / c

-- for hyperbola
-- a semi_major_awis distance between center and vertex
-- b semi_mminor_axis distance between center and covertex b/a slope of asymptote
-- c = math.sqrt(a^2 - b^2) and p = b^2 / a
-- h = b^2 / c
---------------------------------------------------------------------------
conic = {}
conic.__index = conic
function conic:new(Fa, Di, ecc) -- focus, directrix, eccentricity
	local type = "conic"
	local e = ecc
	local subtype = get_subtype(e)
	local K = projection_(Di.pa, Di.pb, Fa)
	local h = length_(K, Fa)
	local a = get_a(h, e)
	local b = get_b(h, e) -- only hy and el
	local c = get_c(h, e)
	local p = e * h -- demi latus rectum if hy and el b^2/a
	local slope = slope_(K, Fa)
	local major_axis = line:new(K, Fa)
	local directrix = Di
	local vertex = get_vertex(Fa, K, e, h)
	-- pb No center, no covertex, no Fb with parabola
	local center = conic_center(Fa, K, e, h)
	local Fb = next_focus(Fa, K, e, h)
	local covertex = get_covertex(Fa, K, e, h)
	local minor_axis = get_minor_axis(Fa, K, e, h)
	local o = {
		type = type,
		subtype = subtype,
		K = K,
		e = e,
		h = h,
		a = a,
		b = b,
		c = c,
		p = p,
		Rx = a,
		Ry = b,
		Fa = Fa,
		Fb = Fb,
		center = center,
		vertex = vertex,
		covertex = covertex,
		major_axis = major_axis,
		minor_axis = minor_axis,
		directrix = directrix,
		slope = slope,
	}
	setmetatable(o, self)
	return o
end

setmetatable(conic, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

function conic:points(ta, tb, nb, swap)
	swap = (swap == "swap")
	if not swap then
		return path(get_points_conic_(self, ta, tb, nb))
	else
		return path(get_points_sym_conic_(self, ta, tb, nb))
	end
end

function conic:point(t, swap)
	swap = (swap == "swap")
	if not swap then
		return get_one_point_conic_(self, t)
	else
		return get_one_point_hyperbola_ii(self, t)
	end
end

function conic:antipode(pt)
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then
		tex.error("Undefined antipode for a parabola.")
	else
		return 2 * self.center - pt
	end
end

function conic:tangent_at(pt) -- actually  only parabola
	local u, v
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then -- Parabola
		local h = self.directrix:projection(pt)
		u = self.vertex:identity(pt) and ll_from_(pt, self.directrix.pa, self.directrix.pb)
			or in_center_(pt, h, self.Fa)
	elseif self.e > 1 then -- Hyperbola
		u = self.vertex:identity(pt) and ll_from_(pt, self.directrix.pa, self.directrix.pb)
			or in_center_(pt, self.Fb, self.Fa)
	elseif self.e < 1 then -- Ellipse
		local zi = in_center_(self.Fa, pt, self.Fb)
		u = pt + (zi - pt) * point(0, 1)
	end

	u = normalize_(pt, u)
	v = pt:symmetry(u)

	return line:new(u, v)
end

-- Intersection entre une parabole et une droite
function conic:inter_Pa_line(pa, pb)
	local function solve_para_line(p, m, n)
		return tkz.solve_quadratic_(1, -2 * p * m, -2 * p * n)
	end

	-- Crée un système de coordonnées basé sur l'axe majeur et le sommet de la parabole
	local sys = occs:new(self.major_axis, self.vertex)

	-- Calcule les coordonnées des points pa et pb dans ce système
	local Xa, Ya = sys:coordinates(pa)
	local Xb, Yb = sys:coordinates(pb)

	-- Résout l'intersection entre la parabole et la droite définie par les points pa et pb
	local r1, r2 = solve_para_line(self.h, tkz.line_coefficients(Xa, Ya, Xb, Yb))
	-- Si les résultats de l'intersection sont invalides (false), retourne deux faux
	if r1 == false then
		local s1 = false
		local s2 = false
		return s1, s2
	else
		-- Sinon, calcule les points d'intersection
		local s1, s2 = self:point(r1), self:point(r2)

		-- Retourne les points d'intersection dans l'ordre croissant de distance par rapport à pa
		if length_(pa, s1) < length_(pa, s2) then
			return s1, s2
		else
			return s2, s1
		end
	end
end

function conic:tangent_from(pt)
	if self.e == 1 then -- Parabola
		local sys = occs:new(self.major_axis, self.vertex)
		local Xb, Yb = sys:coordinates(pt)
		local p1, p2 = tkz.solve_quadratic_(self.h, -2 * Xb, 2 * Yb)
		local s1 = self:point(self.h * p1)
		local s2 = self:point(self.h * p2)
		return line:new(pt, s1), line:new(pt, s2)
	elseif self.e > 1 then -- Hyperbola
		local C = circle:radius(self.Fb, 2 * self.a)
		local m, n = intersection_cc_(pt, self.Fa, self.Fb, C.through)
		local u, v = mediator_(m, self.Fa)
		local x, y = mediator_(n, self.Fa)
		local T1, T2 = line:new(u, v), line:new(x, y)
		local d1, d2 = T1:distance(self.Fa), T2:distance(self.Fa)
		if d2 < d1 then
			T2, T1 = T1, T2
		end
		local Fbsym = symmetry_axial_(T1.pa, T1.pb, self.Fb)
		local t1 = intersection_ll_(Fbsym, self.Fa, T1.pa, T1.pb)
		local Fasym = symmetry_axial_(T2.pa, T2.pb, self.Fa)
		local t2 = intersection_ll_(Fasym, self.Fb, T2.pa, T2.pb)
		return line:new(pt, t1), line:new(pt, t2)
	elseif self.e < 1 then -- Ellipse
		local w = report_(self.Fb, self.Fa, 2 * self.a)
		local s1, s2 = intersection_cc_(pt, self.Fa, self.Fb, w)
		local u, v = mediator_(s1, self.Fa)
		local U = intersection_ll_(u, v, self.Fb, s1)
		u, v = mediator_(s2, self.Fa)
		local V = intersection_ll_(u, v, self.Fb, s2)
		return line:new(pt, U), line:new(pt, V)
	end
end

-- intersection line hyperbola
function conic:inter_Hy_line(pa, pb)
	local function hyp_fct(x)
		return self.a * math.sqrt(1 + (x ^ 2) / self.b ^ 2)
	end

	local function solve_hyper_line(a, b, m, p)
		local A = a ^ 2 / b ^ 2 - m ^ 2
		local B = -2 * m * p
		local C = a ^ 2 - p ^ 2
		return tkz.solve_quadratic_(A, B, C)
	end

	local sys = occs:new(self.major_axis, self.center)
	local XA, YA = sys:coordinates(pa)
	local XB, YB = sys:coordinates(pb)
	if math.abs(XA - XB) < tkz.epsilon then
		local xs = XA
		local fa, c = self.Fa, self.center
		local s1, s2 = hyp_fct(xs), -hyp_fct(xs)
		local wx = report_(self.directrix.pa, self.directrix.pb, xs, report_(c, fa, s1))
		local wy = report_(self.directrix.pa, self.directrix.pb, xs, report_(c, fa, s2))

		return wx, wy
	else
		local r, s = tkz.line_coefficients(XA, YA, XB, YB)
		local t1, t2 = solve_hyper_line(self.a, self.b, r, s)
		if t1 == false then
			return pa, pb
		else
			local s1, s2 = self:point(t1), self:point(t2)

			if r * t2 + s < 0 then
				s2 = self:point(t2, "swap")
			end
			if r * t1 + s < 0 then
				s1 = self:point(t1, "swap")
			end

			if length_(pa, s1) < length_(pa, s2) then
				return s1, s2
			else
				return s2, s1
			end
		end
	end
end

-- Fonction pour déterminer si un point est à l'intérieur ou à l'extérieur de la conique
function conic:in_out(pt)
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then -- Parabole
		return PA_in_out(self, pt)
	elseif self.e > 1 then -- Hyperbole
		return HY_in_out(self, pt)
	elseif self.e < 1 then -- Ellipse
		return EL_in_out(self, pt)
	end
end

-- Fonction pour obtenir l'orthoptique de la conique
function conic:orthoptic()
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then -- Parabole
		return self.directrix
	elseif self.e > 1 and self.e < math.sqrt(2) then -- Hyperbole
		local r = math.sqrt(self.a * self.a - self.b * self.b)
		local th = report_(self.center, self.vertex, r)
		return circle:new(self.center, th)
	elseif self.e < 1 then -- Ellipse
		local r = math.sqrt(self.a * self.a + self.b * self.b)
		local th = report_(self.center, self.vertex, r)
		return circle:new(self.center, th)
	end
end

-- Fonction pour calculer les asymptotes de la conique
function conic:asymptotes()
	if self.e > 1 then -- Hyperbole
		-- Calcul du point sur les foyers et des asymptotes
		local pa = report_(self.Fa, self.Fb, self.a, self.center)
		local p1 = (pa - self.center):orthogonal(self.b):at(pa)
		local p2 = (pa - self.center):orthogonal(-self.b):at(pa)

		-- Symétrie des points d'asymptotes par rapport au centre
		local q1 = symmetry_(self.center, p1)
		local q2 = symmetry_(self.center, p2)

		-- Retourne les deux asymptotes
		return line:new(p1, q1), line:new(p2, q2)
	else
		-- Si ce n'est pas une hyperbole, renvoie une erreur
		tex.error("An error has occurred", { "It's not an hyperbola" })
		return
	end
end

function conic:path(za, zb, nb, mode)
	nb = nb or 20
	local ta = self:get_t_from_point(za)
	local tb = self:get_t_from_point(zb)
	local dt = tb - ta

	-- Correction pour ellipse uniquement en mode "short"
	if self.e < 1 and mode == "short" then
		if dt > 0.5 then
			dt = dt - 1
		end
		if dt < -0.5 then
			dt = dt + 1
		end
	end

	local P = path()
	for i = 0, nb do
		local t = i / nb
		local t_interp
		if self.e < 1 then
			t_interp = (ta + t * dt) % 1
		else
			t_interp = ta + t * dt
		end
		local pt = self:point(t_interp)
		if not pt or not pt.re or not pt.im then
			tex.error("Invalid point at t = " .. tostring(t_interp))
		end
		P:add_point(pt)
	end

	return P
end

function conic:get_t_from_point(z)
	local e = self.e
	if e == 1 then
		return self:get_t_parabola(z)
	elseif e < 1 then
		return self:get_t_ellipse(z)
	else
		return self:get_t_hyperbola(z)
	end
end

function conic:get_t_parabola(z)
	local H = self.directrix:projection(z)
	local d = length_(H, self.K)
	local u = self.directrix.pb - self.directrix.pa
	local v = H - self.K

	if v .. u >= 0 then
		return d
	else
		return -d
	end
end

function conic:get_t_hyperbola(z)
	local H = self.directrix:projection(z)
	local d = length_(H, self.K)
	local u = self.directrix.pb - self.directrix.pa
	local v = H - self.K

	if v .. u >= 0 then
		return d
	else
		return -d
	end
end

function conic:get_t_ellipse(z)
	local M = inverse_affinity_ellipse(self, z)
	local a = get_angle_normalize_(self.center, self.vertex, M)
	if a < 0 then
		a = a + 2 * math.pi
	end
	return a / (2 * math.pi)
end

return conic
