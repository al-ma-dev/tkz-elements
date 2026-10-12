-- File: tkz_elements_conic.lua
-- Copyright (c) 2026 Alain Matthes
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
	if type(Fa) ~= "table" or Fa.type ~= "point" then
		error("tkz-elements: conic: the first argument (focus) must be a point", 2)
	end
	if type(Di) ~= "table" or Di.type ~= "line" then
		error("tkz-elements: conic: the second argument (directrix) must be a line", 2)
	end
	if type(ecc) ~= "number" or ecc <= 0 then
		error("tkz-elements: conic: the eccentricity must be a positive number", 2)
	end
	if Di:distance(Fa) < tkz.epsilon then
		error("tkz-elements: conic: the focus must not lie on the directrix", 2)
	end
	local type = "conic"
	local e = ecc
	local subtype = get_subtype_(e)
	local K = projection_(Di.pa, Di.pb, Fa)
	local h = length_(K, Fa)
	local a = get_a_(h, e)
	local b = get_b_(h, e) -- only hy and el
	local c = get_c_(h, e)
	local p = e * h -- demi latus rectum if hy and el b^2/a
	local slope = slope_(K, Fa)
	local major_axis = line:new(K, Fa)
	local directrix = Di
	local vertex = get_vertex_(Fa, K, e, h)
	-- pb No center, no covertex, no Fb with parabola
	local center = conic_center_(Fa, K, e, h)
	local Fb = next_focus_(Fa, K, e, h)
	local covertex = get_covertex_(Fa, K, e, h)
	local minor_axis = get_minor_axis_(Fa, K, e, h)
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

function conic:get(i)
	local p1, p2, p3

	if self.subtype == "parabola" then
		p1 = self.vertex
		p2 = self.Fa
		p3 = nil
	else
		p1 = self.center
		p2 = self.Fa
		p3 = self.Fb
	end

	if i == nil then
		return p1, p2, p3
	elseif i == 1 then
		return p1
	elseif i == 2 then
		return p2
	elseif i == 3 then
		return p3
	else
		return nil
	end
end


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
		return get_one_point_hyperbola_ii_(self, t)
	end
end

function conic:antipode(pt)
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then
		error("tkz-elements: conic:antipode: undefined for a parabola", 2)
	end
	return 2 * self.center - pt
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

-- Intersection of a parabola and a line (internal).
-- Returns two points, one point (line parallel to the axis) or nothing.
function conic:inter_Pa_line_(pa, pb, EPS)
	EPS = EPS or tkz.epsilon
	local function solve_para_line(p, m, n)
		return tkz.solve_quadratic_(1, -2 * p * m, -2 * p * n)
	end

	-- coordinate system based on the axis and the vertex of the parabola
	local sys = occs:new(self.major_axis, self.vertex)

	-- coordinates of pa and pb in this system
	local Xa, Ya = sys:coordinates(pa)
	local Xb, Yb = sys:coordinates(pb)

	-- line parallel to the axis: a single intersection point
	if math.abs(Xa - Xb) < EPS then
		return self:point(Xa)
	end

	-- solve the intersection of the parabola and the line (pa, pb)
	local r1, r2 = solve_para_line(self.h, tkz.line_coefficients(Xa, Ya, Xb, Yb))
	if not r1 then
		return nil, nil -- no intersection
	end
	local s1, s2 = self:point(r1), self:point(r2)

	-- points sorted by increasing distance from pa
	if length_(pa, s1) < length_(pa, s2) then
		return s1, s2
	else
		return s2, s1
	end
end

function conic:tangent_from(pt)
	if self:position(pt) == "IN" then
		error("tkz-elements: conic:tangent_from: the point must not be inside the conic", 2)
	end
	if math.abs(self.e - 1) < tkz.epsilon then -- Parabola
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

-- Intersection of a hyperbola and a line (internal).
-- Returns two points, one point (line parallel to an asymptote) or nothing.
function conic:inter_Hy_line_(pa, pb, EPS)
	EPS = EPS or tkz.epsilon
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
	if math.abs(XA - XB) < EPS then
		local xs = XA
		local fa, c = self.Fa, self.center
		local s1, s2 = hyp_fct(xs), -hyp_fct(xs)
		local wx = report_(self.directrix.pa, self.directrix.pb, xs, report_(c, fa, s1))
		local wy = report_(self.directrix.pa, self.directrix.pb, xs, report_(c, fa, s2))

		return wx, wy
	else
		local r, s = tkz.line_coefficients(XA, YA, XB, YB)
		local A = self.a ^ 2 / self.b ^ 2 - r ^ 2
		if math.abs(A) < EPS then
			-- line parallel to an asymptote: at most one point
			local B = -2 * r * s
			if math.abs(B) < EPS then
				return nil, nil -- the line is an asymptote
			end
			local t = -(self.a ^ 2 - s ^ 2) / B
			if r * t + s < 0 then
				return self:point(t, "swap")
			end
			return self:point(t)
		end
		local t1, t2 = solve_hyper_line(self.a, self.b, r, s)
		if not t1 then
			return nil, nil -- no intersection
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

-- ============================================================
-- in_out -> boolean (historical API)
-- ============================================================
function conic:in_out(pt, EPS)
	EPS = EPS or tkz.epsilon
	local e = self.e
	if math.abs(e - 1) <= EPS then        -- parabola
		return PA_in_out_(self, pt, EPS)     -- boolean
	elseif e > 1 then                     -- hyperbola
		return HY_in_out_(self, pt, EPS)     -- boolean
	else                                  -- ellipse
		return EL_in_out_(self, pt, EPS)     -- boolean
	end
end


-- ============================================================
-- position -> "IN" | "ON" | "OUT"
-- ============================================================
function conic:position(pt, EPS)
	EPS = EPS or tkz.epsilon
	local e = self.e
	if math.abs(e - 1) <= EPS then
		return PA_position_(self, pt, EPS)
	elseif e > 1 then
		return HY_position_(self, pt, EPS)
	else
		return EL_position_(self, pt, EPS)
	end
end


-- ============================================================
-- Ellipse: position
-- (same principle as EL_in_out_, with the "ON" case)
-- ============================================================
function EL_position_(CO, pt, EPS)
	EPS = EPS or tkz.epsilon

	local d = point.abs(pt - CO.center)
	if d <= EPS then return "IN" end -- the center
	local L = line(CO.center, pt)
	local x, y = intersection(L, CO)
	if not x then return "OUT" end

	local dx = point.abs(x - CO.center)
	if y then
		local dy = point.abs(y - CO.center)
		if dy < dx then dx = dy end
	end

	local diff = d - dx
	if math.abs(diff) <= EPS then return "ON" end
	if diff < -EPS then return "IN" else return "OUT" end
end


-- ============================================================
-- Parabola/Hyperbola: position
-- Convention: "IN" if pt lies between the two intersection points
-- of the conic and the chord through pt perpendicular to the axis
-- ============================================================
local function AX_position_(CO, pt, EPS)
	EPS = EPS or tkz.epsilon

	local D  = CO.major_axis
	local Dp = D:ortho_from(pt)
	local x, y = intersection(Dp, CO)

	if not x then return "OUT" end
	if not y then
		return (point.abs(pt - x) <= EPS) and "ON" or "OUT"
	end

	-- "ON" test (useful when pt is computed and lies on the conic)
	if point.abs(pt - x) <= EPS or point.abs(pt - y) <= EPS then
		return "ON"
	end

	local chord = line(x, y)
	return chord:on_segment(pt, EPS) and "IN" or "OUT"
end

function PA_position_(PA, pt, EPS) return AX_position_(PA, pt, EPS) end
function HY_position_(HY, pt, EPS) return AX_position_(HY, pt, EPS) end


-- Orthoptic of the conic: directrix (parabola) or director circle
function conic:orthoptic()
	local e = self.e
	if math.abs(e - 1) < tkz.epsilon then -- Parabola
		return self.directrix
	elseif self.e > 1 then -- Hyperbola
		if self.a <= self.b + tkz.epsilon then
			error("tkz-elements: conic:orthoptic: undefined for a hyperbola with e >= sqrt(2)", 2)
		end
		local r = math.sqrt(self.a * self.a - self.b * self.b)
		local th = report_(self.center, self.vertex, r)
		return circle:new(self.center, th)
	elseif self.e < 1 then -- Ellipse
		local r = math.sqrt(self.a * self.a + self.b * self.b)
		local th = report_(self.center, self.vertex, r)
		return circle:new(self.center, th)
	end
end

-- Asymptotes of a hyperbola
function conic:asymptotes()
	if self.subtype == "hyperbola" then
		-- points on the asymptotes
		local pa = report_(self.Fa, self.Fb, self.a, self.center)
		local p1 = (pa - self.center):orthogonal(self.b):at(pa)
		local p2 = (pa - self.center):orthogonal(-self.b):at(pa)

		-- symmetric points with respect to the center
		local q1 = symmetry_(self.center, p1)
		local q2 = symmetry_(self.center, p2)

		return line:new(p1, q1), line:new(p2, q2)
	else
		error("tkz-elements: conic:asymptotes: the conic is not a hyperbola", 2)
	end
end

-- conic:path(za, zb, nb, mode, dir)
-- mode : nil/"direct" (default) -> direct (short) arc
--        "swap"                -> complementary arc (the other part)
-- dir  : "ccw" | "cw" (optional, used to remove the ambiguity when dt = ±0.5
--                      or when za == zb -> full turn)
function conic:path(za, zb, nb, mode, dir)
	nb   = nb or 20
	mode = (mode or "direct"):lower()

	local ta = self:get_t_from_point(za)
	local tb = self:get_t_from_point(zb)
	local dt = tb - ta

	local function sgn(x) return (x < 0) and -1 or 1 end
	local function frac1(x)
		x = x % 1
		if x < 0 then x = x + 1 end
		return x
	end

	-- ellipse only: periodic parameter on [0,1)
	if self.e and self.e < 1 then
		ta = frac1(ta)

		-- normalize dt in (-0.5, 0.5]
		local eps = 1e-12
		dt = ((dt + 0.5) % 1) - 0.5

		-- antipodal points: deterministic choice
		if math.abs(math.abs(dt) - 0.5) < eps then
			-- choice according to dir (default "cw")
			dt = (dir == "ccw") and  0.5 or -0.5
		end

		if mode == "swap" then
			-- take the other part (complementary arc)
			if math.abs(dt) < eps then
				-- same points: full turn
				dt = (dir == "cw") and -1 or 1
			else
				-- shift by ±1 to get the arc with |dt| > 0.5
				dt = dt - sgn(dt)
			end
		end
		-- "direct" (default): dt is kept in (-0.5, 0.5]
	end
	-- parabola/hyperbola: no periodicity, dt is not modified

	-- sampling
	local P = path()
	for i = 0, nb do
		local t = i / nb
		local t_interp
		if self.e and self.e < 1 then
			t_interp = (ta + t * dt) % 1
			if t_interp < 0 then t_interp = t_interp + 1 end
		else
			t_interp = ta + t * dt
		end
		local pt = self:point(t_interp)
		if not pt or not pt.re or not pt.im then
			error("tkz-elements: conic:path: invalid point at t = " .. tostring(t_interp), 2)
		end
		P:add_point(pt)
	end
	return P
end


function conic:get_t_from_point(z)
	local e = self.e
	if e == 1 then
		return self:get_t_parabola_(z)
	elseif e < 1 then
		return self:get_t_ellipse_(z)
	else
		return self:get_t_hyperbola_(z)
	end
end

function conic:get_t_parabola_(z)
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

function conic:get_t_hyperbola_(z)
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

function conic:get_t_ellipse_(z)
	local M = inverse_affinity_ellipse_(self, z)
	local a = get_angle_normalize_(self.center, self.vertex, M)
	if a < 0 then
		a = a + tkz.tau
	end
	return a / tkz.tau
end

-- Common tangents of two ellipses.
-- which: "external" (default) or "internal"
-- Returns: L1, L2
function conic:common_tangent(E2, which)
	which = which or "external"
	if self.subtype ~= "ellipse" or E2.subtype ~= "ellipse" then
		return nil, nil
	end

	local E1 = self
	local C1 = E1.center
	local C2 = E2.center
	local A1 = E1.a
	local B1 = E1.b

	local a, b  = E1.directrix:get()
	local ll    = line(b, a)
	local sys   = occs(ll, C1)
	local X2, Y2 = sys:coordinates(E2.center)
	local c2_loc = point(X2, Y2)
	local u2_loc, v2_loc
	do
		local vx, vy = sys:coordinates(E2.vertex)
		local cvx, cvy = vx - c2_loc.re, vy - c2_loc.im
		local nv = math.sqrt(cvx * cvx + cvy * cvy)
		if nv > 0 then cvx, cvy = cvx / nv, cvy / nv end
		u2_loc = point(cvx, cvy)

		local wx, wy = sys:coordinates(E2.covertex)
		local cwx, cwy = wx - c2_loc.re, wy - c2_loc.im
		local nw = math.sqrt(cwx * cwx + cwy * cwy)
		if nw > 0 then cwx, cwy = cwx / nw, cwy / nw end
		v2_loc = point(cwx, cwy)
	end

	local A2 = E2.a
	local B2 = E2.b

	-- local parametrization of E2 (in the sys frame)
	local function pE2_loc(t)
		return {
			x = c2_loc.re + A2 * math.cos(t) * u2_loc.re + B2 * math.sin(t) * v2_loc.re,
			y = c2_loc.im + A2 * math.cos(t) * u2_loc.im + B2 * math.sin(t) * v2_loc.im
		}
	end
	local function dE2_loc(t)
		return {
			x = -A2 * math.sin(t) * u2_loc.re + B2 * math.cos(t) * v2_loc.re,
			y = -A2 * math.sin(t) * u2_loc.im + B2 * math.cos(t) * v2_loc.im
		}
	end

	-- elliptic inner product (E1) and associated norm
	local function ps_loc(z1, z2)
		return (z1.x * z2.x) / (A1 * A1) + (z1.y * z2.y) / (B1 * B1)
	end
	local function N2_loc(z)
		return ps_loc(z, z)
	end

	-- tangency discriminant
	local function D_of_t(t)
		local p  = pE2_loc(t)
		local dp = dE2_loc(t)
		return ps_loc(p, dp) ^ 2 - N2_loc(dp) * (N2_loc(p) - 1)
	end

	local function is_external(A, u)
		local px, py = -u.y, u.x
		local s1 = A.x * px + A.y * py
		local dx, dy = A.x - c2_loc.re, A.y - c2_loc.im
		local s2 = dx * px + dy * py
		return s1 * s2 >= 0
	end

	local function is_internal(A, u)
		local px, py = -u.y, u.x
		local s1 = A.x * px + A.y * py
		local dx, dy = A.x - c2_loc.re, A.y - c2_loc.im
		local s2 = dx * px + dy * py
		return s1 * s2 <= 0
	end

	local function to_global(O, origin, a, b)
		local ox, oy = origin.re, origin.im
		local ux, uy = O.x.re - ox, O.x.im - oy
		local vx, vy = O.y.re - ox, O.y.im - oy
		return point(ox + a * ux + b * vx, oy + a * uy + b * vy)
	end

	local function almost_eq(P, Q, eps)
		if (not P) or (not Q) then return false end
		eps = eps or 1e-4
		return (math.abs(P.x - Q.x) < eps) and (math.abs(P.y - Q.y) < eps)
	end

	local E1_raw, E2_raw = {}, {}
	local T = tkz.fsolve(D_of_t, -math.pi, math.pi, 720)
	if T == nil then T = {} end
	if type(T) ~= "table" then T = { T } end

	for _, t in ipairs(T) do
		local A = pE2_loc(t)
		local u = dE2_loc(t)
		if A and u then
			local uu = ps_loc(u, u)
			if uu and uu > 0 then
				local lam = -ps_loc(A, u) / uu
				local B = { x = A.x + lam * u.x, y = A.y + lam * u.y }
				local is_which = (which == "external") and is_external or is_internal
				if is_which(A, u) then
					local n = utils.table_getn(E1_raw)
					local dup = false
					for i = 1, n do
						if almost_eq(B, E1_raw[i]) and almost_eq(A, E2_raw[i]) then
							dup = true
							break
						end
					end
					if not dup then
						E1_raw[n + 1] = B
						E2_raw[n + 1] = A
						if n + 1 >= 2 then break end
					end
				end
			end
		end
	end

	-- back to global coordinates through the sys frame centred at C1
	local a = E1_raw[1] and to_global(sys, C1, E1_raw[1].x, E1_raw[1].y) or nil
	local b = E1_raw[2] and to_global(sys, C1, E1_raw[2].x, E1_raw[2].y) or nil
	local c = E2_raw[1] and to_global(sys, C1, E2_raw[1].x, E2_raw[1].y) or nil
	local d = E2_raw[2] and to_global(sys, C1, E2_raw[2].x, E2_raw[2].y) or nil
	return line:new(a, b), line:new(c, d)
end

return conic
