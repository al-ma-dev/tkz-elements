-- File: tkz_elements_functions_intersections.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-------------------------------------------------------------------------
-- intersection of lines
-------------------------------------------------------------------------
function intersection_ll(la, lb, EPS)
	return intersection_ll_(la.pa, la.pb, lb.pa, lb.pb, EPS)
end
---------------------------------------------------------------------------
-- intersection of a line and a circle
---------------------------------------------------------------------------
function intersection_lc(D, C, EPS)
	return intersection_lc_(D.pa, D.pb, C.center, C.through, EPS)
end
---------------------------------------------------------------------------
-- intersection of two circles
---------------------------------------------------------------------------
function intersection_cc(Ca, Cb, EPS)
	return intersection_cc_(Ca.center, Ca.through, Cb.center, Cb.through, EPS)
end

---------------------------------------------------------------------------
-- intersection of a line and an ellipse
-- Returns:
--   - false, false if there is no intersection (or if the line is degenerate)
--   - P, P if the line is tangent
--   - P1, P2 otherwise (ordered by angle around the center, measured from
--     the major axis)
---------------------------------------------------------------------------
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
	-- degenerate line: pa == pb
	local L2 = (Bx - Ax) ^ 2 + (By - Ay) ^ 2
	if L2 < EPS * EPS then
		return false, false
	end
	a = Rx ^ 2 * (By - Ay) ^ 2 + Ry ^ 2 * (Bx - Ax) ^ 2
	b = 2 * Rx ^ 2 * Ay * (By - Ay) + 2 * Ry ^ 2 * Ax * (Bx - Ax)
	c = Rx ^ 2 * Ay ^ 2 + Ry ^ 2 * Ax ^ 2 - Rx ^ 2 * Ry ^ 2
	d = b ^ 2 - 4 * a * c
	-- relative test: h2 is the square of the half-chord (in length units);
	-- near a tangency h2 ~ 2 R delta, where delta is the distance between
	-- the line and the tangent
	local h2 = d / (4 * a * a) * L2
	local tol = 2 * EPS * math.max(Rx, Ry)

	if h2 > tol then
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
		end
	elseif h2 >= -tol then
		t1 = -b / (2 * a)
		z1 = point(Ax + (Bx - Ax) * t1, Ay + (By - Ay) * t1)
		return z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center,
			z1 * (point(math.cos(E.slope), math.sin(E.slope))) + E.center
	else
		return false, false
	end
end

---------------------------------------------------------------------------
-- intersection of the lines (a, b) and (c, d)
-- Returns false if a line is degenerate or if the lines are parallel.
---------------------------------------------------------------------------
function intersection_ll_(a, b, c, d, EPS)
	local x1, y1 = a.re, a.im
	local x2, y2 = b.re, b.im
	local x3, y3 = c.re, c.im
	local x4, y4 = d.re, d.im

	EPS = EPS or tkz.epsilon

	-- degenerate cases: a "line" reduced to a point
	local la = math.sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
	local lb = math.sqrt((x4 - x3) ^ 2 + (y4 - y3) ^ 2)
	if la < EPS or lb < EPS then
		return false
	end

	local DN = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

	-- parallel or almost parallel lines (relative test: DN = la * lb * sin)
	if math.abs(DN) < EPS * la * lb then
		return false
	end

	local NX = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
	local NY = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)

	return point(NX / DN, NY / DN)
end

---------------------------------------------------------------------------
-- intersection of the line (pa, pb) and the circle (c, p)
-- pa, pb : two points of the line
-- c      : center of the circle
-- p      : point of the circle (gives the radius)
-- Returns:
--   - false, false if there is no intersection
--   - P, P if the line is tangent
--   - P1, P2 otherwise (ordered by angle around c)
---------------------------------------------------------------------------
function intersection_lc_(pa, pb, c, p, EPS)
	EPS = EPS or tkz.epsilon

	-- radius of the circle
	local r = point.abs(c - p)

	-- direction vector of the line (pa -> pb)
	local ab = pb - pa
	local abx, aby = ab.re, ab.im
	local lab2 = abx * abx + aby * aby

	-- degenerate line: pa == pb
	if lab2 < EPS * EPS then
		return false, false
	end

	local lab = math.sqrt(lab2)
	local ex, ey = abx / lab, aby / lab -- unit vector of the line

	-- projection of the center c on the line (pa, pb)
	local zh = projection_(pa, pb, c)
	local dh = point.abs(c - zh) -- distance from the center to the line

	-- no intersection: the line is too far from the circle
	if dh > r + EPS then
		return false, false
	end

	-- tangency: a single point (returned twice)
	if math.abs(dh - r) <= EPS then
		return zh, zh
	end

	-- general case: two intersection points.
	-- half-chord length, measured from the foot of the perpendicular
	local h2 = r * r - dh * dh
	local R2 = math.max(r * r, lab2)

	if h2 < -EPS * R2 then
		-- numerically negative => no real solution
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

	-- ordered by angle around the center c, as in intersection_cc_
	local a1 = angle_normalize_(point.arg(z1 - c))
	local a2 = angle_normalize_(point.arg(z2 - c))

	if a1 <= a2 then
		return z1, z2
	else
		return z2, z1
	end
end

---------------------------------------------------------------------------
-- intersection of the circles (ca, pa) and (cb, pb)
-- ca, cb : centers; pa, pb : points of the circles
-- Returns:
--   - false, false if there is no intersection (or if the circles coincide)
--   - P, P if the circles are tangent
--   - P1, P2 otherwise (ordered by angle around ca)
---------------------------------------------------------------------------
function intersection_cc_(ca, pa, cb, pb, EPS)
	EPS = EPS or tkz.epsilon
	local ra = point.abs(ca - pa)
	local rb = point.abs(cb - pb)

	-- vector between the centers
	local dc = cb - ca
	local dx, dy = dc.re, dc.im
	local d2 = dx * dx + dy * dy
	local d = math.sqrt(d2)

	-- (almost) concentric circles: no intersection point, or infinitely
	-- many if the circles coincide; false, false is returned in both cases
	if d < EPS then
		return false, false
	end

	-- simple relative positions: too far apart, or one inside the other
	-- without contact (these tests avoid spurious negative roots)
	if d > ra + rb + EPS then
		return false, false
	end
	if d < math.abs(ra - rb) - EPS then
		return false, false
	end

	-- projection of the intersection points on the line of centers
	local a = (ra * ra - rb * rb + d2) / (2 * d)

	-- tangency (external or internal), tested on the distances as in
	-- intersection_lc_: the point of contact is on the line of centers,
	-- at the distance ra from ca (h = sqrt(h2) would amplify rounding errors)
	if math.abs(d - (ra + rb)) <= EPS or math.abs(d - math.abs(ra - rb)) <= EPS then
		local k = (a >= 0) and ra / d or -ra / d
		local P = ca + k * dc
		return P, P
	end
	local h2 = ra * ra - a * a

	-- relative tolerance on h2
	local R2 = math.max(ra * ra, rb * rb, d2)
	if h2 < -EPS * R2 then
		-- numerically negative -> no real solution
		return false, false
	end

	if h2 < 0 then
		-- slightly negative because of rounding errors
		h2 = 0
	end

	-- "middle" point on the line (ca -> cb)
	local ux, uy = dx / d, dy / d -- unit vector ca -> cb
	local mx = ca.re + a * ux
	local my = ca.im + a * uy

	local h = math.sqrt(h2)

	-- h ~ 0: tangency
	if h < EPS * d then
		local P = point(mx, my)
		return P, P
	end

	-- unit normal vector
	local px, py = -uy, ux

	local x1 = mx + h * px
	local y1 = my + h * py
	local x2 = mx - h * px
	local y2 = my - h * py

	local z1 = point(x1, y1)
	local z2 = point(x2, y2)

	-- ordered by increasing angle around ca
	local c1 = angle_normalize_(point.arg(z1 - ca))
	local c2 = angle_normalize_(point.arg(z2 - ca))

	if c1 <= c2 then
		return z1, z2
	else
		return z2, z1
	end
end

---------------------------------------------------------------------------
-- intersection(X, Y, opts)
-- X, Y : line/line, line/circle, circle/circle or line/conic (any order)
-- opts : number (tolerance, old style) or table { known = pt, near = pt,
--        eps = number } (EPS is accepted as an alias of eps)
-- Returns the intersection points (nothing if there is no intersection).
---------------------------------------------------------------------------
function intersection(X, Y, opts)
	-- arguments
	local EPS
	if type(opts) == "number" then
		-- old style: intersection(X, Y, EPS)
		EPS = opts
		opts = nil
	elseif type(opts) == "table" then
		EPS = opts.eps or opts.EPS or tkz.epsilon
	elseif opts == nil then
		EPS = tkz.epsilon
	else
		error("tkz-elements: intersection: invalid third argument (table or number expected)", 2)
	end
	opts = opts or {}

	local function kind(O)
		if type(O) == "table" then
			if O.type == "conic" then
				return O.subtype or "conic"
			end
			return O.type
		end
		return type(O)
	end
	local function unsupported()
		error("tkz-elements: intersection: unsupported objects ("
			.. tostring(kind(X)) .. ", " .. tostring(kind(Y)) .. ")", 3)
	end
	if type(X) ~= "table" or type(Y) ~= "table" then
		unsupported()
	end

	local t = {} -- intersection points

	local function add(z1, z2)
		if z1 then
			table.insert(t, z1)
		end
		if z2 then
			table.insert(t, z2)
		end
	end

	-- line and conic, in any order
	local function line_conic(Li, CO)
		if CO.subtype == "parabola" then
			add(CO:inter_Pa_line_(Li.pa, Li.pb, EPS))
		elseif CO.subtype == "hyperbola" then
			add(CO:inter_Hy_line_(Li.pa, Li.pb, EPS))
		elseif CO.subtype == "ellipse" then
			add(intersection_le(Li, CO, EPS))
		else
			unsupported()
		end
	end

	if X.type == "circle" and Y.type == "circle" then
		add(intersection_cc(X, Y, EPS))
	elseif X.type == "circle" and Y.type == "line" then
		add(intersection_lc(Y, X, EPS))
	elseif X.type == "line" and Y.type == "circle" then
		add(intersection_lc(X, Y, EPS))
	elseif X.type == "line" and Y.type == "line" then
		add(intersection_ll(X, Y, EPS))
	elseif X.type == "line" and Y.type == "conic" then
		line_conic(X, Y)
	elseif X.type == "conic" and Y.type == "line" then
		line_conic(Y, X)
	else
		unsupported()
	end

	-- order of the points according to the options
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

	return table.unpack(t)
end
