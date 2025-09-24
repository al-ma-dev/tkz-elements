-- File: tkz_elements_triangles.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

triangle = {}
triangle.__index = triangle
function triangle:new(za, zb, zc)
	local type = "triangle"
	local circumcenter = circum_center_(za, zb, zc)
	local centroid = barycenter_({ za, 1 }, { zb, 1 }, { zc, 1 })
	local incenter = in_center_(za, zb, zc)
	local orthocenter = ortho_center_(za, zb, zc)
	local eulercenter = euler_center_(za, zb, zc)
	local spiekercenter = spieker_center_(za, zb, zc)
	local orientation = find_orientation_(za, zb, zc)
	local c = point.abs(zb - za)
	local a = point.abs(zc - zb)
	local b = point.abs(za - zc)
	local alpha = find_angle_(za, zb, zc)
	local beta = find_angle_(zb, zc, za)
	local gamma = find_angle_(zc, za, zb)
	local ab = line(za, zb)
	local ca = line(zc, za)
	local bc = line(zb, zc)
	local semiperimeter = (a + b + c) / 2
	local area = math.sqrt(semiperimeter * (semiperimeter - a) * (semiperimeter - b) * (semiperimeter - c))
	local inradius = area / semiperimeter
	local circumradius = (a * b * c) / (4 * area)
	local tr = {
		pa = za,
		pb = zb,
		pc = zc,
		type = type,
		circumcenter = circumcenter,
		centroid = centroid,
		incenter = incenter,
		eulercenter = eulercenter,
		orthocenter = orthocenter,
		spiekercenter = spiekercenter,
		a = a,
		b = b,
		c = c,
		ab = ab,
		ca = ca,
		bc = bc,
		alpha = alpha,
		beta = beta,
		gamma = gamma,
		angle_alpha = angle_alpha,
		angle_beta = angle_beta,
		angle_gamma = angle_gamma,
		semiperimeter = semiperimeter,
		area = area,
		inradius = inradius,
		circumradius = circumradius,
		orientation = orientation,
	}
	setmetatable(tr, self)
	return tr
end

setmetatable(triangle, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

function triangle:get(i)
	if i == 1 then
		return self.pa
	elseif i == 2 then
		return self.pb
	elseif i == 3 then
		return self.pc
	else
		return self.pa, self.pb, self.pc
	end
end
-----------------------
-- Result -> boolean
-----------------------

function triangle:in_out(pt)
	return in_out_(self.pa, self.pb, self.pc, pt)
end

function triangle:check_equilateral()
	return check_equilateral_(self.pa, self.pb, self.pc)
end
triangle.is_equilateral = triangle.check_equilateral

function triangle:check_acutangle()
	local asq = self.a * self.a
	local bsq = self.b * self.b
	local csq = self.c * self.c
	if asq + bsq > csq and bsq + csq > asq and csq + asq > bsq then
		return true
	else
		return false
	end
end
triangle.is_acute = triangle.check_acutangle
-----------------------
-- Result -> real
-----------------------

function triangle:area() -- obsolete
	return area_(self.pa, self.pb, self.pc)
end

function triangle:barycentric_coordinates(pt)
	local xA, yA = self.pa:get()
	local xB, yB = self.pb:get()
	local xC, yC = self.pc:get()
	local xP, yP = pt:get()
	local orient2d_ABC = orient2d_(xA, yA, xB, yB, xC, yC)
	local l_A = orient2d_(xP, yP, xB, yB, xC, yC) / orient2d_ABC
	local l_B = orient2d_(xA, yA, xP, yP, xC, yC) / orient2d_ABC
	local l_C = orient2d_(xA, yA, xB, yB, xP, yP) / orient2d_ABC
	return l_A, l_B, l_C
end

function triangle:trilinear_coordinates(pt)
	local la, lb, lc = self:barycentric_coordinates(pt)
	local ta, tb, tc = la / self.a, lb / self.b, lc / self.c
	-- normalize
	local N = ta + tb + tc
	return ta / N, tb / N, tc / N
end

function triangle:trilinear_to_d(x, y, z)
	return trilinear_to_d_(x, y, z, self.a, self.b, self.c)
end

function triangle:get_angle(n)
	local a, b, c
	a = self.pa
	b = self.pb
	c = self.pc
	if n == 1 then
		return point.arg((a - b) / (c - b))
	elseif n == 2 then
		return point.arg((b - c) / (a - c))
	else
		return point.arg((c - a) / (b - a))
	end
end

function triangle:parameter(p)
	local a, b, c = self.a, self.b, self.c -- a=BC, b=CA, c=AB
	local A, B, C = self.pa, self.pb, self.pc
	local AB, BC, CA = self.ab, self.bc, self.ca
	local ptotal = a + b + c

	if AB:in_out_segment(p) then
		return length_(A, p) / ptotal
	elseif BC:in_out_segment(p) then
		return (c + length_(B, p)) / ptotal
	elseif CA:in_out_segment(p) then
		return (c + a + length_(C, p)) / ptotal
	else
		tex.error("The point is not on the boundary of the triangle.")
	end
end

-----------------------
-- Result -> point
-----------------------
function triangle:barycenter(ka, kb, kc)
	return barycenter_({ self.pa, ka }, { self.pb, kb }, { self.pc, kc })
end
triangle.barycentric = triangle.barycenter

function triangle:base(u, v) -- (ab,ac) base coord u,v
	return barycenter_({ self.pa, (1 - u - v) }, { self.pb, u }, { self.pc, v })
end

function triangle:point(t)
	local p = self.a + self.b + self.c -- périmètre
	local t1 = self.c / p -- fin de AB
	local t2 = (self.c + self.a) / p -- fin de AB + BC

	if t <= t1 then
		return self.ab:point(t / t1)
	elseif t <= t2 then
		return self.bc:point((t - t1) / (t2 - t1))
	else
		return self.ca:point((t - t2) / (1 - t2))
	end
end

function triangle:random(inside)
	inside = (inside == "inside")
	math.randomseed(os.time())
	if inside then
		local x = self.bc:random()
		local L = line:new(self.pa, x)
		return L:random()
	else
		return self:point(math.random())
	end
end

function triangle:trilinear(a, b, c)
	return barycenter_({ self.pa, a * self.a }, { self.pb, b * self.b }, { self.pc, c * self.c })
end

function triangle:kimberling(n)
	local cos, sin, tan, pi = math.cos, math.sin, math.tan, math.pi
	local A, B, C = self.alpha, self.beta, self.gamma
	local a, b, c = self.a, self.b, self.c
	local pi = math.pi
	if n == 1 then
		return self:trilinear(1, 1, 1) -- incenter
	elseif n == 2 then
		return self:barycentric(1, 1, 1) -- centroid
	elseif n == 3 then
		return self:trilinear(cos(A), cos(B), cos(C)) --circumcenter
	elseif n == 4 then
		return self:barycentric(tan(A), tan(B), tan(C)) -- orthocenter
	elseif n == 5 then
		return self:trilinear(cos(B - C), cos(C - A), cos(A - B)) --nine
	elseif n == 6 then
		return self:trilinear(a, b, c) -- lemoine
	elseif n == 7 then
		return self:barycentric(1 / (b + c - a), 1 / (a + c - b), 1 / (a + b - c)) -- gergonne
	elseif n == 8 then
		return self:barycentric(b + c - a, a + c - b, a + b - c) -- nagel
	elseif n == 9 then
		return self:barycentric(a * (b + c - a), b * (a + c - b), c * (a + b - c)) -- mittenpunkt
	elseif n == 10 then
		return self:barycentric(b + c, a + c, a + b) -- spieker
	elseif n == 11 then
		return self:trilinear(1 - cos(B - C), 1 - cos(C - A), 1 - cos(A - B)) -- feuerbach
	elseif n == 13 then
		return self:trilinear(1 / sin(A + pi / 3), 1 / sin(B + pi / 3), 1 / sin(C + pi / 3)) -- first fermat
	elseif n == 14 then
		return self:trilinear(1 / sin(A - pi / 3), 1 / sin(B - pi / 3), 1 / sin(C - pi / 3)) -- second fermat
	elseif n == 19 then
		return self:trilinear(tan(A), tan(B), tan(C)) -- clawson
	elseif n == 20 then
		return self:barycentric(tan(B) + tan(C) - tan(A), tan(A) + tan(C) - tan(B), tan(A) + tan(B) - tan(C)) --de Longchamps
	elseif n == 39 then
		return self:trilinear(a * (b ^ 2 + c ^ 2), b * (c ^ 2 + a ^ 2), c * (a ^ 2 + b ^ 2)) --brocard midpoint
	elseif n == 55 then
		return self:trilinear(a * (b + c - a), b * (c + a - b), c * (a + b - c))
	elseif n == 56 then
		return self:trilinear(a / (b + c - a), b / (c + a - b), c / (a + b - c))
	elseif n == 110 then
		return self:trilinear(a / (b ^ 2 - c ^ 2), b / (c ^ 2 - a ^ 2), c / (a ^ 2 - b ^ 2)) -- kiepert parabola
	elseif n == 115 then
		return self:barycentric((b ^ 2 - c ^ 2) ^ 2, (c ^ 2 - a ^ 2) ^ 2, (a ^ 2 - b ^ 2) ^ 2) -- kiepert hyperbola
	elseif n == 175 then
		ra, rb, rc = radius_excircle_(self.pa, self.pb, self.pc)
		return self:barycentric(a - ra, b - rb, c - rc)
	elseif n == 176 then
		ra, rb, rc = radius_excircle_(self.pa, self.pb, self.pc)
		return self:barycentric(a + ra, b + rb, c + rc)
	elseif n == 213 then
		return self:trilinear((b + c) * a ^ 2, (c + a) * b ^ 2, (a + b) * c ^ 2)
	elseif n == 264 then
		return self:barycentric(1 / sin(2 * A), 1 / sin(2 * B), 1 / sin(2 * C))
	elseif n == 371 then
		return self:trilinear(cos(A - pi / 4), cos(B - pi / 4), cos(C - pi / 4))
	elseif n == 598 then
		return self:trilinear(
			b * c / (a ^ 2 - 2 * b ^ 2 - 2 * c ^ 2),
			c * a / (b ^ 2 - 2 * c ^ 2 - 2 * a ^ 2),
			a * b / (c ^ 2 - 2 * a ^ 2 - 2 * b ^ 2)
		) -- to get lemoine triangle
	else
		tex.error({ " Your number is not yet in the list" })
	end
end

function triangle:isogonal(pt)
	local pa = self.bc:reflection(pt)
	local pb = self.ca:reflection(pt)
	local pc = self.ab:reflection(pt)
	return circum_center_(pa, pb, pc)
end

function triangle:excenter(pt)
	if pt == self.pa then
		return self:trilinear(-1, 1, 1)
	elseif pt == self.pb then
		return self:trilinear(1, -1, 1)
	elseif pt == self.pc then
		return self:trilinear(1, 1, -1)
	else
		tex.error("The argument must be a vertex of the triangle")
	end
end

function triangle:projection(p)
	local p1 = projection_(self.pb, self.pc, p)
	local p2 = projection_(self.pa, self.pc, p)
	local p3 = projection_(self.pa, self.pb, p)
	return p1, p2, p3
end

function triangle:parallelogram()
	local x = self.pc + self.pa - self.pb
	return x
end

function triangle:bevan_point()
	return circum_center_(excentral_tr_(self.pa, self.pb, self.pc))
end

function triangle:mittenpunkt_point()
	return lemoine_point_(excentral_tr_(self.pa, self.pb, self.pc))
end

function triangle:gergonne_point()
	return gergonne_point_(self.pa, self.pb, self.pc)
end

function triangle:nagel_point()
	return nagel_point_(self.pa, self.pb, self.pc)
end

function triangle:feuerbach_point()
	return feuerbach_point_(self.pa, self.pb, self.pc)
end

function triangle:symmedian_point()
	return lemoine_point_(self.pa, self.pb, self.pc)
end

triangle.lemoine_point = triangle.symmedian_point
triangle.grebe_point = triangle.symmedian_point

function triangle:spieker_center()
	return spieker_center_(self.pa, self.pb, self.pc)
end

function triangle:euler_points()
	H = self.orthocenter
	return midpoint_(H, self.pa), midpoint_(H, self.pb), midpoint_(H, self.pc)
end

function triangle:nine_points()
	local ma, mb, mc, ha, hb, hc
	-- Calculate the medial triangle
	ma, mb, mc = medial_tr_(self.pa, self.pb, self.pc)
	-- Calculate the orthic triangle
	ha, hb, hc = orthic_tr_(self.pa, self.pb, self.pc)
	-- Calculate the orthocenter
	H = self.orthocenter
	-- Return the points of the nine-point circle
	return ma, mb, mc, ha, hb, hc, midpoint_(H, self.pa), midpoint_(H, self.pb), midpoint_(H, self.pc)
end

function triangle:first_lemoine_points()
	local lc = self:lemoine_point()
	local p1 = intersection_ll_(lc, ll_from_(lc, self.pa, self.pb), self.pa, self.pc)
	local p2 = intersection_ll_(lc, ll_from_(lc, self.pb, self.pc), self.pb, self.pa)
	local p3 = intersection_ll_(lc, ll_from_(lc, self.pc, self.pa), self.pc, self.pb)
	local p5 = intersection_ll_(lc, ll_from_(lc, self.pa, self.pb), self.pb, self.pc)
	local p6 = intersection_ll_(lc, ll_from_(lc, self.pb, self.pc), self.pa, self.pc)
	local p4 = intersection_ll_(lc, ll_from_(lc, self.pc, self.pa), self.pa, self.pb)
	return p1, p2, p3, p4, p5, p6
end

function triangle:second_lemoine_points()
	local lc = self:lemoine_point()
	local ha, hb, hc = orthic_tr_(self.pa, self.pb, self.pc)
	local p1 = intersection_ll_(lc, ll_from_(lc, ha, hb), self.pa, self.pc)
	local p2 = intersection_ll_(lc, ll_from_(lc, hb, hc), self.pb, self.pa)
	local p3 = intersection_ll_(lc, ll_from_(lc, hc, ha), self.pc, self.pb)
	local p5 = intersection_ll_(lc, ll_from_(lc, ha, hb), self.pb, self.pc)
	local p6 = intersection_ll_(lc, ll_from_(lc, hb, hc), self.pa, self.pc)
	local p4 = intersection_ll_(lc, ll_from_(lc, hc, ha), self.pa, self.pb)
	return p1, p2, p3, p4, p5, p6
end

function triangle:soddy_points()
	return soddy_center_(self.pa, self.pb, self.pc)
end

-- soddy centers
function triangle:soddy_center(outer)
	outer = (outer == "outer")
	local index = outer and 175 or 176
	return self:kimberling(index)
end

function triangle:conway_points()
	local a1 = report_(self.pb, self.pa, length_(self.pb, self.pc), self.pa)
	local a2 = report_(self.pc, self.pa, length_(self.pb, self.pc), self.pa)
	local b1 = report_(self.pa, self.pb, length_(self.pa, self.pc), self.pb)
	local b2 = report_(self.pc, self.pb, length_(self.pa, self.pc), self.pb)
	local c1 = report_(self.pb, self.pc, length_(self.pb, self.pa), self.pc)
	local c2 = report_(self.pa, self.pc, length_(self.pb, self.pa), self.pc)
	return a1, a2, b1, b2, c1, c2
end

function triangle:conway_center()
	return self.incenter
end

function triangle:first_fermat_point()
	local x = equilateral_tr_(self.pb, self.pa)
	local y = equilateral_tr_(self.pa, self.pc)
	return intersection_ll_(x, self.pc, y, self.pb)
end

function triangle:second_fermat_point()
	local x = equilateral_tr_(self.pa, self.pb)
	local y = equilateral_tr_(self.pc, self.pa)
	return intersection_ll_(x, self.pc, y, self.pb)
end

function triangle:orthic_axis_points()
	return orthic_axis_(self.pa, self.pb, self.pc)
end

function triangle:kenmotu_point()
	return self:kimberling(371)
end

function triangle:kenmotu_points()
	local ck = self:kenmotu_circle()
	local p1, p2 = intersection_lc_(self.pa, self.pb, ck.center, ck.through)
	local p3, p4 = intersection_lc_(self.pb, self.pc, ck.center, ck.through)
	local p5, p6 = intersection_lc_(self.pc, self.pa, ck.center, ck.through)
	return p1, p2, p3, p4, p5, p6
end

function triangle:taylor_points()
	local a, b, c = orthic_tr_(self.pa, self.pb, self.pc)
	return projection_(self.pa, self.pc, a),
		projection_(self.pa, self.pb, a),
		projection_(self.pb, self.pa, b),
		projection_(self.pb, self.pc, b),
		projection_(self.pc, self.pb, c),
		projection_(self.pc, self.pa, c)
end

function triangle:adams_points()
	local i = self:kimberling(1)
	local g = self:kimberling(7)
	local l = projection_(self.pa, self.pc, i)
	local n = projection_(self.pa, self.pb, i)
	local m = projection_(self.pb, self.pc, i)
	local x = ll_from_(g, m, n)
	local y = ll_from_(g, n, l)
	local z = ll_from_(g, m, l)
	local t1 = intersection_ll_(x, g, self.pa, self.pb)
	local t4 = intersection_ll_(x, g, self.pb, self.pc)
	local t2 = intersection_ll_(y, g, self.pa, self.pb)
	local t5 = intersection_ll_(y, g, self.pa, self.pc)
	local t3 = intersection_ll_(z, g, self.pb, self.pc)
	local t6 = intersection_ll_(z, g, self.pa, self.pc)
	return t1, t2, t3, t4, t5, t6
end

function triangle:lamoen_points()
	local ma, mb, mc = medial_tr_(self.pa, self.pb, self.pc)
	local g = centroid_(self.pa, self.pb, self.pc)
	local p1 = circum_center_(self.pa, mc, g)
	local p2 = circum_center_(self.pb, ma, g)
	local p3 = circum_center_(self.pb, mc, g)
	local p4 = circum_center_(self.pc, mb, g)
	local p5 = circum_center_(self.pc, ma, g)
	local p6 = circum_center_(self.pa, mb, g)
	return p1, p2, p3, p4, p5, p6
end

function triangle:yiu_centers()
	local ap = symmetry_axial_(self.pb, self.pc, self.pa)
	local bp = symmetry_axial_(self.pa, self.pc, self.pb)
	local cp = symmetry_axial_(self.pa, self.pb, self.pc)
	local oa = circum_center_(self.pa, bp, cp)
	local ob = circum_center_(self.pb, ap, cp)
	local oc = circum_center_(self.pc, bp, ap)
	return oa, ob, oc
end

function triangle:first_brocard_point()
	return self:trilinear(self.c / self.b, self.a / self.c, self.b / self.a)
end

function triangle:second_brocard_point()
	return self:trilinear(self.b / self.c, self.c / self.a, self.a / self.b)
end

function triangle:brocard_midpoint()
	return self:kimberling(39)
end

function triangle:macbeath_point()
	return self:kimberling(264)
end

function triangle:poncelet_point(pt)
	local A, B, C, D = self.pa, self.pb, self.pc, pt
	local e1 = euler_center_(A, B, C)
	local e2 = euler_center_(A, C, D)
	local m = midpoint_(A, C)
	local x, y = intersection_cc_(e1, m, e2, m)

	if not x or not y then
		tex.error("Poncelet construction failed: no intersection.")
	end

	if x == m then
		return y
	else
		return x
	end
end

function triangle:orthopole(l)
	local ap, bp, cp = l:projection(self.pa, self.pb, self.pc)
	local bpp = self.ca:projection(bp)
	local app = self.bc:projection(ap)
	local la = line(ap, app)
	local lb = line(bp, bpp)
	return intersection(la, lb)
end
-------------------
-- Result -> line
-------------------

function triangle:symmedian_line(arg)
	local pts = { self.pa, self.pb, self.pc }
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]
	local L = self:lemoine_point()

	local P = intersection_ll_(A, L, B, C)
	return line:new(A, P)
end


function triangle:altitude(arg)
	local pts = { self.pa, self.pb, self.pc }
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	return line:new(A, projection_(B, C, A))
end


function triangle:bisector(arg)
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local pts = { self.pa, self.pb, self.pc }
	local I = self.incenter

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	local P = intersection_ll_(A, I, B, C)
	return line:new(A, P)
end


function triangle:bisector_ext(arg)
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local pts = { self.pa, self.pb, self.pc }
	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	return line:new(A, bisector_ext_(A, B, C))
end


function triangle:mediator(arg)
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local pts = { self.pa, self.pb, self.pc }
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	return line:new(B, C):mediator()
end


function triangle:trisector(arg)
	local pts = { self.pa, self.pb, self.pc }
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	local an = get_angle_(A, B, C)
	local pti = report_(A, B, 1)
	local ptj = rotation_(A, an / 3, pti)
	local ptk = rotation_(A, an / 3, ptj)

	return line(A, ptj), line(A, ptk)
end

function triangle:antiparallel(pt, arg)
	local a, b, c = self.pa, self.pb, self.pc
	local I = self.incenter
	local i = resolve_triangle_index(self, arg)

	local S = { a, b, c }
	local A = S[i + 1] -- sommet de départ
	local B = S[(i + 1) % 3 + 1] -- premier sommet adjacent
	local C = S[(i + 2) % 3 + 1] -- second sommet adjacent

	-- u, v sont les symétriques de B et C par rapport à la bissectrice en I
	local u = symmetry_axial_(A, I, B)
	local v = symmetry_axial_(A, I, C)
	local w = ll_from_(pt, u, v)

	return line:new(intersection_ll_(pt, w, B, A), intersection_ll_(pt, w, C, A))
end

function triangle:orthic_axis()
	local x, y, z = orthic_axis_(self.pa, self.pb, self.pc)
	return line:new(x, z)
end

-- N,H,G,O -- check_equilateral_ (a,b,c)
function triangle:euler_line()
	if check_equilateral_(self.pa, self.pb, self.pc) then
		tex.error("An error has occurred", { "No euler line with equilateral triangle" })
	else
		local a, b, c = orthic_axis_(self.pa, self.pb, self.pc)
		local x = projection_(a, c, self.eulercenter)
		return line:new(self.eulercenter, x)
	end
end

-- with pt on the circumcircle
function triangle:steiner_line(pt)
	local u = symmetry_axial_(self.pa, self.pb, pt)
	local v = symmetry_axial_(self.pa, self.pc, pt)
	return line:new(u, v)
end

function triangle:lemoine_axis() -- revoir car problème
	local C = self:circum_circle()
	local pl = self:lemoine_point()
	return C:polar(pl)
end

function triangle:fermat_axis()
	local x = self:first_fermat_point()
	local y = self:second_fermat_point()
	return line:new(x, y)
end

function triangle:brocard_axis()
	return line:new(self.circumcenter, self:lemoine_point())
end

function triangle:simson_line(pt) -- pt on circumcircle
	local x = self.ab:projection(pt)
	local y = self.bc:projection(pt)
	return line:new(x, y)
end

function triangle:soddy_line()
	return line:new(self:soddy_center(), self:soddy_center("outer"))
end

-----------------------
--- Result -> circles --
-----------------------
function triangle:euler_circle()
	return circle:new(euler_center_(self.pa, self.pb, self.pc), midpoint_(self.pb, self.pc))
end

function triangle:circum_circle()
	return circle:new(circum_circle_(self.pa, self.pb, self.pc), self.pa)
end

function triangle:in_circle()
	return circle:new(self.incenter, projection_(self.pb, self.pc, self.incenter))
end
-- see excenter ??


function triangle:ex_circle(arg)
	local pts = { self.pa, self.pb, self.pc }
	local i = resolve_triangle_index(self, arg)
	if i < 0 or i > 2 then
		return nil
	end

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	local O = ex_center_(B, C, A)
	return circle:new(O, projection_(C, A, O))
end

function triangle:spieker_circle()
	local ma, mb, mc, sp
	ma, mb, mc = medial_tr_(self.pa, self.pb, self.pc)
	sp = in_center_(ma, mb, mc)
	return circle:new(sp, projection_(ma, mb, sp))
end

function triangle:cevian_circle(p)
	local pta, ptb, ptc = cevian_(self.pa, self.pb, self.pc, p)
	return circle:new(circum_circle_(pta, ptb, ptc), pta)
end

function triangle:symmedial_circle()
	local pta, ptb, ptc, p
	p = lemoine_point_(self.pa, self.pb, self.pc)
	pta, ptb, ptc = cevian_(self.pa, self.pb, self.pc, p)
	return circle:new(circum_circle_(pta, ptb, ptc), pta)
end

function triangle:conway_circle()
	local t
	t = report_(self.pb, self.pa, length_(self.pb, self.pc), self.pa)
	return circle:new(self.incenter, t)
end

function triangle:pedal_circle(pt)
	local x, y, z, c
	x = projection_(self.pb, self.pc, pt)
	y = projection_(self.pa, self.pc, pt)
	z = projection_(self.pa, self.pb, pt)
	c = circum_center_(x, y, z)
	return circle:new(c, x)
end

function triangle:first_lemoine_circle()
	local lc, oc
	lc = self:lemoine_point()
	oc = self.circumcenter
	return circle:new(midpoint_(lc, oc), intersection_ll_(lc, ll_from_(lc, self.pa, self.pb), self.pa, self.pc))
end

function triangle:second_lemoine_circle()
	local lc, a, b, c, r, th
	lc = self:lemoine_point()
	a = point.abs(self.pc - self.pb)
	b = point.abs(self.pa - self.pc)
	c = point.abs(self.pb - self.pa)
	r = (a * b * c) / (a * a + b * b + c * c)
	th = lc + point(r, 0)
	return circle:new(lc, th)
end

function triangle:bevan_circle()
	local o, r, s, t
	o = circum_center_(excentral_tr_(self.pa, self.pb, self.pc))
	r, s, t = excentral_tr_(self.pa, self.pb, self.pc)
	return circle:new(o, r)
end

function triangle:taylor_circle()
	local a, b, c = orthic_tr_(self.pa, self.pb, self.pc)
	local d = projection_(self.pa, self.pb, a)
	local e = projection_(self.pb, self.pc, b)
	local f = projection_(self.pc, self.pa, c)
	return circle:new(circum_circle_(d, e, f), d)
end

function triangle:adams_circle()
	local i = self:kimberling(1)
	local g = self:kimberling(7)
	local m = projection_(self.pb, self.pc, i)
	local n = projection_(self.pa, self.pb, i)
	local x = ll_from_(g, m, n)
	local t = intersection_ll_(x, g, self.pa, self.pb)
	return circle:new(i, t)
end

function triangle:lamoen_circle()
	local ma, mb, mc = medial_tr_(self.pa, self.pb, self.pc)
	local g = centroid_(self.pa, self.pb, self.pc)
	local p1 = circum_center_(self.pa, mb, g)
	local p2 = circum_center_(self.pb, ma, g)
	local p3 = circum_center_(self.pc, ma, g)
	local c = circum_center_(p1, p2, p3)
	return circle:new(c, p1)
end

-- soddy circles
function triangle:soddy_circle(outer)
	outer = (outer == "outer")
	local index = outer and 175 or 176
	local c = self:kimberling(index)
	local ra = (self.b + self.c - self.a) / 2
	local ta = report_(self.pa, self.pb, ra, self.pa)
	local x, y = intersection_lc_(self.pa, c, self.pa, ta)
	local t
	if outer then
		t = (length_(x, c) > length_(y, c)) and x or y
	else
		t = (length_(x, c) < length_(y, c)) and x or y
	end
	return circle:new(c, t)
end

function triangle:yiu_circles()
	local x, y, z = self:yiu_centers()
	return circle:new(x, self.pa), circle:new(y, self.pb), circle:new(z, self.pc)
end

function triangle:kenmotu_circle()
	local a, b, c = self.a, self.b, self.c
	local area = self.area
	local rho = (math.sqrt(2) * a * b * c) / (4 * area + (a ^ 2 + b ^ 2 + c ^ 2))
	local center = self:kimberling(371)
	return circle:new(through(center, rho))
end

-- Circle tangent to two straight lines passing through a given point
function triangle:c_ll_p(p)
	-- Compute the bisector of the triangle
	local lbi = bisector(self.pa, self.pb, self.pc)

	if lbi:in_out(p) then
		-- Orthogonal projection of p onto the bisector
		local lp = lbi:ortho_from(p)

		-- Intersection of line from p to its projection with self.pa and self.pb
		local i = intersection_ll_(p, lp.pb, self.pa, self.pb)

		-- Intersection points of the line with the circle defined by (i, p)
		local t1, t2 = intersection_lc_(self.pa, self.pb, i, p)

		-- Create the main line and find orthogonal projections from t1 and t2
		local lab = line:new(self.pa, self.pb)
		local x = lab:ortho_from(t1).pb
		local y = lab:ortho_from(t2).pb

		-- Return two circles based on the orthogonal projections and points t1, t2
		return circle:new(intersection_ll_(x, t1, self.pa, p), t1), circle:new(intersection_ll_(y, t2, self.pa, p), t2)
	else
		local lab = line:new(self.pa, self.pb)
		-- Reflection of p across the bisector
		local q = lbi:reflection(p)

		-- Compute circles from the Wallis construction
		local c1, c2 = lab:c_l_pp(p, q)

		-- Return two circles with centers and points on their circumference
		return c1, c2
	end
end


-- Circle tangent to the sides originating from p and to the circle passing through the other two vertices.
-- argument the vertex of the chosen triangle, the center of the circle passing through the other two vertices.

function triangle:c_c(p, C)
	local a, b, c
	local i = resolve_triangle_index(self, p)
	if i == 0 then
		a, b, c = self.pa, self.pb, self.pc
	elseif i == 1 then
		a, b, c = self.pb, self.pc, self.pa
	elseif i == 2 then
		a, b, c = self.pc, self.pa, self.pb
	end
	local Lm = self:mediator(p)
	local n = intersection(Lm, C)
	local i = self.incenter
	local m = line(c, i):reflection(n)
	local L = line(c, m):parallel_from(i)
	local Lac = line(a, c)
	local e = intersection(L, Lac)
	local Lia = line(i, a)
	local Le = Lac:orthogonal_from(e)
	return circle(intersection(Lia, Le), e)
end
triangle.thebault = triangle.c_c

function triangle:three_tangent_circles()
	local i = self.incenter
	local a, b, c = self:projection(i)
	return circle:new(self.pa, c), circle:new(self.pb, a), circle:new(self.pc, b)
end


function triangle:mixtilinear_incircle(arg)
	local i = resolve_triangle_index(self, arg)

	local incenter = self.incenter
	local pts = { self.pa, self.pb, self.pc }
	local segs = { self.ab, self.bc, self.ca }

	local A = pts[i + 1]
	local B = pts[(i + 1) % 3 + 1]
	local C = pts[(i + 2) % 3 + 1]

	local AB = segs[i + 1]
	local BC = segs[(i + 1) % 3 + 1]
	local CA = segs[(i + 2) % 3 + 1]

	local l = line:new(A, incenter)
	local lp = l:orthogonal_from(incenter)
	local P1 = intersection(lp, CA)
	local P2 = intersection(lp, AB)
	local l1 = CA:orthogonal_from(P1)
	local l2 = AB:orthogonal_from(P2)
	local W = intersection(l1, l2)

	return circle(W, P1)
end

-------------------
-- Result -> triangle
-------------------
function triangle:orthic()
	return triangle:new(orthic_tr_(self.pa, self.pb, self.pc))
end

function triangle:medial()
	return triangle:new(medial_tr_(self.pa, self.pb, self.pc))
end

function triangle:incentral()
	return triangle:new(incentral_tr_(self.pa, self.pb, self.pc))
end

function triangle:excentral()
	return triangle:new(excentral_tr_(self.pa, self.pb, self.pc))
end

function triangle:intouch()
	return triangle:new(intouch_tr_(self.pa, self.pb, self.pc))
end

function triangle:contact()
	return triangle:new(intouch_tr_(self.pa, self.pb, self.pc))
end
triangle.contact = triangle.intouch
triangle.gergonne = triangle.intouch

function triangle:extouch()
	return triangle:new(extouch_tr_(self.pa, self.pb, self.pc))
end
triangle.nagel = triangle.extouch

function triangle:feuerbach()
	return triangle:new(feuerbach_tr_(self.pa, self.pb, self.pc))
end

function triangle:anti()
	return triangle:new(anti_tr_(self.pa, self.pb, self.pc))
end

triangle.anticomplementary = triangle.anti
triangle.similar = triangle.anti

function triangle:tangential()
	return triangle:new(tangential_tr_(self.pa, self.pb, self.pc))
end

function triangle:cevian(p)
	return triangle:new(cevian_(self.pa, self.pb, self.pc, p))
end

function triangle:symmedian()
	local p = lemoine_point_(self.pa, self.pb, self.pc)
	return triangle:new(cevian_(self.pa, self.pb, self.pc, p))
end
triangle.symmedial = triangle.symmedian

function triangle:lemoine()
	local X = self:kimberling(598)
	return triangle:new(cevian_(self.pa, self.pb, self.pc, X))
end

function triangle:macbeath()
	local X = self:kimberling(264)
	return triangle:new(cevian_(self.pa, self.pb, self.pc, X))
end

function triangle:euler()
	return triangle:new(euler_points_(self.pa, self.pb, self.pc))
end

function triangle:pedal(pt)
	return triangle:new(self:projection(pt))
end

function triangle:yiu()
	return triangle:new(self:yiu_centers())
end


function triangle:reflection()
	return triangle:new(
		symmetry_axial_(self.pb, self.pc, self.pa),
		symmetry_axial_(self.pa, self.pc, self.pb),
		symmetry_axial_(self.pa, self.pb, self.pc)
	)
end

function triangle:circumcevian(pt)
	local circum = self:circum_circle()
	local x = intersection_lc_(self.pa, pt, circum.center, circum.through)
	local y = intersection_lc_(self.pb, pt, circum.center, circum.through)
	local z = intersection_lc_(self.pc, pt, circum.center, circum.through)
	return triangle:new(x, y, z)
end


function triangle:morley()
	local la1, la2 = self:trisector(self.pa)
	local lb1, lb2 = self:trisector(self.pb)
	local lc1, lc2 = self:trisector(self.pc)
	local D = intersection(la1, lb2)
	local E = intersection(lb1, lc2)
	local F = intersection(lc1, la2)
	return triangle:new(D, E, F)
end


function triangle:napoleon(swap)
	swap = (swap == "swap")
	if swap then
		local a = self.ab:equilateral().centroid
		local b = self.bc:equilateral().centroid
		local c = self.ca:equilateral().centroid
		return triangle:new(a, b, c)
	else
		local a = self.ab:equilateral("swap").centroid
		local b = self.bc:equilateral("swap").centroid
		local c = self.ca:equilateral("swap").centroid
		return triangle:new(a, b, c)
	end
end


function triangle:soddy(outer)
	local ci = self:soddy_circle(outer)
	local a, b, c = self.pa, self.pb, self.pc
	outer = (outer == "outer")

	local ce = ci.center
	local ti = ci.through
	local sa = intersection_lc_(ce, a, ce, ti)
	local sb = intersection_lc_(ce, b, ce, ti)
	local sc = intersection_lc_(ce, c, ce, ti)
	return triangle:new(sa, sb, sc)
end

-------------------
-- Result -> square
-------------------

-- Method for inscribing a square in a triangle with vertex rotation
function triangle:square_inscribed(permutation)
	-- Set a default value for rotation if not supplied
	permutation = permutation or 0
	permutation = permutation + 1
	local vertices = { self.pa, self.pb, self.pc }
	local i = (permutation - 1) % 3 + 1
	local j = i % 3 + 1
	local k = j % 3 + 1
	local a = vertices[i]
	local b = vertices[j]
	local c = vertices[k]
	return square:new(square_inscribed_(a, b, c))
end
-------------------
-- Result -> conic
-------------------
function triangle:steiner_inellipse()
	return steiner_(self.pa, self.pb, self.pc)
end

function triangle:steiner_circumellipse()
	local e = steiner_(self.pa, self.pb, self.pc)
	local v = report_(e.center, e.vertex, e.a, e.vertex)
	local cv = report_(e.center, e.covertex, e.b, e.covertex)
	return conic:new(EL_points(e.center, v, cv))
end

function triangle:euler_ellipse()
	if self:check_acutangle() then
		local x, y = intersection_lc_(self.orthocenter, self.circumcenter, self.eulercenter, self.ab.mid)
		local a = 0.5 * length_(x, y)
		return conic:new(EL_bifocal(self.orthocenter, self.circumcenter, a))
	end
end

--  kiepert hyperbola
function triangle:kiepert_hyperbola()
	-- center
	local center = self:kimberling(115)
	local O = self.circumcenter
	local G = self.centroid
	--  Brocard axis
	local L = self:brocard_axis()
	local M, N = intersection_lc_(L.pa, L.pb, O, self.pa)
	-- simson lines
	local Lsa = self:simson_line(M)
	local Lsb = self:simson_line(N)
	local axis = bisector(center, Lsa.pa, Lsb.pa)
	-- new frame system
	local minor = axis:ortho_from(center)
	local sys = occs:new(minor, center)
	local x, y = sys:coordinates(self.pa)
	local a = math.sqrt(x ^ 2 - y ^ 2)
	local c = math.sqrt(2) * a
	local Fa = report_(center, axis.pb, -c, center)
	local Fb = report_(center, axis.pb, c, center)
	local K = report_(center, axis.pb, -a ^ 2 / c, center)
	local directrix = axis:ortho_from(K)
	return conic:new(Fa, directrix, math.sqrt(2))
end

--  kiepert parabola
function triangle:kiepert_parabola()
	if (self.a == self.b) or (self.a == self.c) or (self.c == self.b) then
		tex.error("An error has occurred", { "No X(110)" })
	else
		L = self:euler_line()
		F = self:kimberling(110)
		return conic:new(F, L, 1)
	end
end

--lemoine inellipse
-- bifocal a besoin des deux foyers et d'un point sur l'ellipse ou bien de a
function triangle:lemoine_inellipse()
	local G = self.centroid
	local L = self:lemoine_point()
	local Xa = self:lemoine().pa
	return conic:new(EL_bifocal(G, L, Xa))
end

--brocard inellipse
function triangle:brocard_inellipse()
	local B1 = self:first_brocard_point()
	local B2 = self:second_brocard_point()
	local Ka = self:symmedian().pa
	return conic:new(EL_bifocal(B2, B1, Ka))
end

-- orthic inellipse
function triangle:macbeath_inellipse()
	if self:check_acutangle() then
		local O = self.circumcenter
		local H = self.orthocenter
		local Xa = self:macbeath().pa
		return conic:new(EL_bifocal(O, H, Xa))
	else
		tex.error("This function is only valid if none of the  angles is obtuse.")
	end
end

function triangle:mandart_inellipse()
	local T = self
	local M = T:mittenpunkt_point()
	local Xa, Yb, Zc = T:extouch():get()
	local X, Y = M:symmetry(Xa, Yb)
	local coefficients = search_ellipse_(X, Y, Xa, Yb, Zc)
	local center, ra, rb, angle = ellipse_axes_angle(coefficients)
	return conic:new(EL_radii(center, ra, rb, angle))
end

function triangle:orthic_inellipse()
	if self:check_acutangle() then
		local T = self
		local K = T:symmedian_point()
		local Ha, Hb, Hc = T:orthic():get()
		local Sa, Sb = K:symmetry(Ha, Hb)
		local coefficients = search_ellipse_(Ha, Hb, Hc, Sa, Sb)
		local center, ra, rb, angle = ellipse_axes_angle(coefficients)
		return conic:new(EL_radii(center, ra, rb, angle))
	else
		tex.error("This function is only valid if none of the angles is obtuse.")
	end
end

function triangle:path(p1, p2)
	local list = {}

	-- Liste des côtés et sommets
	local sides = {
		{ seg = self.ab, A = self.pa, B = self.pb },
		{ seg = self.bc, A = self.pb, B = self.pc },
		{ seg = self.ca, A = self.pc, B = self.pa },
	}

	-- Trouver les indices des segments contenant p1 et p2
	local function locate(p)
		for i, s in ipairs(sides) do
			if s.seg:in_out_segment(p) then
				return i
			end
		end
		tex.error("Point not on triangle boundary.")
	end

	local i1 = locate(p1)
	local i2 = locate(p2)

	-- Cas simple : même segment
	if i1 == i2 then
		table.insert(list, "(" .. checknumber_(p1.re) .. "," .. checknumber_(p1.im) .. ")")
		table.insert(list, "(" .. checknumber_(p2.re) .. "," .. checknumber_(p2.im) .. ")")
		return path:new(list)
	end

	-- Sinon, commencer par p1
	table.insert(list, "(" .. checknumber_(p1.re) .. "," .. checknumber_(p1.im) .. ")")

	-- Parcourir les sommets intermédiaires dans l’ordre trigonométrique
	local i = i1
	repeat
		local s = sides[i]
		table.insert(list, "(" .. checknumber_(s.B.re) .. "," .. checknumber_(s.B.im) .. ")")
		i = i % 3 + 1
	until i == i2

	-- Terminer par p2
	table.insert(list, "(" .. checknumber_(p2.re) .. "," .. checknumber_(p2.im) .. ")")

	return path:new(list)
end

return triangle
