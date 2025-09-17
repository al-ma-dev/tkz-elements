-- File: tkz_elements_functions_conics.lua
-- Version: 4.20c   Date: 2025/09/17
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

function get_a(h, e)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	elseif e < 1 then
		return e * h / (1 - e ^ 2)
	elseif e > 1 then
		return e * h / (e ^ 2 - 1)
	end
end

function get_b(h, e)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	elseif e < 1 then
		return e * h / math.sqrt(1 - e ^ 2)
	elseif e > 1 then
		return e * h / math.sqrt(e ^ 2 - 1)
	end
end

function get_c(h, e)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	elseif e < 1 then
		return e ^ 2 * h / (1 - e ^ 2)
	elseif e > 1 then
		return e ^ 2 * h / (e ^ 2 - 1)
	end
end

function next_focus(F, K, e, h)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	elseif e > 1 then
		return report_(F, K, 2 * get_c(h, e))
	elseif e < 1 then
		return report_(F, K, -2 * get_c(h, e))
	end
end

function conic_center(F, K, e, h)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	elseif e > 1 then
		return report_(F, K, get_c(h, e))
	elseif e < 1 then
		return report_(F, K, -get_c(h, e))
	end
end

function get_subtype(e)
	if math.abs(e - 1) < tkz.epsilon then
		return "parabola"
	elseif e < 1 then
		return "ellipse"
	elseif e > 1 then
		return "hyperbola"
	end
end

function get_vertex(F, K, e, h)
	if math.abs(e - 1) < tkz.epsilon then
		return report_(K, F, h / 2)
	else
		local center = conic_center(F, K, e, h)
		return report_(center, F, get_a(h, e))
	end
end

function get_covertex(F, K, e, h)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	else
		local center = conic_center(F, K, e, h)
		return report_(center, ortho_from_(center, center, F), get_b(h, e))
	end
end

function get_minor_axis(F, K, e, h)
	if math.abs(e - 1) < tkz.epsilon then
		return nil
	else
		local center = conic_center(F, K, e, h)
		local pa = get_covertex(F, K, e, h)
		local pb = symmetry_(center, pa)
		return line:new(pa, pb)
	end
end

---------------------------------------------------------------------------
---------------------------------------------------------------------------

function get_points_conic_(Co, ta, tb, nb)
	if math.abs(Co.e - 1) < tkz.epsilon then
		return get_points_parabola(Co, ta, tb, nb)
	elseif Co.e > 1 then
		return get_points_hyperbola(Co, ta, tb, nb)
	elseif Co.e < 1 then
		return get_points_ellipse(Co, ta, tb, nb)
	end
end

function get_points_sym_conic_(Co, ta, tb, nb)
	return get_points_hyperbola_sym(Co, ta, tb, nb)
end

function get_points_parabola(C, ta, tb, nb)
	local points = {}
	--Function to add points in a given range
	for t = ta, tb, 1 / nb do
		local T = C.directrix:report(t, C.K)
		local LL = C.major_axis:ll_from(T)
		local x, y = mediator_(C.Fa, T)
		local pt = intersection_ll_(x, y, LL.pa, LL.pb)
		table.insert(points, "(" .. checknumber_(pt.re) .. "," .. checknumber_(pt.im) .. ")")
	end
	return points
end

function get_points_hyperbola(C, ta, tb, nb)
	local points = {}
	local LC = C.minor_axis
	local LS = LC:ll_from(C.vertex)
	for t = ta, tb, 1 / nb do
		local T = C.directrix:report(t, C.K)
		local LT = C.major_axis:ll_from(T)
		local D = intersection_ll_(LC.pa, LC.pb, C.Fa, T)
		local E = intersection_ll_(LS.pa, LS.pb, C.Fa, T)
		local P, Q = intersection_lc_(LT.pa, LT.pb, D, E)
		if length_(P, C.Fa) > length_(Q, C.Fa) then
			P, Q = Q, P
		end
		table.insert(points, "(" .. checknumber_(P.re) .. "," .. checknumber_(P.im) .. ")")
	end
	return points
end

function get_points_hyperbola_sym(C, ta, tb, nb)
	local points = {}
	local LC = C.minor_axis
	local LS = LC:ll_from(C.vertex)
	for t = ta, tb, 1 / nb do
		local T = C.directrix:report(t, C.K)
		local LT = C.major_axis:ll_from(T)
		local D = intersection_ll_(LC.pa, LC.pb, C.Fa, T)
		local E = intersection_ll_(LS.pa, LS.pb, C.Fa, T)
		local M, N = intersection_lc_(LT.pa, LT.pb, D, E)
		local P = symmetry_(C.center, M)
		local Q = symmetry_(C.center, N)
		if length_(P, C.Fb) > length_(Q, C.Fb) then
			P, Q = Q, P
		end
		table.insert(points, "(" .. checknumber_(P.re) .. "," .. checknumber_(P.im) .. ")")
	end
	return points
end

function get_points_ellipse(C, x, y, nb)
	local points = {}
	for t = x, y + 1 / nb, 1 / nb do
		-- point on circle
		CI = circle:new(C.center, C.vertex)
		local M = CI:point(t)
		-- point on ellipse
		local P = affinity_(C.Fa, C.Fb, C.center, C.covertex, C.b / C.a, M)
		table.insert(points, "(" .. checknumber_(P.re) .. "," .. checknumber_(P.im) .. ")")
	end
	return points
end
---------------------------------------------------------------------------
---------------------------------------------------------------------------

function get_one_point_conic_(C, t)
	if C.e < 1 then
		return get_one_point_ellipse(C, t)
	else
		local a, b = C.directrix.pa, C.directrix.pb
		local c, d = C.Fa, C.K
		local angle = tkz_angle_between_vectors_(a, b, c, d)
		local newDir = (angle > 0) and line:new(b, a) or line:new(a, b)

		local T = newDir:report(t, C.K)
		local LT = C.major_axis:ll_from(T)

		if math.abs(C.e - 1) < tkz.epsilon then
			return get_one_point_parabola(C, T, LT)
		else
			return get_one_point_hyperbola(C, T, LT)
		end
	end
end

function get_one_point_parabola(C, T, LT)
	local x, y = mediator_(C.Fa, T)
	return intersection_ll_(x, y, LT.pa, LT.pb)
end

function get_one_point_hyperbola(C, T, LT)
	local LC = C.minor_axis
	local LS = LC:ll_from(C.vertex)
	local D = intersection_ll_(LC.pa, LC.pb, C.Fa, T)
	local E = intersection_ll_(LS.pa, LS.pb, C.Fa, T)
	local P, Q = intersection_lc_(LT.pa, LT.pb, D, E)
	if length_(P, C.Fa) > length_(Q, C.Fa) then
		P, Q = Q, P
	end
	return P
end

function get_one_point_hyperbola_ii(C, t)
	local T = C.directrix:report(t, C.K)
	local LT = C.major_axis:ll_from(T)
	local p = get_one_point_hyperbola(C, T, LT)
	local D = C.minor_axis
	return symmetry_axial_(D.pa, D.pb, p)
end

function get_one_point_ellipse(C, t)
	-- point on circle
	CI = circle:new(C.center, C.vertex)
	local M = CI:point(t)
	return affinity_(C.Fa, C.Fb, C.center, C.covertex, C.b / C.a, M)
end

function EL_in_out(CO, pt)
	local d = point.abs(pt - CO.center)
	local L = line:new(CO.center, pt)
	local x, y = intersection(L, CO)
	local dx = point.abs(x - CO.center)
	if d < dx then
		return true
	else
		return false
	end
end

function PA_in_out(PA, pt)
	local D = PA.major_axis
	local Dp = D:ortho_from(pt)
	local x, y = intersection(Dp, PA)
	if x == false then
		return false
	else
		local L = line:new(x, y)
		return L:in_out_segment(pt)
	end
end

function HY_in_out(HY, pt)
	local D = HY.major_axis
	local Dp = D:ortho_from(pt)
	local x, y = intersection(Dp, HY)
	if x == false then
		return false
	else
		local L = line:new(x, y)
		return L:in_out_segment(pt)
	end
end

function EL_points(center, vertex, covertex)
	local a = length_(center, vertex)
	local b = length_(center, covertex)
	local c = math.sqrt(a ^ 2 - b ^ 2)
	local F = report_(center, vertex, c)
	local e = c / a
	local h = b ^ 2 / c
	local K = report_(center, F, b ^ 2 / c, F)
	local axis = line:new(vertex, center)
	local L = axis:ortho_from(K)
	return F, L, e
end

function EL_bifocal(Fa, Fb, x)
	local a
	if type(x) == "number" then
		a = x
	else -- x is a point
		a = (length_(Fa, x) + length_(Fb, x)) / 2
	end
	local center = midpoint_(Fa, Fb)
	local c = length_(center, Fa)
	local e = c / a
	local b = math.sqrt(a ^ 2 - c ^ 2)
	local h = b ^ 2 / c
	local K = report_(center, Fa, b ^ 2 / c, Fa)
	local vertex = report_(center, Fa, a)
	local axis = line:new(vertex, center)
	local L = axis:ortho_from(K)
	return Fa, L, e
end

function HY_bifocal(Fa, Fb, x)
	local a
	if type(x) == "number" then
		a = x
	else -- x is a point
		a = math.abs((length_(Fa, x) - length_(Fb, x))) / 2
	end
	local center = midpoint_(Fa, Fb)
	local c = length_(center, Fa)
	local e = c / a
	local b = math.sqrt(c ^ 2 - a ^ 2)
	local h = b ^ 2 / c
	local K = report_(center, Fa, -b ^ 2 / c, Fa)
	local vertex = report_(center, Fa, a)
	local axis = line:new(vertex, center)
	local L = axis:ortho_from(K)
	return Fa, L, e
end

function PA_dir(F, A, B)
	local CA = circle:new(A, F)
	local CB = circle:new(B, F)
	return CA:common_tangent(CB)
end

function PA_focus(D, pA, pB)
	local HA = D:projection(pA)
	local HB = D:projection(pB)
	local x, y = intersection_cc_(pA, HA, pB, HB)
	if x == false then
		tex.error("An error has occurred. Bad configuration")
		return
	else
		return x, y
	end
end

function search_center_ellipse(coefficients)
	local A, B, C, D, E, F =
		coefficients.A, coefficients.B, coefficients.C, coefficients.D, coefficients.E, coefficients.F
	local delta = B ^ 2 - 4 * A * C
	local xc = (2 * C * D - B * E) / delta
	local yc = (2 * A * E - B * D) / delta
	return point:new(xc, yc)
end

function matrix_conic(coefficients)
	local A, B, C, D, E, F =
		coefficients.A, coefficients.B, coefficients.C, coefficients.D, coefficients.E, coefficients.F
	return matrix:square(3, A, B, D, B, C, E, D, E, F)
end

function search_ellipse(...)
	local function create_coordinates_table(...)
		local T = {}
		local pointNames = { ... }

		for i, name in ipairs(pointNames) do
			T[i] = { z[name].re, z[name].im }
		end
		return T
	end
	Names = { ... }
	points = create_coordinates_table(table.unpack(Names))
	local ma = matrix:create(5, 6)
	for i, point in ipairs(points) do
		local x, y = point[1], point[2]
		ma.set[i][1] = x * x
		ma.set[i][2] = x * y
		ma.set[i][3] = y * y
		ma.set[i][4] = x
		ma.set[i][5] = y
		ma.set[i][6] = 1
	end
	local mm = ma:gauss_jordan_rect()
	local coefficients = {}
	for i = 1, 5 do
		coefficients[i] = -mm.set[i][6]
	end
	table.insert(coefficients, 1)
	local A = coefficients[1]
	local B = coefficients[2]
	local C = coefficients[3]
	local D = coefficients[4]
	local E = coefficients[5]
	local F = coefficients[6]
	return { A = A, B = B, C = C, D = D, E = E, F = F }
end

function search_ellipse_(...)
	local function create_coordinates_table_(...)
		local T = {}
		local pointNames = { ... }

		for i, name in pairs(pointNames) do
			T[i] = { name.re, name.im }
		end
		return T
	end
	Names = { ... }
	points = create_coordinates_table_(table.unpack(Names))
	local ma = matrix:create(5, 6)
	for i, point in ipairs(points) do
		local x, y = point[1], point[2]
		ma.set[i][1] = x * x
		ma.set[i][2] = x * y
		ma.set[i][3] = y * y
		ma.set[i][4] = x
		ma.set[i][5] = y
		ma.set[i][6] = 1
	end
	local mm = ma:gauss_jordan_rect()
	local coefficients = {}
	for i = 1, 5 do
		coefficients[i] = -mm.set[i][6]
	end
	table.insert(coefficients, 1)
	local A = coefficients[1]
	local B = coefficients[2]
	local C = coefficients[3]
	local D = coefficients[4]
	local E = coefficients[5]
	local F = coefficients[6]
	return { A = A, B = B, C = C, D = D, E = E, F = F }
end

function test_angle_major(Center, radius, temp_angle, A, B, C, D, E, F)
	local new_angle = 0
	local V = Center + point(math.cos(temp_angle), math.sin(temp_angle))
	local test_point = report_(Center, V, radius)
	if math.abs(test_ellipse(test_point.re, test_point.im, A, B, C, D, E, F)) < tkz.epsilon then
		new_angle = temp_angle
	else
		new_angle = temp_angle + math.pi / 2
	end
	return new_angle
end

-- Utility function to calculate ellipse axes and angles
function ellipse_axes_angle(coefficients)
	local A, B, C, D, E, F =
		coefficients.A, coefficients.B, coefficients.C, coefficients.D, coefficients.E, coefficients.F

	local discriminant = 4 * A * C - B * B
	if math.abs(discriminant) < tkz.epsilon then
		tex.error("Degenerate ellipse (discriminant close to zero)")
	end

	local radicand_ra = 2
		* (A * E ^ 2 + C * D ^ 2 - B * D * E + (B ^ 2 - 4 * A * C) * F)
		* ((A + C) + math.sqrt((A - C) ^ 2 + B ^ 2))
	local radicand_rb = 2
		* (A * E ^ 2 + C * D ^ 2 - B * D * E + (B ^ 2 - 4 * A * C) * F)
		* ((A + C) - math.sqrt((A - C) ^ 2 + B ^ 2))

	if radicand_ra < 0 or radicand_rb < 0 then
		tex.error("Negative square root in ellipse axis calculation")
	end

	local ra = math.sqrt(radicand_ra) / discriminant
	local rb = math.sqrt(radicand_rb) / discriminant
	local center = search_center_ellipse(coefficients)
	local angle = 0
	if math.abs(A - C) < tkz.epsilon then
		angle = math.pi / 4
	else
		local temp_angle = 0.5 * math.atan(B / (A - C))
		local V = center + point(math.cos(temp_angle), math.sin(temp_angle))
		local test_point = report_(center, V, ra)
		if math.abs(test_ellipse(test_point, coefficients)) < tkz.epsilon then
			angle = temp_angle
		else
			angle = temp_angle + math.pi / 2
		end
	end
	return center, ra, rb, angle
end

function define_ellipse(coefficients)
	local center, ra, rb, angle = ellipse_axes_angle(coefficients)
	local vertex, covertex = EL_radii(center, ra, rb, angle)
	return EL_points(center, vertex, covertex)
end

-- Utility function for testing a point relative to the ellipselocal
function test_ellipse(pt, coefficients)
	local x, y = pt:get()
	local A, B, C, D, E, F =
		coefficients.A, coefficients.B, coefficients.C, coefficients.D, coefficients.E, coefficients.F
	return A * x ^ 2 + B * x * y + C * y ^ 2 + D * x + E * y + F
end

-- Function for creating ellipses from points and axes.
function EL_radii(center_point, ra, rb, angle)
	local P = center_point + point(math.cos(angle), math.sin(angle))
	local Q = center_point + point(-math.sin(angle), math.cos(angle))
	-- get the conic with center, vertex and covertex
	-- P and Q with good directions
	local V, CV = report_(center_point, P, ra), report_(center_point, Q, rb)
	return EL_points(center_point, V, CV)
end

function inverse_affinity_ellipse(C, z)
	local ortho = C.major_axis:ortho_from(z)
	local CI = circle:new(C.center, C.vertex)
	return intersection(CI, ortho, { near = z })
end
