-- File: tkz_elements_functions_circles.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

--  define a circle by the center and a radius
function through(c, r, an)
    if an then
        return c, c + point(r * math.cos(an), r * math.sin(an))
    else
        return c, c + point(r, 0)
    end
end

function diameter(a, b, option)
    local center = midpoint_(a, b)
    if type(option) == "string" and option == "swap" then
        return center, a
    elseif type(option) == "number" then
        local u = (b - a) / point.abs(b - a)
        local rotated = point.polar(point.abs((b - a)) / 2, option)
        return center, center + rotated * u
    else
        return center, b
    end
end

-----------------------
-- Result -> boolean
-----------------------
function circle:in_out_(a, b, pt)
    return math.abs(point.abs(pt - a) - point.abs(b - a)) < tkz.epsilon
end

function circle:in_out_disk_(a, b, pt)
    return point.abs(pt - a) <= point.abs(b - a)
end

function midarc_(o, a, b) -- a -> b
    local phi = 0.5 * get_angle_(a, o, b)
    return rotation_(o, phi, b)
end

function tangent_from_(c, p, pt)
    local o
    o = midpoint_(c, pt)
    return intersection_cc_(o, c, c, p)
end

function tangent_at_(a, p)
    return rotation_(p, math.pi / 2, a), rotation_(p, -math.pi / 2, a)
end

function orthogonal_from_(a, b, p)
    return tangent_from_(a, b, p)
end

-- pb avec l'inversion
-- 1) si l'un des points est sur le cercle
-- 2) si l'un des points est l'inverse de l'autre par rapport au cercle
function orthogonal_through_(a, b, x, y)
    if math.abs(point.mod(x - a) - point.mod(b - a)) < tkz.epsilon then
        local ta, tb = tangent_at_(a, x)
        local mx, my = mediator_(x, y)
        local z = intersection_ll_(ta, tb, mx, my)
        return z
    else
        local z = inversion_(a, b, x)
        return circum_center_(x, y, z)
    end
end

function are_inverses_(a, b, x, y)
    local z = inversion_(a, b, x)
    return point.mod(z - y) < tkz.epsilon
end

function orthogonal_circle_through_(a, b, x, y)
    local r = point.mod(b - a)
    local d1 = math.abs(point.mod(x - a) - r)
    local d2 = math.abs(point.mod(y - a) - r)
    local eps = tkz.epsilon

    local x_on = (d1 < eps)
    local y_on = (d2 < eps)

    -- Cas 4 : x et y sont inverses par rapport à C(a,b)
    if are_inverses_(a, b, x, y) then
        -- Their perpendicular bisector passes through the center of the orthogonal circle
        local m1, m2 = mediator_(x, y)
        local t1, t2, n1, n2
        if point.abs(x - a) > point.abs(y - a) then
            t1, t2 = tangent_from_(a, b, x)
            n1, n2 = mediator_(t1, x)
        else
            t1, t2 = tangent_from_(a, b, y)
            n1, n2 = mediator_(t1, y)
        end
        local c = intersection_ll_(n1, n2, m1, m2)
        return c, x
    end
end

function inversion_(c, p, pt)
    local ry = point.abs(c - p) -- radius of reference circle (c,p)
    local d = point.abs(c - pt) -- distance from c to pt

    if d < tkz.epsilon then
        -- Avoid division by zero: return point at infinity or raise error
        tex.error("Inversion undefined at center", { "inversion_(c, p, pt): pt == c" })
        return nil
    end

    local r = (ry * ry) / d -- inversion radius
    return c + polar_(r, point.arg(pt - c))
end

function circles_position_(c1, r1, c2, r2)
    local d = point.mod(c1 - c2)
    local max = r1 + r2
    local min = math.abs(r1 - r2)

    if d > max then
        return "outside"
    elseif math.abs(d - max) < tkz.epsilon then
        return "outside tangent"
    elseif math.abs(d - min) < tkz.epsilon then
        return "inside tangent"
    elseif d < min then
        return "inside"
    else
        return "intersect"
    end
end

function radical_axis_(c1, p1, c2, p2)
    local r1 = point.abs(c1 - p1)
    local r2 = point.abs(c2 - p2)
    local d = point.abs(c1 - c2)
    local h = (r1 * r1 - r2 * r2 + d * d) / (2 * d)

    local ck = radical_center_(c1, p1, c2, p2)
    local cj = rotation_(ck, -math.pi / 2, c1)
    local ci = symmetry_(ck, cj)

    return cj, ci
end

function radical_center_(c1, p1, c2, p2)
    local d, r1, r2, h
    r1 = point.abs(c1 - p1)
    r2 = point.abs(c2 - p2)
    d = point.abs(c1 - c2)
    h = (r1 * r1 - r2 * r2 + d * d) / (2 * d)
    return h * (c2 - c1) / d + c1
end

function radical_center3(C1, C2, C3)
    local t1, t2, t3, t4
    t1, t2 = radical_axis_(C1.center, C1.through, C2.center, C2.through)
    if C3 == nil then
        return intersection_ll_(t1, t2, C1.center, C2.center)
    else
        t3, t4 = radical_axis_(C3.center, C3.through, C2.center, C2.through)
        return intersection_ll_(t1, t2, t3, t4)
    end
end

function south_pole_(c, p)
    return c - point(0, point.abs(c - p))
end

function north_pole_(c, p)
    return c + point(0, point.abs(c - p))
end

function antipode_(c, pt)
    return 2 * c - pt
end

function internal_similitude_(c1, r1, c2, r2)
    return barycenter_({ c2, r1 }, { c1, r2 })
end

function external_similitude_(c1, r1, c2, r2)
    return barycenter_({ c2, r1 }, { c1, -r2 })
end

function circlepoint_(c, t, k)
    local phi = 2 * k * math.pi
    return rotation_(c, phi, t)
end

function midcircle_(C1, C2)
    local state, r, s, t1, t2, T1, T2, p, a, b, c, d, Cx, Cy, i, j
    state = circles_position_(C1.center, C1.radius, C2.center, C2.radius)
    i = barycenter_({ C2.center, C1.radius }, { C1.center, -C2.radius })
    j = barycenter_({ C2.center, C1.radius }, { C1.center, C2.radius })
    t1, t2 = tangent_from_(C1.center, C1.through, i)
    T1, T2 = tangent_from_(C2.center, C2.through, i)

    if (state == "outside") or (state == "outside tangent") then
        p = math.sqrt(point.mod(i - t1) * point.mod(i - T1))
        return circle:radius(i, p)
    elseif state == "intersect" then
        r, s = intersection(C1, C2)
        return circle:radius(i, point.mod(r - i)), circle:radius(j, point.mod(r - j))
    elseif (state == "inside") or (state == "inside tangent") then
        a, b = intersection_lc_(C1.center, C2.center, C1.center, C1.through)
        c, d = intersection_lc_(C1.center, C2.center, C2.center, C2.through)

        -- Ensure the smaller radius circle is used first
        if C1.radius < C2.radius then
            z.u, z.v, z.r, z.s = a, b, c, d
        else
            z.u, z.v, z.r, z.s = c, d, a, b
        end

        -- Determine circle orientation and return orthogonal from j
        if in_segment_(z.s, z.v, z.u) then
            Cx = circle:diameter(z.r, z.v)
            Cy = circle:diameter(z.u, z.s)
        else
            Cx = circle:diameter(z.s, z.v)
            Cy = circle:diameter(z.u, z.r)
        end

        -- Return the circle with the smaller radius orthogonal from j
        if Cx.radius < Cy.radius then
            return Cy:orthogonal_from(j)
        else
            return Cx:orthogonal_from(j)
        end
    end
end

midcircle_cc = midcircle_
-- Midcircle(s) between a line L and a circle C.
-- Conventions used:
-- - circle:new(center, through_point)
-- - intersection_lc_(X, Y, O, T): line (X,Y) with circle (O,T)
-- - ortho_from_(P, X, Y): line through P perpendicular to line (X,Y)
-- - distance_(L, P): distance from point P to line L
-- - antipode_(O, S): point opposite to S on circle centered at O and through S (your helper)
--
-- Returns:
--   * secant case: two circles (centered at A and B) passing through S1 (hence tangent to L at S1)
--   * tangent case: your original behavior kept (center at antipode of T, through T)
--   * disjoint case: one (or two) circles of radius R = sqrt(2*r*d), with centers found
--     geometrically as intersections of C with parallels to L at distance R.

function midcircle_cl(C, L)
    local O, T, r = C.center, C.through, C.radius
    local x, y = L.pa, L.pb
    -- Foot H of the perpendicular from O to line (x,y)
    local H = projection_(x, y, O)
    local A, B = intersection_lc_(O, H, O, T)
    -- Stable ordering: make A the farther one from H
    if length_(B, H) > length_(A, H) then
        A, B = B, A
    end
    -- Intersections of L with C (nil if disjoint; doubled if tangent)
    local S1, S2 = intersection_lc_(x, y, O, T)
    local S = S1 or S2
    local eps = (tkz and tkz.epsilon) or 1e-9

    -- test tangence via d(O,D) ≈ r
    local OL = distance_(L, O)
    local is_tangent = math.abs(OL - r) <= eps

    -- Disjoint case: no intersection with the line
    if not S then
        -- Pole A of line L: R^2 = AL * AT = AL * (2r)
        local AL = distance_(L, A)
        local R = math.sqrt(2 * r * AL)
        return circle:radius(A, R)
    end

    -- Tangent case
    if is_tangent then
        local K = antipode_(O, S)
        return circle:new(K, S)
    end

    -- Secant case: return the two circles with centers A and B through S
    return circle:new(A, S), circle:new(B, S)
end

function common_tangent_(C1, C2)
    local A, rA = C1.center, C1.radius
    local B, rB = C2.center, C2.radius

    if rA == rB then
        -- Circles of the same radius
        -- The point of similarity is ill-defined
        -- → the two parallel tangents are calculated directly

        -- direction of vector AB
        local vx, vy = B.x - A.x, B.y - A.y
        local len = math.sqrt(vx * vx + vy * vy)
        vx, vy = vx / len, vy / len

        -- vector normal to AB (tangent direction)
        local nx, ny = -vy, vx

        -- contact points on each circle
        local P1 = A + point:new(nx * rA, ny * rA)
        local Q1 = B + point:new(nx * rB, ny * rB)
        local P2 = A - point:new(nx * rA, ny * rA)
        local Q2 = B - point:new(nx * rB, ny * rB)

        -- construire les droites tangentes
        return P1, Q1, P2, Q2
    else
        -- Cas général : cercles de rayons différents
        local S = external_similitude_(A, rA, B, rB)
        local T1a, T1b = tangent_from_(A, C1.through, S)
        local T2a, T2b = tangent_from_(B, C2.through, S)

        return T1a, T2a, T1b, T2b
    end
end
