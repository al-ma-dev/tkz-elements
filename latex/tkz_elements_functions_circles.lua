-- File: tkz_elements_functions_circles.lua
-- Copyright (c) 2026 Alain Matthes
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
from_radius = through

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
from_diameter = diameter
-----------------------
-- Result -> boolean
-----------------------
function on_circle_(o, t, p, EPS)
  EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  local d = point.abs(p - o)
  return math.abs(d - r) <= EPS
end

function in_disk_(o, t, p, EPS)
  EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  return point.abs(p - o) <= r + EPS
end

function in_disk_strict_(o, t, p, EPS)
  EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  local rr = r - EPS
  if rr <= 0 then return false end
  return point.abs(p - o) < rr
end

function out_disk_strict_(o, t, p, EPS)
  EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  return point.abs(p - o) > r + EPS
end

-- internal helper
function point_circle_position_(o, t, p, EPS)
  EPS = EPS or tkz.epsilon
  local d = point.abs(p - o)
  local r = point.abs(t - o)
  if math.abs(d - r) <= EPS then
    return "ON"
  elseif d < r - EPS then
    return "IN"
  else
    return "OUT"
  end
end

function circle_point_position_(C, p, EPS)
  EPS = EPS or tkz.epsilon
  local d = point.abs(p - C.center)
  local r = C.radius

  if is_zero_(d - r, EPS) then
    return "ON"
  elseif d < r then
    return "IN"
  else
    return "OUT"
  end
end


function in_out_disk_(a, b, pt)
    return point.abs(pt - a) <= point.abs(b - a)
end


function is_tangent_(a, b, o, t, EPS)
   EPS = EPS or tkz.epsilon
  local r = length_(o, t)
  local d = distance_(a, b, o)
  return math.abs(d - r) <= EPS
end




function act_(O1, T1, O2, T2, opts)
  opts = opts or {}
  local eps_abs = (opts.abs ~= nil) and opts.abs or 1e-4
  local eps_rel = (opts.rel ~= nil) and opts.rel or 1e-6

  local R1 = length_(O1, T1)
  local R2 = length_(O2, T2)
  local d  = length_(O1, O2)

  local sum = R1 + R2
  local dif = math.abs(R1 - R2)

  local eE_abs = math.abs(d - sum)
  local eI_abs = math.abs(d - dif)

  local eE_rel = eE_abs / math.max(1.0, sum)
  local eI_rel = eI_abs / math.max(1.0, dif, 1.0)

  return (eE_abs <= eps_abs) or (eI_abs <= eps_abs)
      or (eE_rel <= eps_rel) or (eI_rel <= eps_rel)
end

-- distance from point C to the infinite line (A,B)
-- you already have distance_(A,B,C) apparently
function line_circle_position_(L, C, EPS)
  EPS = EPS or tkz.epsilon
  local r = C.radius
  local d = distance_(L.pa, L.pb, C.center)

  if d > r + EPS then
    return "DISJOINT"
  elseif is_zero_(d - r, EPS) then
    return "TANGENT"
  else
    return "SECANT"
  end
end
-----------------------
-- Result -> point
-----------------------

function midarc_(o, a, b) -- a -> b
    local phi = 0.5 * get_angle_(a, o, b)
    return rotation_(o, phi, b)
end


function tangent_point_two_circles_(c1, r1, c2, r2)
  local dc = c2 - c1
  local d  = point.abs(dc)
  if d == 0 or r1 == 0 then
    return false
  end
  local u = dc / d  -- vecteur unitaire c1 -> c2
  return c1 + r1 * u
end

function tangent_from_(c, p, pt)
    local o = midpoint_(c, pt)
    return intersection_cc_(o, c, c, p)
end

function tangent_at_(a, p)
    return rotation_(p, math.pi / 2, a), rotation_(p, -math.pi / 2, a)
end

function orthogonal_from_(a, b, p)
    return tangent_from_(a, b, p)
end

-- problems with the inversion
-- 1) if one of the points is on the circle
-- 2) if one point is the inverse of the other with respect to the circle
function orthogonal_through_(a, b, x, y, EPS)
   EPS = EPS or tkz.epsilon
    if math.abs(point.mod(x - a) - point.mod(b - a)) < EPS then
        local ta, tb = tangent_at_(a, x)
        local mx, my = mediator_(x, y)
        local z = intersection_ll_(ta, tb, mx, my)
        return z
    else
        local z = inversion_(a, b, x)
        return circum_center_(x, y, z)
    end
end

function are_inverses_(a, b, x, y, EPS)
   EPS = EPS or tkz.epsilon
  local z = inversion_(a, b, x)
    return point.mod(z - y) < EPS
end

function inversion_(c, p, pt, EPS)
   EPS = EPS or tkz.epsilon
    local ry = point.abs(c - p) -- radius of reference circle (c,p)
    local d = point.abs(c - pt) -- distance from c to pt

    if d < EPS then
        -- Avoid division by zero: return point at infinity or raise error
        tex.error("Inversion undefined at center", { "inversion_(c, p, pt): pt == c" })
        return nil
    end

    local r = (ry * ry) / d -- inversion radius
    return c + polar_(r, point.arg(pt - c))
end

function line_position_(c, r, a, b, EPS)
  EPS = EPS or tkz.epsilon
  local d = distance_(a, b, c)

  if d > r + EPS then
    return "DISJOINT"

  elseif math.abs(d - r) <= EPS then
    return "TANGENT"

  else
    return "SECANT"
  end
end



-- Relation between two circles (O1,r1) and (O2,r2)
-- returns: COINCIDENT, CONCENTRIC, DISJOINT_EXT, TANGENT_EXT, SECANT,
--          TANGENT_INT, DISJOINT_INT
function circles_position_(o1, r1, o2, r2, EPS)
  EPS = EPS or tkz.epsilon

  local d = point.abs(o2 - o1)
  local rp = r1 + r2
  local rm = math.abs(r1 - r2)

  -- same center?
  if is_zero_(d, EPS) then
    if is_zero_(r1 - r2, EPS) then
      return "IDENTICAL"
    else
      return "CONCENTRIC"
    end
  end

  -- compare d with r1+r2 and |r1-r2|
  if d > rp + EPS then
    return "DISJOINT_EXT"
  elseif is_zero_(d - rp, EPS) then
    return "TANGENT_EXT"
  elseif d < rp - EPS and d > rm + EPS then
    return "SECANT"
  elseif is_zero_(d - rm, EPS) then
    return "TANGENT_INT"
  else
    -- d < |r1-r2| - EPS
    return "DISJOINT_INT"
  end
end


function radical_axis_(c1, p1, c2, p2)
    local r1 = point.abs(c1 - p1)
    local r2 = point.abs(c2 - p2)
    local d = point.abs(c1 - c2)
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

function radical_center3_(C1, C2, C3)
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

--
-- midcircle_cc = midcircle_

function midcircle_cc_(o1, t1, o2, t2, EPS)
   EPS = EPS or tkz.epsilon
  local state, r, s, tg1, TG1, p, a, b, c, d, Cx, Cy, i, j, r1, r2
  r1 = length_(o1, t1)
  r2 = length_(o2, t2)
  state = circles_position_(o1, r1, o2, r2, EPS)

  -- identical circles: no midcircle
  if state == "IDENTICAL" then
    error("tkz-elements: midcircle: the two circles are identical", 2)
  end

  -- concentric circles: the concentric circle of radius sqrt(r1 r2)
  if state == "CONCENTRIC" then
    return circle:radius(o1, math.sqrt(r1 * r2))
  end

  -- internal center of similitude
  j = barycenter_({ o2, r1 }, { o1, r2 })

  -- equal radii: the external center of similitude is at infinity and
  -- the inversion becomes the reflection in the perpendicular bisector
  -- of the centers, returned as a line (circle "of infinite radius")
  if math.abs(r1 - r2) <= EPS then
    local u, v = mediator_(o1, o2)
    if (state == "DISJOINT_EXT") or (state == "TANGENT_EXT") then
      return line:new(u, v)
    elseif state == "SECANT" then
      r, s = intersection_cc_(o1, t1, o2, t2, EPS)
      return line:new(u, v), circle:radius(j, point.mod(r - j))
    end
  end

  -- external center of similitude
  i = barycenter_({ o2, r1 }, { o1, -r2 })

    if (state == "DISJOINT_EXT") or (state == "TANGENT_EXT") then
        tg1, _ = tangent_from_(o1, t1, i)
        TG1, _ = tangent_from_(o2, t2, i)
        p = math.sqrt(point.mod(i - tg1) * point.mod(i - TG1))
        return circle:radius(i, p)
    elseif state == "SECANT" then
        r, s = intersection_cc_(o1, t1, o2, t2, EPS)
        return circle:radius(i, point.mod(r - i)), circle:radius(j, point.mod(r - j))
    elseif (state == "DISJOINT_INT") or (state == "TANGENT_INT") then
        a, b = intersection_lc_(o1, o2, o1, t1, EPS)
        c, d = intersection_lc_(o1, o2, o2, t2, EPS)

        -- Ensure the smaller radius circle is used first
        local u, v, rr, ss
        if r1 < r2 then
          u, v, rr, ss = a, b, c, d
        else
          u, v, rr, ss = c, d, a, b
        end

        if in_segment_(ss, v, u) then
          Cx = circle:diameter(rr, v)
          Cy = circle:diameter(u, ss)
        else
          Cx = circle:diameter(ss, v)
          Cy = circle:diameter(u, rr)
        end

        -- Return the circle with the smaller radius orthogonal from j
        if Cx.radius < Cy.radius then
            return Cy:orthogonal_from(j)
        else
            return Cx:orthogonal_from(j)
        end
    end
end

-- Midcircle(s) between a line L and a circle C.
-- Conventions used:
-- - circle:new(center, through_point)
-- - intersection_lc_(X, Y, O, T): line (X,Y) with circle (O,T)
-- - ortho_from_(P, X, Y): line through P perpendicular to line (X,Y)
-- - distance_(a, b, P): distance from point P to line L =(a,b)
-- - antipode_(O, S): point opposite to S on circle centered at O and through S
--
-- Returns:
--   * secant case: two circles (centered at A and B) passing through S1 (hence tangent to L at S1)
--   * tangent case: your original behavior kept (center at antipode of T, through T)
--   * disjoint case: one (or two) circles of radius R = sqrt(2*r*d), with centers found
--     geometrically as intersections of C with parallels to L at distance R.

-- Midcircle between a circle C(O,r) and a line L
-- Returns:
--   - disjoint: two circles (centers A and B on OH)
--   - tangent : one circle (center K = antipode of S on C)
--   - secant  : two circles (centers A and B through an intersection S)
function midcircle_cl_(O, T, x, y, EPS)
  EPS  = EPS or tkz.epsilon
  local r = length_(T, O)

  -- Foot H of the perpendicular from O to line (x,y)
  local H = projection_(x, y, O)

  -- A,B = intersections of line (O,H) with circle C
  local A, B = intersection_lc_(O, H, O, T, EPS)
  -- Stable ordering: make A the farther from H
  if length_(B, H) > length_(A, H) then A, B = B, A end

  -- Intersections of L with C (nil if disjoint; doubled if tangent)
  local S1, S2 = intersection_lc_(x, y, O, T, EPS)
  local S      = S1 or S2

  -- test "tangent" via d(O, L) ≈ r
  local OL         = distance_(x, y, O)
  local is_tangent = math.abs(OL - r) <= EPS

  -- Disjoint case: TWO solutions (centers A and B)
  if not S then
    -- Power condition -> R^2 = 2 r d(center, L)
    local AL = distance_(x, y, A)
    local BL = distance_(x, y, B)
    local R1 = math.sqrt(2 * r * AL)
    local R2 = math.sqrt(2 * r * BL)
    return circle:radius(A, R1), circle:radius(B, R2)
  end

  -- Tangent case: ONE solution
  if is_tangent then
    local K = antipode_(O, S1) -- S1==S2
    return circle:new(K, S1)
  end

  -- Secant case: TWO solutions (pick either S1 or S2, both give same radii)
  local Sref = S1
  return circle:new(A, Sref), circle:new(B, Sref)
end


--- =========== Common tangents of two circles =======-----
-- common_tangents_(C1, C2, mode, EPS)
-- mode: "external" (default), "internal", "both" ("all" is an alias of "both").
-- Returns a list of pairs { P, Q } (each pair defines a tangent line) and
-- the relative position of the circles. The list is empty when no common
-- tangent of the requested family exists. No error is raised here.
--   external family: DISJOINT_EXT, TANGENT_EXT, SECANT -> 2 ;
--                    TANGENT_INT -> 1 (tangent at the contact point)
--   internal family: DISJOINT_EXT -> 2 ; TANGENT_EXT -> 1 (at the contact point)
function common_tangents_(C1, C2, mode, EPS)
  EPS = EPS or tkz.epsilon
  mode = mode or "external"
  if mode == "all" then mode = "both" end

  local A, rA = C1.center, C1.radius
  local B, rB = C2.center, C2.radius
  local pos = circles_position_(A, rA, B, rB, EPS)

  -- pair the tangent points so that each pair lies on the same tangent
  local function pair_with_inout(S, t1a, t1b, t2a, t2b)
    if line_in_out_(t1a, S, t2a) then
      return t1a, t2a, t1b, t2b
    else
      return t1a, t2b, t1b, t2a
    end
  end

  -- two tangents through a center of similitude S
  local function via_S(S)
    local t1a, t1b = tangent_from_(A, C1.through, S)
    local t2a, t2b = tangent_from_(B, C2.through, S)
    local P1, Q1, P2, Q2 = pair_with_inout(S, t1a, t1b, t2a, t2b)
    return { { P1, Q1 }, { P2, Q2 } }
  end

  -- equal radii: the external center of similitude is at infinity,
  -- the two external tangents are parallel to (AB)
  local function externals_equal_radii()
    local v = B - A
    return { { v:orthogonal(rA):at(A), v:orthogonal(rA):at(B) },
             { v:orthogonal(-rA):at(A), v:orthogonal(-rA):at(B) } }
  end

  -- tangent circles: the common tangent at the contact point
  local function at_contact()
    local T = intersection(C1, C2)
    local U = T:shift_orthogonal_to(A, rA)
    return { { T, U } }
  end

  local function externals()
    if pos == "DISJOINT_EXT" or pos == "TANGENT_EXT" or pos == "SECANT" then
      if math.abs(rA - rB) < EPS then
        return externals_equal_radii()
      end
      return via_S(external_similitude_(A, rA, B, rB))
    elseif pos == "TANGENT_INT" then
      return at_contact()
    end
    return {}
  end

  local function internals()
    if pos == "DISJOINT_EXT" then
      return via_S(internal_similitude_(A, rA, B, rB))
    elseif pos == "TANGENT_EXT" then
      return at_contact()
    end
    return {}
  end

  local list = {}
  if mode == "external" or mode == "both" then
    for _, t in ipairs(externals()) do list[#list + 1] = t end
  end
  if mode == "internal" or mode == "both" then
    for _, t in ipairs(internals()) do list[#list + 1] = t end
  end
  return list, pos
end

-- Old interface (kept for internal use): the points of the first two
-- tangents, or nil.
function common_tangent_(C1, C2, mode, EPS)
  local list = common_tangents_(C1, C2, mode, EPS)
  local t1, t2 = list[1] or {}, list[2] or {}
  return t1[1], t1[2], t2[1], t2[2]
end

-- nouvel ajout for CLL
--====== CLL =-=====-

-- ==== Helpers for CLL ==== ---

function orient_ray_(i, a, b)
  return (a - i) ^ (b - i)
end

function left_of_ray_(i, a, p)
  return orient_ray_(i, a, p) > 0
end

function point_in_open_sector_(i, a, b, p)
  return left_of_ray_(i, a, p) and not left_of_ray_(i, b, p)
end

function circle_single_sector_(i, u, up, v, vp, o, r)
  local S = {
    {u,  v},
    {v,  up},
    {up, vp},
    {vp, u},
  }
  for k = 1, 4 do
    local a, b = S[k][1], S[k][2]
    if point_in_open_sector_(i, a, b, o) then
      local dA = distance_(i, a, o)
      local dB = distance_(i, b, o)
      if dA >= r and dB >= r then
        return k
      end
    end
  end
  return nil
end


function circle_touched_sectors_(i, u, up, v, vp, o, r)

  if (o - i):mod() <= r + tkz.epsilon then
    return {1, 2, 3, 4}
  end

  local adjacent = {
    u  = {4, 1},
    v  = {1, 2},
    up = {2, 3},
    vp = {3, 4},
  }

  local hit = { false, false, false, false }

  local k0 = (function()
    if     point_in_open_sector_(i, u,  v,  o) then return 1
    elseif point_in_open_sector_(i, v,  up, o) then return 2
    elseif point_in_open_sector_(i, up, vp, o) then return 3
    elseif point_in_open_sector_(i, vp, u,  o) then return 4
    else return nil end
  end)()
  if k0 then hit[k0] = true end

function touch_border_(ray_label, a)
    local d = distance_(i, a, o)
    if d <= r + tkz.epsilon then
      local s1, s2 = adjacent[ray_label][1], adjacent[ray_label][2]
      hit[s1], hit[s2] = true, true
    end
  end

  touch_border_('u',  u)
  touch_border_('v',  v)
  touch_border_('up', up)
  touch_border_('vp', vp)

  local out = {}
  for k = 1, 4 do
    if hit[k] then out[#out+1] = k end
  end
  return out
end

function classify_by_sector_(i, u, up, v, vp, circles)
  local buckets = { [1]={}, [2]={}, [3]={}, [4]={} }
  for _, C in ipairs(circles) do
    local w = C.center
    local k =
      (point_in_open_sector_(i, u,  v,  w) and 1) or
      (point_in_open_sector_(i, v,  up, w) and 2) or
      (point_in_open_sector_(i, up, vp, w) and 3) or
      (point_in_open_sector_(i, vp, u,  w) and 4) or
      nil
    if k then table.insert(buckets[k], C) end
    -- (limit cases: w on a border, to be handled later)
  end
  return buckets
end

function circle_sector_signature_(i, u, up, v, vp, o, r)

  -- 0) circle containing the vertex i -> 4 sectors
  if (o - i):mod() <= r + tkz.epsilon then
    return {1, 2, 3, 4}
  end

  -- helper orientation strict / non-strict
  local function in_open(i,a,b,p)
    return ((a - i) ^ (p - i)) >  0 and
           ((b - i) ^ (p - i)) <= 0
  end

  -- 1) strict sector of the center (k0)
  local k0 =
    (in_open(i, u,  v,  o) and 1) or
    (in_open(i, v,  up, o) and 2) or
    (in_open(i, up, vp, o) and 3) or
    (in_open(i, vp, u,  o) and 4) or
    0  -- 0 = center on a border (limit case)

  -- 2) distances to the underlying LINES (not to the 4 rays)
  --    L1 = (a,b) porte u/up ; L2 = (c,d) porte v/vp
  --    IMPORTANT: only once per line.
  local D1 = distance_(u, up, o)  -- distance to the line L1
  local D2 = distance_(v, vp, o)  -- distance to the line L2
  local T1 = D1 <= r + tkz.epsilon
  local T2 = D2 <= r + tkz.epsilon

  -- 3) if k0 is known: only the two borders of k0
  if k0 == 1 then
    if     T1 and not T2 then return {4, 1}   -- touche u
    elseif T2 and not T1 then return {1, 2 }  -- touche v
    elseif T1 and T2     then return {4, 1, 2} -- touches u and v (3 sectors, real case)
    else                      return {1}
    end
  elseif k0 == 2 then
    if     T2 and not T1 then return {1, 2}   -- v
    elseif T1 and not T2 then return {2, 3}   -- up
    elseif T1 and T2     then return {1, 2, 3}
    else                      return {2}
    end
  elseif k0 == 3 then
    if     T1 and not T2 then return {2, 3}  -- up
    elseif T2 and not T1 then return {3, 4}   -- vp
    elseif T1 and T2     then return {2, 3, 4}
    else                      return {3}
    end
  elseif k0 == 4 then
    if     T2 and not T1 then return {3, 4}  -- vp
    elseif T1 and not T2 then return {4, 1}  -- u
    elseif T1 and T2     then return {3, 4, 1}
    else                      return {4}
    end
  end

  -- 4) limit case: center on a border.
  --    Avoid counting twice by grouping by LINE:
  local Tu  = distance_(i, u,  o) <= r + tkz.epsilon
  local Tv  = distance_(i, v,  o) <= r + tkz.epsilon
  local Tup = distance_(i, up, o) <= r + tkz.epsilon
  local Tvp = distance_(i, vp, o) <= r + tkz.epsilon
  local L1_touched = (Tu or Tup)   -- only once for the line L1
  local L2_touched = (Tv or Tvp)   -- same for L2

  if     L1_touched and not L2_touched then
    -- tangent to L1 only -> exactly 2 sectors,
    -- choose the adjacent pair on the side of o.
    -- The sector "on the right" of the border is used.
    -- Here, the safest pair is returned: "4/1" (side u)
    return  {4, 1}
  elseif L2_touched and not L1_touched then
    return {1, 2}
  elseif L1_touched and L2_touched then
    -- tangent to both lines while the center is on a border:
    -- reasonably 3 sectors (real geometry)
    return {4, 1, 2}
  else
    -- no tangency detected -> 1 neighbouring sector
    return {1}
  end
end

---- ==== end helpers for CLL ====-----

function lines_secant_circle_(o, r, a, b, c, d)
  local i = intersection_ll_(a, b, c, d)
  if not i then
    -- fallback (in theory never reached)
    return lines_parallel_circle_(o, r, a, b, c, d)
  end
  local s = ((b - a) ^ (d - c)) >= 0 and 1 or -1
  local u  = report_(a, b,  1,  i)
  local up = report_(a, b, -1,  i)
  local v  = report_(c, d,  s,  i)
  local vp = report_(c, d, -s,  i)
  return circle_sector_signature_(i, u, up, v, vp, o, r)
end
-- L1=(a,b), L2=(c,d), circle=(o,r)
-- Secteurs : 1=entre L1/L2, 2=outside L1, 3=outside L2
function lines_parallel_circle_(o, r, a, b, c, d)
  local d1 = distance_(a, b, o)           -- dist(o, L1)
  local d2 = distance_(c, d, o)           -- dist(o, L2)
  local D  = distance_(a, b, c)           -- distance between the parallel lines

  local inside_strip  = tkz.approx(d1 + d2, D)            -- o between L1 and L2
  local outside_strip = tkz.approx(math.abs(d1 - d2), D)  -- o on the same side

  local mn, mx = math.min(d1, d2), math.max(d1, d2)
  local nearer_is_L1 = (d1 <= d2)         -- if equal, side L1
  local OUT = nearer_is_L1 and 2 or 3     -- outer sector on the near side

  if inside_strip then
    if r < mn and not tkz.approx(r, mn) then return {1} end
    if tkz.approx(r, mn)                  then return {1, OUT} end
    if r > mn and r < mx                  then return {1, OUT} end
    -- r >= mx  (tangent to the farthest line, or beyond)
    return {1,2,3}
  end

  if outside_strip then
    if r < mn and not tkz.approx(r, mn)   then return {OUT} end
    if tkz.approx(r, mn)                  then return {OUT,1} end
    if r > mn and r < mx                  then return {OUT,1} end
    -- r >= mx
    return {1,2,3}
  end

  -- numerical grey zone: the nearest scenario is chosen
  if math.abs((d1 + d2) - D) <= math.abs(math.abs(d1 - d2) - D) then
    -- traiter comme inside_strip
    if r < mn and not tkz.approx(r, mn) then return {1} end
    if tkz.approx(r, mn)                then return {1, OUT} end
    if r > mn and r < mx                then return {1, OUT} end
    return {1,2,3}
  else
    -- traiter comme outside_strip
    if r < mn and not tkz.approx(r, mn) then return {OUT} end
    if tkz.approx(r, mn)                then return {OUT,1} end
    if r > mn and r < mx                then return {OUT,1} end
    return {1,2,3}
  end
end
