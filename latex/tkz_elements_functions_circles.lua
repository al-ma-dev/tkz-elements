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
function in_out_(a, b, pt)
    return math.abs(point.abs(pt - a) - point.abs(b - a)) < tkz.epsilon
end

function in_out_disk_(a, b, pt)
    return point.abs(pt - a) <= point.abs(b - a)
end

function midarc_(o, a, b) -- a -> b
    local phi = 0.5 * get_angle_(a, o, b)
    return rotation_(o, phi, b)
end

function is_tangent_(a, b, o, t, EPS)
   EPS = EPS or tkz.epsilon
  local r = length_(o, t)
  local d = distance_(a, b, o)
  return math.abs(d - r) <= EPS
end

-- p est SUR le cercle (bande de tolérance)
function on_circle_(o, t, p ,EPS)
   EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  local d = point.abs(p - o)
  return math.abs(d - r) <= EPS
end

-- Inclusif : disque fermé (<=)
function in_disk_(o, t, p, EPS)
   EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  return point.abs(p - o) <= r + EPS
end

-- Strict : disque ouvert (<)  — utile pour tes dispatchers
function in_disk_strict_(o, t, p, EPS)
   EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  return point.abs(p - o) < r - EPS
end

-- Strict : extérieur pur (>)
function out_disk_strict_(o, t, p,EPS)
   EPS = EPS or tkz.epsilon
  local r = point.abs(t - o)
  return point.abs(p - o) > r + EPS
end

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

function act_(O1, T1, O2, T2, EPS)
   EPS = EPS or tkz.epsilon
  local R1 = length_(O1, T1)
  local R2 = length_(O2, T2)
  local d = length_(O1, O2)
  local max = R1 + R2
  local min = math.abs(R1 - R2)
  return (math.abs(d - max) < EPS) or  ( math.abs(d - min) < EPS)
end
-----------------------
-- Result -> point
-----------------------
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

-- pb avec l'inversion
-- 1) si l'un des points est sur le cercle
-- 2) si l'un des points est l'inverse de l'autre par rapport au cercle
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

function orthogonal_circle_through_(a, b, x, y, EPS)
   EPS = EPS or tkz.epsilon
    local r = point.mod(b - a)
    local d1 = math.abs(point.mod(x - a) - r)
    local d2 = math.abs(point.mod(y - a) - r)
    local x_on = (d1 < EPS)
    local y_on = (d2 < EPS)

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
  if d > r + 0.0001 then
    return "disjoint"
  elseif math.abs(d - r) <= EPS then
    return "tangent"
  else
    return "secant"
  end
end


function circles_position_(c1, r1, c2, r2, EPS)
     EPS = EPS or tkz.epsilon
    local d = point.mod(c1 - c2)
    local max = r1 + r2
    local min = math.abs(r1 - r2)

    if d > max then
        return "outside"
    elseif math.abs(d - max) < EPS then
        return "outside tangent"
    elseif math.abs(d - min) < EPS then
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

--
-- midcircle_cc = midcircle_

function midcircle_cc_(o1, t1, o2, t2, EPS)
   EPS = EPS or tkz.epsilon
  local state, r, s, tg1, TG1, p, a, b, c, d, Cx, Cy, i, j, r1, r2
  r1 = length_(o1, t1)
  r2 = length_(o2, t2)
  state = circles_position_(o1, r1, o2, r2, EPS)
  i = barycenter_({ o2, r1 }, { o1, -r2 })
  j = barycenter_({ o2, r1 }, { o1, r2 })
  tg1, _ = tangent_from_(o1, t1, i)
  TG1, _ = tangent_from_(o2, t2, i)

    if (state == "outside") or (state == "outside tangent") then
        p = math.sqrt(point.mod(i - tg1) * point.mod(i - TG1))
        return circle:radius(i, p)
    elseif state == "intersect" then
        r, s = intersection_cc_(o1, t1, o2, t2, EPS)
        return circle:radius(i, point.mod(r - i)), circle:radius(j, point.mod(r - j))
    elseif (state == "inside") or (state == "inside tangent") then
        a, b = intersection_lc_(o1, o2, o1, t1, EPS)
        c, d = intersection_lc_(o1, o2, o2, t2, EPS)

        -- Ensure the smaller radius circle is used first
        if r1 < r2 then
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

-- Midcircle(s) between a line L and a circle C.
-- Conventions used:
-- - circle:new(center, through_point)
-- - intersection_lc_(X, Y, O, T): line (X,Y) with circle (O,T)
-- - ortho_from_(P, X, Y): line through P perpendicular to line (X,Y)
-- - distance_(a, b, P): distance from point P to line L =(a,b)
-- - antipode_(O, S): point opposite to S on circle centered at O and through S (your helper)
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
  r = length_(T, O)

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
    -- Power condition → R^2 = 2 r · d(center, L)
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
function common_tangent_(C1, C2, mode, EPS)
   EPS = EPS or tkz.epsilon
  mode = mode or "external" -- "external" | "internal" | "both"

  local A, rA = C1.center, C1.radius
  local B, rB = C2.center, C2.radius

  local pos   = circles_position_(A, rA, B, rB, EPS)
  -- "inside" | "outside" | "intersect" | "inside tangent" | "outside tangent"
  local S_ext = external_similitude_(A, rA, B, rB) -- peut être nil si rA == rB
  local S_int = internal_similitude_(A, rA, B, rB) -- pour rA == rB : milieu AB

  local function equal_radii()
    return math.abs(rA - rB) < EPS
  end

  -- --------- Helpers factorisés ----------
  local function pair_with_inout(S, t1a, t1b, t2a, t2b)
    if line_in_out_(t1a, S, t2a) then
      return t1a, t2a, t1b, t2b
    else
      return t1a, t2b, t1b, t2a
    end
  end

  local function externals_via_S(S)
    local t1a, t1b = tangent_from_(A, C1.through, S)
    local t2a, t2b = tangent_from_(B, C2.through, S)
    local P1, Q1, P2, Q2 = pair_with_inout(S, t1a, t1b, t2a, t2b)
    return P1, Q1, P2, Q2
  end

  local function externals_equal_radii()
    -- S_ext à l’infini → deux tangentes externes parallèles
    local v  = (B - A)
    local P1 = v:orthogonal(rA):at(A)
    local Q1 = v:orthogonal(rA):at(B)
    local P2 = v:orthogonal(-rA):at(A)
    local Q2 = v:orthogonal(-rA):at(B)
    return P1, Q1, P2, Q2
  end

  local function internals_via_S(S)
    local t1a, t1b = tangent_from_(A, C1.through, S)
    local t2a, t2b = tangent_from_(B, C2.through, S)
    local P1, Q1, P2, Q2 = pair_with_inout(S, t1a, t1b, t2a, t2b)
    return P1, Q1, P2, Q2
  end

  local function internals_equal_radii()
    -- rA == rB : S_int = milieu(AB) → on passe par la voie générale
    return internals_via_S(S_int)
  end

  local function one_tangent_proc()
    -- Cas “une seule tangente commune” (cercles tangents)
    local T = intersection(C1, C2)            -- (unique) point de contact
    local U = T:shift_orthogonal_to(A, rA)    -- point distinct sur la tangente en T à C1
    return T, U, T, U
  end

  -- --------- Procédures par position ----------
  local function inside_proc()
    -- Aucune tangente commune
    return nil, nil
  end

  local function intersect_proc()
    -- Deux tangentes externes seulement
    if mode == "internal" then
      tex.error("internal tangents non-existent (intersect)")
      return nil, nil
    end
    if equal_radii() then
      return externals_equal_radii()
    else
      return externals_via_S(S_ext)
    end
  end

local function outside_tangent_proc()
    -- Cercles tangents extérieurement.
    if mode == "internal" or mode == "both" then
      -- La famille "interne" dégénère : une seule tangente commune (au point de contact).
      return one_tangent_proc()
    elseif mode == "external" then
      -- Les deux tangentes externes existent toujours.
      if equal_radii() then
        return externals_equal_radii()      -- deux parallèles si rA == rB
      else
        return externals_via_S(S_ext)       -- deux tangentes via le centre de similitude externe
      end
    else
      -- Par défaut, on suit le comportement "external".
      if equal_radii() then
        return externals_equal_radii()
      else
        return externals_via_S(S_ext)
      end
    end
  end


local function inside_tangent_proc()
    -- Cercles tangents intérieurement.
    if mode == "external" then
      tex.error("external tangents non-existent (inside tangent)")
      return nil, nil, nil, nil
    end
    -- La famille "interne" dégénère : une seule tangente au point de contact.
    return one_tangent_proc()
  end

  local function outside_proc()
    -- Les 4 existent (2 externes + 2 internes). On respecte 'mode'.
    if mode == "internal" then
      if equal_radii() then
        return internals_equal_radii()
      else
        return internals_via_S(S_int)
      end
    elseif mode == "external" then
      if equal_radii() then
        return externals_equal_radii()
      else
        return externals_via_S(S_ext)
      end
    end
  end

  -- --------- Dispatcher clair ----------
  if     pos == "inside"           then return inside_proc()
  elseif pos == "intersect"        then return intersect_proc()
  elseif pos == "inside tangent"   then return inside_tangent_proc()
  elseif pos == "outside tangent"  then return outside_tangent_proc()
  elseif pos == "outside"          then return outside_proc()
  end

  tex.error("common_tangent_: invalid configuration ('"..tostring(pos).."', mode='"..tostring(mode).."').")
  return nil, nil, nil, nil
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
    -- (cas limites: si w est sur un bord, à traiter plus tard comme tu l’as noté)
  end
  return buckets
end

function circle_sector_signature_(i, u, up, v, vp, o, r)

  -- 0) cercle contenant le sommet i -> 4 secteurs
  if (o - i):mod() <= r + tkz.epsilon then
    return {1, 2, 3, 4}
  end

  -- helper orientation strict / non-strict
  local function in_open(i,a,b,p)
    return ((a - i) ^ (p - i)) >  0 and
           ((b - i) ^ (p - i)) <= 0
  end

  -- 1) secteur strict du centre (k0)
  local k0 =
    (in_open(i, u,  v,  o) and 1) or
    (in_open(i, v,  up, o) and 2) or
    (in_open(i, up, vp, o) and 3) or
    (in_open(i, vp, u,  o) and 4) or
    0  -- 0 = centre sur un bord (cas limite)

  -- 2) distances aux DROITES sous-jacentes (pas aux 4 demi-droites)
  --    L1 = (a,b) porte u/up ; L2 = (c,d) porte v/vp
  --    IMPORTANT : une seule fois par droite.
  local D1 = distance_(u, up, o)  -- équiv. distance à la droite de L1
  local D2 = distance_(v, vp, o)  -- équiv. distance à la droite de L2
  local T1 = D1 <= r + tkz.epsilon
  local T2 = D2 <= r + tkz.epsilon

  -- 3) si k0 connu : ne considérer que les deux bords qui bornent k0
  if k0 == 1 then
    if     T1 and not T2 then return {4, 1}   -- touche u
    elseif T2 and not T1 then return {1, 2 }  -- touche v
    elseif T1 and T2     then return {4, 1, 2} -- touche u et v (3 secteurs, cas réel)
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

  -- 4) cas limite : centre sur un bord.
  --    Éviter le double-compte en regroupant par DROITE :
  local Tu  = distance_(i, u,  o) <= r + tkz.epsilon
  local Tv  = distance_(i, v,  o) <= r + tkz.epsilon
  local Tup = distance_(i, up, o) <= r + tkz.epsilon
  local Tvp = distance_(i, vp, o) <= r + tkz.epsilon
  local L1_touched = (Tu or Tup)   -- une seule fois pour la droite L1
  local L2_touched = (Tv or Tvp)   -- idem pour L2

  if     L1_touched and not L2_touched then
    -- tangence à la seule L1 -> exactement 2 secteurs,
    -- choisir la paire adjacente qui correspond au côté où se trouve o.
    -- On discrimine avec le secteur "à droite" du bord considéré.
    -- Ici, renvoyer la paire la plus sûre : "4/1" (côté u)
    return  {4, 1}
  elseif L2_touched and not L1_touched then
    return {1, 2}
  elseif L1_touched and L2_touched then
    -- tangence aux deux droites alors que le centre est sur un bord :
    -- raisonnablement 3 secteurs (géométrie réelle)
    return {4, 1, 2}
  else
    -- pas de tangence détectée → considérer 1 secteur voisin
    return {1}
  end
end

---- ==== end helpers for CLL ====-----

function lines_secant_circle_(o, r, a, b, c, d)
  local i = intersection_ll_(a, b, c, d)
  if not i then
    -- fallback (en théorie jamais atteint si la détection est faite au-dessus)
    return lines_parallel_circle_(o, r, a, b, c, d)
  end
  local s = ((b - a) ^ (d - c)) >= 0 and 1 or -1
  local u  = report_(a, b,  1,  i)
  local up = report_(a, b, -1,  i)
  local v  = report_(c, d,  s,  i)
  local vp = report_(c, d, -s,  i)
  return circle_sector_signature_(i, u, up, v, vp, o, r)
end
-- L1=(a,b), L2=(c,d), cercle=(o,r)
-- Secteurs : 1=entre L1/L2, 2=outside L1, 3=outside L2
function lines_parallel_circle_(o, r, a, b, c, d)
  local d1 = distance_(a, b, o)           -- dist(o, L1)
  local d2 = distance_(c, d, o)           -- dist(o, L2)
  local D  = distance_(a, b, c)           -- écart des parallèles

  local inside_strip  = tkz.approx(d1 + d2, D)            -- o entre L1 et L2
  local outside_strip = tkz.approx(math.abs(d1 - d2), D)  -- o du même côté

  local mn, mx = math.min(d1, d2), math.max(d1, d2)
  local nearer_is_L1 = (d1 <= d2)         -- en cas d’égalité, côté L1
  local OUT = nearer_is_L1 and 2 or 3     -- secteur extérieur du côté proche

  if inside_strip then
    if r < mn and not tkz.approx(r, mn) then return {1} end
    if tkz.approx(r, mn)                  then return {1, OUT} end
    if r > mn and r < mx                  then return {1, OUT} end
    -- r ≥ mx  (tangence à la plus éloignée ou dépassement)
    return {1,2,3}
  end

  if outside_strip then
    if r < mn and not tkz.approx(r, mn)   then return {OUT} end
    if tkz.approx(r, mn)                  then return {OUT,1} end
    if r > mn and r < mx                  then return {OUT,1} end
    -- r ≥ mx
    return {1,2,3}
  end

  -- zone grise numérique : choisir le scénario le plus proche
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
