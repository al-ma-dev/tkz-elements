-- File: tkz_elements_circles.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
---------------------------------------------------------------------------
--                           circles
---------------------------------------------------------------------------
circle = {}
circle.__index = circle
function circle:new(c, t) -- c --> center t --> through
  local type = "circle"
  local ct = line:new(c, t)
  local opp = antipode_(c, t)
  local radius = point.abs(c - t)
  local south = c - point(0, radius)
  local east = c + point(radius, 0)
  local north = c + point(0, radius)
  local west = c - point(radius, 0)
  local perimeter = 2 * math.pi * radius
  local area = 4 * math.pi * radius * radius
  local cir = {
    center = c,
    through = t,
    ct = ct,
    opp = opp,
    radius = radius,
    south = south,
    east = east,
    north = north,
    west = west,
    type = type,
    perimeter = perimeter,
    area = area,
  }
  setmetatable(cir, self)
  return cir
end

setmetatable(circle, {
  __call = function(cls, ...)
    return cls:new(...)
  end,
})

function circle:get()
  return self.center, self.through
end

-- other definition
function circle:radius(center, radius)
  return circle:new(center, center + point(radius, 0))
end

function circle:diameter(za, zb)
  return circle:new(midpoint_(za, zb), zb)
end
-----------------------
-- Result -> boolean
-----------------------

-- p est SUR le cercle (bande de tolérance)
function circle:on_circle(p)
  local d = point.abs(p - self.center)
  return math.abs(d - self.radius) <= tkz.epsilon
end

-- Inclusif : disque fermé (<=)
function circle:in_disk(p)
  return point.abs(p - self.center) <= self.radius + tkz.epsilon
end

-- Strict : disque ouvert (<)  — utile pour tes dispatchers
function circle:in_disk_strict(p)
  return point.abs(p - self.center) < self.radius - tkz.epsilon
end

-- Strict : extérieur pur (>)
function circle:out_disk_strict(p)
  return point.abs(p - self.center) > self.radius + tkz.epsilon
end

-- (Option) alias pour compatibilité avec ton code existant
circle.in_out         = circle.on_circle        -- "SUR"
circle.in_out_disk    = circle.in_disk          -- inclusif
circle.in_out_disk_strict = circle.in_disk_strict


function circle:is_tangent(l)
  local a, b = intersection(self, l)
  -- Checks whether the intersection produces valid points
  if not a or not b then
    return false
  end
  -- Checks whether the distance between the two intersection points is less than a given tolerance
  return (point.abs(b - a) < tkz.epsilon)
end
------------------------
-- string --------------
------------------------

function circle:circles_position(C)
  return circles_position_(self.center, self.radius, C.center, C.radius)
end

-----------------------
-- Result -> real
-----------------------
function circle:power(pt)
  local d = point.abs(self.center - pt)
  return d * d - self.radius * self.radius
end
-----------------------
-- Result -> point
-----------------------
function circle:antipode(pt)
  return 2 * self.center - pt
end

function circle:midarc(z1, z2)
  local phi = 0.5 * get_angle_normalize_(self.center, z1, z2)
  return rotation_(self.center, phi, z1)
end

function circle:point(t)
  local phi = 2 * t * math.pi
  return rotation_(self.center, phi, self.through)
end

function circle:random(inside)
  inside = (inside == "inside")
  math.randomseed(os.time())
  if inside then
    local x = self:point(math.random())
    local L = line:new(self.center, x)
    return L:random()
  else
    return self:point(math.random())
  end
end

function circle:internal_similitude(C)
  return internal_similitude_(self.center, self.radius, C.center, C.radius)
end

function circle:external_similitude(C)
  return external_similitude_(self.center, self.radius, C.center, C.radius)
end


function circle:radical_center(C1, C2)
  if C2 == nil then
    if self.radius > C1.radius then
      return radical_center_(self.center, self.through, C1.center, C1.through)
    else
      return radical_center_(C1.center, C1.through, self.center, self.through)
    end
  else
    return radical_center3(self, C1, C2)
  end
end

-----------------------
-- Result -> line
-----------------------
function circle:tangent_at(pt)
  return line:new(rotation_(pt, math.pi / 2, self.center), rotation_(pt, -math.pi / 2, self.center))
end

function circle:tangent_from(pt)
  local t1, t2 = tangent_from_(self.center, self.through, pt)
  return line:new(pt, t1), line:new(pt, t2)
end

function circle:tangent_parallel(L)
  local LP = L:orthogonal_from(self.center)
  local X, Y = intersection(LP, self)
  return self:tangent_at(X), self:tangent_at(Y)
end


function circle:radical_axis(C)
  local t1, t2
  if self.radius > C.radius then
    t1, t2 = radical_axis_(self.center, self.through, C.center, C.through)
  else
    t1, t2 = radical_axis_(C.center, C.through, self.center, self.through)
  end
  return line:new(t1, t2)
end

function circle:polar(p)
  local q = self:inversion(p)
  local qa = (p - q):orthogonal(1):at(q)
  local qb = (q - p):orthogonal(1):at(q)
  return line:new(qa, qb)
end

function circle:external_tangent(C)
  local i, t1, t2, k, T1, T2
  -- Find the barycenter of the two circles
  i = barycenter_({ C.center, self.radius }, { self.center, -C.radius })

  -- Calculate the tangents from the circle to the point of intersection
  t1, t2 = tangent_from_(self.center, self.through, i)

  -- Calculate the scaling factor for the homothety
  k = point.mod((C.center - i) / (self.center - i))

  -- Apply homothety to the tangents
  T1 = homothety_(i, k, t1)
  T2 = homothety_(i, k, t2)

  -- Return the two tangent lines
  return line:new(t1, T1), line:new(t2, T2)
end

function circle:internal_tangent(C)
  local i, t1, t2, k, T1, T2
  -- Find the barycenter of the two circles with opposite signs for radii
  i = barycenter_({ C.center, self.radius }, { self.center, C.radius })

  -- Calculate the tangents from the circle to the point of intersection
  t1, t2 = tangent_from_(self.center, self.through, i)

  -- Calculate the scaling factor for the homothety
  k = -point.mod((C.center - i) / (self.center - i))

  -- Apply homothety to the tangents
  T1 = homothety_(i, k, t1)
  T2 = homothety_(i, k, t2)

  -- Return the two tangent lines
  return line:new(t1, T1), line:new(t2, T2)
end

function circle:common_tangent(C, mode)
  local x, y, z, t = common_tangent_(self, C, mode)
  return line:new(x, y), line:new(z, t)
end

-----------------------
-- Result -> circle
-----------------------
--[[
  Constructs a circle orthogonal to the current circle (`self`) and passing through a given external point `pt`.

  The resulting circle:
    - has its center at the given point `pt`,
    - is orthogonal to `self`,
    - and uses the distance from `pt` to a point of tangency as its radius.

  Geometric condition:
    - The given point `pt` must lie **outside** the current circle.
    - This is verified using `in_out_disk`.

  If the condition is not met, the function raises a TeX error.

  Returns:
    A new circle (center = `pt`, radius = distance to the tangent point).
--]]

function circle:orthogonal_from(pt)
  -- test if pt is outside the circle
  if self:in_out_disk(pt) then
    tex.error("circle:orthogonal_from", "The point must lie outside the circle to construct an orthogonal circle.")
  else
    -- Calculate tangents from point ‘pt’.
    local t1, t2 = tangent_from_(self.center, self.through, pt)
    -- Return a circle with the center in 'pt' and one of the tangents as the radius
    return circle:new(pt, t1)
  end
end

function circle:orthogonal_through(pta, ptb)
  -- Case where the three points are collinear
  if is_linear_(self.center, pta, ptb) then
    -- and the two points are inverses of each other
    if are_inverses_(self.center, self.through, pta, ptb) then
      local c = midpoint_(pta, ptb)
      -- Write a note to the log instead of throwing an error
      texio.write_nl(
        "term and log",
        "[tkz-elements] circle:orthogonal_through: the two points are inverses; "
          .. "the midpoint is used as the centre, but any point on the perpendicular bisector could serve as centre."
      )

      tex.print(
        "\\PackageWarningNoLine{tkz-elements}{The two points are inverses; the midpoint is used as the centre, but any point on the perpendicular bisector could serve as centre.}"
      )

      return circle:new(c, pta)
    else
      -- Error case: collinear but not inverses
      tex.error("circle:orthogonal_through", "The points are aligned but not inverses.")
      return nil
    end

  -- General (non‑collinear) case
  else
    local o = orthogonal_through_(self.center, self.through, pta, ptb)
    return circle:new(o, pta)
  end
end

-- alias retained
circle.orthogonal_circle_through = circle.orthogonal_through

function circle:radical_circle(C1, C2)
  local rc = self:radical_center(C1, C2)
  if C2 == nil then
    return self:orthogonal_from(rc)
  else
    return C1:orthogonal_from(rc)
  end
end


function circle:midcircle(obj)
  -- Retourne le cercle médian entre 'self' et 'C'
  if type(obj) == "table" and obj.type == "circle" then
    return midcircle_cc(self, obj)
  elseif type(obj) == "table" and obj.type == "line" then
    return midcircle_cl(self, obj)
  else
    tex.error("midcircle: unsupported types (expect circle/circle or circle/line)")
  end
end

-- -----------------------------------------------------------
-- Circle tangent to a circle passing through two points
function circle:c_c_pp(a, b)
  -- test If one point is inside the disk and the other is outside, there is no solution.
  if (self:in_out_disk(a) and not self:in_out_disk(b)) or (self:in_out_disk(b) and not self:in_out_disk(a)) then
    tex.error("An error has occurred", { "Bad configuration. Only one point is in the disk" })
    return
  end

  -- Find the mediator of the current line
  local lab = line:new(a, b)
  local lmed = lab:mediator()

  if self:is_tangent(lab) then
    local c = intersection(self, lab)
    local d = self:antipode(c)

    return circle:new(circum_circle_(a, b, d), a), circle:new(circum_circle_(a, b, d), a)
  end

  -- pb are (AB) tgt to circle A and B equidistant of O tgt and equidistant
  if lab:is_equidistant(self.center) then
    local t1, t2 = intersection(lmed, self)
    return circle:new(circum_circle_(a, b, t1), t1), circle:new(circum_circle_(a, b, t2), t2)
  else
    -- Create a circumcircle passing through a, b, and a point on C
    local Cc = circle:new(circum_circle_(a, b, self.center), a)
    -- Find the intersection points of C and Cc
    local c, d = intersection(self, Cc)
    -- Create a line passing through the two intersection points
    local lcd = line:new(c, d)
    -- Find the intersection of the current line (self) with the line lcd
    local i = intersection(lab, lcd)
    -- Create tangents from the intersection point to C
    local lt, ltp = self:tangent_from(i)
    -- Get the tangent points
    local t, tp = lt.pb, ltp.pb
    -- Return two new circles tangent to C and passing through the tangent points
    return circle:new(intersection(lmed, line:new(self.center, t)), t),
      circle:new(intersection(lmed, line:new(self.center, tp)), tp)
  end
end
--=============== END of c_c_pp ============-----


--====================== START of c_cc_p ========---
-------------------------------------
-- ===== Helpers communs =====
-------------------------------------

-- Tri-état canonique : "IN", "ON", "OUT"
local function loc(C, p)
  local d = point.abs(p - C.center)
  local r = C.radius
  local e = tkz.epsilon
  if math.abs(d - r) <= e then
    return "ON"
  elseif d < r - e then
    return "IN"
  else
    return "OUT"
  end
end

-- (Option) alias utile ailleurs
local function strictly_inside(K, p)
  return loc(K, p) == "IN"
end

-- Regroupement des positions relatives en 3 familles
local function config_from_pos(pos)
  if pos == "outside" or pos == "outside tangent" then
    return "disjoint"
  elseif pos == "intersect" then
    return "secant"
  else
    return "nested" -- "inside" / "inside tangent"
  end
end

-------------------------------------
-- ===== Helpers géométriques =====
-------------------------------------
-- Inversion en p → tangentes communes → ré-inversion
local function build_common_tangents_by_inversion(self, C, p, mode)
  local T = self.through
  if T:identity(p) then T = self.center end
  local Cinv = circle:new(p, T)
  local C1   = Cinv:inversion(self)
  local C2   = Cinv:inversion(C)
  local L1, L2 = C1:common_tangent(C2, mode)
  local S1 = L1 and Cinv:inversion(L1) or nil
  local S2 = L2 and Cinv:inversion(L2) or nil
  return S1, S2
end

-- Cas “p sur l’un + strictement dedans l’autre” en configuration sécante
local function build_secant_on_in_by_bisectors(self, C, p)
  local X, Y = intersection(self, C)          -- points d'intersection des 2 cercles
  local Cinv = circle:new(X, p)               -- inversion centrée en X, passant par p
  local L1   = Cinv:inversion(self)           -- droites images
  local L2   = Cinv:inversion(C)
  local K    = intersection(L1, L2)
  local LP   = L2:orthogonal_from(p)          -- perpendiculaire en p à L2
  local LB1  = tkz.bisector(K, p, L1.pa)      -- bissectrices en p des droites (Kp, L1)
  local LB2  = tkz.bisector(K, p, L1.pb)
  local w1   = intersection(LP, LB1)
  local w2   = intersection(LP, LB2)
  local C1   = circle:new(w1, p)        -- cercles orthogonaux en p aux droites inverses
  local C2   = circle:new(w2, p)
  local S1   = Cinv:inversion(C1)             -- ré-inversion : cercles tangents cherchés
  local S2   = Cinv:inversion(C2)
  return S1, S2
end

-- Cas “nested” avec p sur exactement un cercle (méthode midcircle/inversion)
local function build_nested_on_border_midcircle(self, C, p)
  local on_self = self:in_out(p)              -- p sur self ?
  local Mid = self:midcircle(C)
  local i, j = intersection_lc_(self.center, C.center, C.center, C.through)
  if  p:identity(i) or p:identity(j) then
    local k, l = intersection_lc_(self.center, C.center, self.center, self.through)
    local ca, cb = midpoint_(p, k), midpoint_(p, l)
    return circle:new(ca, p), circle:new(cb, p)
  else
    local q   = Mid:inversion(p)
    local Lp, Lq
    if on_self then
      Lp = line:new(p, self.center)
      Lq = line:new(q, C.center)
    else
      Lp = line:new(q, self.center)
      Lq = line:new(p, C.center)
    end
    local w = intersection(Lp, Lq)
    return circle:new(w, p)
  end
end

-------------------------------------
-- ===== Solveur DISJOINT =====
-------------------------------------
local function solve_disjoint(self, C, p, a, b, pos, mode)
  a = a or loc(self, p)
  b = b or loc(C,    p)

  if a == "ON" and b == "ON" then
    return nil
  end

  if a == "ON" and b ~= "ON" then
    local T = self.through
    if T:identity(p) then T = self.center end
    local Cinv = circle:new(p, T)
    local C1   = Cinv:inversion(self) -- droite
    local C2   = Cinv:inversion(C)    -- cercle
    local L1, L2 = C2:tangent_parallel(C1)
    local S1 = L1 and Cinv:inversion(L1) or nil
    local S2 = L2 and Cinv:inversion(L2) or nil
    return S1, S2
  elseif b == "ON" and a ~= "ON" then
    local T = self.through
    if T:identity(p) then T = self.center end
    local Cinv = circle:new(p, T)
    local C1   = Cinv:inversion(self) -- cercle
    local C2   = Cinv:inversion(C)    -- droite
    local L1, L2 = C1:tangent_parallel(C2)
    local S1 = L1 and Cinv:inversion(L1) or nil
    local S2 = L2 and Cinv:inversion(L2) or nil
    return S1, S2
  end

  return build_common_tangents_by_inversion(self, C, p, mode)
end

-------------------------------------
-- ===== Solveur SECANT =====
-------------------------------------
local function solve_secant(self, C, p, a, b, pos, mode)
  a = a or loc(self, p)
  b = b or loc(C,    p)

  if a == "ON" and b == "ON" then
    return nil
  end

  if (a == "ON" and b == "IN") or (a == "IN" and b == "ON") then
    return build_secant_on_in_by_bisectors(self, C, p)
  end

  if (a == "ON" and b == "OUT") or (a == "OUT" and b == "ON") then
    return solve_disjoint(self, C, p, a, b, pos, mode)
  end

  return build_common_tangents_by_inversion(self, C, p, mode)
end

-------------------------------------
-- ===== Solveur NESTED =====
-------------------------------------
local function solve_nested(self, C, p, a, b, pos, mode)
  a = a or loc(self, p)
  b = b or loc(C,    p)

  if a == "ON" or b == "ON" then
    return build_nested_on_border_midcircle(self, C, p)
  end

  if (a == "IN" and b == "OUT") or (a == "OUT" and b == "IN") then
    return build_common_tangents_by_inversion(self, C, p, mode)
  end

  if a == "IN" and b == "IN" then
    return build_common_tangents_by_inversion(self, C, p, mode)
  end

  return solve_disjoint(self, C, p, a, b, pos, mode)
end

-------------------------------------
-- ===== Dispatcher principal =====
-------------------------------------
function circle:c_cc_p(C, p, mode)
  mode = mode or "external"
  local pos = self:circles_position(C)
  local cfg = config_from_pos(pos)
  local a   = loc(self, p)
  local b   = loc(C,    p)

  if cfg == "disjoint" then
    return solve_disjoint(self, C, p, a, b, pos, mode)
  elseif cfg == "secant" then
    return solve_secant(self, C, p, a, b, pos, mode)
  else
    return solve_nested(self, C, p, a, b, pos, mode)
  end
end

--====================== end of c_cc_p ========---


-- Circle  tangent to one circle, on line and passing through a point
function circle:c_lc_p(l, p, inside)
  inside = (inside == "inside")

  -- Check whether point p is inside or outside the circle.
  if self:in_out(p) then
    -- Trouve les intersections de la ligne avec les deux bords du cercle
    local t1 = intersection_ll_(self.north, p, l.pa, l.pb)
    local t2 = intersection_ll_(self.south, p, l.pa, l.pb)

    local l1 = l:ortho_from(t1)
    local l2 = l:ortho_from(t2)

    local o1 = intersection_ll_(self.center, p, l1.pa, l1.pb)
    local o2 = intersection_ll_(self.center, p, l2.pa, l2.pb)

    return circle:new(o1, t1), circle:new(o2, t2)
  else
    -- If point p is outside the circle, check whether p is inside or outside the line
    if l:in_out(p) then
      -- Calculates the projection of 'self.center' onto the line
      local i = l:projection(self.center)
      local lortho = l:ortho_from(p)
      local u = lortho:report(self.radius, p)
      local v = lortho:report(-self.radius, p)
      -- Find the medians between the center of the circle and the ratio points
      local ux, uy = mediator_(self.center, u)
      local vx, vy = mediator_(self.center, v)
      -- Find mediator intersections
      local o1 = intersection_ll_(u, v, ux, uy)
      local o2 = intersection_ll_(u, v, vx, vy)
      -- Returns circles defined by points of intersection and p
      return circle:new(o1, p), circle:new(o2, p)
    else
      -- General case
      local u = self.north
      local v = self.south
      local h = intersection_ll_(u, v, l.pa, l.pb)

      if inside then
        -- Calculate the circumscribed circle between p, u and h
        local o = circum_circle_(p, u, h)

        -- Finds the intersection of the circle and returns the result
        local q = intersection_lc_(p, v, o, p)
        return self:c_c_pp(p, q)
      else
        -- if 'inside' is false, we calculate the other circumscribed circle
        local o = circum_circle_(p, v, h)

        -- Finds the intersection of the circle and returns the result
        local q = intersection_lc_(u, p, o, v)
        return self:c_c_pp(p, q)
      end
    end
  end
end
-- ======= END of c_lc_p

-----------------------------------------------------------
-- Result -> drawing
-----------------------------------------------------------
function circle:draw()
  -- Récupère les coordonnées du centre et le rayon du cercle
  local x, y = self.center:get()
  local r = self.radius

  -- Format de la commande LaTeX pour dessiner le cercle
  local frmt = "\\draw (%0.3f,%0.3f) circle [radius=%0.3f];"

  -- Affiche la commande LaTeX en formatant les valeurs de x, y et r
  tex.sprint(string.format(frmt, x, y, r))
end

function circle:set_inversion(...)
  local tp = table.pack(...)
  local i
  local t = {}
  for i = 1, tp.n do
    table.insert(t, inversion_(self.center, self.through, tp[i]))
  end
  return table.unpack(t)
end

function circle:inversion_L(L)
  -- Vérifie si le centre du cercle est à l'intérieur ou à l'extérieur de la ligne L
  if L:in_out(self.center) then
    return L -- Retourne la ligne L inchangée si le centre est du bon côté
  else
    -- Calcul de la projection du centre sur la ligne L
    local p = L:projection(self.center)

    -- Inversion du point projeté par rapport au cercle défini par 'self.center' et 'self.through'
    local q = inversion_(self.center, self.through, p)

    -- Retourne un cercle avec le centre au milieu de 'self.center' et 'q' et comme rayon 'q'
    return circle:new(midpoint_(self.center, q), q)
  end
end

function circle:inversion_C(C)
  local p, q, x, y, X, Y
  if C:in_out(self.center) then
    p = C:antipode(self.center)
    q = inversion_(self.center, self.through, p)
    x = ortho_from_(q, self.center, p)
    y = ortho_from_(q, p, self.center)
    return line:new(x, y)
  else
    x, y = intersection_lc_(self.center, C.center, C.center, C.through)
    X = inversion_(self.center, self.through, x)
    Y = inversion_(self.center, self.through, y)
    return circle:new(midpoint_(X, Y), X)
  end
end

function circle:inversion(...)
  local tp = table.pack(...)
  local obj = tp[1]
  local nb = tp.n
  if nb == 1 then
    if obj.type == "point" then
      return inversion_(self.center, self.through, obj)
    elseif obj.type == "line" then
      return self:inversion_L(obj)
    else
      return self:inversion_C(obj)
    end
  else
    local t = {}
    for i = 1, nb do
      table.insert(t, inversion_(self.center, self.through, tp[i]))
    end
    return table.unpack(t)
  end
end

function circle:path(za, zb, nb)
  local P = path()
  for i = 0, nb do
    local t = i / nb
    local pt = self:point_arc(za, zb, t)
    P:add_point(pt)
  end
  return P
end

function circle:point_arc(za, zb, t)
  local anglezazb = get_angle_normalize_(self.center, za, zb)
  local phi = t * anglezazb
  return rotation_(self.center, phi, za)
end

return circle
