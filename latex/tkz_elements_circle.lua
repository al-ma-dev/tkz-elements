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
circle.in_out = circle.on_circle -- "SUR"
circle.in_out_disk = circle.in_disk -- inclusif
circle.in_out_disk_strict = circle.in_disk_strict

-- Tri-état canonique : "IN", "ON", "OUT"
function circle:point_position(p)
  return point_circle_position_(self.center, self.through, p)
end

-- Boolean shortcuts (same default tolerance)
function circle:is_disjoint(L)
  return self:line_position(L) == "disjoint"
end

function circle:is_tangent(L)
  return self:line_position(L) == "tangent"
end

function circle:is_secant(L)
  return self:line_position(L) == "secant"
end

function circle:line_position(L)
  -- radius computed from center and through points
  -- (safer than accessing a stored self.r field)
  local r = self.radius
  local d = distance_(L.pa, L.pb, self.center)

  -- classification with tolerance
  if d > r + tkz.epsilon then
    return "disjoint"
  elseif math.abs(d - r) <= tkz.epsilon then
    -- near-tangency treated as tangency
    return "tangent"
  else
    -- secant
    return "secant"
  end
end

-- Relative position between two lines and a circle-

function circle:lines_position(L1, L2, mode)
  local a, b = L1:get()
  local c, d = L2:get()
  local o = self.center
  local r = self.radius

  if mode == "parallel" or is_parallel_(a, b, c, d) then
    return lines_parallel_circle_(o, r, a, b, c, d)
  else
    return lines_secant_circle_(o, r, a, b, c, d)
  end
end

function circle:are_circles_tangents(C)
    local d = point.mod(self.center - C.center)
    local max = self.radius + C.radius
    local min = math.abs(self.radius - C.radius)
 return (math.abs(d - max) < tkz.epsilon) or  ( math.abs(d - min) < tkz.epsilon)
end
circle.act = circle.are_circles_tangents
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

-- Pôle d’une droite L par rapport au cercle self
-- Retour : le point P (ou nil,"INFINITE" si L passe par O)
function circle:pole(L)
  local O = self.center
  local R = self.radius

  -- 1) H = projection de O sur L
  local H = L:projection(O)
  if not H then
    return nil, "NO_PROJECTION"  -- cas pathologique
  end

  -- 2) Si L passe par O -> pôle à l’infini (direction ⟂ L)
  if length_(O, H) < tkz.epsilon then
    return nil, "INFINITE"       -- le pôle est au point à l’infini
  end

  -- 3) P = inversion de H dans le cercle (centre O, rayon R)
  --     OP * OH = R^2, P sur la demi-droite OH
  local P = self:inversion(H)
  return P
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
    return midcircle_cc_(self.center, self.through, obj.center, obj.through)
  elseif type(obj) == "table" and obj.type == "line" then
    return midcircle_cl_(self.center, self.through, obj.pa, obj.pb)
  else
    tex.error("midcircle: unsupported types (expect circle/circle or circle/line)")
  end
end


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

-- Alias simple : inversion de rapport < 0
function circle:inversion_neg(obj)
  return self.center:symmetry(self:inversion(obj))
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

---===  PB CONTACT APOLLONIUS ===---


-- -----------------------------------------------------------
-- CPP  CCP  CLP CLL
-- CCC(?) CCL(?)
-- ================== CPP ==========================
function circle:CPP(a, b)

  local o  = self.center
  local th = self.through
  local u, v = mediator_(a, b)

  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  local ain = in_disk_(o, th, a)
  local bin = in_disk_(o, th, b)

  ------------------------------------------------------------
  -- 0) Un point dedans et l’autre dehors → aucune solution
  ------------------------------------------------------------
  if (ain and not bin) or (bin and not ain) then
    return pa_center, pa_through, 0

  ------------------------------------------------------------
  -- 1) Droite (AB) tangent au cercle (self)
  ------------------------------------------------------------
  elseif is_tangent_(a, b, o, th) and (not is_equidistant_(a, b, o)) then
    local w = circum_center_(a, b, o)
    local c, d = intersection_cc_(o, th, w, a)
    local i = intersection_ll_(c, d, a, b)
    local t, tp = tangent_from_(o, th, i)

    if is_parallel_(o, t, u, v) then
      local w1 = intersection_ll_(o, tp, u, v)
      push(w1, tp)
    elseif is_parallel_(o, tp, u, v) then
      local w1 = intersection_ll_(o, t, u, v)
      push(w1, t)
    else
      local w1 = intersection_ll_(u, v, o, t)
      local w2 = intersection_ll_(u, v, o, tp)
      push(w1, t)
      push(w2, tp)
    end
    return pa_center, pa_through, n

  ------------------------------------------------------------
  -- 2) Droite (AB) parallèle à une tangente au cercle
  ------------------------------------------------------------
  elseif is_equidistant_(a, b, o) then
    local t1, t2 = intersection_lc_(u, v, o, th)
    local w1 = circum_center_(a, b, t1)
    local w2 = circum_center_(a, b, t2)
    push(w1, t1)
    push(w2, t2)
    return pa_center, pa_through, n

  ------------------------------------------------------------
  -- 3) Deux points EXTERIEURS : cas classique (ton cas général)
  ------------------------------------------------------------
  elseif (not ain) and (not bin) then
    local w = circum_center_(a, b, o)
    local c, d = intersection_cc_(o, th, w, a)
    local i = intersection_ll_(a, b, c, d)
    local t, tp = tangent_from_(o, th, i)
    local w1 = intersection_ll_(u, v, o, t)
    local w2 = intersection_ll_(u, v, o, tp)
    push(w1, t)
    push(w2, tp)
    return pa_center, pa_through, n

  ------------------------------------------------------------
  -- 4) Deux points INTERIEURS → inversion
  ------------------------------------------------------------
  elseif ain and bin then
    local Cinv = circle:new(a, b)
    local Csol = Cinv:inversion(self)
    if not Csol then return pa_center, pa_through, 0 end

    local LTA, LTB = Csol:tangent_from(b)
    if LTA then
      local C1 = Cinv:inversion(LTA)
      if C1 then push(C1.center, a) end
    end
    if LTB then
      local C2 = Cinv:inversion(LTB)
      if C2 then push(C2.center, a) end
    end
    return pa_center, pa_through, n
  end

end

circle.c_c_pp = circle.CPP


--====================== START of CCP ========---
-------------------------------------

--------------------------------------------------------
local function sp_c_cc_p(p, o1, t1, o2, t2, r1, r2, ex)
  local n = 0
  local pa_center  = path:new()
  local pa_through = path:new()

  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  -- NB: on suppose que report_ accepte (A,B,scale[,origin]) comme ailleurs.
  local x = report_(o1, o2, r1)       -- point sur la direction o1->o2, distance ~r1
  local y = report_(o2, o1, r2)       -- point sur la direction o2->o1, distance ~r2
  local o = circum_center_(p, x, y)   -- centre du cercle (p,x,y)

  -- intersections de cercles : (o,p) et (o2,t2)
  local u, v  = intersection_cc_(o, p, o2, t2)
  if not u or not v then
    return pa_center, pa_through, 0
  end

  -- choisir u tel que u != y (échange sinon)
  if u:identity(y) then u, v = v, u end

  -- ligne (y,u) croise (ex,p) en i
  local i = intersection_ll_(y, u, ex, p)
  if not i then
    return pa_center, pa_through, 0
  end

  -- tangentes depuis i au cercle (o2,t2)
  local tau1, tau2 = tangent_from_(o2, t2, i)  -- renommé pour ne pas masquer paramètres
  if not tau1 or not tau2 then
    return pa_center, pa_through, 0
  end

  -- centres = intersections des droites (o2,tau_k) et (p,o)
  local w1 = intersection_ll_(o2, tau1, p, o)
  local w2 = intersection_ll_(o2, tau2, p, o)
  if not w1 or not w2 then
    -- on pousse ce qui existe
    if w1 then push(w1, tau1) end
    if w2 then push(w2, tau2) end
    return pa_center, pa_through, n
  end

  -- ordre : w1 le plus proche de p
  if length_(w1, p) > length_(w2, p) then w1, w2, tau1, tau2 = w2, w1, tau2, tau1 end
  push(w1, tau1)
  push(w2, tau2)
  return pa_center, pa_through, n
end


-- ===================================================
-- solve_intersect(Cself, C, p, mode)
--  → Cherche les cercles tangents à Cself et C passant par p
--  → version simple, sans inversion, sans dépendances externes
-- ===================================================

-- Cercle tangent à self et C, passant par p (cas général)
function solve_intersect(Cself, C, p, mode)
  if Cself:on_circle(p) then
    local tA, tB = tangent_at_(Cself.center, p)
    local T = line:new(tA, tB)
    return C:CLP(T, p)
  elseif C:on_circle(p) then
    local tA, tB = tangent_at_(C.center, p)
    local T = line:new(tA, tB)
    return Cself:CLP(T, p)
  else
    local pa_center = path:new()
    local pa_through = path:new()
    local n = 0
    local function push(o, t)
      if o and t then
        pa_center:add_point(o)
        pa_through:add_point(t)
        n = n + 1
      end
    end
    local I = midpoint_(Cself.center, C.center)
    local K = circle:new(p, I)
    local A = K:inversion(Cself)
    local B = K:inversion(C)
    local T1, T2 =  A:common_tangent(B, "external")
    local C1 = K:inversion(T1)
    local C2 = K:inversion(T2)
    local O1, T1 = C1:get()
    local O2, T2 = C2:get()
    push(O1, T1)
    push(O2, T2)
    return pa_center, pa_through, n
  end
end

--------------------------------------------------------
-- CCP : deux cercles tangents intérieurement, cercle solution passant par p
-- (Uniformisation push/paths, logique intacte)
--------------------------------------------------------
function solve_inside_tangent(Cself, C, p, mode)
  if Cself:on_circle(p) then
    local tA, tB = tangent_at_(Cself.center, p)
    local T = line:new(tA, tB)
    return C:CLP(T, p)
  elseif C:on_circle(p) then
    local tA, tB = tangent_at_(C.center, p)
    local T = line:new(tA, tB)
    return Cself:CLP(T, p)
  else

  local T = intersection(Cself, C)           -- point de tangence commun
  if p:identity(T) then
    return path:new(), path:new(), 0        -- P = T -> aucune solution
  end

  -- Inversion en p
  local K = circle:new(p, Cself.through or (p + point(1,0)))
  local A = K:inversion(Cself)               -- typiquement une droite (p ∈ Cself ?)
  local B = K:inversion(C)
  local L1, L2 = A:common_tangent(B, "all")
  local function valid(L) return L and (not L:on_line(p)) end
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

    if valid(L1) then
      local S1 = K:inversion(L1)
      if S1 and (S1.radius or 0) > tkz.epsilon then push(S1.center, S1.through) end
    end
    if valid(L2) then
      local S2 = K:inversion(L2)
      if S2 and (S2.radius or 0) > tkz.epsilon then push(S2.center, S2.through) end
    end

  return pa_center, pa_through, n
  end
end

--------------------------------------------------------
-- CCP : cas "inside"
--------------------------------------------------------

function solve_inside(Cself, C, p, mode)
  local a = Cself:point_position(p)
  local b = C:point_position(p)

  -- Cas spéciaux (0 solution)
  if (a == "OUT" and b == "ON") or (a == "IN" and b == "IN") then
    return path:new(), path:new(), 0
  else
  -- Création du cercle d'inversion K (exactement comme tu as proposé)
  local K = circle:new(C.through, Cself.through)
  local A = K:inversion(Cself)
  local B = K:inversion(C)

  -- Image inversée du point p
  local ip = K:inversion(p)

  -- On crée les chemins de sortie
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end
  local pc_inv, pt_inv, m = A:CLP(B, ip)

  if pc_inv and pt_inv and m and m > 0 then
    for i = 1, m do
      local o_inv = pc_inv:get(i)
      local t_inv = pt_inv:get(i)
      if o_inv and t_inv then
        -- Reconstitue le cercle dans le plan inversé
        local Cinv = circle:new(o_inv, t_inv)
        -- Retour dans le plan original
        local S = K:inversion(Cinv)
        push(S.center, S.through)
        -- if S and S.center and S.through then
        --   local r = length_(S.center, S.through)
        --   if r and r > tkz.epsilon then
        --
        --   end
        -- end
      end
    end
  end

  return pa_center, pa_through, n
  end
end

--------------------------------------------------------
-- CCP : cas "outside"
-- (Uniformisation + garde-fous, même structure)
--------------------------------------------------------

-- ==================== CAS SPÉCIAL r1 == r2 ====================
  local function solve_outside_equal_radii(Cself, C, p, mode)
  mode = mode or "external"
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

    local M
    if C:on_circle(Cself.through) then
       M = antipode_(Cself.center, Cself.through)
    else
       M = Cself.through
     end
    local K = circle:new(M, C.center)
    local A = K:inversion(Cself)
    local B = K:inversion(C)
    local ip = K:inversion(p)
    local pc_inv, pt_inv, m = B:CLP(A, ip)
    if pc_inv and pt_inv and m then
      for i = 1, m do
        local oi = pc_inv:get(i)
        local ti = pt_inv:get(i)
        if oi and ti then
          local S = K:inversion(circle:new(oi, ti))
          if S and S.center and S.through then
            push(S.center, S.through)
          end
        end
      end
    end

    return pa_center, pa_through, n
  end

-- ================== FIN CAS r1 == r2 ==================


function solve_outside(Cself, C, p, mode)
  local o1, t1, r1 = Cself.center, Cself.through, Cself.radius
  local o2, t2, r2 = C.center,   C.through,   C.radius
  local pos1 = Cself:point_position(p)  -- "IN", "ON", "OUT"
  local pos2 = C:point_position(p)

  if pos1 == "IN" or pos2 == "IN" then
  return path:new(), path:new(), 0

  elseif pos1 == "ON" then
    local tA, tB = tangent_at_(o1, p)   -- deux points sur la tangente à self en p
    local T = line:new(tA, tB)
    return C:CLP(T, p)

  elseif pos2 == "ON"  then
    local tA, tB = tangent_at_(o2, p)   -- deux points sur la tangente à self en p
    local T = line:new(tA, tB)
  return Cself:CLP(T, p)

  else
    if mode == "all" then
      local PCe, PTe, ne = Cself:CCP(C, p, "external")
      local PCi, PTi, ni = Cself:CCP(C, p, "internal")
      return (PCe + PCi), (PTe + PTi), (ne + ni)
    end

    -- ======= DÉLÉGATION SI r1 == r2 =======
    if tkz.approx(r1, r2) then
      return solve_outside_equal_radii(Cself, C, p, mode)
    end

    if mode == "internal" then
      local i = Cself:internal_similitude(C)
      local x = report_(o1, o2, -r1, o1)
      local y = report_(o2, o1,  r2, o2)
      local o = circum_center_(x, y, p)
      local r, s = intersection_lc_(i, p, o, p)  -- r ≠ p
      if not r or not s then
        return path:new(), path:new(), 0
      end
      if r:identity(p) then r, s = s, r end
      return Cself:CPP(p, r)
    else
      -- external
      local Cmid = Cself:midcircle(C)
      local ex   = Cmid.center
      local q    = Cmid:inversion(p)
      if p:identity(q) then
        return sp_c_cc_p(p, o1, t1, o2, t2, r1, r2, ex)
      else
        return Cself:CPP(p, q)
      end
    end
  end
end



--------------------------------------------------------
-- Routeur CCP (inchangé structurellement)
--------------------------------------------------------
function circle:CCP(C, p, mode)
  mode = mode or "all"
  local pos = self:circles_position(C)

    if     pos == "outside"         then return solve_outside(self, C, p, mode)
    elseif pos == "intersect"       then return solve_intersect(self, C, p, mode)
    elseif pos == "outside tangent" then return solve_outside(self, C, p, mode)
    elseif pos == "inside tangent"  then return solve_inside_tangent(self, C, p, mode)
    else                                 return solve_inside(self, C, p, mode)
    end
end


----==== END of CCP ======

--=====================
  -- ======= START of CLP
  --=====================
-- Circle tangent to one circle, on line and passing through a point
-- Returns: PA.center, PA.through, n   (paths of centers / through-points, and count)

--=====================
-- CLP : circle tangent to a line and passing through a point
-- Retour : pa_center, pa_through, n
--=====================
function circle:CLP(l, p, which)
  which = which or "all"

  local pos_line = self:line_position(l)
  local os, ts, rs = self.center, self.through, self.radius
  local ip = projection_(l.pa, l.pb, p)     -- ou l:projection(p)
  local i  = projection_(l.pa, l.pb, os)    -- ou l:projection(os)
  local no = report_(os, i, -rs)
  local so = report_(os, i,  rs)
  local pos = intersection_ll_(l.pa, l.pb, os, p)
  local sO, sP = l:side_line(os), l:side_line(p)

  -- chemins + compteur
  local pa_center, pa_through = path:new(), path:new()
  local n = 0
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  if pos_line == "disjoint" then
    -- (1) P = i
    if p:identity(i) then
      local o1 = midpoint_(p, no)
      local o2 = midpoint_(p, so)
      push(o1, p)
      push(o2, p)
      return pa_center, pa_through, n

    -- (2) P sur l (P ≠ i)
    elseif l:on_line(p) then
      if not l.length or l.length == 0 then return pa_center, pa_through, n end
      local k  = rs / l.length
      local u  = orthogonal_at_(l.pa, l.pb, p,  k)
      local v  = orthogonal_at_(l.pa, l.pb, p, -k)
      local ux, uy = mediator_(os, u)
      local vx, vy = mediator_(os, v)
      local o1 = intersection_ll_(u, v, ux, uy)
      local o2 = intersection_ll_(u, v, vx, vy)
      push(o1, p)
      push(o2, p)
      return pa_center, pa_through, n

    -- (3) P du mauvais côté
    elseif sO ~= sP then
      return pa_center, pa_through, n

    -- (4) P strictement dans le disque
    elseif self:in_disk_strict(p) then
      return pa_center, pa_through, n

    -- (5) P sur C
    elseif self:on_circle(p) then
      if p:identity(no) then
        push(midpoint_(no, i), p)
        return pa_center, pa_through, n
      elseif p:identity(so) then
        push(midpoint_(so, i), p)
        return pa_center, pa_through, n
      else
        local i1 = intersection_ll_(l.pa, l.pb, no, p)
        local i2 = intersection_ll_(l.pa, l.pb, so, p)
        if i1 then
          local j1 = orthogonal_at_(l.pa, l.pb, i1)
          local o1 = intersection_ll_(i1, j1, os, p)
          push(o1, p)
        end
        if i2 then
          local j2 = orthogonal_at_(l.pa, l.pb, i2)
          local o2 = intersection_ll_(i2, j2, os, p)
          push(o2, p)
        end
        return pa_center, pa_through, n
      end

    -- (6) Cas général : inversion en P
    else
      local Cinv = circle:new(p, ip)
      local C1   = Cinv:inversion(self)
      local C2   = Cinv:inversion(l)

      if which == "external" or which == "all" then
        local L1, L2 = C1:common_tangent(C2, "external")
        if L1 then
          local S = Cinv:inversion(L1)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        if L2 then
          local S = Cinv:inversion(L2)
          if S and S.center and S.through then push(S.center, S.through) end
        end
      end
      if which == "internal" or which == "all" then
        local L1, L2 = C1:common_tangent(C2, "internal")
        if L1 then
          local S = Cinv:inversion(L1)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        if L2 then
          local S = Cinv:inversion(L2)
          if S and S.center and S.through then push(S.center, S.through) end
        end
      end
      return pa_center, pa_through, n
    end

  elseif pos_line == "tangent" then
    -- (1) P = i
    if p:identity(i) then
      push(os, i)
      return pa_center, pa_through, n

    -- (2) P sur l (config tangente)
    elseif l:on_line(p) then
      local Cinv  = circle:new(i, p)
      local Linv  = Cinv:inversion(self)         -- image de C : une droite
      local pac, pat, m = l:LLP(Linv, p)
      for k = 1, (m or 0) do
        local o = pac:get(k)
        local t = pat:get(k)
        if o and t then
          local S1 = Cinv:inversion(circle:new(o, t))
          if S1 then
            local oc, tc = S1:get()
            push(oc, tc)
          end
        end
      end
      return pa_center, pa_through, n

    -- (3) P du mauvais côté
    elseif sO ~= sP then
      return pa_center, pa_through, n

    -- (4) P strictement dans le disque
    elseif self:in_disk_strict(p) then
      return pa_center, pa_through, n

    -- (5) P sur C, au point singulier (no/so)
    elseif self:on_circle(p) and (p:identity(no) or p:identity(so)) then
      push(os, ts)
      return pa_center, pa_through, n

    -- (6) P sur C, cas général
    elseif self:on_circle(p) then
      local Cinv  = circle:new(i, p)
      local Lself = Cinv:inversion(self)
      local mpt   = midpoint_(p, ip)
      local S1    = Cinv:inversion(circle:new(mpt, p))
      if S1 then
        local oc, tc = S1:get()
        push(oc, tc)
      end
      return pa_center, pa_through, n

    -- (7) Cas général tangent
    else
      local Cinv  = circle:new(i, p)
      local Lself = Cinv:inversion(self)
      local pac, pat, m = l:LLP(Lself, p)
      for k = 1, (m or 0) do
        local o = pac:get(k)
        local t = pat:get(k)
        if o and t then
          local S1 = Cinv:inversion(circle:new(o, t))
          if S1 then
            local oc, tc = S1:get()
            push(oc, tc)
          end
        end
      end
      return pa_center, pa_through, n
    end

  else -- "secant"
      if l:on_line(p) and self:on_circle(p) then
        return pa_center, pa_through, 0

elseif self:on_circle(p) then

        local Cinv  = circle:new(p, ip)
        local Lself = Cinv:inversion(self)   -- droite
        local C2    = Cinv:inversion(l)      -- cercle

        if not (Lself and C2 and C2.center and C2.radius) then
          return pa_center, pa_through, n
        end

        local d  = (Lself.pb - Lself.pa)
        local dn = point.abs(d)
        if dn <= tkz.epsilon then
          return pa_center, pa_through, n
        end
        -- normale unitaire : n = perp(d)/|d|
        local nux =  (d.im / dn)
        local nuy = -(d.re / dn)
        local nvec = point(nux, nuy)

        -- Deux points à distance r2 du centre de C2, le long de la normale à Lself
        local O2 = C2.center
        local r2 = C2.radius
        local Q1 = O2 + r2 * nvec
        local Q2 = O2 - r2 * nvec

        -- Deux droites parallèles à Lself (donc "tangentes" à Lself au sens requis) et tangentes à C2
        local L1  = line:new(Q1, Q1 + d)
        local L2  = line:new(Q2, Q2 + d)

        -- Ré-inversion → deux cercles solutions (passant par P, tangents à C et à l)
        if L1 then local S = Cinv:inversion(L1); if S and S.center and S.through then push(S.center, S.through) end end
        if L2 then local S = Cinv:inversion(L2); if S and S.center and S.through then push(S.center, S.through) end end

        return pa_center, pa_through, n

      elseif l:on_line(p) then

        local _, tp = through(p, 1)
        local Cinv  = circle:new(p, tp)
        local C1    = Cinv:inversion(self)
        if not C1 or not C1.center or not C1.radius then
          return pa_center, pa_through, n
        end
        local o1, r1 = C1.center, C1.radius

        if not l.length or l.length == 0 then return pa_center, pa_through, n end
        local k  = r1 / l.length
        local q1 = orthogonal_at_(l.pa, l.pb, o1,  k)
        local q2 = orthogonal_at_(l.pa, l.pb, o1, -k)

        local dir = (l.pb - l.pa)
        local L1  = line:new(q1, q1 + dir)
        local L2  = line:new(q2, q2 + dir)

        if L1 then
          local S = Cinv:inversion(L1)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        if L2 then
          local S = Cinv:inversion(L2)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        return pa_center, pa_through, n

      else

        local Cinv = circle:new(p, ip)
        local C1   = Cinv:inversion(self)    -- cercle
        local C2   = Cinv:inversion(l)       -- cercle (car p ∉ l)

        local Le1, Le2 = C1:common_tangent(C2, "external")
        if Le1 then
          local S = Cinv:inversion(Le1)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        if Le2 then
          local S = Cinv:inversion(Le2)
          if S and S.center and S.through then push(S.center, S.through) end
        end
        return pa_center, pa_through, n
      end
    end

end
-- Aliases
circle.c_lc_p = circle.CLP
circle.c_cl_p = circle.CLP
--=====================
-- ======= END of CLP
--=====================



--========================================
-- CLL : cercle tangent à 2 droites L1,L2
--========================================

--==============================
-- Helper : cercle dégénéré ?
--==============================
local function is_degenerate_circle(C, I)
  if not C then return true end
  if math.abs(C.radius or 0) < tkz.epsilon then return true end
  if C.through and C.through:identity(I) then return true end
  return false
end


-- first sector u,i,v ; then v,i,up ; up,i,vp ; vp,i,u
function circle:CLL(L1, L2, choice, inside)
  choice = choice or "all"
  if choice == "all" then
    return self:CLL_all(L1, L2)
  else
    local a, b = L1:get()
    local c, d = L2:get()
    local o = self.center
    local r = self.radius
    local i = intersection_ll_(a, b, c, d)

    -- Orientation relative des droites
    local s  = (((b - a) ^ (d - c)) >= 0) and 1 or -1
    local u  = report_(a, b,  1,  i)
    local up = report_(a, b, -1,  i)
    local v  = report_(c, d,  s,  i)
    local vp = report_(c, d, -s,  i)

    -- Choix du secteur (1..4)
    local x, y
    if     choice == 1 then x, y = u,  v
    elseif choice == 2 then x, y = v,  up
    elseif choice == 3 then x, y = up, vp
    elseif choice == 4 then x, y = vp, u
    else
      tex.error("Bad choice ! Integer must be between 1 and 4")
    end

    -- Gestion inside : rayon changé de signe
    inside = (inside == "inside")
    if inside then r = -r end

    -- Deux droites à distance ±r depuis I vers x/y
    local LL1 = line(collinear_at_distance_(i, x, -r))
    local LL2 = line(collinear_at_distance_(i, y,  r))

    -- LLP (version paths) : centres sur LL2, tangence sur LL1, passant par o
    local pc, pt, m = LL2:c_ll_p(LL1, o)

    -- Reconstitution des cercles solution (projeter sur la droite (i,x))
    local S1, S2 = nil, nil
    if (m or 0) >= 1 then
      local w1 = pc:get(1)
      local t1 = line(i, x):projection(w1)
      S1 = circle:new(w1, t1)
    end
    if (m or 0) >= 2 then
      local w2 = pc:get(2)
      local t2 = line(i, x):projection(w2)
      S2 = circle:new(w2, t2)
    end

    -- Filtrage dégénérés
    local d1 = S1 and is_degenerate_circle(S1, i)
    local d2 = S2 and is_degenerate_circle(S2, i)
    if d1 and (not d2) and S2 then
      S1, S2 = S2, S1
      d1, d2 = d2, d1
    elseif d1 and d2 then
      S1, S2 = nil, nil
    end

    -- Emballage en paths avec push "à la CLP"
    local pa_center  = path:new()
    local pa_through = path:new()
    local n = 0

    local function push(o, t)
      if o and t then
        pa_center:add_point(o)
        pa_through:add_point(t)
        n = n + 1
      end
    end

    if S1 then push(S1.center, S1.through) end
    if S2 then push(S2.center, S2.through) end

    return pa_center, pa_through, n
  end
end
circle.c_cll = circle.CLL

--=====================================================
-- CLL_all : toutes les solutions selon les “sectors”
--=====================================================
function circle:CLL_all(L1, L2)
  local sectors = self:lines_position(L1, L2)

  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  -- push "à la CLP" (centralisé ici)
  local function push(o_, t_)
    if o_ and t_ then
      pa_center:add_point(o_)
      pa_through:add_point(t_)
      n = n + 1
    end
  end

  -- Ingestion (pc, pt, m) → paths locaux via push
  local function push_from_paths(pc, pt, m)
    if not pc or not pt or not m then return end
    for i = 1, m do
      push(pc:get(i), pt:get(i))
    end
  end

  if not sectors or #sectors == 0 then
    return pa_center, pa_through, 0
  end

  local function add_choice(choice, inside)
    local pc, pt, m = self:CLL(L1, L2, choice, inside)
    push_from_paths(pc, pt, m)
  end

  if #sectors == 1 then
    local m = sectors[1]
    add_choice(m, nil)        -- 2
    add_choice(m, "inside")   -- 2 -> total 4
  elseif #sectors == 2 then
    local m, n = sectors[1], sectors[2] -- adjacents
    add_choice(m, nil)        -- 2
    add_choice(n, nil)        -- 2 -> total 4
  elseif #sectors == 3 then
    local l, m, n = sectors[1], sectors[2], sectors[3]
    add_choice(l, nil)        -- 2
    add_choice(n, nil)        -- 2
    add_choice(m, nil)        -- 2
    add_choice(m, "inside")   -- 2 -> total 8
  else
    -- {1,2,3,4} -> 8 solutions (2 par secteur)
    add_choice(1, nil)
    add_choice(2, nil)
    add_choice(3, nil)
    add_choice(4, nil)
  end

  return pa_center, pa_through, n
end


---===== START  CCC by Viète =====-------
-- === Test de tangence robuste (absolu ET relatif) ============================
local function act_(O1, T1, O2, T2, opts)
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

-- === CCC : toutes les solutions (0..8) ======================================
function circle:CCC_viete_core(C2, C3, opts)
  opts = opts or {}
  local C1, R3, W3 = self, C3.radius, C3.center

  local pc, pt, n = path:new(), path:new(), 0
  local function push(o,t) pc:add_point(o); pt:add_point(t); n = n + 1 end

  -- dédup : compare centre et rayon avec tolérance
  local function exists_(o,t)
    local r = length_(o,t)
    for i=1,n do
      local oi, ti = pc:get(i), pt:get(i)
      if oi and ti then
        local same_center = length_(o, oi) <= (opts.abs or 1e-4)
        local same_radius = math.abs(r - length_(oi,ti)) <= (opts.abs or 1e-4)
        if same_center and same_radius then return true end
      end
    end
    return false
  end
  local function push_unique(o,t) if o and t and not exists_(o,t) then push(o,t) end end

  -- ta variante “garder centre, ajouter ΔR au rayon”
  local function grow_keep_center(C, dR)
    return circle:new(C.center, report_(C.center, C.through, dR, C.through))
  end

  -- test "rayon ~ 0" robuste (absolu OU relatif)
  local function radius_is_zero_(r, ref)
    local eps_abs = (opts.abs ~= nil) and opts.abs or 1e-4
    local eps_rel = (opts.rel ~= nil) and opts.rel or 1e-6
    return (math.abs(r) <= eps_abs) or (math.abs(r)/math.max(ref,1.0) <= eps_rel)
  end

  -- petit helper : à partir d'une liste de centres (path pcv, nv),
  -- produire les deux relèvements +/- R3 (comme dans ton code initial)
  local function lift_from_centers_(pcv, nv)
    for i=1,nv do
      local O = pcv:get(i)
      if O then
        local Tplus  = report_(O, W3,  R3, W3)  -- r = |OW3| + R3  (tangence interne à C3)
        local Tminus = report_(O, W3, -R3, W3)  -- r = |OW3| - R3  (tangence externe à C3)

        if Tplus then
          local S = circle:new(O, Tplus)
          if act_(S.center, S.through, C1.center, C1.through, opts)
          and act_(S.center, S.through, C2.center, C2.through, opts)
          and act_(S.center, S.through, C3.center, C3.through, opts) then
            push_unique(O, Tplus)
          end
        end

        if Tminus then
          local S = circle:new(O, Tminus)
          if act_(S.center, S.through, C1.center, C1.through, opts)
          and act_(S.center, S.through, C2.center, C2.through, opts)
          and act_(S.center, S.through, C3.center, C3.through, opts) then
            push_unique(O, Tminus)
          end
        end
      end
    end
  end

  -- Les 4 branches de Viète (±R3 sur C1 et C2)
  local mods = { {-R3,-R3}, {-R3, R3}, { R3,-R3}, { R3, R3} }

  for _, m in ipairs(mods) do
    local C1T = grow_keep_center(C1, m[1])   -- (O1, R1 + s1*R3)
    local C2T = grow_keep_center(C2, m[2])   -- (O2, R2 + s2*R3)

    local r1t = C1T.radius
    local r2t = C2T.radius

    local ref = math.max(C1.radius, C2.radius, R3, 1.0)
    local z1  = radius_is_zero_(r1t, ref)
    local z2  = radius_is_zero_(r2t, ref)

    if z1 and z2 then
      -- === PPP : les deux transformés sont des points => cercle passant par O1,O2,O3
      local pcv, ptv, nv = C1.center:PPP(C2.center, W3)
      lift_from_centers_(pcv, nv)

    elseif z1 then
      -- === CPP : un seul transformé est un point => tangent à C2T et passant par O1 et O3
      local pcv, ptv, nv = C2T:CPP(C1.center, W3)
      -- on n’utilise que les centres pour garder la même logique de relèvement
      lift_from_centers_(pcv, nv)

    elseif z2 then
      -- === CPP : un seul transformé est un point => tangent à C1T et passant par O2 et O3
      local pcv, ptv, nv = C1T:CPP(C2.center, W3)
      lift_from_centers_(pcv, nv)

    else
      -- === CCP classique : cercle (préimage) passant par W3 et tangent à C1T et C2T
      local pcv, _, nv = C1T:CCP(C2T, W3, "all")
      lift_from_centers_(pcv, nv)
    end
  end

  return pc, pt, n

end

------------------------------------------------------------
-- Dispatcher CCC : classification 3 cercles → résolution
-- - route "non-sécants" vers ta version qui marche (CCC_viete_ok)
-- - isole tous les cas "sécants" pour un traitement dédié (CCC_secant)
------------------------------------------------------------

-- === Tolérances (abs + rel) ====================================
local function approx_(a, b, opts)
  opts = opts or {}
  local eps_abs = (opts.abs ~= nil) and opts.abs or 1e-4
  local eps_rel = (opts.rel ~= nil) and opts.rel or 1e-6
  local d = math.abs(a - b)
  return (d <= eps_abs) or (d <= eps_rel * math.max(math.abs(a), math.abs(b), 1.0))
end

-- === Relation entre deux cercles Ci, Cj =========================
-- retourne une étiquette parmi :
--  "concentric", "equal", "disjoint_ext", "tangent_ext",
--  "secant", "tangent_int", "disjoint_one_in_other"
local function circle_pair_relation(Ci, Cj, opts)
  local Oi, Ri = Ci.center, Ci.radius
  local Oj, Rj = Cj.center, Cj.radius
  local d = length_(Oi, Oj)

  -- cas concentriques
  if approx_(d, 0, opts) then
    if approx_(Ri, Rj, opts) then return "equal" end
    return "concentric"
  end

  local sum = Ri + Rj
  local dif = math.abs(Ri - Rj)

  if d > sum then
    return "disjoint_ext"
  elseif approx_(d, sum, opts) then
    return "tangent_ext"
  elseif (d < sum) and (d > dif) then
    return "secant"
  elseif approx_(d, dif, opts) then
    return "tangent_int"
  else -- d < dif
    return "disjoint_one_in_other"
  end
end

-- === Signature des trois paires =================================
local function three_circles_signature(C1, C2, C3, opts)
  local r12 = circle_pair_relation(C1, C2, opts)
  local r23 = circle_pair_relation(C2, C3, opts)
  local r31 = circle_pair_relation(C3, C1, opts)

  local counts = { secant=0, tangent=0, disjoint=0, inside=0, concentric=0, equal=0 }
  local function acc(tag)
    if tag == "secant" then counts.secant = counts.secant + 1
    elseif tag == "tangent_ext" or tag == "tangent_int" then counts.tangent = counts.tangent + 1
    elseif tag == "disjoint_ext" then counts.disjoint = counts.disjoint + 1
    elseif tag == "disjoint_one_in_other" then counts.inside = counts.inside + 1
    elseif tag == "concentric" then counts.concentric = counts.concentric + 1
    elseif tag == "equal" then counts.equal = counts.equal + 1
    end
  end
  acc(r12); acc(r23); acc(r31)

  return { r12=r12, r23=r23, r31=r31, counts=counts }
end

local function CCC_viete_ok(C1, C2, C3, opts)
  return C1:CCC_viete_core(C2, C3, opts)  -- <-- ajuste le nom si différent
end

-- === 2) Stub pour les cas sécants (à implémenter ensuite) =========
-- Cas CCC : les trois cercles sont deux à deux sécants
-- Stratégie : inversion en X = C1∩C2  →  C1,C2 ↦ lignes
--             résoudre (S3:CLL(L1,L2)) ou (L1:LLL(L2,L3))  →  ré-inversion
local function CCC_secant(C1, C2, C3, opts)
  opts = opts or {}

  -- --- sorties (toujours renvoyées)
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  -- --- push unique (style unifié)
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  -- --- 1) intersections de C1 et C2 (doivent exister dans ce cas)
  local X, Y = intersection(C1, C2)
  if (not X) or (not Y) then
    -- garde-fou : si pas sécants, on ne traite pas ici
    return pa_center, pa_through, 0
  end

  -- --- 2) cercle d'inversion : centre X, rayon arbitraire (on prend XY)
  local Cinv = circle:new(X, Y)

  -- --- 3) objets inversés
  local L1 = Cinv:inversion(C1)  -- attendu : ligne
  local L2 = Cinv:inversion(C2)  -- attendu : ligne
  local S3 = Cinv:inversion(C3)  -- cercle OU ligne (si C3 passe par X)

  -- --- helper : ré-inverser une solution donnée par (o_i, t_i) dans l'espace inversé
  local function push_back_from_inverted(o_i, t_i)
    if (not o_i) or (not t_i) then return end
    local S_i  = circle:new(o_i, t_i)         -- cercle solution dans l'espace inversé
    local S_o  = Cinv:inversion(S_i)          -- ré-inversion → cercle solution original
    local t_out = Cinv:inversion(t_i)         -- ré-inversion du point de tangence/through
    if S_o and S_o.center and t_out then
      push(S_o.center, t_out)
    end
  end

  -- --- 4) Résolution dans l'espace inversé puis ré-inversion
  if S3 and S3.center then
    -- 4.a) S3 est un cercle : problème CLL (cercle tangent à 2 lignes L1,L2 et à S3)
    --     On demande toutes les solutions (secteurs) ; adaptez via opts.which / opts.inside si besoin.
    local pc, pt, m = S3:CLL(L1, L2, "all")
    if m and m > 0 then
      for i = 1, m do
        push_back_from_inverted(pc:get(i), pt:get(i))  -- pt[i] est le point de tangence sur S3
      end
    end

  elseif S3 then
    -- 4.b) S3 est une ligne (C3 passait par X) : problème LLL (trois lignes)
    local pc, pt, m = L1:LLL(L2, S3, "all")
    if m and m > 0 then
      for i = 1, m do
        push_back_from_inverted(pc:get(i), pt:get(i))  -- pt[i] : pied sur L1 (OK)
      end
    end
  else
    -- inversion de C3 échouée (hautement improbable) → pas de solution
    return pa_center, pa_through, 0
  end

  return pa_center, pa_through, n
end

-- === 3) Cas spécial : "deux tangents + un disjoint" ==============
local function CCC_two_tangent_one_disjoint(C1, C2, C3, sig, opts)
  -- Ex : r12 tangent, r23 tangent, r31 disjoint (ou toute permutation)
  return CCC_viete_ok(C1, C2, C3, opts)
end
-- === 4) Dispatcher principal =====================================
function circle:CCC(C2, C3, opts)
  local C1 = self
  local sig = three_circles_signature(C1, C2, C3, opts)
  local k = sig.counts
  -- (A) Si AU MOINS une paire est sécante → on isole et on traite à part
  if k.secant > 0 then
    return CCC_secant(C1, C2, C3, opts)
  end
  -- (B) Cas spécial "deux tangents + un disjoint"
  if (k.tangent == 2) and (k.disjoint == 1) and (k.inside == 0) and (k.concentric == 0) then
    return CCC_two_tangent_one_disjoint(C1, C2, C3, sig, opts)
  end

  -- (C) Tous disjoints (0..3) et/ou tangents (0..3), sans sécants →
  return CCC_viete_ok(C1, C2, C3, opts)
end




----=================================
----=========== <END CCC viète ===========
----=================================

-- =========================================================

function circle:CCL(C2, D)
  local pa_center, pa_through, n = path:new(), path:new(), 0
  local n = 0
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  local function circle_with_new_radius_keep_center(C, newR)
    local Tp = report_(C.center, C.through, newR)
    return circle:new(C.center, Tp)
  end

  function circle_distance_(c, t, p)
    local r = length_(c, t)
    local d = length_(c, p)
    return d - r
  end

  local C1, O1, R1 = self, self.center, self.radius
  local O2, R2     = C2.center, C2.radius
  local EPS = 0.00001

  local function signed_side_(L, P)
    local a, b = L:get()
    local z = (b - a) ^ (P - a)
    if math.abs(z) <= EPS then return 0 end
    return (z > 0) and 1 or -1
  end

  local function line_separates_disks_(L, O1, R1, O2, R2)
    local s1 = signed_side_(L, O1)
    local s2 = signed_side_(L, O2)
    if s1 == 0 or s2 == 0 or s1 * s2 >= 0 then
      return false  -- centres du même côté ou sur la droite -> pas “entre”
    end
    local d1 = D:distance(O1)
    local d2 = D:distance(O2)
    return (d1 > R1 + EPS) and (d2 > R2 + EPS)  -- strictement en dehors des deux disques
  end



  if line_separates_disks_(D, O1, R1, O2, R2) then
    return pa_center, pa_through, 0
  end
  ----------------------------------------------------------
  -- ====== FAMILLE R1 - R2 : droite translatée -R2 ======
  ----------------------------------------------------------
  do
    local newR = R1 - R2
    local pc, pt, m
    local Lplus  = D:collinear_at_distance(-R2)
    if math.abs(newR) < EPS then
        pc, pt, m = Lplus:LPP(O1, O2)
    else
      local Cminus = circle_with_new_radius_keep_center(C1, newR)
       pc, pt, m = Cminus:CLP(Lplus, O2)
    end

    for i = 1, m do
      local wi = pc:get(i)
      local ti = pt:get(i)

      local r0 = length_(wi, ti)
      local r  = r0 - R2
      local Ti_new = report_(wi, ti, r)

      local di = D:distance(wi)
      local li = length_(wi, Ti_new)

      if      act_(O1, C1.through, wi, Ti_new)
          and act_(O2, C2.through, wi, Ti_new)
          and (math.abs(li - di) < EPS)
      then
        pa_center:add_point(wi)
        pa_through:add_point(Ti_new)
      end
    end
  end

  ----------------------------------------------------------
  -- ====== FAMILLE R1 - R2 : droite translatée +R2 ======
  ----------------------------------------------------------
  do
    local newR = R1 - R2
    local pc, pt, m
    local Lplus  = D:collinear_at_distance(R2)
    if math.abs(newR) < EPS then
        pc, pt, m = Lplus:LPP(O1, O2)
    else
      local Cminus = circle_with_new_radius_keep_center(C1, newR)
       pc, pt, m = Cminus:CLP(Lplus, O2)
    end


    for i = 1, m do
      local wi = pc:get(i)
      local ti = pt:get(i)

      local r0 = length_(wi, ti)
      local r  = r0 + R2
      local Ti_new = report_(wi, ti, r)

      local di = D:distance(wi)
      local li = length_(wi, Ti_new)

      if      act_(O1, C1.through, wi, Ti_new)
          and act_(O2, C2.through, wi, Ti_new)
          and (math.abs(li - di) < EPS)
      then
        pa_center:add_point(wi)
        pa_through:add_point(Ti_new)
      end
    end
  end

  ----------------------------------------------------------
  -- ====== FAMILLE R1 + R2 : droite translatée -R2 ======
  ----------------------------------------------------------
  do
    local newR = R1 + R2
    local Cplus = circle_with_new_radius_keep_center(C1, newR)
    local Lplus = D:collinear_at_distance(-R2)
    local pc, pt, m = Cplus:CLP(Lplus, O2)

    for i = 1, m do
      local wi = pc:get(i)
      local ti = pt:get(i)

      local r0 = length_(wi, ti)
      local r  = r0 - R2                     -- même principe que R1-R2 / -R2
      local Ti_new = report_(wi, ti, r)

      local di = D:distance(wi)
      local li = length_(wi, Ti_new)

      if      act_(O1, C1.through, wi, Ti_new)
          and act_(O2, C2.through, wi, Ti_new)
          and (math.abs(li - di) < EPS)
      then
        pa_center:add_point(wi)
        pa_through:add_point(Ti_new)
      end
    end
  end

  ----------------------------------------------------------
  -- ====== FAMILLE R1 + R2 : droite translatée +R2 ======
  ----------------------------------------------------------
  do
    local newR = R1 + R2
    local Cplus = circle_with_new_radius_keep_center(C1, newR)
    local Lminus = D:collinear_at_distance( R2)
    local pc, pt, m = Cplus:CLP(Lminus, O2)

    for i = 1, m do
      local wi = pc:get(i)
      local ti = pt:get(i)

      local r0 = length_(wi, ti)
      local r  = r0 + R2                     -- même principe que R1-R2 / +R2
      local Ti_new = report_(wi, ti, r)

      local di = D:distance(wi)
      local li = length_(wi, Ti_new)

      if      act_(O1, C1.through, wi, Ti_new)
          and act_(O2, C2.through, wi, Ti_new)
          and (math.abs(li - di) < EPS)
      then
        pa_center:add_point(wi)
        pa_through:add_point(Ti_new)
      end
    end
  end

  return pa_center, pa_through, n
end


-- CCC_gergonne : cercles tangents aux trois cercles (self, C2, C3)
-- Implémente la construction de Gergonne :
--  - lignes des centres de similitude (JJJ et JII),
--  - pôles Xi de ces lignes par rapport aux trois cercles,
--  - points de contact = (wX) ∩ (chaque cercle),
--  - centres solutions = cercles circonscrits à 3 points de contact
-- Retour : (PA.center, PA.through, n)  — chemins + compteur

function circle:CCC_gergonne(C2, C3, opts)
  opts = opts or {}
  local EPS = opts.eps or (tkz and tkz.epsilon) or 1e-6

  local C1 = self
  local O1, T1 = C1.center, C1.through
  local O2, T2 = C2.center, C2.through
  local O3, T3 = C3.center, C3.through

  -- chemins résultat
  local PA_center  = path:new()
  local PA_through = path:new()
  local n = 0

  local function push(o_, t_)
    if o_ and t_ then
      PA_center:add_point(o_)
      PA_through:add_point(t_)
      n = n + 1
    end
  end

  -- --- utilitaires locaux -----------------------------------------------
  local function d_(a,b) return length_(a,b) end

  -- test : un cercle (O,R) est tangent au cercle Ci
  local function tangent_to(O, R, Ci)
    local d = d_(O, Ci.center)
    local r = Ci.radius
    return tkz.approx(math.abs(d - (R + r)), 0, EPS)
        or tkz.approx(math.abs(d - math.abs(R - r)), 0, EPS)
  end

  -- dédoublonnage (même centre)
  local function already_kept(O)
    for i = 1, n do
      if d_(O, PA_center:get(i)) <= 1e-4 then return true end
    end
    return false
  end

  -- points de similitude externes/internes
  local J12 = C1:external_similitude(C2)
  local J13 = C1:external_similitude(C3)
  local J23 = C2:external_similitude(C3)
  local I12 = C1:internal_similitude(C2)
  local I13 = C1:internal_similitude(C3)
  local I23 = C2:internal_similitude(C3)

  -- centre radical (intersection de deux axes radicaux)
  local w = C1:radical_center(C2, C3)

  -- Les 4 droites de Gergonne : 1× JJJ et 3× JII
  local lines = {
    line(J12, J23),   -- JJJ (passe aussi par J13)
    line(J12, I13),   -- JII
    line(J13, I12),   -- JII
    line(J23, I12),   -- JII  (peu importe laquelle des deux "I" on prend)
  }

  -- Pour chaque droite : pôles, points de contact, combinaisons candidates
  for _, L in ipairs(lines) do
    -- Pôles par rapport aux trois cercles
    local X1 = C1:pole(L)
    local X2 = C2:pole(L)
    local X3 = C3:pole(L)

    -- Intersections (wXi) avec chaque cercle = 2 points de contact par cercle
    local a1, a2 = intersection_lc_(w, X1, O1, T1)
    local b1, b2 = intersection_lc_(w, X2, O2, T2)
    local c1, c2 = intersection_lc_(w, X3, O3, T3)

    -- candidates = 4 trios "à parité impaire" sur {1,2}^3
    -- (112), (121), (211) + leurs complémentaires (221), (212), (122)
    -- On les calcule toutes puis on filtre par le test de tangence.
    local triples = {
      {a1,b1,c2}, {a1,b2,c1}, {a2,b1,c1}, {a2,b2,c1},
      {a2,b2,c1}, {a2,b1,c2}, {a1,b2,c2}, {a1,b1,c2},
    }

    for _, tri in ipairs(triples) do
      local A, B, C = tri[1], tri[2], tri[3]
      if A and B and C then
        local O = circum_center_(A, B, C)
        if O then
          local R = d_(O, A)            -- rayon = distance au point de contact choisi
          -- garde seulement si tangent aux trois cercles
          if tangent_to(O, R, C1) and tangent_to(O, R, C2) and tangent_to(O, R, C3) then
            if not already_kept(O) then
              push(O, A)                -- "through" = un des points de contact
            end
          end
        end
      end
    end
  end

  return PA_center, PA_through, n
end


 return circle
