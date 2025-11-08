-- File: tkz_elements_line.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
-- -------------------------------------------------------------------------
--                           Lines
-- -------------------------------------------------------------------------
line = {}
line.__index = line
-- Function to create a new line object with two points za and zb.
function line:new(za, zb)
  local type = "line"
  local mid = (za + zb) / 2
  local north_pa = rotation_(za, math.pi / 2, zb)
  local south_pa = rotation_(za, -math.pi / 2, zb)
  local north_pb = rotation_(zb, -math.pi / 2, za)
  local south_pb = rotation_(zb, math.pi / 2, za)
  local west = rotation_(za, math.pi / 2, north_pa)
  local east = rotation_(zb, math.pi / 2, south_pb)
  local slope = angle_normalize_(point.arg(zb - za))
  local length = point.mod(zb - za)
  local vec = vector:new(za, zb)
  local li = {
    pa = za,
    pb = zb,
    north_pa = north_pa,
    south_pa = south_pa,
    west = west,
    east = east,
    north_pb = north_pb,
    south_pb = south_pb,
    slope = slope,
    mid = mid,
    type = type,
    vec = vec,
    length = length,
  }
  setmetatable(li, self)
  return li
end

setmetatable(line, {
  __call = function(cls, ...)
    return cls:new(...)
  end,
})

function line:get(i)
  if i == 1 then
    return self.pa
  elseif i == 2 then
    return self.pb
  else
    return self.pa, self.pb
  end
end

-------------------
-- Result -> real
-------------------

-- Calculate the distance between a point and a line
function line:distance(pt)
  return distance_(self.pa, self.pb, pt)
end

function line:side_line(pt)
  local det = (self.pa - self.pb) ^ (self.pa - pt)
  if math.abs(det) < tkz.epsilon then return 0 end
return (det > 0) and 1 or -1
end
-------------------
-- Result -> boolean
-------------------

-- Checks whether a point is on the line (outside the segment)
function line:in_out(pt)
  return line_in_out_(self.pa, self.pb, pt)
end
line.on_line = line.in_out

-- Checks if a point is on the line segment
function line:in_out_segment(pt)
  return point.mod(pt - self.pa) + point.mod(pt - self.pb) - point.mod(self.pb - self.pa) <= tkz.epsilon
end
line.on_segment = line.in_out_segment

function line:is_parallel(L)
  local pa, pb = self:get()
  local pc, pd = L:get()
  return is_parallel_(pa, pb, pc, pd)
end

function line:is_orthogonal(L)
  local pa, pb = self:get()
  local pc, pd = L:get()
  return is_orthogonal_(pa, pb, pc, pd)
end

function line:is_equidistant(p)
  return is_equidistant_(self.pa, self.pb, p)
end

-------------------
-- Result -> point
-------------------

-- Returns a point on the line to a distance d from pa (optionally modified by pt).
function line:report(d, pt)
  if not self.length or math.abs(self.length) < tkz.epsilon then
    tex.error("self.length must be non-zero")
  end
  local t = d / self.length
  local result = barycenter_({ self.pa, 1 - t }, { self.pb, t })
  if pt then
    return result + pt - self.pa
  else
    return result
  end
end
line.point_at_distance = line.report

-- Calculates the barycenter of two points on the line with weights ka and kb
function line:barycenter(ka, kb)
  return barycenter_({ self.pa, ka }, { self.pb, kb })
end

function line:point(t)
  return barycenter_({ self.pa, 1 - t }, { self.pb, t })
end

function line:midpoint()
  return (self.pa + self.pb) / 2
end

function line:harmonic_int(pt)
  return div_harmonic_int_(self.pa, self.pb, pt)
end

function line:harmonic_ext(pt)
  return div_harmonic_ext_(self.pa, self.pb, pt)
end

function line:harmonic_both(k)
  return div_harmonic_both_(self.pa, self.pb, k)
end

-- Returns the point corresponding to the gold ratio on the line
function line:gold_ratio()
  return self.pa + (self.pb - self.pa) * tkz.invphi
end

-- Normalize line (point at one unit distance)
function line:normalize()
  return self.pa + (self.pb - self.pa) / point.mod(self.pb - self.pa)
end

-- Reverse normalizes the line
function line:normalize_inv()
  return normalize_(self.pb, self.pa)
end

function line:collinear_at(pt, k)
  if k == nil then
    return collinear_at_(self.pa, self.pb, pt, 1)
  else
    return collinear_at_(self.pa, self.pb, pt, k)
  end
end
line.colinear_at = line.collinear_at

function line:orthogonal_at(pt, k)
  if k == nil then
    return orthogonal_at_(self.pa, self.pb, pt, 1)
  else
    return orthogonal_at_(self.pa, self.pb, pt, k)
  end
end


function line:random()
  math.randomseed(os.time())
  return self:point(math.random())
end

-------------------
-- Result -> line
-------------------

function line:ll_from(pt)
  return line:new(pt, pt + self.pb - self.pa)
end

line.parallel_from = line.ll_from

function line:ortho_from(pt)
  return line:new(pt + (self.pb - self.pa) * point(0, -1), pt + (self.pb - self.pa) * point(0, 1))
end

line.orthogonal_from = line.ortho_from

function line:mediator()
  local m = midpoint_(self.pa, self.pb)
  return line:new(rotation_(m, -math.pi / 2, self.pb), rotation_(m, math.pi / 2, self.pb))
end
line.perpendicular_bisector = line.mediator

function line:swap_line()
  self.pa, self.pb = self.pb, self.pa -- Inverse les points pa et pb
  return line:new(self.pa, self.pb) -- Crée et retourne la ligne avec les points permutés
end

function line:collinear_at_distance(d)
 local a, b = self.pa, self.pb
 local u = orthogonal_at_(a, b, a, d / self.length)
 local v = collinear_at_(a, b, u)
 return line:new(u, v)
end


function line:equidistant_with(L2, choice)
  return lines_equidistant_(self, L2, choice)
end
-------------------
-- Result -> circle
-------------------

function line:circle(swap)
  swap = (swap == "swap")
  if swap then
    return circle:new(self.pb, self.pa)
  else
    return circle:new(self.pa, self.pb)
  end
end

function line:circle_swap()
  return circle:new(self.pb, self.pa)
end

function line:diameter()
  local c = midpoint_(self.pa, self.pb)
  return circle:new(c, self.pb)
end

function line:apollonius(k)
  local z1, z2, c
  z1 = barycenter_({ self.pa, 1 }, { self.pb, k })
  z2 = barycenter_({ self.pa, 1 }, { self.pb, -k })
  c = midpoint_(z1, z2)
  return circle:new(c, z2)
end


----------------------
-- Result -> triangle
----------------------
function line:equilateral(swap)
  swap = (swap == "swap")
  if swap then
    return triangle:new(self.pa, self.pb, rotation_(self.pa, -math.pi / 3, self.pb))
  else
    return triangle:new(self.pa, self.pb, rotation_(self.pa, math.pi / 3, self.pb))
  end
end

function line:isosceles(a, swap)
  swap = (swap == "swap")
  local c1, c2, pta, ptb, pt1, pt2
  -- Create circles with radius 'a' centered on self.pa and self.pb
  c1 = circle:radius(self.pa, a)
  c2 = circle:radius(self.pb, a)
  -- Calculating the points of intersection of the two circles
  pta, ptb = intersection_cc(c1, c2)
  if get_angle_normalize_(self.pa, self.pb, pta) < get_angle_normalize_(self.pa, self.pb, ptb) then
    pt1 = pta
    pt2 = ptb
  else
    pt1 = ptb
    pt2 = pta
  end
  if swap then
    return triangle:new(self.pa, self.pb, pt2)
  else
    return triangle:new(self.pa, self.pb, pt1)
  end
end

function line:school(swap)
  local pta, ptb, pt
  swap = (swap == "swap")

  if swap then
    pta = rotation_(self.pa, math.pi / 3, self.pb)
    ptb = rotation_(self.pb, -math.pi / 6, self.pa)
  else
    pta = rotation_(self.pa, math.pi / 6, self.pb)
    ptb = rotation_(self.pb, -math.pi / 3, self.pa)
  end

  pt = intersection_ll_(self.pa, pta, self.pb, ptb)
  return triangle:new(self.pa, self.pb, pt)
end

function line:half(swap)
  local x, pt
  x = midpoint_(self.pa, self.pb)
  swap = (swap == "swap")

  if swap then
    pt = rotation_(self.pa, math.pi / 2, x)
  else
    pt = rotation_(self.pb, -math.pi / 2, x)
  end

  return triangle:new(self.pa, self.pb, pt)
end

function line:two_angles(alpha, beta, swap)
  local pta, ptb, pt
  swap = (swap == "swap")

  if swap then
    pta = rotation_(self.pa, -alpha, self.pb)
    ptb = rotation_(self.pb, beta, self.pa)
  else
    pta = rotation_(self.pa, alpha, self.pb)
    ptb = rotation_(self.pb, -beta, self.pa)
  end

  pt = intersection_ll_(self.pa, pta, self.pb, ptb)
  return triangle:new(self.pa, self.pb, pt)
end

line.asa = line.two_angles
line.a_a = line.two_angles

function line:s_s(a, b, swap)
  local pta, ptb, i, j
  swap = (swap == "swap")

  pta = self.pa + point(a, 0)
  ptb = self.pb + point(-b, 0)

  local i, j = intersection_cc_(self.pa, pta, self.pb, ptb)
  local a = get_angle_normalize_(self.pa, self.pb, i)
  if a < 0 then
    i, j = j, i
  end
  if swap then
    return triangle:new(self.pa, self.pb, j)
  else
    return triangle:new(self.pa, self.pb, i)
  end
end
line.sss = line.s_s

function line:s_a(a, phi, swap)
  local x = rotation_(self.pb, -phi, self.pa)
  local y = report_(self.pa, self.pb, a)
  local u, v = intersection_lc_(self.pb, x, self.pa, y)

  if not u then -- Pas d'intersection
    tex.error("Invalid configuration. No solution found.")
    return nil
  end

  if point.mod(u - v) < tkz.epsilon then -- Solution unique
    return triangle:new(self.pa, self.pb, u)
  end

  -- Deux solutions
  if length_(self.pb, u) < length_(self.pb, v) then
    u, v = v, u
  end

  if swap == "swap" then
    if point.mod(v - self.pb) < tkz.epsilon then
      tex.error("Degenerated triangle.")
      return nil
    else
      return triangle:new(self.pa, self.pb, v)
    end
  else
    return triangle:new(self.pa, self.pb, u)
  end
end
line.ssa = line.s_a

function line:a_s(a, phi, swap)
  local x = rotation_(self.pa, phi, self.pb)
  local y = report_(self.pb, self.pa, a)
  local u, v = intersection_lc_(self.pa, x, self.pb, y)

  if not u then -- No intersection
    tex.error("Invalid configuration. No solution found.")
    return nil
  end

  if point.mod(u - v) < tkz.epsilon then -- One-stop solution
    return triangle:new(self.pa, self.pb, u)
  end

  -- Two solutions
  if length_(self.pa, u) < length_(self.pa, v) then
    u, v = v, u
  end

  if swap == "swap" then
    if point.mod(v - self.pa) < tkz.epsilon then
      tex.error("Degenerated triangle.")
      return nil
    else
      return triangle:new(self.pa, self.pb, v)
    end
  else
    return triangle:new(self.pa, self.pb, u)
  end
end

function line:sa_(a, phi, swap)
  local x, pt
  swap = (swap == "swap")
  x = report_(self.pa, self.pb, a)
  if swap then
    pt = rotation_(self.pa, -phi, x)
  else
    pt = rotation_(self.pa, phi, x)
  end
  return triangle:new(self.pa, self.pb, pt)
end

function line:_as(a, phi, swap)
  local x, pt
  swap = (swap == "swap")
  x = report_(self.pb, self.pa, a)
  if swap then
    pt = rotation_(self.pb, phi, x)
  else
    pt = rotation_(self.pb, -phi, x)
  end
  return triangle:new(self.pa, self.pb, pt)
end

--------------------------
-- Result --> sacred triangles
--------------------------

function line:gold(swap)
  local pt
  swap = (swap == "swap")

  if swap then
    pt = rotation_(self.pa, -math.pi / 2, self.pb)
  else
    pt = rotation_(self.pa, math.pi / 2, self.pb)
  end

  return triangle:new(self.pa, self.pb, self.pa + (pt - self.pa) * tkz.invphi)
end

function line:golden(swap)
  local pta, ptb, pt
  swap = (swap == "swap")
  local angle = 2 * math.pi / 5

  if swap then
    pta = rotation_(self.pa, -angle, self.pb)
    ptb = rotation_(self.pb, angle, self.pa)
  else
    pta = rotation_(self.pa, angle, self.pb)
    ptb = rotation_(self.pb, -angle, self.pa)
  end

  pt = intersection_ll_(self.pa, pta, self.pb, ptb)
  return triangle:new(self.pa, self.pb, pt)
end
line.sublime = line.golden
line.euclid = line.golden

function line:divine(swap)
  swap = (swap == "swap")
  local angle = math.pi / 5
  local pta = rotation_(self.pa, swap and -angle or angle, self.pb)
  local ptb = rotation_(self.pb, swap and angle or -angle, self.pa)
  local pt = intersection_ll_(self.pa, pta, self.pb, ptb)
  return triangle:new(self.pa, self.pb, pt)
end

function line:egyptian(swap)
  swap = (swap == "swap")
  local n = rotation_(self.pb, swap and math.pi / 2 or -math.pi / 2, self.pa)
  local pt = self.pb + (n - self.pb) / point.mod(n - self.pb) * self.length * 0.75
  return triangle:new(self.pa, self.pb, pt)
end

line.pythagoras = line.egyptian
line.isis = line.egyptian

line.golden_gnomon = line.divine

------------------------------
-- Résultat -> carré
------------------------------
function line:square(swap)
  swap = (swap == "swap")
  if swap then
    return square:side(self.pa, self.pb, "swap")
  else
    return square:side(self.pa, self.pb)
  end
end

-------------------
-- Transformations
-------------------

-- Translation of a given point in the direction of the line (pb - pa)
function line:translation_pt(pt)
  return translation_(self.pb - self.pa, pt)
end

-- Translation of a circle object in line direction
function line:translation_C(obj)
  local pa = obj.center
  local pb = obj.through
  local x, y = set_translation_(self.pb - self.pa, pa, pb)
  return circle:new(x, y)
end

--Translation of a triangle object in line direction
function line:translation_T(obj)
  local pa = obj.pa
  local pb = obj.pb
  local pc = obj.pc
  local x, y, z = set_translation_(self.pb - self.pa, pa, pb, pc)
  return triangle:new(x, y, z)
end

-- Translation of a line object along the line direction
function line:translation_L(obj)
  local pa = obj.pa
  local pb = obj.pb
  local x, y = set_translation_(self.pb - self.pa, pa, pb)
  return line:new(x, y)
end

-- General function for translating one or more objects
function line:translation(...)
  local tp = table.pack(...) -- Groups all arguments in a table
  local obj = tp[1] -- The first object is retrieved
  local nb = tp.n -- Number of objects to be processed

  --If only one object is passed
  if nb == 1 then
    if obj.type == "point" then
      return translation_(self.pb - self.pa, obj)
    elseif obj.type == "line" then
      return self:translation_L(obj)
    elseif obj.type == "triangle" then
      return self:translation_T(obj)
    elseif obj.type == "circle" then
      return self:translation_C(obj)
    else
      tex.error("Unsupported object type for translation")
    end
  else
    -- If several objects are passed, they are translated one by one
    local t = {}
    for i = 1, nb do
      -- Translation of each object with the translation vector
      table.insert(t, translation_(self.pb - self.pa, tp[i]))
    end
    return table.unpack(t) --Returns translated objects
  end
end

-- Function for translating an object using line direction
function line:set_translation(...)
  return set_translation_(self.pb - self.pa, ...)
end

function line:projection_ll(...)
  local tp = table.pack(...)
  local c, d = tp[1].pa, tp[1].pb -- c and d are the two points that define the projection line
  local obj = tp[2] -- The object to be projected
  local nb = tp.n -- Number of objects passed as parameters

  if nb == 2 then
    --Projection of line onto object
    return projection_ll_(self.pa, self.pb, c, d, obj)
  else
    local t = {}
    for i = 2, tp.n do
      -- Projection of each object on the line
      table.insert(t, projection_ll_(self.pa, self.pb, c, d, tp[i]))
    end
    return table.unpack(t) -- Retourne les résultats de la projection pour tous les objets
  end
end

function line:set_projection_ll(...)
  local tp = table.pack(...)
  local c, d = tp[1].pa, tp[1].pb -- c and d are the two points defining the projection line
  local t = {}

  for i = 2, tp.n do
    -- Projection de chaque objet sur la ligne
    table.insert(t, projection_ll_(self.pa, self.pb, c, d, tp[i]))
  end
  return table.unpack(t) -- Returns projection results for all objects
end

function line:projection(...)
  local tp = table.pack(...) -- Groups arguments in a table
  local obj = tp[1] -- Retrieves the first object
  local nb = tp.n -- Number of objects to be processed

  -- If only one object is passed
  if nb == 1 then
    return projection_(self.pa, self.pb, obj) -- Projects object onto line
  else
    local t = {}
    -- If several objects are passed, each is projected onto the line
    for i = 1, tp.n do
      table.insert(t, projection_(self.pa, self.pb, tp[i])) -- Projection of each object
    end
    return table.unpack(t) --Return projections
  end
end

function line:set_projection(...)
  local tp = table.pack(...) -- Groups arguments in a table
  local t = {}
  -- Projects each object onto the line
  for i = 1, tp.n do
    table.insert(t, projection_(self.pa, self.pb, tp[i])) --Projection of each object
  end
  return table.unpack(t) -- Return projections
end

function line:symmetry_axial_L(obj)
  local pa = obj.pa
  local pb = obj.pb
  local x, y = self:set_reflection(pa, pb)
  return line:new(x, y)
end

function line:symmetry_axial_T(obj)
  local pa = obj.pa
  local pb = obj.pb
  local pc = obj.pc
  local x, y, z = self:set_reflection(pa, pb, pc)
  return triangle:new(x, y, z)
end

function line:symmetry_axial_C(obj)
  local pa = obj.center
  local pb = obj.through
  local x, y = self:set_reflection(pa, pb)
  return circle:new(x, y)
end

function line:reflection(...)
  local tp = table.pack(...)
  local obj = tp[1]
  local nb = tp.n

  if nb == 1 then
    if obj.type == "point" then
      return symmetry_axial_(self.pa, self.pb, obj)
    elseif obj.type == "line" then
      return self:symmetry_axial_L(obj)
    elseif obj.type == "triangle" then
      return self:symmetry_axial_T(obj)
    elseif obj.type == "circle" then
      return self:symmetry_axial_C(obj)
    else
      tex.error("Unsupported object type for reflection")
    end
  else
    local t = {}
    for i = 1, tp.n do
      table.insert(t, symmetry_axial_(self.pa, self.pb, tp[i]))
    end
    return table.unpack(t)
  end
end

function line:set_reflection(...)
  return set_symmetry_axial_(self.pb, self.pa, ...)
end

function line:affinity(...)
  local tp = table.pack(...)
  local c, d = tp[1].pa, tp[1].pb
  local k = tp[2]
  local obj = tp[3]
  local nb = tp.n
  if nb == 3 then
    return affinity_(self.pa, self.pb, c, d, k, obj)
  else
    local t = {}
    for i = 3, tp.n do
      table.insert(t, affinity_(self.pa, self.pb, c, d, k, tp[i]))
    end
    return table.unpack(t)
  end
end

function line:set_affinity(...)
  local tp = table.pack(...) -- Gather all arguments passed to table tp
  local c, d = tp[1].pa, tp[1].pb --Extract the two points of the reference line (tp[1])
  local k = tp[2] -- The affinity coefficient
  local t = {}

  -- Apply affinity for each object passed in parameter (from 3rd object upwards)
  for i = 3, tp.n do
    table.insert(t, affinity_(self.pa, self.pb, c, d, k, tp[i])) --Apply affinity to each object
  end

  return table.unpack(t) --Returns transformed objects
end

function line:path(nb)
  nb = nb or 20
  local list = {}
  for i = 0, nb do
    local t = i / nb
    local P = self:point(t)
    table.insert(list, "(" .. checknumber_(P.re) .. "," .. checknumber_(P.im) .. ")")
  end
  return path:new(list)
end

---  =====   CONTACT PROBLEM APOLLONIUS ===---
--- LLL   LLP   LPP

-- which ∈ {"all","in","pa","pb","pc"}   -- comme triangle:c_lll
-- "all" (défaut) : renvoie 4 cercles (inscrit + 3 exinscrits)
-- line: LLL — circles tangent to three lines (self, L1, L2)
-- Return value (like CLL): centers_path, through_path, n
function line:LLL(L1, L2, which)
  -- --- outputs (always returned)
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  -- --- unpack
  local a0, b0 = self:get()
  local a1, b1 = L1:get()
  local a2, b2 = L2:get()

  -- --- small helper: push one solution (center o, through = tangency on self)
  local function push(o)
    if not o then return end
    local t = projection_(a0, b0, o)  -- tangency on the receiver line (self)
    if t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  -- --- parallel flags
  local p01 = is_parallel_(a0, b0, a1, b1)
  local p12 = is_parallel_(a1, b1, a2, b2)
  local p20 = is_parallel_(a2, b2, a0, b0)
  local npar = (p01 and 1 or 0) + (p12 and 1 or 0) + (p20 and 1 or 0)

  -- === Case 1: triangle (no parallel pairs) -> incircle + 3 excircles
  if npar == 0 then
    -- A = L1 ∩ L2 ; B = L2 ∩ self ; C = self ∩ L1
    local A = intersection_ll_(a1, b1, a2, b2)
    local B = intersection_ll_(a2, b2, a0, b0)
    local C = intersection_ll_(a0, b0, a1, b1)
    if not (A and B and C) then
      return pa_center, pa_through, 0
    end

    local T = triangle:new(A, B, C)

    -- which = "all" | "in" | "pa" | "pb" | "pc" | 1..4
    local sel = which or "all"
    local function normalize_selector(w)
      if type(w) == "number" then
        return (w == 1 and "in")
            or (w == 2 and "pa")
            or (w == 3 and "pb")
            or (w == 4 and "pc")
            or "all"
      end
      return w
    end
    sel = normalize_selector(sel)

    if sel == "all" then
      local c1, c2, c3, c4 = T:c_lll("all")  -- four circles
      local list = {c1, c2, c3, c4}
      for _, Ck in ipairs(list) do
        if Ck and Ck.center then push(Ck.center) end
      end
      return pa_center, pa_through, n
    elseif sel == "in" or sel == "pa" or sel == "pb" or sel == "pc" then
      local Ck = T:c_lll(sel)  -- one circle
      if Ck and Ck.center then push(Ck.center) end
      return pa_center, pa_through, n
    else
      return pa_center, pa_through, 0
    end
  end

  -- === Case 2: exactly one parallel pair -> 2 solutions
  if npar == 1 then
    -- Identify the parallel pair (P // Q) and the remaining line R
    local P, Q, R
    if p01 then P, Q, R = self, L1, L2
    elseif p12 then P, Q, R = L1, L2, self
    else              P, Q, R = L2, self, L1
    end

    -- distance between the parallels (P, Q)
    local D = distance_(P.pa, P.pb, Q.pa)
    if not D or D <= tkz.epsilon then
      return pa_center, pa_through, 0
    end
    local r = 0.5 * D

    -- Midline M between P and Q (same direction as P/Q)
    local h     = Q.pa
    local hproj = projection_(P.pa, P.pb, h)
    if not hproj then
      return pa_center, pa_through, 0
    end
    local mpt   = (h + hproj) * 0.5
    local dir   = P.pb - P.pa
    local M     = line:new(mpt, mpt + dir)

    -- Offsets of R at ±r
    local Rp = R:collinear_at_distance( r)
    local Rm = R:collinear_at_distance(-r)

    -- Centers are intersections of M with these offsets
    local W1 = Rp and intersection_ll_(M.pa, M.pb, Rp.pa, Rp.pb) or nil
    local W2 = Rm and intersection_ll_(M.pa, M.pb, Rm.pa, Rm.pb) or nil

    -- Always push with respect to self (the receiver), not P/Q/R arbitrarily
    push(W1)
    push(W2)

    return pa_center, pa_through, n
  end

  -- === Case 3: degenerate (≥ 2 parallel pairs) -> no unique circle
  return pa_center, pa_through, 0
end
--- === END ===


--===================================================
-- LPP (paths) — Circle tangent to self (a line) and
--               passing through two points a, b
-- Sortie unifiée : PA.center, PA.through, n
--===================================================
function line:LPP(a, b)
  -- Paths de sortie
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

  -- Données de base
  local lab  = line:new(a, b)              -- droite (a,b)
  local Cab  = circle:diameter(a, b)       -- cercle de diamètre [a,b]
  local i    = intersection(lab, self)     -- intersection (ab) ∩ self


  -- 1) Un des points est sur la droite self (un seul cercle)
  --    NB: on conserve ta logique avec in_out() telle quelle.
  if (self:on_line(a)) and (not self:on_line(b) )then
  local lmed = lab:mediator()
  local laperp = self:ortho_from(a)
  local o = intersection(lmed, laperp)
    push(o, a)
    return pa_center, pa_through, n
  end
  if (self:on_line(b)) and (not self:on_line(a) )then
 local lmed = lab:mediator()
 local laperp = self:ortho_from(b)
 local o = intersection(lmed, laperp)
    push(o, b)
    return pa_center, pa_through, n
  end

  -- 2) (ab) ⟂ self
  if self:is_orthogonal(lab) and i then
    local m  = lab.mid
    local r  = length_(m, i)
    local t1 = lab:isosceles(r).pc
    local t2 = lab:isosceles(r, "swap").pc
    push(t1, a)
    push(t2, a)
    return pa_center, pa_through, n
  end

  -- 3) (ab) ∥ self
  if lab:is_parallel(self) then
    local mid  = midpoint_(a, b)
    local proj = self:projection(mid)
    local o    = circum_center_(a, b, proj)
    push(o, proj)            -- une seule solution (centre sur médiatrice, rayon = proj→a)
    return pa_center, pa_through, n
  end

  -- Cas dégénéré : i existe et est sur le segment [a,b] → aucune solution
  if i and lab:in_out_segment(i) then
    return pa_center, pa_through, n
  end

  -- 4) Cas général (deux solutions en général)
  if i then
    local tpt   = Cab:tangent_from(i).pb          -- point de tangence sur Cab
    local circ  = circle:new(i, tpt)               -- cercle (i, tpt)
    local x, y  = intersection(self, circ)         -- tangences sur self

    if x then
      local o1 = intersection(self:ortho_from(x), lab:mediator())
      push(o1, x)
    end
    if y and (not x or (x.identity and not x:identity(y)) or (not x.identity and (x ~= y))) then
      local o2 = intersection(self:ortho_from(y), lab:mediator())
      push(o2, y)
    end
  end

 return pa_center, pa_through, n
end

line.c_l_pp = line.LPP

--========= END of LPP =====================
--=============================================

--===================== c_LLP (paths) =================--
-- Cercles tangents à self et L, passant par p
-- Retour : PA.center, PA.through, n
-- (port fidèle de ton ancien code, sans changer l'algo)
--=====================================================--
function line:LLP(L, p)
  -- Conteneurs de solutions (paths)
  local pa_center  = path:new()
  local pa_through = path:new()
  local n = 0

  -- ===== Helpers, version CLP =====
  local function push(o, t)
    if o and t then
      pa_center:add_point(o)
      pa_through:add_point(t)
      n = n + 1
    end
  end

  local function push_from_paths(centers, throughs, m)
    if (not centers) or (not throughs) or (not m) then return end
    for i = 1, m do
      push(centers:get(i), throughs:get(i))
    end
  end
  -- =================================

  -- ======= CAS PARALLÈLES =======
  if self:is_parallel(L) then
    local x  = self.pa
    local d  = L:distance(x)
    local ds = self:distance(p)
    local dl = L:distance(p)

    if ds + dl > d then
      return pa_center, pa_through, 0
    elseif utils.almost_equal(ds, 0) then
      local q = L:projection(p)
      push(midpoint_(p, q), p)
      return pa_center, pa_through, n
    elseif utils.almost_equal(dl, 0) then
      local q = self:projection(p)
      push(midpoint_(p, q), p)
      return pa_center, pa_through, n
    else
      local u  = self:projection(p)
      local v  = L:projection(p)
      local ma, mb = mediator_(u, v)
      local ci = circle:new(through(p, d / 2))
      local c1, c2 = intersection_lc_(ma, mb, p, ci.through)
      push(c1, p)
      push(c2, p)
      return pa_center, pa_through, n
    end
  end
  -- ======= FIN PARALLÈLES =======

  -- ======= CAS SÉCANTS =======
  local s    = intersection_ll_(self.pa, self.pb, L.pa, L.pb)
  local caux = circle:new(through(s, 2))
  local a, b = intersection_lc_(self.pa, self.pb, s, caux.through)
  local c, d = intersection_lc_(L.pa,   L.pb,   s, caux.through)
  local lbi  = bisector(s, a, c)
  local lbj  = bisector(s, a, d)

  -- ======= CAS SPÉCIAUX : p sur une des droites =======
  if self:on_line(p) or L:on_line(p) then
    if self:on_line(p) and L:on_line(p) then
      return pa_center, pa_through, 0
    end

    local N = self:on_line(p) and self:ortho_from(p) or L:ortho_from(p)

    local O1 = intersection_ll_(N.pa, N.pb, lbi.pa, lbi.pb)
    local O2 = intersection_ll_(N.pa, N.pb, lbj.pa, lbj.pb)

    if O1 and O2 then
      if point.abs(O2 - O1) > tkz.epsilon then
        push(O1, p)
        push(O2, p)
      else
        push(O1, p)
      end
      return pa_center, pa_through, n
    elseif O1 then
      push(O1, p)
      return pa_center, pa_through, n
    elseif O2 then
      push(O2, p)
      return pa_center, pa_through, n
    else
      return pa_center, pa_through, n
    end
  end
  -- ======= FIN CAS SPÉCIAUX =======

  -- --- p sur une bissectrice ---
  if lbi:on_line(p) or lbj:on_line(p) then
    local bis = lbi:on_line(p) and lbi or lbj
    local lp  = bis:ortho_from(p)
    local pi  = intersection_ll_(lp.pa, lp.pb, self.pa, self.pb)

    local t1, t2 = intersection_lc_(self.pa, self.pb, pi, p)
    if t1 then
      local x = self:ortho_from(t1).pb
      local u = intersection_ll_(x, t1, s, p)
      if u then push(u, t1) end
    end
    if t2 then
      local y = self:ortho_from(t2).pb
      local v = intersection_ll_(y, t2, s, p)
      if v then push(v, t2) end
    end
    return pa_center, pa_through, n
  end

  -- --- cas général ---
  local u = lbi:reflection(p)
  local v = lbj:reflection(p)
  local sp = self:side_line(p)
  local su = self:side_line(u)
  local pt = (sp == su) and u or v

  local centers, throughs, m = self:c_l_pp(p, pt)
  push_from_paths(centers, throughs, m)

  return pa_center, pa_through, n
  -- ======= FIN SÉCANTS =======
end

-- alias conservé
line.c_ll_p = line.LLP



return line
