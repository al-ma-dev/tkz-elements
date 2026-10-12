-- File: tkz_elements_line.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
-- -------------------------------------------------------------------------
--                           Lines
-- -------------------------------------------------------------------------
line = {}
line.__index = line
-- Function to create a new line object with two points za and zb.
function line:new(za, zb)
  if type(za) ~= "table" or za.type ~= "point" or type(zb) ~= "table" or zb.type ~= "point" then
    error("tkz-elements: line: two points are expected", 2)
  end
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
  if i == nil then
    return self.pa, self.pb
  elseif i == 1 then
    return self.pa
  elseif i == 2 then
    return self.pb
  else
    return nil
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



-- -- --- legacy API (compat)
function line:position(obj, EPS)
  EPS = EPS or tkz.epsilon

  local mt = getmetatable(obj)

  if mt == point then
    return line_on_(self.pa, self.pb, obj, EPS) and "ON" or "OUT"

  elseif mt == line then
    return line_line_position_(self, obj, EPS)

  elseif mt == circle then
    return line_circle_position_(self, obj, EPS)
  end

  error("Unsupported object in line:position()")
end

------------------------------------------
-- Checks whether a point is on the (infinite) line
function line:in_out(pt, EPS)
  return line_on_(self.pa, self.pb, pt, EPS)
end

-- Keep the alias, but route through the EPS-aware method
line.on_line = line.in_out
line.is_on_line = line.in_out

-- Checks if a point is on the line segment

function line:position_segment(pt, EPS)
  EPS = EPS or tkz.epsilon
  if self:position(pt, EPS) ~= "ON" then
    return "OUT"
  end
  return on_segment_(self.pa, self.pb, pt, EPS) and "ON" or "OUT"
end


function line:on_segment(pt, EPS)
  return on_segment_(self.pa, self.pb, pt, EPS)
end
line.in_out_segment = line.on_segment
line.is_on_segment = line.on_segment

function line:is_equidistant(p)
  return is_equidistant_(self.pa, self.pb, p)
end


-- Classification along the oriented segment [pa,pb], but only if pt is on the line
-- Returns -1 (before pa), 0 (between), +1 (after pb), or nil if pt is not on the line.
function line:where_on_line(pt, EPS)
  EPS = EPS or tkz.epsilon

  if self:position(pt, EPS) ~= "ON" then
    return nil
  end

  local t = self:abscissa_(pt, EPS)
  if t == nil then return nil end

  if t < -EPS    then return "BEFORE"  end
  if t >  1+EPS  then return "AFTER"   end
  return "BETWEEN"
end


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

-- ------------------------------------------------------------
-- line / line relative position
-- returns: "IDENTICAL" | "PARALLEL" | "SECANT"
-- ------------------------------------------------------------
function line_line_position_(L1, L2, EPS)
  EPS = EPS or tkz.epsilon

  if is_parallel_(L1.pa, L1.pb, L2.pa, L2.pb) then
    -- parallel or identical
    if line_on_(L1.pa, L1.pb, L2.pa, EPS) then
      return "IDENTICAL"
    else
      return "PARALLEL"
    end
  else
    return "INTERSECT"
  end
end


-------------------
-- Result -> point
-------------------

-- Returns a point on the line to a distance d from pa (optionally modified by pt).
function line:report(d, pt)
  if not self.length or math.abs(self.length) < tkz.epsilon then
    error("tkz-elements: line:report: the line is degenerate (its two points are identical)", 2)
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

-- Unified harmonic division interface
-- Usage:
--   L:harmonic(k)                --> "both" mode, ratio k (internal, external)
--   L:harmonic("both", k)        --> idem
--   L:harmonic("int",  pt)       --> internal harmonic conjugate of pt
--   L:harmonic("ext",  pt)       --> external harmonic conjugate of pt
function line:harmonic(mode, arg)
  -- if the first argument is not a string, mode = "both"
  if type(mode) ~= "string" then
    arg  = mode
    mode = "both"
  end
  if mode == "int" then mode = "internal" end
  if mode == "ext" then mode = "external" end


  if mode == "internal" then
    return div_harmonic_int_(self.pa, self.pb, arg)

  elseif mode == "external" then
    return div_harmonic_ext_(self.pa, self.pb, arg)

  elseif mode == "both" then
    return div_harmonic_both_(self.pa, self.pb, arg)

  else
    error("tkz-elements: line:harmonic: invalid mode " .. tostring(mode)
      .. " (use \"internal\", \"external\" or \"both\")", 2)
  end
end




-- Returns the point corresponding to the gold ratio on the line
function line:gold_ratio()
  return self.pa + (self.pb - self.pa) * tkz.invphi
end

-- Normalize line (point at one unit distance)
function line:normalize()
  return self.pa + (self.pb - self.pa) / self.length
end


-- Reverse normalizes the line
function line:normalize_inv()
  return normalize_(self.pb, self.pa)
end

function line:collinear_at(pt, k)
  if type(k) == "table" then
    local L = k.length or k.len
    if not L then
       -- no length given: default behaviour
       return collinear_at_(self.pa, self.pb, pt, 1)
     end
     return collinear_at_(self.pa, self.pb, pt, L / self.length)
  else
    if k == nil then
      return collinear_at_(self.pa, self.pb, pt, 1)
    else
      return collinear_at_(self.pa, self.pb, pt, k)
    end
  end
end
line.colinear_at = line.collinear_at

function line:orthogonal_at(pt, k)
  if type(k) == "table" then
     local L = k.length or k.len
     if not L then
       -- no length given: default behaviour
       return orthogonal_at_(self.pa, self.pb, pt, 1)
     end
     return orthogonal_at_(self.pa, self.pb, pt, L / self.length)
  else
    if k == nil then
      return orthogonal_at_(self.pa, self.pb, pt, 1)
    else
      return orthogonal_at_(self.pa, self.pb, pt, k)
    end
  end
end


function line:random()
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
  local v = (self.pb - self.pa) * point(0, 1)
  return line:new(pt - v, pt + v)
end

line.orthogonal_from = line.ortho_from

function line:mediator()
  local m = midpoint_(self.pa, self.pb)
  return line:new(rotation_(m, -math.pi / 2, self.pb), rotation_(m, math.pi / 2, self.pb))
end
line.perpendicular_bisector = line.mediator
line.bisector = line.mediator

function line:swap_line()
  self.pa, self.pb = self.pb, self.pa
  return self
end

-- d : signed distance from the line (positive / negative side)
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
  local pta, ptb
  swap = (swap == "swap")

  pta = self.pa + point(a, 0)
  ptb = self.pb + point(-b, 0)

  local i, j = intersection_cc_(self.pa, pta, self.pb, ptb)
  local an = get_angle_normalize_(self.pa, self.pb, i)
  if an < 0 then
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

  if not u then -- no intersection
    error("tkz-elements: line:s_a: no solution for these data", 2)
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
      error("tkz-elements: line:s_a: the second solution is a degenerate triangle", 2)
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

  if not u then -- no intersection
    error("tkz-elements: line:a_s: no solution for these data", 2)
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
      error("tkz-elements: line:a_s: the second solution is a degenerate triangle", 2)
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
  local angle = tkz.tau / 5

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
-- Result -> square
------------------------------
function line:square(swap)
  return square_from_side_(self.pa, self.pb, swap)
end

-------------------
-- Transformations
-------------------

-- Translation by the vector pb - pa.
-- z.B = L:translation(z.A) ; C.new = L:translation(C.old)
-- z.B, L.new, T.new = L:translation(z.A, L.AB, T.ABC)
function line:translation(...)
  local u = self.pb - self.pa
  return map_objects_(function(p) return translation_(u, p) end, "line:translation", ...)
end

-- Arguments that must be points (projection, affinity): an object
-- would not keep its class (a projected triangle is not a triangle).
local function check_point(p, name)
  if type(p) ~= "table" or p.type ~= "point" then
    local got = type(p) == "table" and tostring(p.type) or type(p)
    error("tkz-elements: " .. name .. ": expected a point, got a " .. got, 3)
  end
end

local function check_line(L, name)
  if type(L) ~= "table" or L.type ~= "line" then
    error("tkz-elements: " .. name .. ": the first argument must be a line", 3)
  end
end

-- Projection onto self, parallel to the line L.
-- z.H = L.AB:projection_ll(L.CD, z.M) ; z.H, z.K = L.AB:projection_ll(L.CD, z.M, z.N)
function line:projection_ll(L, ...)
  check_line(L, "line:projection_ll")
  local n = select("#", ...)
  local t = {}
  for i = 1, n do
    local p = select(i, ...)
    check_point(p, "line:projection_ll")
    t[i] = projection_ll_(self.pa, self.pb, L.pa, L.pb, p)
  end
  return table.unpack(t, 1, n)
end

-- Orthogonal projection onto self.
-- z.H = L.AB:projection(z.M) ; z.H, z.K = L.AB:projection(z.M, z.N)
function line:projection(...)
  local n = select("#", ...)
  local t = {}
  for i = 1, n do
    local p = select(i, ...)
    check_point(p, "line:projection")
    t[i] = projection_(self.pa, self.pb, p)
  end
  return table.unpack(t, 1, n)
end

-- Reflection (axial symmetry) about the line.
-- z.B = L:reflection(z.A) ; T.new = L:reflection(T.ABC)
-- z.B, L.new, T.new = L:reflection(z.A, L.AB, T.ABC)
function line:reflection(...)
  local pa, pb = self.pa, self.pb
  return map_objects_(function(p) return symmetry_axial_(pa, pb, p) end, "line:reflection", ...)
end

-- Affinity with axis self, direction L and ratio k.
-- Each argument may be a point or an object: a line, a triangle, a
-- quadrilateral and a parallelogram keep their class; a square and a
-- rectangle become parallelograms; a circle and an ellipse become an
-- ellipse (or a circle).
-- z.P = L.AB:affinity(L.CD, k, z.M) ; CO.E = L.AB:affinity(L.CD, k, C.OA)
function line:affinity(L, k, ...)
  check_line(L, "line:affinity")
  local f = function(p) return affinity_(self.pa, self.pb, L.pa, L.pb, k, p) end
  local t, n = affine_objects_(f, "line:affinity", false, math.abs(k) <= tkz.epsilon, ...)
  return table.unpack(t, 1, n)
end

function line:path(nb)
  nb = nb or 1
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

-- which in {"all","in","pa","pb","pc"}   -- as triangle:c_lll
-- "all" (default): returns 4 circles (incircle + 3 excircles)
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

    -- Offsets of R at +-r
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

  -- === Case 3: degenerate (at least 2 parallel pairs) -> no unique circle
  return pa_center, pa_through, 0
end
--- === END ===


--===================================================
-- LPP (paths) — Circle tangent to self (a line) and
--               passing through two points a, b
-- Output: PA.center, PA.through, n
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

  -- Data
  local lab  = line:new(a, b)              -- droite (a,b)
  local Cab  = circle:diameter(a, b)       -- circle with diameter [a,b]
  local i    = intersection(lab, self)     -- intersection (ab) ∩ self


  -- 1) One of the points is on the line self (a single circle)
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

  -- 2) (ab) orthogonal to self
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
    push(o, proj)            -- a single solution (center on the perpendicular bisector, radius = proj-a)
    return pa_center, pa_through, n
  end

  -- Degenerate case: i exists and is on the segment [a,b] -> no solution
  if i and lab:on_segment(i) then
    return pa_center, pa_through, n
  end

  -- 4) General case (two solutions in general)
  if i then
    local tpt   = Cab:tangent_from(i).pb          -- point of tangency on Cab
    local circ  = circle:new(i, tpt)               -- circle (i, tpt)
    local x, y  = intersection(self, circ)         -- points of tangency on self

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
-- Circles tangent to self and L, through p
-- Retour : PA.center, PA.through, n
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

  -- ======= SECANT LINES =======
  local s    = intersection_ll_(self.pa, self.pb, L.pa, L.pb)
  local caux = circle:new(through(s, 2))
  local a, b = intersection_lc_(self.pa, self.pb, s, caux.through)
  local c, d = intersection_lc_(L.pa,   L.pb,   s, caux.through)
  local lbi  = tkz.bisector(s, a, c)
  local lbj  = tkz.bisector(s, a, d)

  -- ======= SPECIAL CASES: p on one of the lines =======
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
  -- ======= END OF SPECIAL CASES =======

  -- --- p on a bisector ---
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

  -- --- general case ---
  local u = lbi:reflection(p)
  local v = lbj:reflection(p)
  local sp = self:side_line(p)
  local su = self:side_line(u)
  local pt = (sp == su) and u or v

  local centers, throughs, m = self:c_l_pp(p, pt)
  push_from_paths(centers, throughs, m)

  return pa_center, pa_through, n
  -- ======= END OF SECANT LINES =======
end

-- alias retained
line.c_ll_p = line.LLP



return line
