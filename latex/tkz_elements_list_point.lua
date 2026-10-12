-- File: tkz_elements_list_point.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes
-- -------------------------------------------------------------------------
--                           List of points
-- ------------------------------------------------------------------------

list_point = {}
list_point.__index = list_point

-- -------------------------------------------------------------------------
-- Constructor
-- -------------------------------------------------------------------------
function list_point:new(...)
  local o = { ... }
  for i = 1, select("#", ...) do
    if not is_point_(o[i]) then
      -- level 2: __call is a tail call (return cls:new(...))
      error("tkz-elements: list_point: argument #" .. i .. " is not a point", 2)
    end
  end
  o.type = "list_point"
  return setmetatable(o, self)
end

setmetatable(list_point, {
  __call = function(cls, ...)
    return cls:new(...)
  end,
})

-- -------------------------------------------------------------------------
-- Basic accessors
-- -------------------------------------------------------------------------

-- number of stored points
function list_point:len()
  return #self
end

-- get() returns all the points; get(i) returns the i-th point
function list_point:get(i)
  if i == nil then
    return table.unpack(self)
  end
  return self[i]
end

-- unpack points (useful for Lua / TikZ bridges)
function list_point:unpack()
  return table.unpack(self)
end

-- -------------------------------------------------------------------------
-- Mutators
-- -------------------------------------------------------------------------

-- add one point
function list_point:add(p)
  if not is_point_(p) then
    error("tkz-elements: list_point:add: a point is expected", 2)
  end
  self[#self + 1] = p
  return self
end

-- append another points list
function list_point:extend(pl)
  for i, p in ipairs(pl) do
    if not is_point_(p) then
      error("tkz-elements: list_point:extend: element #" .. i .. " is not a point", 2)
    end
    self[#self + 1] = p
  end
  return self
end

-- remove all points
function list_point:clear()
  for i = #self, 1, -1 do
    self[i] = nil
  end
  return self
end

-- -------------------------------------------------------------------------
-- Iteration helpers
-- -------------------------------------------------------------------------

-- apply a function to each point
function list_point:foreach(f)
  for i, p in ipairs(self) do
    f(p, i)
  end
  return self
end

-- map → returns a new points object
function list_point:map(f)
  local res = list_point()
  for i, p in ipairs(self) do
    local q = f(p, i)
    if not is_point_(q) then
      error("tkz-elements: list_point:map: the function must return a point", 2)
    end
    res[#res + 1] = q
  end
  return res
end

-- -------------------------------------------------------------------------
-- Geometric helpers (lightweight)
-- -------------------------------------------------------------------------

-- barycenter of the list
function list_point:barycenter()
  local n = #self
  if n == 0 then return nil end
  local z = self[1]
  for i = 2, n do
    z = z + self[i]
  end
  return z / n
end

-- bounding box (min/max)
function list_point:bbox()
  local n = #self
  if n == 0 then return nil end
  local xmin, xmax = self[1].re, self[1].re
  local ymin, ymax = self[1].im, self[1].im
  for i = 2, n do
    local p = self[i]
    xmin = math.min(xmin, p.re)
    xmax = math.max(xmax, p.re)
    ymin = math.min(ymin, p.im)
    ymax = math.max(ymax, p.im)
  end
  return xmin, ymin, xmax, ymax
end

-- -------------------------------------------------------------------------
-- Output helpers (for tkz / TikZ)
-- -------------------------------------------------------------------------

-- export as path (x1,y1) -- (x2,y2) ... (point:to_tikz is in point.lua)
function list_point:to_tikz_path()
  local t = {}
  for i = 1, self:len() do
    t[#t + 1] = self:get(i):to_tikz()
  end
  return table.concat(t, " -- ")
end

function list_point:as_path()
  local P = path()
  for i = 1, self:len() do
    P:add_point(self:get(i))
  end
  return P
end

return list_point
