-- File: tkz_elements_param_function.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- Global class pfct
-- Parametric function: (x(t), y(t)) -> path() or file
-- Requires tkz-elements objects: path(), point(x,y)

pfct = pfct or {}
pfct.__index = pfct

-- ------------------------------------------------------------
-- helpers
-- ------------------------------------------------------------
local function is_finite_(v)
  return type(v) == "number" and (v == v) and v ~= math.huge and v ~= -math.huge
end

-- compile an expression in varname; returns the function, or nil and a message
local function compile_1var_(expr, env, varname)
  env = env or math
  varname = varname or "t"
  expr = tostring(expr or "0")

  local chunk, err = load(
    "return function(" .. varname .. ") return " .. expr .. " end",
    nil, "t", env
  )
  if not chunk then
    return nil, err
  end
  local ok, fn = pcall(chunk)
  if not ok or type(fn) ~= "function" then
    return nil, fn
  end
  return fn
end

local function compile_coord_(e, env, name)
  if type(e) == "function" then
    return e
  end
  if type(e) ~= "string" and type(e) ~= "number" and e ~= nil then
    -- level 3: compile_coord_ -> new -> user code (__call is a tail call)
    error("tkz-elements: pfct: " .. name .. ": an expression (string) or a function is expected", 3)
  end
  local fn, err = compile_1var_(e, env, "t")
  if not fn then
    error("tkz-elements: pfct: invalid expression for " .. name .. " '" .. tostring(e)
      .. "': " .. tostring(err), 3)
  end
  return fn
end

-- ------------------------------------------------------------
-- constructor
-- ------------------------------------------------------------
-- The expressions are compiled at once, so that an invalid expression is
-- reported at the line of the user code.
function pfct:new(exprx, expry, env)
  local o = setmetatable({}, self)
  o.exprx = exprx
  o.expry = expry
  o.env   = env or math
  o.fx = compile_coord_(exprx, o.env, "x(t)")
  o.fy = compile_coord_(expry, o.env, "y(t)")
  o.sx = 1
  o.sy = 1
  o.mapfn = nil
  return o
end

setmetatable(pfct, {
  __call = function(_, exprx, expry, env)
    return pfct:new(exprx, expry, env)
  end
})

-- copy (used by scale and map)
function pfct:copy_()
  local o = setmetatable({}, getmetatable(self))
  for k, v in pairs(self) do
    o[k] = v
  end
  return o
end

-- ------------------------------------------------------------
-- compile (the expressions are compiled by the constructor)
-- ------------------------------------------------------------
function pfct:compile()
  return self
end

function pfct:set_scale(sx, sy)
  self.sx = tonumber(sx) or 1
  self.sy = tonumber(sy) or 1
  return self
end

function pfct:scale(sx, sy)
  local o = self:copy_()
  return o:set_scale(sx, sy)
end

-- ------------------------------------------------------------
-- evaluation: nan if a coordinate is not defined at t
-- ------------------------------------------------------------
function pfct:eval(t)
  local okx, x = pcall(self.fx, t)
  local oky, y = pcall(self.fy, t)

  x = okx and tonumber(x) or 0 / 0
  y = oky and tonumber(y) or 0 / 0

  -- scale
  x = (self.sx or 1) * x
  y = (self.sy or 1) * y

  -- map
  if self.mapfn then
    local ok, X, Y = pcall(self.mapfn, x, y, t, self)
    if ok then
      x, y = tonumber(X) or 0 / 0, tonumber(Y) or 0 / 0
    else
      x, y = 0 / 0, 0 / 0
    end
  end

  return x, y
end

-- ------------------------------------------------------------
-- path
-- ------------------------------------------------------------
function pfct:path(tmin, tmax, n)
  tmin = tonumber(tmin) or 0
  tmax = tonumber(tmax) or 1
  n    = tonumber(n) or 200
  if n <= 0 then n = 1 end

  local p = path()
  local step = (tmax - tmin) / n

  for i = 0, n do
    local t = tmin + i * step
    local x, y = self:eval(t)
    if is_finite_(x) and is_finite_(y) then
      p:add_point(point(x, y))
    end
  end

  return p
end

function pfct:set_map(fn)
  if fn ~= nil and type(fn) ~= "function" then
    error("tkz-elements: pfct:set_map: a function is expected", 2)
  end
  self.mapfn = fn
  return self
end

function pfct:map(fn)
  if fn ~= nil and type(fn) ~= "function" then
    error("tkz-elements: pfct:map: a function is expected", 2)
  end
  local o = self:copy_()
  return o:set_map(fn)
end

-- ------------------------------------------------------------
-- file (TikZ plot file)
-- ------------------------------------------------------------
function pfct:file(tmin, tmax, n, filename)
  local a, b = tonumber(tmin), tonumber(tmax)
  n = tonumber(n) or 200
  if not (a and b) then
    error("tkz-elements: pfct:file: tmin and tmax must be numbers", 2)
  end
  if n <= 0 then
    error("tkz-elements: pfct:file: n must be positive", 2)
  end
  if type(filename) ~= "string" or filename == "" then
    error("tkz-elements: pfct:file: a file name is expected", 2)
  end

  local out, err = io.open(filename, "w")
  if not out then
    error("tkz-elements: pfct:file: cannot open '" .. filename .. "': " .. tostring(err), 2)
  end
  local step = (b - a) / n

  for i = 0, n do
    local t = a + i * step
    local x, y = self:eval(t)
    if is_finite_(x) and is_finite_(y) then
      out:write(checknumber_(x), " ", checknumber_(y), "\n")
    end
  end

  out:close()
  return filename
end

-- convenience: jobname-suffix.table
function pfct:jobfile(tmin, tmax, n, suffix)
  local job = (tex and tex.jobname) or "job"
  local fname = job .. "-" .. tostring(suffix or "param") .. ".table"
  local r = self:file(tmin, tmax, n, fname)
  return r
end

-- ------------------------------------------------------------
-- scalar evaluations
-- ------------------------------------------------------------
function pfct:x(t)
  local x = self:eval(t)
  return x
end

function pfct:y(t)
  local _, y = self:eval(t)
  return y
end

-- ------------------------------------------------------------
-- point on parametric curve
-- ------------------------------------------------------------
function pfct:point(t)
  local x, y = self:eval(t)
  if not (is_finite_(x) and is_finite_(y)) then
    error("tkz-elements: pfct:point: the curve is not defined at t = " .. tostring(t), 2)
  end
  return point(x, y)
end

-- alias
pfct.at = pfct.point
return pfct
