-- File: tkz_elements_function.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- Usage:
--   F.f = fct("sin(x)+x^2")
--   y   = F.f:eval(1.2)
--   PA.c = F.f:path(-2, 5, 200)

fct = {}
fct.__index = fct

local function is_finite(v)
  return type(v) == "number" and v == v and v ~= math.huge and v ~= -math.huge
end

-- compile an expression in x; returns the function, or nil and a message
local function compile_expr(expr, env)
  local chunk = "return function(x) return (" .. expr .. ") end"
  local loader, err = load(chunk, "tkz.fct", "t", env)
  if not loader then
    return nil, err
  end
  local ok, fn_or_err = pcall(loader)
  if not ok or type(fn_or_err) ~= "function" then
    return nil, fn_or_err
  end
  return fn_or_err
end

-- Constructor: an expression in x (string) or a Lua function.
-- The expression is compiled at once, so that an invalid expression is
-- reported at the line of the user code.
function fct:new(expr_or_fn)
  local o = setmetatable({}, self)
  o.expr = nil
  o.fn   = nil
  o.env  = math   -- implicit environment
  if type(expr_or_fn) == "function" then
    o.fn = expr_or_fn
  elseif type(expr_or_fn) == "string" or type(expr_or_fn) == "number" or expr_or_fn == nil then
    o.expr = tostring(expr_or_fn or "0")
    local fn, err = compile_expr(o.expr, o.env)
    if not fn then
      -- level 2: __call is a tail call
      error("tkz-elements: fct: invalid expression '" .. o.expr .. "': " .. tostring(err), 2)
    end
    o.fn = fn
  else
    error("tkz-elements: fct: an expression (string) or a function is expected", 2)
  end
  return o
end

setmetatable(fct, { __call = function(_, ...) return fct:new(...) end })

-- the Lua function f(x)
function fct:compile()
  return self.fn
end

-- Evaluate f(x): nan or an infinite value if f is not defined at x
function fct:eval(x)
  local xx = tonumber(x)
  if not xx then
    error("tkz-elements: fct:eval: a number is expected, got " .. tostring(x), 2)
  end
  local ok, y = pcall(self.fn, xx)
  if not ok then
    return 0 / 0
  end
  return tonumber(y) or 0 / 0
end

function fct:path(xmin, xmax, n)
  xmin = tonumber(xmin) or 0
  xmax = tonumber(xmax) or 1
  n    = tonumber(n) or 100
  if n <= 0 then n = 1 end

  local p = path()
  local f = self.fn
  local step = (xmax - xmin) / n
  local sx = self.sx or 1
  local sy = self.sy or 1

  for i = 0, n do
    local x = xmin + i * step
    local ok, y = pcall(f, x)
    if ok then
      y = tonumber(y)
      if is_finite(y) then
        -- same point as fct:point(x)
        p:add_point(point(sx * x, sy * y))
      end
    end
  end

  return p
end

function fct:set_scale(sx, sy)
  self.sx = tonumber(sx) or 1
  self.sy = tonumber(sy) or 1
  return self
end

-- ------------------------------------------------------------
-- scalar evaluations
-- ------------------------------------------------------------

-- point (x, f(x)) with the scale factors; error if f is not defined at x
local function curve_point(self, x, name, level)
  local y = self:eval(x)
  if not is_finite(y) then
    error("tkz-elements: fct:" .. name .. ": f is not defined at x = " .. tostring(x), level)
  end
  return point((self.sx or 1) * x, (self.sy or 1) * y)
end

function fct:point(x)
  local xx = tonumber(x)
  if not xx then
    error("tkz-elements: fct:point: a number is expected, got " .. tostring(x), 2)
  end
  -- level 3: curve_point -> fct:point -> user code
  local P = curve_point(self, xx, "point", 3)
  return P
end

-- left and right half-tangents at x0 (unit vectors from P)
local function half_tangents(self, x0, h, name)
  h = h or (1e-4 * (1 + math.abs(x0)))

  -- level 4: curve_point -> half_tangents -> method -> user code
  local X0, Y0 = curve_point(self, x0, name, 4):get()
  local Xp, Yp = curve_point(self, x0 + h, name, 4):get()
  local Xm, Ym = curve_point(self, x0 - h, name, 4):get()

  local mr = (Yp - Y0) / (Xp - X0)
  local ml = (Y0 - Ym) / (X0 - Xm)

  local function unit_point(m, dir)
    local d = 1 / math.sqrt(1 + m * m)
    return point(X0 + dir * d, Y0 + dir * m * d)
  end

  return unit_point(ml, -1), unit_point(mr, 1), ml, mr, X0, Y0
end

function fct:half_tangents(x0, h)
  local Pl, Pr, ml, mr = half_tangents(self, x0, h, "half_tangents")
  return Pl, Pr, ml, mr
end

function fct:tangent(x0, h, tol)
  tol = tol or 1e-3
  local Pl, Pr, ml, mr, X0, Y0 = half_tangents(self, x0, h, "tangent")

  if math.abs(mr - ml) > tol then
    return Pl, Pr
  end

  local m = 0.5 * (ml + mr)
  local d = 1 / math.sqrt(1 + m * m)

  local Pr2 = point(X0 + d, Y0 + m * d)
  local Pl2 = point(X0 - d, Y0 - m * d)

  return Pl2, Pr2
end

return fct
