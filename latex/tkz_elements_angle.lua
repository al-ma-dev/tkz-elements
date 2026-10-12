-- File: tkz_elements_angle.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

angle = {}
angle.__index = angle

-- angle(ps, pa, pb): angle at the vertex ps, from the ray [ps pa] to the
-- ray [ps pb]. All the values are computed at creation.
function angle:new(ps, pa, pb)
  local pts = { ps, pa, pb }
  for i = 1, 3 do
    local p = pts[i]
    if type(p) ~= "table" or p.type ~= "point" then
      error("tkz-elements: angle: argument #" .. i .. " must be a point", 2)
    end
  end
  if point.abs(pa - ps) <= tkz.epsilon or point.abs(pb - ps) <= tkz.epsilon then
    error("tkz-elements: angle: the angle is undefined (a point is identical to the vertex)", 2)
  end
  local raw  = get_angle_(ps, pa, pb)       -- oriented angle, in (-pi, pi]
  local norm = angle_normalize_(raw)        -- oriented angle, in [0, 2pi)
  local value = math.abs(raw)               -- non-oriented angle, in [0, pi]
  local deg = math.deg(value)

  local obj = {
    type = "angle",
    ps   = ps,      -- vertex
    pa   = pa,      -- point of the first ray
    pb   = pb,      -- point of the second ray
    raw  = raw,     -- oriented angle
    norm = norm,    -- normalized oriented angle
    value = value,  -- non-oriented angle (radians)
    deg = deg       -- non-oriented angle (degrees)
  }
  return setmetatable(obj, self)
end

setmetatable(angle, {
  __call = function(cls, ...)
    return cls:new(...)
  end
})

-- The defining points: vertex, first point, second point
function angle:get()
  return self.ps, self.pa, self.pb
end

-- The angle is direct if raw > 0
function angle:is_direct()
  return self.raw > 0
end

return angle
