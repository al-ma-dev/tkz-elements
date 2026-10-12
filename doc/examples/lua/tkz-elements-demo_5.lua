-- tkz-elements-demo_5.lua : the six circles theorem
-- r (argument of loadfile) = distance from A to the center of the first circle
local r = ...

-- circle tangent to the sides (T.pb T.pa) and (T.pb T.pc) and externally tangent to C
local function newcircle(T, C)
  -- homothety of the triangle from the incenter
  local k  = 1 + C.radius / T.inradius
  local NT = T.incenter:homothety(k, T)

  -- the two sides around the circle we are looking for
  local Lba = line(NT.pb, NT.pa)
  local Lbc = line(NT.pb, NT.pc)

  -- circles tangent to Lba and Lbc and passing through C.center
  local pc, pa = Lba:LLP(Lbc, C.center)

  -- small helper: builds candidate number idx
  local function candidate(idx)
    local w = pc:get(idx)
    if not w then return nil end
    local t = T.bc:projection(w)
    local r_new = w:length(t)              -- radius of the new circle
    local d     = w:length(C.center)       -- distance between centers
    return w, t, r_new, d
  end

  local w1, t1, r1, d1 = candidate(1)
  local w2, t2, r2, d2 = candidate(2)

  local EPS = tkz.epsilon or 1e-6

  -- external tangency test with C
  local function good_ext(d, r_new)
    return math.abs(d - (r_new + C.radius)) < 10 * EPS
  end

  -- choice of the right candidate
  if w1 and good_ext(d1, r1) and not (w2 and good_ext(d2, r2)) then
    return w1, t1
  elseif w2 and good_ext(d2, r2) and not (w1 and good_ext(d1, r1)) then
    return w2, t2
  elseif w1 and w2 then
    -- if both or none pass the test, take by default
    -- the circle with the smaller radius (usually the inner one)
    if r1 <= r2 then
      return w1, t1
    else
      return w2, t2
    end
  else
    -- no usable solution
    return nil, nil
  end
end



z.A = point:new(0, 0)
z.B = point:new(8, 0)
z.C = point:new(2, 6)
T.ABC = triangle:new(z.A, z.B, z.C)
L.bA = T.ABC:bisector()
z.c1 = L.bA:report(r)
z.t1 = T.ABC.ab:projection(z.c1)
C.last = circle:new(z.c1, z.t1)

local vertices = { "A", "B", "C" }
for i = 2, 6 do
  T.used = triangle:new(
    z[vertices[math.fmod(i - 2, 3) + 1]],
    z[vertices[math.fmod(i - 1, 3) + 1]],
    z[vertices[math.fmod(i, 3) + 1]]
  )
  z["c" .. i], z["t" .. i] = newcircle(T.used, C.last)
  C.last = circle:new(z["c" .. i], z["t" .. i])
end