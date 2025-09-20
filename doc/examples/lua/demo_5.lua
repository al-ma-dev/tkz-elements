local r = ...

function newcircle(T, C)
  local NT, L, NC, c, t
  NT = T.incenter:homothety((1 + C.radius / T.inradius), T)
  L = line(NT.pb, NT.pa)
  _, NC = L:c_ll_p(NT.pc, C.center)
  return NC.center, T.bc:projection(NC.center)
end

init_elements()
z.A = point(0, 0)
z.B = point(8, 0)
z.C = point(2, 6)
T.ABC = triangle(z.A, z.B, z.C)
L.bA = T.ABC:bisector()
z.c1 = L.bA:report(r)
z.t1 = T.ABC.ab:projection(z.c1)
C.last = circle(z.c1, z.t1)

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