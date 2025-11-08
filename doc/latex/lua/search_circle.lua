-- search_circle.lua
local r = ...

function newcircle(T, C)
	local NT, NC, c, t
	NT = T.incenter:homothety((1 + C.radius / T.inradius), T)
	local Lba = line(NT.pb, NT.pa)
	local Lbc = line(NT.pb, NT.pc)
	local pc, pa = Lba:LLP(Lbc, C.center)
	local w = pc:get(2)
	return w, T.bc:projection(w)
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
