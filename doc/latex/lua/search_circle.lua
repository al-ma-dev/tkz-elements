-- search_circle.lua
local r = ...

function newcircle(T, C)
	-- homothétie du triangle à partir de l’incentre
	local k  = 1 + C.radius / T.inradius
	local NT = T.incenter:homothety(k, T)

	-- les deux côtés qui encadrent le cercle cherché
	local Lba = line(NT.pb, NT.pa)
	local Lbc = line(NT.pb, NT.pc)

	-- cercles tangent à Lba et Lbc et passant par C.center
	local pc, pa = Lba:LLP(Lbc, C.center)

	-- petite fonction utilitaire : construit le candidat n
	local function candidate(idx)
		local w = pc:get(idx)
		if not w then return nil end
		local t = T.bc:projection(w)
		local r_new = w:length(t)              -- rayon du nouveau cercle
		local d     = w:length(C.center)       -- distance entre centres
		return w, t, r_new, d
	end

	local w1, t1, r1, d1 = candidate(1)
	local w2, t2, r2, d2 = candidate(2)

	local EPS = tkz.epsilon or 1e-6

	-- test de tangence externe avec C
	local function good_ext(d, r_new)
		return math.abs(d - (r_new + C.radius)) < 10 * EPS
	end

	-- choix du bon candidat
	if w1 and good_ext(d1, r1) and not (w2 and good_ext(d2, r2)) then
		return w1, t1
	elseif w2 and good_ext(d2, r2) and not (w1 and good_ext(d1, r1)) then
		return w2, t2
	elseif w1 and w2 then
		-- si les deux ou aucun ne passent le test, on prend par défaut
		-- le cercle de plus petit rayon (habituellement le cercle intérieur)
		if r1 <= r2 then
			return w1, t1
		else
			return w2, t2
		end
	else
		-- pas de solution exploitable
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
