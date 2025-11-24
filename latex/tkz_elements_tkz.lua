-- File: tkz_elements_tkz.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- tkz-settings and math

tkz = tkz or {}
tkz.nb_dec = 8
tkz.epsilon = 0.1 ^ tkz.nb_dec
tkz.dc = 2
tkz.phi = (1 + math.sqrt(5)) / 2 -- golden number φ
tkz.invphi = (math.sqrt(5) - 1) / 2
tkz.sqrtphi = math.sqrt(tkz.phi)
tkz.rad = 180 / math.pi
tkz.deg = math.pi / 180
tkz.pt = 254 / 7227
tkz.tau = 2 * math.pi
-- real
function tkz.approx(x, y, EPS)
	 EPS = EPS or tkz.epsilon
	return math.abs(x - y) <= EPS
end

function tkz.midpoint(a, b)
	return (a + b) / 2
end

function tkz.reset_defaults()
	tkz.nb_dec = 10
	tkz.epsilon = 0.1 ^ tkz.nb_dec
	tkz.dc = 2
end

function tkz.set_nb_dec(n)
	tkz.nb_dec = n
	tkz.epsilon = 0.1 ^ n
end

function tkz.midpoints(...)
	return midpoints_(...)
end

function tkz.length(a, b)
	return point.abs(a - b)
end

function tkz.parabola(a, b, c) --bug local
	local xa, xb, xc, ya, yb, yc
	xa = a.re
	ya = a.im
	xb = b.re
	yb = b.im
	xc = c.re
	yc = c.im
	return parabola_(xa, ya, xb, yb, xc, yc)
end
-----------------------------------------
-------------- angles -------------------
-----------------------------------------
function tkz.get_angle(a, b, c)
	return get_angle_(a, b, c)
end

function tkz.inner_angle(pa, pb, pc)
	return inner_angle_(pa, pb, pc)
end

function tkz.angle_normalize(an)
	return angle_normalize_(an)
end

function tkz.is_direct(pa, pb, pc)
	return get_angle_(pa, pb, pc) > 0
end

function tkz.get_angle_normalize(a, b, c)
	return get_angle_normalize_(a, b, c)
end

function tkz.angle_between_vectors(a, b, c, d)
	return tkz_angle_between_vectors_(a, b, c, d)
end

function tkz.round(num, idp)
	local mult = 10 ^ (idp or 0)
	return math.floor(num * mult + 0.5) / mult
end

function tkz.solve_linear_system(M, N)
	if not M or not N then
		return nil, "Error: Input matrices are nil."
	end

	local m, n = M.rows, M.cols

	if N.rows ~= m or N.cols ~= 1 then
		return nil, "Error: Incompatible dimensions between M and N."
	end

	local Aug = {}

	-- Construction of the augmented matrix
	for i = 1, m do
		Aug[i] = {}
		for j = 1, n do
			Aug[i][j] = M.set[i][j]
		end
		Aug[i][n + 1] = N.set[i][1] -- Add column vector
	end

	local MA = matrix:new(Aug)

	local rankM = M:rank()
	local rankMA = MA:rank()

	-- System compatibility check
	if rankM < rankMA then
		return nil, "Incompatible system (no solution)"
	elseif rankM < n then
		return nil, "Indeterminate system (infinite solutions)"
	end

	-- Résolution par Gauss-Jordan
	local sol = MA:gauss_jordan_rect()

	--
	local S = {}
	for i = 1, m do
		S[i] = { sol.set[i][n + 1] } -- n + 1 car n=3, on veut la colonne 4
	end

	return matrix:new(S)
end

function tkz.bisector(a, b, c)
	local i = in_center_(a, b, c)
	return line:new(a, intersection_ll_(a, i, b, c))
end

function tkz.altitude(a, b, c)
	local o, p
	-- Get the orthocenter (which is the point of concurrency of the altitudes)
	o = ortho_center_(a, b, c)

	-- Get the perpendicular projection of point 'a' onto the line defined by 'b' and 'c'
	p = projection_(b, c, a)

	-- Return the altitude, which is the line from point 'a' to the point 'p'
	return line:new(a, p)
end

function tkz.bisector_ext(a, b, c) -- n=1 swap n=2 swap 2
	local i, p
	-- Get the incenter of the triangle
	i = in_center_(a, b, c)

	-- Rotate the incenter by 90 degrees around point 'a' to compute the external bisector
	p = rotation_(a, math.pi / 2, i)

	-- Return the external bisector as the line passing through points 'a' and 'p'
	return line:new(a, p)
end

function tkz.trisector(pa, pb, pc)
	if tkz.is_direct(pa, pb, pc) then
		local a = get_angle_(pa, pb, pc)
		local i = report_(pa, pb, 1)
		local j = rotation_(pa, a / 3, i)
		local k = rotation_(pa, a / 3, j)
		return j, k
	else
		tex.error("angle must be direct")
		tex.print("stop")
	end
end

function tkz.solve(...)
	local params = { ... }
	local np = #params
	if np == 2 then
		local a = params[1]
		local b = params[2]
		return -b / a
	elseif np == 3 then
		return tkz.solve_quadratic_(table.unpack(params))
	elseif np == 4 then
		return tkz.solve_cubic_(table.unpack(params))
	else
		tex.error("solve: Invalid number of parameters (" .. np .. "). Expected 2, 3, or 4.")
	end
end

function tkz.solve_cx_quadratic(a, b, c)
	local d = b * b - 4 * a * c
	local dcx = point.sqrt(d)
	local root1 = (-b + dcx) / (2 * a)
	local root2 = (-b - dcx) / (2 * a)
	return root1, root2
end

function tkz.solve_quadratic_(a, b, c)
	local root1, root2, delta, sqrtdelta
	if (type(a) == "number") and (type(b) == "number") and (type(c) == "number") then
		delta = b * b - 4 * a * c
		if math.abs(delta) < tkz.epsilon then
			delta = 0
		end
		if delta < 0 then
			root1, root2 = false, false --solve_cx_quadratic(a, b, c)
		elseif delta == 0 then
			root1 = -b / (2 * a)
			root2 = -b / (2 * a)
		else
			sqrtdelta = math.sqrt(delta)

			root1 = (-b + sqrtdelta) / (2 * a)
			root2 = (-b - sqrtdelta) / (2 * a)
		end
	else
		root1, root2 = tkz.solve_cx_quadratic(a, b, c)
	end
	return root1, root2 -- Two real roots
end

function tkz.solve_cubic_(a, b, c, d)
	local p = (3 * a * c - b * b) / (3 * a * a)
	local q = (2 * b * b * b - 9 * a * b * c + 27 * a * a * d) / (27 * a * a * a)

	local delta = q * q / 4 + p * p * p / 27
	local epsilon = 1e-10 -- Tolérance pour les comparaisons de nombres flottants

	local function cubert(x)
		return x >= 0 and x ^ (1 / 3) or -(-x) ^ (1 / 3)
	end

	if delta > epsilon then
		local u = cubert((-q + math.sqrt(delta)) / 2)
		local v = cubert((-q - math.sqrt(delta)) / 2)
		local x1 = u + v - b / (3 * a)

		return x1, nil, nil
	elseif math.abs(delta) <= epsilon then
		local u = cubert(-q / 2)
		local x1 = 2 * u - b / (3 * a)
		local x2 = -u - b / (3 * a)

		return x1, x2, x2
	else
		local r = math.sqrt(-p * p * p / 27)
		local t = -q / (2 * r)

		if t > 1 then
			t = 1
		end
		if t < -1 then
			t = -1
		end

		local theta = math.acos(t)
		local x1 = 2 * cubert(r) * math.cos(theta / 3) - b / (3 * a)
		local x2 = 2 * cubert(r) * math.cos((theta + tkz.tau) / 3) - b / (3 * a)
		local x3 = 2 * cubert(r) * math.cos((theta + 2 * tkz.tau) / 3) - b / (3 * a)

		return x1, x2, x3
	end
end

-- straight line from a through b
function tkz.line_coefficients(xa, ya, xb, yb)
	return (yb - ya) / (xb - xa), (xb * ya - xa * yb) / (xb - xa)
end

function tkz.angle_between_vectors(a, b, c, d)
	return tkz_angle_between_vectors_(a, b, c, d)
end

function tkz.barycenter(...)
	return barycenter_(...)
end

function tkz.is_linear(z1, z2, z3)
	return math.abs((z2 - z1) ^ (z3 - z1)) < tkz.epsilon
end
tkz.aligned = tkz.is_linear

function tkz.is_ortho(z1, z2, z3)
	return math.abs((z2 - z1) .. (z3 - z1)) < tkz.epsilon
end

function tkz.is_zero(x)
	return is_zero_(x)
end

function tkz.set_zero(x)
	return set_zero_(x)
end

function tkz.residue(x)
	return residue_(x)
end

-- internal utility: test for NaN
function tkz.is_NaN(x)
	return x ~= x
end

-- internal utility: test for Infinity
function tkz.is_Inf(x)
	return (x == math.huge) or (x == -math.huge)
end

-- internal utility: detect invalid numeric results (NaN or ±Inf)
function tkz.notDef(x)
	return tkz.is_NaN(x) or tkz.is_Inf(x)
end

function tkz.is_integer(x)
	return type(x) == "number"
		and x == x -- exclude NaN
		and x < math.huge -- exclude +inf
		and -x < math.huge -- exclude -inf
		and is_zero_(x - tkz_round_(x))
end

function tkz.near_integer(x)
	return is_zero_(x % 1)
end

function tkz.orient2d(x1, y1, x2, y2, x3, y3)
	return (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)
end

function tkz.cramer(...)
	local t = { ... }
	local n = #t
	if n == 4 then
		local a1, a2, b1, b2 = table.unpack(t)
		return a1 * b2 - a2 * b1
	elseif n == 9 then
		local a1, a2, a3, b1, b2, b3, c1, c2, c3 = table.unpack(t)
		return a1 * b2 * c3 + a3 * b1 * c2 + a2 * b3 * c1 - a3 * b2 * c1 - a1 * b3 * c2 - a2 * b1 * c3
	else
		tex.error("cramer: invalid number of arguments (expected 4 or 9)")
	end
end

function tkz.dot_product(a, b, c)
	return (b - a) .. (c - a)
end

function tkz.round(num, idp)
	return tkz_round_(num, idp)
end

function tkz.display(z)
	return tkz_display_(z)
end

function tkz.poncelet_point(a, b, c, d)
	local e1 = euler_center_(a, b, c)
	local e2 = euler_center_(a, c, d)
	local m = midpoint_(a, c)
	local x, y = intersection_cc_(e1, m, e2, m)

	if not x or not y then
		tex.xs("Poncelet construction failed: no intersection.")
	end

	if x == m then
		return y
	else
		return x
	end
end

function tkz.orthopole(a, b, c, l)
	local ap, bp, cp = l:projection(a, b, c)
	local bpp = self.ca:projection(bp)
	local app = self.bc:projection(ap)
	local la = line:new(ap, app)
	local lb = line:new(bp, bpp)
	return intersection(la, lb)
end

function tkz.nodes_from_paths(PAcenters, PAthrough, wbase, tbase, indice)
	wbase  = wbase  or "w"
	tbase  = tbase  or "t"
	local n = #PAcenters
	local k0 = (indice or 1) - 1   -- offset d’indice

	for k = 1, n do
		local i = k0 + k
		z[wbase .. i] = PAcenters:get(k)
		z[tbase .. i] = PAthrough:get(k)
	end
end

-- tkz.fsolve
-- Cherche des racines de f sur [a,b] en découpant l'intervalle en n sous-intervales
-- et en raffinant par une itération de type Newton avec dérivée numérique.
-- Arguments :
--   f : function(x) -> number
--   a, b : bornes (numbers)
--   n : nombre de sous-intervalles (par défaut 25)
--   opts (optionnel, table) :
--       tol       : tolérance sur |f(r)| (déf. 1e-8)
--       step_tol  : tolérance sur le déplacement de Newton (déf. 1e-10)
--       h         : pas pour la dérivée finie (déf. 1e-6)
--       max_iter  : itérations max par graine (déf. 8)
--       merge_eps : distance pour fusionner deux racines proches (déf. 1e-5)
function tkz.fsolve(f, a, b, n, opts)
	if type(f) ~= "function" or type(a) ~= "number" or type(b) ~= "number" then return end
	if a == b then return end
	if a > b then a, b = b, a end

	n = n or 25
	opts = opts or {}
	local tol       = opts.tol       or 1e-8
	local step_tol  = opts.step_tol  or 1e-10
	local h         = opts.h         or 1e-6
	local max_iter  = opts.max_iter  or 8
	local merge_eps = opts.merge_eps or 1e-5

	local function isfinite(x)
		return (x == x) and (x ~= math.huge) and (x ~= -math.huge)
	end

	local function safe_f(x)
		local ok, y = pcall(f, x)
		if not ok or not isfinite(y) then return nil end
		return y
	end

	local function push_unique(S, r)
		for i = 1, #S do
			if math.abs(S[i] - r) <= merge_eps then
				-- remplace par la moyenne pour stabiliser
				S[i] = 0.5 * (S[i] + r)
				return
			end
		end
		S[#S+1] = r
	end

	local S = {}
	local delta = (b - a) / n
	local x = a
	local fin = x + delta

	while x < b do
		-- Graine : milieu de [x, fin]
		local r = 0.5 * (x + fin)

		-- Optionnel : si f(x) et f(fin) existent et n'encadrent rien,
		-- on tente quand même (peut attraper un zéro au sommet).
		local fr = safe_f(r)
		if fr ~= nil then
			for _ = 1, max_iter do
				local frh = safe_f(r + h)
				if frh == nil then break end
				local denom = frh - fr
				if denom == 0 then break end

				local step = fr * h / denom
				if not isfinite(step) then break end

				local r_next = r - step
				if not isfinite(r_next) then break end

				-- Arrêt sur petit déplacement
				if math.abs(step) <= step_tol * (1 + math.abs(r)) then
					r = r_next
					break
				end

				r = r_next
				fr = safe_f(r)
				if fr == nil then break end
				-- Arrêt sur résidu
				if math.abs(fr) <= tol then break end
			end

			-- Valide si : dans [x, fin) et résidu petit
			local fr_ok = fr ~= nil and math.abs(fr) <= tol
			if fr_ok and (r >= x) and (r < fin) then
				push_unique(S, r)
			end
		end

		x = fin
		fin = x + delta
	end

	if #S > 0 then return S end
end

-- tkz.derivative
-- Compute the numerical derivative f'(x0)
-- using the central difference formula.
-- Optional parameter 'accuracy' controls precision (default = 1e-6).
function tkz.derivative(f, x0, accuracy)
	if type(f) ~= "function" or type(x0) ~= "number" then return nil end
	accuracy = accuracy or 1e-6
	local f1, f2 = f(x0 + accuracy), f(x0 - accuracy)
	if (f1 == nil) or (f2 == nil) then return nil end
	return (f1 - f2) / (2 * accuracy)
end

function tkz.range(a, b, step)
	step = step or 1
	if step == 0 then error("step must not be zero") end
	local tbl = {}
	if (a <= b and step > 0) or (a >= b and step < 0) then
		for k = a, b, step do
			table.insert(tbl, k)
		end
	end
	return tbl
end

return tkz
