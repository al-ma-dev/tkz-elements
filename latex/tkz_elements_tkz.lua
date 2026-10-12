-- File: tkz_elements_tkz.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- tkz-settings and math

tkz = tkz or {}
tkz.nb_dec = 10
tkz.epsilon = 0.1 ^ tkz.nb_dec
tkz.dc = 2
tkz.phi = (1 + math.sqrt(5)) / 2 -- golden number φ
tkz.invphi = (math.sqrt(5) - 1) / 2
tkz.sqrtphi = math.sqrt(tkz.phi)
tkz.rad = 180 / math.pi
tkz.deg = math.pi / 180
tkz.pt = 254 / 7227
tkz.tau = 2 * math.pi
tkz.maxdimen_pt   = 16383.99999
tkz.max_coord_cm  = tkz.maxdimen_pt * tkz.pt      -- ≈ 575.8 cm
tkz.max_coord_safe = 0.9 * tkz.max_coord_cm       -- ≈ 518 cm

tkz.has_coord_warning = false
tkz.coord_warnings    = {}

if not tkz._seeded then
	math.randomseed(os.time())
	tkz._seeded = true
end


function tkz.register_coord_warning(name, x, y, kind)
	tkz.has_coord_warning = true
	table.insert(tkz.coord_warnings, {
		name  = name,
		x     = x,
		y     = y,
		limit = tkz.max_coord_safe,
		kind  = kind or "large"
	})
end

function tkz.flush_coord_warnings()
	if not tkz.has_coord_warning or #tkz.coord_warnings == 0 then
		return
	end

	local bs = string.char(92)

	for _, w in ipairs(tkz.coord_warnings) do
		local msg
		if w.kind == "undefined" then
			msg = string.format(
				"Point `%s' has undefined coordinates (%s, %s). " ..
				"It was placed at (0,0). Check the construction " ..
				"(e.g. an intersection that does not exist).",
				w.name, tostring(w.x), tostring(w.y)
			)
		else
			msg = string.format(
				"Point `%s' has very large coordinates (%.6f, %.6f). " ..
				"Safe limit is about |x|,|y| <= %.2f (TikZ units).",
				w.name, w.x, w.y, w.limit
			)
		end

		-- 1) Log / terminal
		texio.write_nl("term and log",
			"tkz-elements warning (coordinates): " .. msg)

		-- 2) LaTeX warning (outside tikzpicture if flush is called afterwards)
		tex.print(
			bs .. "PackageWarning{tkz-elements}{" .. msg .. "}"
		)
	end

	tkz.has_coord_warning = false
	tkz.coord_warnings    = {}
end



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

function tkz.parabola(a, b, c)
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
	-- not a tail call, so that errors point to the user's code
	local angle = tkz_angle_between_vectors_(a, b, c, d)
	return angle
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

	-- Gauss-Jordan elimination
	local sol = MA:gauss_jordan()

	-- the solution is in the last column (n + 1), one row per unknown
	local S = {}
	for i = 1, n do
		S[i] = { sol.set[i][n + 1] }
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
		error("tkz-elements: tkz.trisector: the angle (pb, pa, pc) must be direct", 2)
	end
end

function tkz.solve(...)
	local params = { ... }
	local np = select("#", ...)
	if (np == 2 or np == 3 or np == 4) and params[1] == 0 then
		error("tkz-elements: tkz.solve: the leading coefficient must not be zero", 2)
	end
	if np == 2 then
		local a = params[1]
		local b = params[2]
		return -b / a
	elseif np == 3 then
		return tkz.solve_quadratic_(table.unpack(params))
	elseif np == 4 then
		return tkz.solve_cubic_(table.unpack(params))
	else
		error("tkz-elements: tkz.solve: invalid number of parameters (" .. np .. "), expected 2, 3 or 4", 2)
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
			root1, root2 = nil, nil --solve_cx_quadratic(a, b, c)
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
	local epsilon = 1e-10 -- tolerance for floating-point comparisons

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

function tkz.near_integer(x, EPS)
	EPS = EPS or tkz.epsilon
	if type(x) ~= "number" then return false end
	if x ~= x or x == math.huge or x == -math.huge then return false end -- NaN / ±Inf
	return math.abs(x - math.floor(x + 0.5)) <= EPS
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
		error("tkz-elements: tkz.cramer: invalid number of arguments (expected 4 or 9)", 2)
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
	local p, err = poncelet_point_(a, b, c, d)
	if not p then
		error("tkz-elements: tkz.poncelet_point: " .. err, 2)
	end
	return p
end

function tkz.orthopole(a, b, c, l)
	local p, q = l:get()
	local ap = projection_(p, q, a)
	local bp = projection_(p, q, b)
	local app = projection_(b, c, ap)
	local bpp = projection_(a, c, bp)
	local la = line:new(ap, app)
	local lb = line:new(bp, bpp)
	return intersection(la, lb)
end

function tkz.nodes_from_paths(PAcenters, PAthrough, wbase, tbase, indice)
	wbase  = wbase  or "w"
	tbase  = tbase  or "t"
	local n = #PAcenters
	local k0 = (indice or 1) - 1   -- index offset

	for k = 1, n do
		local i = k0 + k
		z[wbase .. i] = PAcenters:get(k)
		z[tbase .. i] = PAthrough:get(k)
	end
end

-- tkz.fsolve
-- Searches the roots of f on [a,b] by splitting the interval into n
-- subintervals and refining with a Newton iteration (numerical derivative).
-- Arguments:
--   f : function(x) -> number
--   a, b : bounds (numbers)
--   n : number of subintervals (default 25)
--   opts (optional, table):
--       tol       : tolerance on |f(r)| (default 1e-8)
--       step_tol  : tolerance on the Newton step (default 1e-10)
--       h         : step of the finite difference (default 1e-6)
--       max_iter  : maximum number of iterations per seed (default 8)
--       merge_eps : distance under which two roots are merged (default 1e-5)
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
				-- replaced by the mean, for stability
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
		-- seed: midpoint of [x, fin]
		local r = 0.5 * (x + fin)

		-- even if f(x) and f(fin) have the same sign, try anyway
		-- (this can catch a zero at an extremum).
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

				-- stop on a small step
				if math.abs(step) <= step_tol * (1 + math.abs(r)) then
					r = r_next
					break
				end

				r = r_next
				fr = safe_f(r)
				if fr == nil then break end
				-- stop on a small residual
				if math.abs(fr) <= tol then break end
			end

			-- valid if in [x, fin) with a small residual
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
	if step == 0 then error("tkz-elements: tkz.range: the step must not be zero", 2) end
	local tbl = {}
	if (a <= b and step > 0) or (a >= b and step < 0) then
		for k = a, b, step do
			table.insert(tbl, k)
		end
	end
	return tbl
end

function tkz.GetNodes()
	local bs = string.char(92)

	-- reset for this call
	tkz.has_coord_warning = false
	tkz.coord_warnings    = {}

	for K, V in pairs(z) do
		local name = K
		local n = string.len(name)

		-- final 'p' -> ' and 'pp' -> ''
		if n > 1 then
			local ft, sd = string.match(name, "(.+)(.)")
			if sd == "p" then
				name = ft .. "'"
				local xft, xsd = string.match(ft, "(.+)(.)")
				if xsd == "p" then
					name = xft .. "''"
				end
			end
		end

		local x = V.re
		local y = V.im
		local ox, oy = x, y  -- original values

		-- undefined coordinates (NaN, or missing point) -> warning, (0,0)
		if type(x) ~= "number" or type(y) ~= "number" or x ~= x or y ~= y then
			tkz.register_coord_warning(name, ox, oy, "undefined")
			x, y = 0, 0
		-- coordinates out of range -> warning, replaced by (0,0)
		elseif math.abs(x) > tkz.max_coord_safe or math.abs(y) > tkz.max_coord_safe then
			tkz.register_coord_warning(name, ox, oy)
			x, y = 0, 0
		end

		-- neutral TikZ output: cannot break groups
		tex.sprint(
			bs .. "coordinate (" .. name .. ") at (" .. x .. "," .. y .. ");"
		)
	end
end


return tkz
