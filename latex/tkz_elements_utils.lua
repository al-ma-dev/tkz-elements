-- File: tkz_elements_utils.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

utils = {}

-- ----------------------------
-- Parsing / formatting helpers
-- ----------------------------

-- Parse a string of the form "(x,y)" into two numbers.
-- level (optional) : error level, so that the message points to the
-- user's code when parse_point is called by a method (default 2).
function utils.parse_point(str, level)
	local x, y = tostring(str):match("^%s*%(?%s*([%+%-%.%deE]+)%s*,%s*([%+%-%.%deE]+)%s*%)%s*$")
	x, y = tonumber(x), tonumber(y)
	if not x or not y then
		error("tkz-elements: invalid point string \"" .. tostring(str) .. "\", expected \"(x,y)\"", level or 2)
	end
	return x, y
end

-- Internal: format a number to fixed decimals (with checks).
-- Errors are reported at level 3 (user -> public function -> _fmt_number).
-- A result such as "-0.00" is written "0.00".
local function _fmt_number(x, decimals, ctx)
	decimals = decimals or 5
	if type(decimals) ~= "number" or decimals < 0 or decimals ~= math.floor(decimals) then
		error("tkz-elements: " .. ctx .. ": the number of decimals must be a non-negative integer, got "
			.. tostring(decimals), 3)
	end
	local n = tonumber(x)
	if not n then
		error("tkz-elements: " .. ctx .. ": invalid number " .. tostring(x), 3)
	end
	local s = string.format("%." .. decimals .. "f", n)
	if s:match("^%-0%.?0*$") then
		s = s:sub(2)
	end
	return s
end

-- Public: format a number with a specified number of decimal places.
function utils.format_number(x, decimals)
	local s = _fmt_number(x, decimals, "format_number")
	return s
end

-- Public: alias (kept for backward compatibility)
utils.checknumber       = utils.format_number


-- Coordinates formatting (single component)
function utils.format_coord(x, decimals)
	local s = _fmt_number(x, decimals, "format_coord")
	return s
end

-- Format a complex number (table with .re and .im) as a coordinate string.
function utils.format_point(z, decimals)
	if type(z) ~= "table" or type(z.re) ~= "number" or type(z.im) ~= "number" then
		error("tkz-elements: format_point: a point (table with fields 're' and 'im') is expected", 2)
	end
	decimals = decimals or 5
	local x = _fmt_number(z.re, decimals, "format_point")
	local y = _fmt_number(z.im, decimals, "format_point")
	return "(" .. x .. "," .. y .. ")"
end

-- ----------------------------
-- Numeric helpers
-- ----------------------------

-- Compare two numbers with tolerance (robust default).
function utils.almost_equal(a, b, EPS)
	EPS = EPS or (tkz and tkz.epsilon) or 1e-9
	local na, nb = tonumber(a), tonumber(b)
	if not na or not nb then
		error("tkz-elements: almost_equal: numbers are expected, got " .. tostring(a) .. ", " .. tostring(b), 2)
	end
	return math.abs(na - nb) <= EPS
end

-- Optional: Clamp a number between min and max.
function utils.clamp(x, min_val, max_val)
	return math.max(min_val, math.min(max_val, x))
end

-- Optional: Return the sign of a number.
function utils.sign(x)
	return (x > 0 and 1) or (x < 0 and -1) or 0
end

-- ----------------------------
-- Logging helpers
-- ----------------------------

-- Print formatted message to terminal and log.
function utils.w(...)
	texio.write_nl("term and log", "[tkz-elements] " .. string.format(...))
end

-- Print formatted message to log only.
function utils.wlog(...)
	texio.write_nl("log", "[tkz-elements] " .. string.format(...))
end

-- ----------------------------
-- Table helpers
-- ----------------------------

-- Length of contiguous array part 1..n (stops at first nil).
-- (Name explicit to avoid confusion with #t on sparse tables.)
function utils.table_len_contiguous(t)
	if type(t) ~= "table" then
		error("tkz-elements: table_len_contiguous: a table is expected, got a " .. type(t), 2)
	end
	local i = 0
	while t[i + 1] ~= nil do
		i = i + 1
	end
	return i
end

-- Backward compatibility
utils.table_getn = utils.table_len_contiguous


-- --- internal helper: compile a chunk returning a function -------------------
local function compile_func_(chunk, where, payload, fallback)
	fallback = fallback or function(_) return 0 end

	local loader, err = load(chunk, where, "t", math)
	if not loader then
		tex.error("Invalid function expression", {
			payload or chunk,
			err
		})
		return fallback
	end

	local ok, fn = pcall(loader)
	if not ok or type(fn) ~= "function" then
		tex.error("Invalid function expression", {
			payload or chunk,
			tostring(fn)
		})
		return fallback
	end

	return fn
end

-- Compile a TeX-provided expression into a Lua function y = f(x)
function utils.compile_fx_(expr)
	local chunk = "return function(x) return (" .. expr .. ") end"
	return compile_func_(
		chunk,
		"tkz.compile_fx",
		expr,
		function(_) return 0 end
	)
end

-- Compile a parametric TeX-provided expression into a Lua function (x,y) = f(t)
function utils.compile_ft_(exprx, expry)
	local chunk = ("return function(t) return (%s), (%s) end"):format(exprx, expry)
	-- fallback paramétrique : renvoie (0,0) pour rester compatible avec build_path
	return compile_func_(
		chunk,
		"tkz.compile_ft",
		("x(t)=%s ; y(t)=%s"):format(exprx, expry),
		function(_) return 0, 0 end
	)
end


-- Build a path from a function f(x)
function utils.build_path_(f, xmin, xmax, n)
	xmin   = tonumber(xmin)
	xmax   = tonumber(xmax)
	n      = tonumber(n) or 100

	local p = path()
	local step = (xmax - xmin) / n
	for i = 0, n do
		local x = xmin + i * step
		local y = f(x)
		if y == y and y ~= math.huge and y ~= -math.huge then
			p:add_point(point(x, y))
		end
	end
	return p
end

-- Build a path from a parametric function g(t) -> x, y
function utils.build_param_path_(g, tmin, tmax, n)
	tmin = tonumber(tmin)
	tmax = tonumber(tmax)
	n    = tonumber(n) or 200

	local p = path()
	local step = (tmax - tmin) / n

	for i = 0, n do
		local t = tmin + i * step
		local x, y = g(t)

		if x == x and y == y
			 and x ~= math.huge and x ~= -math.huge
			 and y ~= math.huge and y ~= -math.huge then
			p:add_point(point(x, y))
		end
	end
	return p
end

return utils
