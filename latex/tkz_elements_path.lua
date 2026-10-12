-- File: tkz_elements_path.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

path = {}
path.__index = path

function path:new(data)
	if data ~= nil and type(data) ~= "table" then
		error("Expected table or nil in path constructor, got " .. type(data), 2)
	end
	return setmetatable(data or {}, self)
end

setmetatable(path, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})



-- true if the two entries are the same point (compared numerically,
-- so that "(1,0)" and "(1.00000,0.00000)" are equal)
local function same_entry(a, b)
	if a == b then
		return true
	end
	local ok1, x1, y1 = pcall(utils.parse_point, a)
	local ok2, x2, y2 = pcall(utils.parse_point, b)
	return ok1 and ok2 and math.abs(x1 - x2) <= tkz.epsilon and math.abs(y1 - y2) <= tkz.epsilon
end

function path.__add(p1, p2)
	local result = {}
	for _, pt in ipairs(p1) do
		result[#result + 1] = pt
	end

	if #p1 > 0 and #p2 > 0 and same_entry(p1[#p1], p2[1]) then
		for i = 2, #p2 do
			result[#result + 1] = p2[i]
		end
	else
		for _, pt in ipairs(p2) do
			result[#result + 1] = pt
		end
	end

	return path:new(result)
end



function path.__unm(p)
	local result = {}
	for i = #p, 1, -1 do
		result[#result + 1] = p[i]
	end
	return path:new(result)
end

function path.__sub(p1, p2)
	return p1 + -p2
end

function path:__tostring()
	local t = {}
	for i, v in ipairs(self) do
		t[i] = tostring(v)
	end
	return "path: { " .. table.concat(t, " , ") .. " }"
end


-- Points are stored with 10 decimals by default (as before, the coordinates
-- are TikZ strings). With 5 decimals, a point computed by a method returning
-- paths (CPP, CCP, ...) lost its precision: a tangency was no longer
-- detected by intersection (tkz.epsilon = 1e-10).
function path:add_point(z, decimals)
	decimals = decimals or 10
	if type(z) == "string" then
		local x,y = utils.parse_point(z, 3)
		z = point(x,y)
	end
	table.insert(self, utils.format_point(z, decimals))
end

function path:concat(sep)
	sep = sep or " "
	return table.concat(self, sep)
end

function path:add_pair_to_path(z1, z2, decimals)
	decimals = decimals or 10
	local x1 = utils.format_coord(z1.re, decimals)
	local y1 = utils.format_coord(z1.im, decimals)
	local x2 = utils.format_coord(z2.re, decimals)
	local y2 = utils.format_coord(z2.im, decimals)
	local pt = string.format("%s/%s/%s/%s", x1, y1, x2, y2)
	table.insert(self, pt)
end
path.add_pair = path.add_pair_to_path

function path:show()
	for _, pt in ipairs(self) do
		tex.print(pt)
	end
end

function path:copy()
	local c = {}
	for i, pt in ipairs(self) do
		c[i] = pt
	end
	return path:new(c)
end

-- Translation
function path:translate(dx, dy)
	local moved = {}
	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)
		local newx = x + dx
		local newy = y + dy
		table.insert(moved, string.format("(%s,%s)", checknumber_(newx), checknumber_(newy)))
	end
	return path:new(moved)
end

-- Homothety with center `center` and ratio k
function path:homothety(center, k)
	local scaled = {}
	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)
		local newx = center.re + k * (x - center.re)
		local newy = center.im + k * (y - center.im)
		table.insert(scaled, string.format("(%s,%s)", checknumber_(newx), checknumber_(newy)))
	end
	return path:new(scaled)
end

-- Anisotropic scaling
function path:scale(kx, ky)
	local scaled = {}
	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)
		local newx = x * kx
		local newy = y * ky
		table.insert(scaled,
			string.format("(%s,%s)",
				checknumber_(newx),
				checknumber_(newy)
			)
		)
	end
	return path:new(scaled)
end

-- Rotation about `center` by the angle theta (radians)
function path:rotate(center, theta)
	local rotated = {}
	local cos_t = math.cos(theta)
	local sin_t = math.sin(theta)
	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)
		local dx, dy = x - center.re, y - center.im
		local newx = center.re + dx * cos_t - dy * sin_t
		local newy = center.im + dx * sin_t + dy * cos_t
		table.insert(rotated, string.format("(%s,%s)", checknumber_(newx), checknumber_(newy)))
	end
	return path:new(rotated)
end

-- Close the path (append the first point if needed)
function path:close()
	if #self == 0 then
		return self
	end

	local x1, y1 = utils.parse_point(self[1])
	local x2, y2 = utils.parse_point(self[#self])

	local eps = tkz.epsilon or 1e-9
	if math.abs(x1 - x2) > eps or math.abs(y1 - y2) > eps then
		local closed = {}
		for i, pt in ipairs(self) do
			closed[i] = pt
		end
		closed[#closed + 1] = self[1]
		return path:new(closed)
	else
		return self
	end
end


-- Sub-path from index i1 to i2
function path:sub(i1, i2)
	local subp = {}
	for i = i1 or 1, i2 or #self do
		subp[#subp + 1] = self[i]
	end
	return path:new(subp)
end

-- "x1/y1,x2/y2,..." for \foreach (the coordinates are kept as written)
function path:concat_rawpair(sep)
	sep = sep or ","
	local list = {}
	for i = 1, #self do
		local x, y = tostring(self[i]):match("^%s*%(%s*([^,]-)%s*,%s*([^%)]-)%s*%)%s*$")
		if not x or x == "" or y == "" then
			error("tkz-elements: path:concat_rawpair: entry #" .. i .. " is not a point \""
				.. tostring(self[i]) .. "\" (a path built with add_pair contains pairs of points)", 2)
		end
		list[#list + 1] = x .. "/" .. y
	end
	return table.concat(list, sep)
end

function path:count()
	return #self
end

-- coordinates of the i-th point; level = error level (see below)
local function number_at(self, i, name, level)
	local item = self[i]
	if not item then
		error(("tkz-elements: path:%s: index %s out of bounds (1..%d)"):format(name, tostring(i), #self), level)
	end
	local x, y = utils.parse_point(item, level + 1) -- not a tail call (error level)
	return x, y
end

-- coordinates of the i-th point
function path:get_number_path(i)
	local x, y = number_at(self, i, "get_number_path", 3)
	return x, y
end

-- the i-th point
function path:get_point(i)
	local x, y = number_at(self, i, "get_point", 3)
	return point(x, y)
end

-- get(i): the i-th point ; get(): all the points
function path:get(i)
	if i == nil then
		local t = {}
		for k = 1, #self do
			local x, y = number_at(self, k, "get", 3)
			t[k] = point(x, y)
		end
		return table.unpack(t, 1, #self)
	end
	local x, y = number_at(self, i, "get", 3)
	return point(x, y)
end

--  Practical iterator
-- for i, x, y in PA.A:iter() do
	-- -- do something with (i, x, y)
-- end
function path:iter()
	local i, n = 0, #self
	return function()
		i = i + 1
		if i <= n then
			local x, y = number_at(self, i, "iter", 3)
			return i, x, y
		end
	end
end


function path:map(fx, fy)
	fy = fy or fx

	local mapped = {}

	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)

		local newx = fx(x)
		local newy = fy(y)

		mapped[#mapped+1] =
			string.format("(%s,%s)",
				checknumber_(newx),
				checknumber_(newy)
			)
	end

	return path:new(mapped)
end


return path
