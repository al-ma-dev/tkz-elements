-- File: tkz_elements_path.lua
-- Copyright (c) 2023–2025 Alain Matthes
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

function path.__add(p1, p2)
	local result = {}
	for _, pt in ipairs(p1) do
		result[#result + 1] = pt
	end
	for _, pt in ipairs(p2) do
		result[#result + 1] = pt
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
	return "path: { " .. table.concat(self, " , ") .. " }"
end

function path:add_point(z, decimals)
	table.insert(self, utils.format_point(z, decimals))
end

function path:concat(sep)
	sep = sep or " "
	return table.concat(self, sep)
end

function path:add_pair_to_path(z1, z2, decimals)
	decimals = decimals or 5
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

-- Homothétie de centre (cx, cy), rapport k
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

-- Rotation autour de (cx, cy) d'un angle theta en radians
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

-- Fermeture
function path:close()
	if #self == 0 then
		return self
	end

	local x1, y1 = utils.parse_point(self[1])
	local x2, y2 = utils.parse_point(self[#self])

	if x1 ~= x2 or y1 ~= y2 then
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

-- Sous-chemin
function path:sub(i1, i2)
	local subp = {}
	for i = i1 or 1, i2 or #self do
		subp[#subp + 1] = self[i]
	end
	return path:new(subp)
end

function path:concat_rawpair(sep)
	sep = sep or ","
	local list = {}
	for i = 1, #self do
		local x, y = self[i]:match("%(([^,]+),([^%)]+)%)")
		list[#list + 1] = x .. "/" .. y
	end
	return table.concat(list, sep)
end

function path:get_rawpair_list()
	local result = {}
	for _, pt in ipairs(self) do
		local x, y = utils.parse_point(pt)
		table.insert(result, string.format("%.5f/%.5f", x, y))
	end
	return result
end
function path:count()
	return #self
end

local function parse_xy(entry)
	if type(entry) == "table" then
		-- au cas où tu ranges déjà sous forme table
		if entry.x_num and entry.y_num then return entry.x_num, entry.y_num end
		if entry.x and entry.y then return tonumber(entry.x), tonumber(entry.y) end
	end
	-- formats tolérés: "(x,y)" | "x,y" | "x/y"
	local s = tostring(entry)
	local x, y = s:match("%(?%s*([%+%-]?%d+%.?%d*)%s*[,/ ]%s*([%+%-]?%d+%.?%d*)%s*%)?")
	return tonumber(x), tonumber(y)
end

--  coordinates of the i-th point
function path:get_number_path(i)
	local item = self[i]
	if not item then
		error(("path:get_number_path: index %s out of bounds (1..%d)"):format(tostring(i), #self))
	end
	local x, y = parse_xy(item)
	if not x or not y then
		error(("path:get_number_path: Unable to analyze the input #%d"):format(i))
	end
	return x, y
end


function path:get_point(i)
	local x, y = self:get_number_path(i)
	return point(x, y)
end
path.get = path.get_point

--  Practical iterator
-- for i, x, y in PA.A:iter() do
	-- -- do something with (i, x, y)
-- end
function path:iter()
	local i, n = 0, #self
	return function()
		i = i + 1
		if i <= n then
			local x, y = self:get_number_path(i)
			return i, x, y
		end
	end
end

return path
