-- File: tkz_elements_functions_regular.lua
-- Version: 4.21c   Date: 2025/09/21
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
function regular_(c, th, s)
	-- Center through side
	local dep = angle_normalize_(point.arg(th - c))
	local r = point.mod(th - c)
	local t = {}
	local angle_step = 2 * math.pi / s

	for i = 0, s - 1 do
		table.insert(t, c + point:polar(r, i * angle_step + dep))
	end

	return t
end
