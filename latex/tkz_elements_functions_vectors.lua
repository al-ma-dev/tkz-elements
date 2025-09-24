-- File: tkz_elements_functions_vector.lua
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes


function scale_(v, d)
	return v.tail + point(d * v.norm * math.cos(v.slope), d * v.norm * math.sin(v.slope))
end
