-- File: tkz_elements_functions_vector.lua
-- Version: 4.20c   Date: 2025/09/17
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- sub_version 1.0

function scale_(v, d)
	return v.tail + point(d * v.norm * math.cos(v.slope), d * v.norm * math.sin(v.slope))
end
