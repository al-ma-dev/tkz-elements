-- File: tkz_elements_functions_vectors.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- new head of the vector v scaled by d (not the vector itself)
function scaled_head_(v, d)
	return v.tail + d * (v.head - v.tail)
end
