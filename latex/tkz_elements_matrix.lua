-- File: tkz_elements_matrices.lua
-- Version: 4.20c   Date: 2025/09/17
-- Copyright (c) 2023–2025 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

------------------------------------------------------------------------------
matrix = {}
matrix.__index = matrix
function matrix:new(value)
	local type = "matrix"
	local rows = #value
	local cols = #value[1]
	local set = value
	local det = determinant(value)
	local o = { set = set, rows = rows, cols = cols, det = det, type = type }
	setmetatable(o, self)
	return o
end

setmetatable(matrix, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

function matrix.__mul(m1, m2)
	if getmetatable(m1) ~= matrix then
		return k_mul_matrix(m1, m2)
	end
	if getmetatable(m2) ~= matrix then
		return k_mul_matrix(m2, m1)
	end
	return mul_matrix(m1, m2)
end

function matrix.__add(m1, m2)
	return add_matrix(m1, m2)
end

function matrix.__sub(m1, m2)
	return add_matrix(m1, k_mul_matrix(-1, m2))
end

function matrix.__pow(m, num)
	-- Handle transpose (when num is 'T')
	if num == "T" then
		return transposeMatrix(m)
	end

	-- Handle exponentiation by 0 (returns the identity matrix)
	if num == 0 then
		return matrix:new(#m, "I") -- Identity matrix
	end

	-- Handle negative exponents (invert the matrix)
	if num < 0 then
		local inv_matrix, err = inv_matrix(m)
		if not inv_matrix then
			return nil, err -- Return nil and the error if matrix is non-invertible
		end
		num = -num -- Make exponent positive for easier handling
		m = inv_matrix -- Now use the inverted matrix
	end

	-- Now handle the positive exponentiation
	local result = m
	for i = 2, num do
		result = mul_matrix(result, m) -- Repeated multiplication
	end

	return result
end

function matrix.__tostring(A)
	local mt = (A.type == "matrix" and A.set or A)
	local k = {}
	for i = 1, #mt do
		local n = {}
		for j = 1, #mt[1] do
			n[j] = tkz_display_(mt[i][j])
		end
		k[i] = table.concat(n, " ")
	end
	return table.concat(k)
end

function matrix.__eq(A, B)
	local mt1 = (A.type == "matrix" and A.set or A)
	local mt2 = (B.type == "matrix" and B.set or B)
	if A.type ~= B.type then
		return false
	end

	if #mt1 ~= #mt2 or #mt1[1] ~= #mt2[1] then
		return false
	end

	for i = 1, #mt1 do
		for j = 1, #mt1[1] do
			if mt1[i][j] ~= mt2[i][j] then
				return false
			end
		end
	end
	return true
end

function matrix:square(n, ...)
	local m = {}
	local t = table.pack(...)
	if n * n == #t then
		for i = 1, n do
			m[i] = {}
			for j = 1, n do
				m[i][j] = t[n * (i - 1) + j]
			end
		end
		return matrix:new(m)
	else
		return nil
	end
end

function matrix:vector(...)
	local m = {}
	local t = table.pack(...)
	for i = 1, #t do
		m[i] = {}
		m[i][1] = t[i]
	end
	return matrix:new(m)
end

matrix.column = matrix.vector

function matrix:row_vector(...)
	local m = {}
	local t = table.pack(...)
	m[1] = {}
	for j = 1, #t do
		m[1][j] = t[j]
	end
	return matrix:new(m)
end

function matrix:create(rows, cols)
	local mat = {}
	for i = 1, rows do
		mat[i] = {}
		for j = 1, cols do
			mat[i][j] = 0
		end
	end
	return matrix:new(mat)
end

function matrix:homogenization()
	return homogenization_(self)
end

function matrix:htm_apply(...)
	local obj, nb, t
	local tp = table.pack(...)
	obj = tp[1]
	nb = tp.n
	if nb == 1 then
		if obj.type == "point" then
			return htm_apply_(self, obj)
		elseif obj.type == "line" then
			return htm_apply_L_(self, obj)
		elseif obj.type == "triangle" then
			return htm_apply_T_(self, obj)
		elseif obj.type == "circle" then
			return htm_apply_C(self, obj)
		elseif
			obj.type == "square"
			or obj.type == "rectangle"
			or obj.type == "quadrilateral"
			or obj.type == "parallelogram"
		then
			return htm_apply_Q(self, obj)
		end
	else
		t = {}
		for i = 1, tp.n do
			table.insert(t, htm_apply_(self, tp[i]))
		end
		return table.unpack(t)
	end
end

function matrix:k_mul(n)
	return k_mul_matrix(n, self)
end

function matrix:get(i, j)
	if i == nil and j == nil then
		return self.set -- retourne toute la matrice
	else
		return get_element_(self, i, j)
	end
end

function matrix:inverse()
	return inv_matrix(self)
end

function matrix:adjugate()
	return adjugate_(self)
end

function matrix:transpose()
	return transposeMatrix(self)
end

function matrix:is_square()
	return self.rows == self.cols
end

function matrix:is_diagonal()
	return isDiagonal_(self)
end

function matrix:is_orthogonal()
	return isOrthogonal_(self)
end

function matrix:diagonalize() -- return two matrices D and P
	return diagonalize_(self)
end

function matrix:print(style, fmt)
	local style = (style or "bmatrix")
	local fmt = (fmt or 0)
	return print_matrix(self, style, fmt)
end

function matrix:identity(n)
	return id_matrix(n)
end

-------------------------
-- homogeneous transformation matrix
function matrix:htm(phi, a, b, sx, sy)
	local tx = (a or 0)
	local ty = (b or 0)
	local sx = (sx or 1)
	local sy = (sy or 1)
	local phi = (phi or 0)
	return matrix:square(3, sx * math.cos(phi), -math.sin(phi), tx, math.sin(phi), sy * math.cos(phi), ty, 0, 0, 1)
end
-------------------------

function matrix:is_orthogonal()
	return isOrthogonal_(self)
end

function matrix:swap_rows(row1, row2)
	self.set[row1], self.set[row2] = self.set[row2], self.set[row1]
	return self.set
end

function matrix:k_mul_row(row, k)
	for j = 1, #self.set[row] do
		self.set[row][j] = self.set[row][j] * k
	end
	return self.set
end

-- Ajoute à la ligne target_row la ligne source_row multipliée par k
function matrix:add_k_mul_row(target_row, source_row, k)
	for j = 1, #self.set[target_row] do
		self.set[target_row][j] = self.set[target_row][j] + self.set[source_row][j] * k
	end
	return self.set
end

function matrix:gauss_jordan()
	local rows = self.rows
	local cols = self.cols
	if rows == cols then
		for i = 1, rows do
			local maxRow = i
			for k = i + 1, rows do
				if math.abs(self.set[k][i]) > math.abs(self.set[maxRow][i]) then
					maxRow = k
				end
			end

			if maxRow ~= i then
				self:swap_rows(i, maxRow)
			end

			local pivot = self.set[i][i]

			if math.abs(pivot) < tkz.epsilon then
				return nil, "Matrix is singular or nearly singular"
			end

			self:k_mul_row(i, 1 / pivot)

			for j = 1, rows do
				if i ~= j then
					local scalar = -self.set[j][i]
					self:add_k_mul_row(j, i, scalar)
				end
			end
		end
		return matrix:new(self.set)
	else
		return nil, "Matrix must be square"
	end
end

function matrix:gauss_jordan_rect()
	local rows = self.rows
	local cols = self.cols

	for i = 1, rows do
		local maxRow = i
		for k = i + 1, rows do
			if math.abs(self.set[k][i]) > math.abs(self.set[maxRow][i]) then
				maxRow = k
			end
		end

		if maxRow ~= i then
			local ok = self:swap_rows(i, maxRow)
			--   if not ok then return nil, err end
		end

		local pivot = self.set[i][i]

		if math.abs(pivot) < tkz.epsilon then
			return nil --, "Matrix is singular or nearly singular"
		end

		local ok = self:k_mul_row(i, 1 / pivot)
		-- if not ok then return nil, err end

		for j = 1, rows do
			if i ~= j then
				local scalar = -self.set[j][i]
				local ok = self:add_k_mul_row(j, i, scalar)
				-- if not ok then return nil, err end
			end
		end
	end

	return matrix:new(self.set)
end

function matrix:rank()
	local A = (self.type == "matrix" and self.set or self)
	local m, n = self.rows, self.cols
	local rank = 0

	for col = 1, n do
		local pivot_row = rank + 1

		-- Finding the pivot
		while pivot_row <= m and A[pivot_row][col] == 0 do
			pivot_row = pivot_row + 1
		end

		if pivot_row <= m then
			-- Swap lines if necessary
			A[rank + 1], A[pivot_row] = A[pivot_row], A[rank + 1]

			-- Normaliser le pivot à 1
			local pivot = A[rank + 1][col]
			for j = 1, n do
				A[rank + 1][j] = A[rank + 1][j] / pivot
			end

			-- Eliminate the column
			for i = 1, m do
				if i ~= rank + 1 then
					local factor = A[i][col]
					for j = 1, n do
						A[i][j] = A[i][j] - factor * A[rank + 1][j]
					end
				end
			end

			rank = rank + 1
		end
	end

	return rank
end

return matrix
