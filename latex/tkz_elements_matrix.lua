-- File: tkz_elements_matrix.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

------------------------------------------------------------------------------
-- class matrix
------------------------------------------------------------------------------
matrix = {}

-- The determinant is computed on first access to M.det, then cached.
-- Methods that modify a matrix in place reset the cache (self.det = nil).
matrix.__index = function(M, k)
	local v = matrix[k]
	if v ~= nil then
		return v
	end
	if k == "det" then
		local d = determinant(M)
		rawset(M, "det", d)
		return d
	end
	return nil
end

function matrix:new(value)
	if type(value) ~= "table" or type(value[1]) ~= "table" or #value[1] == 0 then
		error("tkz-elements: matrix: a non-empty table of rows is expected", 2)
	end
	local cols = #value[1]
	for i = 2, #value do
		if type(value[i]) ~= "table" or #value[i] ~= cols then
			error("tkz-elements: matrix: all rows must have the same length (row " .. i .. ")", 2)
		end
	end
	local o = { set = value, rows = #value, cols = cols, type = "matrix" }
	setmetatable(o, self)
	return o
end

setmetatable(matrix, {
	__call = function(cls, ...)
		return cls:new(...)
	end,
})

------------------------------------------------------------------------------
-- metamethods
------------------------------------------------------------------------------
local function is_matrix(m)
	return getmetatable(m) == matrix
end

function matrix.__mul(m1, m2)
	if not is_matrix(m1) then
		return k_mul_matrix_(m1, m2)
	end
	if not is_matrix(m2) then
		return k_mul_matrix_(m2, m1)
	end
	if m1.cols ~= m2.rows then
		error("tkz-elements: matrix: product of a " .. m1.rows .. "x" .. m1.cols
			.. " matrix by a " .. m2.rows .. "x" .. m2.cols .. " matrix", 2)
	end
	return mul_matrix_(m1, m2)
end

local function check_same_size(m1, m2, op)
	if not (is_matrix(m1) and is_matrix(m2)) then
		error("tkz-elements: matrix: " .. op .. " of a matrix and a non-matrix", 3)
	end
	if m1.rows ~= m2.rows or m1.cols ~= m2.cols then
		error("tkz-elements: matrix: " .. op .. " of matrices of different sizes ("
			.. m1.rows .. "x" .. m1.cols .. " and " .. m2.rows .. "x" .. m2.cols .. ")", 3)
	end
end

function matrix.__add(m1, m2)
	check_same_size(m1, m2, "sum")
	return add_matrix_(m1, m2)
end

function matrix.__sub(m1, m2)
	check_same_size(m1, m2, "difference")
	return add_matrix_(m1, k_mul_matrix_(-1, m2))
end

function matrix.__pow(m, exp)
	if not is_matrix(m) then
		error("tkz-elements: matrix: the left operand of ^ must be a matrix", 2)
	end
	-- transpose shortcut (A^"T")
	if exp == "T" then
		return transposeMatrix(m)
	end
	-- only integer powers are supported
	if type(exp) ~= "number" or exp ~= math.floor(exp) then
		error("tkz-elements: matrix: the exponent must be an integer (or 'T' for the transpose)", 2)
	end
	if not m:is_square() then
		error("tkz-elements: matrix: power of a non-square matrix", 2)
	end

	local n = m.rows

	-- A^0 = I
	if exp == 0 then
		return id_matrix_(n)
	end

	-- exp < 0: power of the inverse
	local base = m
	if exp < 0 then
		local inv, err = inv_matrix_(m)
		if not inv then
			error("tkz-elements: matrix: " .. err, 2)
		end
		base = inv
		exp = -exp
	end

	-- binary exponentiation
	local result = id_matrix_(n)
	local p = exp
	local b = base
	while p > 0 do
		if (p % 2) == 1 then
			result = mul_matrix_(result, b)
		end
		p = math.floor(p / 2)
		if p > 0 then
			b = mul_matrix_(b, b)
		end
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
	return table.concat(k, " ; ")
end

-- Equality with the tolerance tkz.epsilon.
function matrix.__eq(A, B)
	if not (is_matrix(A) and is_matrix(B)) then
		return false
	end
	if A.rows ~= B.rows or A.cols ~= B.cols then
		return false
	end
	for i = 1, A.rows do
		for j = 1, A.cols do
			if not near_entries_(A.set[i][j], B.set[i][j]) then
				return false
			end
		end
	end
	return true
end

------------------------------------------------------------------------------
-- constructors
------------------------------------------------------------------------------
function matrix.square(n, ...)
	local t = table.pack(...)
	if type(n) ~= "number" or n < 1 or n * n ~= t.n then
		error("tkz-elements: matrix.square: " .. tostring(n) .. "x" .. tostring(n)
			.. " values expected, got " .. t.n, 2)
	end
	local m = {}
	for i = 1, n do
		m[i] = {}
		for j = 1, n do
			m[i][j] = t[n * (i - 1) + j]
		end
	end
	return matrix:new(m)
end

function matrix.vector(...)
	local m = {}
	local t = table.pack(...)
	for i = 1, t.n do
		m[i] = { t[i] }
	end
	return matrix:new(m)
end

matrix.column = matrix.vector

function matrix.row_vector(...)
	local t = table.pack(...)
	local m = { {} }
	for j = 1, t.n do
		m[1][j] = t[j]
	end
	return matrix:new(m)
end

function matrix.create(rows, cols)
	local mat = {}
	for i = 1, rows do
		mat[i] = {}
		for j = 1, cols do
			mat[i][j] = 0
		end
	end
	return matrix:new(mat)
end

function matrix.identity(n)
	return id_matrix_(n)
end

-- Homogeneous transformation matrix: scaling (sx, sy), then rotation of
-- angle phi, then translation (tx, ty).
function matrix.htm(phi, tx, ty, sx, sy)
	phi = phi or 0
	tx = tx or 0
	ty = ty or 0
	sx = sx or 1
	sy = sy or 1
	local c, s = math.cos(phi), math.sin(phi)
	return matrix.square(3,
		sx * c, -sy * s, tx,
		sx * s,  sy * c, ty,
		0, 0, 1)
end

------------------------------------------------------------------------------
-- methods
------------------------------------------------------------------------------
function matrix:get(i, j)
	if i == nil and j == nil then
		return self.set -- the whole table
	else
		return get_element_(self, i, j)
	end
end

function matrix:is_square()
	return self.rows == self.cols
end

function matrix:is_diagonal()
	return is_diagonal_matrix_(self)
end

function matrix:is_orthogonal()
	return is_orthogonal_matrix_(self)
end

function matrix:inverse()
	local inv, err = inv_matrix_(self)
	if not inv then
		error("tkz-elements: matrix:inverse: " .. err, 2)
	end
	return inv
end

function matrix:adjugate()
	local adj = adjugate_(self)
	if not adj then
		error("tkz-elements: matrix:adjugate: only 2x2 and 3x3 matrices are supported", 2)
	end
	return adj
end

function matrix:transpose()
	return transposeMatrix(self)
end

function matrix:diagonalize() -- returns two matrices D and P
	local D, P = diagonalize_(self)
	return D, P
end

function matrix:homogenization()
	return homogenization_(self)
end

-- A similarity keeps the class of the objects; another affine map changes
-- a circle into an ellipse, a square or a rectangle into a parallelogram.
function matrix:htm_apply(...)
	local f, similar, degenerate = htm_map_(self)
	local t, n = affine_objects_(f, "matrix:htm_apply", similar, degenerate, ...)
	return table.unpack(t, 1, n)
end

function matrix:print(style, fmt)
	style = style or "bmatrix"
	fmt = fmt or 0
	return print_matrix(self, style, fmt)
end

function matrix:gauss_jordan()
	local A = copy_matrix_(self.set)
	local m, n = self.rows, self.cols
	local r, lead = 1, 1

	while r <= m and lead <= n do
		-- pivot row i >= r with a nonzero entry in column lead
		local i = r
		while i <= m and math.abs(A[i][lead]) <= tkz.epsilon do
			i = i + 1
		end

		if i > m then
			lead = lead + 1
		else
			-- swap i <-> r
			A[i], A[r] = A[r], A[i]

			-- normalize the pivot row
			local pivot = A[r][lead]
			for j = lead, n do
				A[r][j] = A[r][j] / pivot
			end

			-- eliminate all other rows
			for k = 1, m do
				if k ~= r then
					local factor = A[k][lead]
					if math.abs(factor) > tkz.epsilon then
						for j = lead, n do
							A[k][j] = A[k][j] - factor * A[r][j]
						end
					end
				end
			end

			r = r + 1
			lead = lead + 1
		end
	end

	return matrix:new(A)
end

function matrix:rank()
	local A = copy_matrix_(self.set)
	local m, n = self.rows, self.cols
	local rank = 0
	local row = 1

	for col = 1, n do
		-- search a pivot
		local pivot_row = nil
		for i = row, m do
			if math.abs(A[i][col]) > tkz.epsilon then
				pivot_row = i
				break
			end
		end

		if pivot_row then
			A[row], A[pivot_row] = A[pivot_row], A[row]

			-- normalize the pivot row
			local pivot = A[row][col]
			for j = col, n do
				A[row][j] = A[row][j] / pivot
			end

			-- eliminate below
			for i = row + 1, m do
				local factor = A[i][col]
				if math.abs(factor) > tkz.epsilon then
					for j = col, n do
						A[i][j] = A[i][j] - factor * A[row][j]
					end
				end
			end

			rank = rank + 1
			row = row + 1

			if row > m then break end
		end
	end

	return rank
end

-- [A | B]
function matrix:augment_right(B)
	if not is_matrix(B) or B.rows ~= self.rows then
		error("tkz-elements: matrix:augment_right: a matrix with "
			.. self.rows .. " rows is expected", 2)
	end
	local R = {}
	for i = 1, self.rows do
		R[i] = {}
		for j = 1, self.cols do
			R[i][j] = self.set[i][j]
		end
		for j = 1, B.cols do
			R[i][self.cols + j] = B.set[i][j]
		end
	end
	return matrix:new(R)
end

function matrix:submatrix(r1, r2, c1, c2)
	if not (r1 and r2 and c1 and c2)
		or r1 < 1 or c1 < 1 or r2 > self.rows or c2 > self.cols
		or r1 > r2 or c1 > c2 then
		error("tkz-elements: matrix:submatrix: invalid bounds for a "
			.. self.rows .. "x" .. self.cols .. " matrix", 2)
	end
	local R = {}
	for i = r1, r2 do
		local row = {}
		for j = c1, c2 do
			row[#row + 1] = self.set[i][j]
		end
		R[#R + 1] = row
	end
	return matrix:new(R)
end

------------------------------------------------------------------------------
-- internal methods
------------------------------------------------------------------------------
function matrix:k_mul_(n)
	return k_mul_matrix_(n, self)
end

function matrix:swap_rows_(i, j)
	local A = copy_matrix_(self.set)
	A[i], A[j] = A[j], A[i]
	return matrix:new(A)
end

function matrix:k_mul_row_(row, k)
	local A = copy_matrix_(self.set)
	for j = 1, self.cols do
		A[row][j] = A[row][j] * k
	end
	return matrix:new(A)
end

-- adds k times the row source_row to the row target_row
function matrix:add_k_mul_row_(target_row, source_row, k)
	local A = copy_matrix_(self.set)
	for j = 1, self.cols do
		A[target_row][j] = A[target_row][j] + A[source_row][j] * k
	end
	return matrix:new(A)
end

function matrix:swap_rows_inplace_(i, j)
	self.set[i], self.set[j] = self.set[j], self.set[i]
	self.det = nil
	return self
end

function matrix:k_mul_row_inplace_(row, k)
	for j = 1, self.cols do
		self.set[row][j] = self.set[row][j] * k
	end
	self.det = nil
	return self
end

function matrix:add_k_mul_row_inplace_(target_row, source_row, k)
	for j = 1, self.cols do
		self.set[target_row][j] = self.set[target_row][j] + self.set[source_row][j] * k
	end
	self.det = nil
	return self
end

function matrix:left_block_(k)
	return self:submatrix(1, self.rows, 1, k)
end

function matrix:right_block_(k)
	return self:submatrix(1, self.rows, self.cols - k + 1, self.cols)
end

return matrix
