-- File: tkz_elements_functions_matrices.lua
-- Copyright (c) 2026 Alain Matthes
-- SPDX-License-Identifier: LPPL-1.3c
-- Maintainer: Alain Matthes

-- ----------------------------------------------------------------------------
-- display
-- ----------------------------------------------------------------------------
function print_matrix(m, mstyle, fmt)
	mstyle = mstyle or "bmatrix"
	fmt = fmt or 0

	-- a matrix object: use its table of rows
	if m.type == "matrix" then
		m = m.set
	end

	tex.sprint("$")
	tex.sprint("\\begin{" .. mstyle .. "}")
	for i = 1, #m do
		for j = 1, #m[1] do
			tex.sprint(tkz_display_(m[i][j], fmt))
			if j < #m[1] then
				tex.sprint(" & ")
			end
		end
		tex.sprint("\\\\")
	end
	tex.sprint("\\end{" .. mstyle .. "}")
	tex.sprint("$")
end

function print_array(M)
	local mdata = (M.type == "matrix" and M.set or M)
	tex.sprint("\\{%")
	for i = 1, #mdata do
		local row = mdata[i]
		local row_str = "{\\{"
		for j = 1, #row do
			row_str = row_str .. " " .. tostring(row[j])
			if j < #row then
				row_str = row_str .. "  ,"
			end
		end
		if i < #mdata then
			row_str = row_str .. " \\}},"
		else
			row_str = row_str .. " \\}}"
		end
		tex.sprint(row_str)
	end
	tex.sprint("\\}")
end

-- ----------------------------------------------------------------------------
-- helpers
-- ----------------------------------------------------------------------------
function copy_matrix_(M)
	local C = {}
	for i = 1, #M do
		C[i] = {}
		for j = 1, #M[i] do
			C[i][j] = M[i][j]
		end
	end
	return C
end

-- comparison of two entries (numbers, or complex numbers given as points)
function near_entries_(x, y, EPS)
	EPS = EPS or tkz.epsilon
	if type(x) == "number" and type(y) == "number" then
		return math.abs(x - y) <= EPS
	end
	return x == y
end

local function entry_abs(x)
	if type(x) == "number" then
		return math.abs(x)
	end
	return point.abs(x)
end

-- ----------------------------------------------------------------------------
-- operations
-- ----------------------------------------------------------------------------
function mul_matrix_(A, B)
	local adata = (A.type == "matrix" and A.set or A)
	local bdata = (B.type == "matrix" and B.set or B)
	local C = {}
	for i = 1, #adata do
		C[i] = {}
		for j = 1, #bdata[1] do
			local num = adata[i][1] * bdata[1][j]
			for k = 2, #adata[1] do
				num = num + adata[i][k] * bdata[k][j]
			end
			C[i][j] = num
		end
	end
	return matrix:new(C)
end

function add_matrix_(A, B)
	local adata = (A.type == "matrix" and A.set or A)
	local bdata = (B.type == "matrix" and B.set or B)
	local S = {}
	for i = 1, #adata do
		S[i] = {}
		for j = 1, #adata[1] do
			S[i][j] = adata[i][j] + bdata[i][j]
		end
	end
	return matrix:new(S)
end

function k_mul_matrix_(n, A)
	local adata = (A.type == "matrix" and A.set or A)
	local S = {}
	for i = 1, #adata do
		S[i] = {}
		for j = 1, #adata[1] do
			S[i][j] = n * adata[i][j]
		end
	end
	return matrix:new(S)
end

function transposeMatrix(A)
	local mdata = (A.type == "matrix" and A.set or A)
	local T = {}
	for i = 1, #mdata[1] do
		T[i] = {}
		for j = 1, #mdata do
			T[i][j] = mdata[j][i]
		end
	end
	return matrix:new(T)
end

-- Determinant of a square matrix (nil if the matrix is not square).
-- Explicit formulas up to 3x3 (exact results with integers), Gaussian
-- elimination with partial pivoting beyond.
function determinant(A)
	local m = (A.type == "matrix" and A.set or A)
	local n = #m
	if n ~= #m[1] then
		return nil
	end
	if n == 1 then
		return m[1][1]
	elseif n == 2 then
		return m[1][1] * m[2][2] - m[1][2] * m[2][1]
	elseif n == 3 then
		return m[1][1] * (m[2][2] * m[3][3] - m[2][3] * m[3][2])
			- m[1][2] * (m[2][1] * m[3][3] - m[2][3] * m[3][1])
			+ m[1][3] * (m[2][1] * m[3][2] - m[2][2] * m[3][1])
	end
	local a = copy_matrix_(m)
	local det = 1
	for col = 1, n do
		-- partial pivoting
		local p = col
		for i = col + 1, n do
			if math.abs(a[i][col]) > math.abs(a[p][col]) then
				p = i
			end
		end
		if a[p][col] == 0 then
			return 0
		end
		if p ~= col then
			a[p], a[col] = a[col], a[p]
			det = -det
		end
		local pivot = a[col][col]
		det = det * pivot
		for i = col + 1, n do
			local factor = a[i][col] / pivot
			if factor ~= 0 then
				for j = col, n do
					a[i][j] = a[i][j] - factor * a[col][j]
				end
			end
		end
	end
	return det
end

function id_matrix_(n)
	local id = {}
	for i = 1, n do
		id[i] = {}
		for j = 1, n do
			id[i][j] = (i == j) and 1 or 0
		end
	end
	return matrix:new(id)
end

-- adjugate of a 2x2 or 3x3 matrix (nil otherwise)
function adjugate_(A)
	local m = (A.type == "matrix" and A.set or A)
	if #m ~= #m[1] then
		return nil
	end
	if #m == 2 then
		local a, b, c, d = m[2][2], -m[1][2], -m[2][1], m[1][1]
		return matrix:new({ { a, b }, { c, d } })
	elseif #m == 3 then
		local a, b, c = m[1][1], m[1][2], m[1][3]
		local d, e, f = m[2][1], m[2][2], m[2][3]
		local g, h, i = m[3][1], m[3][2], m[3][3]
		return transposeMatrix(matrix:new({
			{ e * i - f * h, -(d * i - f * g), d * h - e * g },
			{ -(b * i - c * h), a * i - c * g, -(a * h - b * g) },
			{ b * f - c * e, -(a * f - c * d), a * e - b * d },
		}))
	else
		return nil
	end
end

-- inverse by Gauss-Jordan on [A | I]
-- returns the inverse, or nil and a message
function inv_matrix_(A)
	if not A:is_square() then
		return nil, "the matrix is not square"
	end
	if math.abs(A.det) < tkz.epsilon then
		return nil, "the matrix is not invertible (det close to 0)"
	end
	local n = A.rows
	local aug = {}
	for i = 1, n do
		aug[i] = {}
		for j = 1, n do
			aug[i][j] = A.set[i][j]
			aug[i][n + j] = (i == j) and 1 or 0
		end
	end
	local R = matrix:new(aug):gauss_jordan()
	local inv = {}
	for i = 1, n do
		inv[i] = {}
		for j = 1, n do
			inv[i][j] = R.set[i][n + j]
		end
	end
	return matrix:new(inv)
end

function inv_matrix(A)
	local inv, err = inv_matrix_(A)
	if not inv then
		error("tkz-elements: inv_matrix: " .. err, 2)
	end
	return inv
end

-- Diagonalization of a 2x2 matrix: returns D (eigenvalues) and P
-- (eigenvectors as columns) with A = P D P^-1. Complex eigenvalues are
-- given as points (complex numbers).
function diagonalize_(A)
	if A.rows ~= 2 or A.cols ~= 2 then
		error("tkz-elements: matrix:diagonalize: only 2x2 matrices are supported", 3)
	end
	local m = A.set
	local a, b = m[1][1], m[1][2]
	local c, d = m[2][1], m[2][2]
	local EPS = tkz.epsilon

	-- already diagonal
	if math.abs(b) <= EPS and math.abs(c) <= EPS then
		return matrix:new({ { a, 0 }, { 0, d } }), id_matrix_(2)
	end

	local trace = a + d
	local det = a * d - b * c
	local D = trace * trace - 4 * det
	if math.abs(D) <= EPS then
		error("tkz-elements: matrix:diagonalize: the matrix is not diagonalizable "
			.. "(double eigenvalue)", 3)
	end

	local va1, va2
	if D > 0 then
		va1 = (trace + math.sqrt(D)) / 2
		va2 = (trace - math.sqrt(D)) / 2
	else
		va1 = point(trace / 2, math.sqrt(-D) / 2)
		va2 = point(trace / 2, -math.sqrt(-D) / 2)
	end

	-- eigenvector of va: (1, (va - a) / b) if b ~= 0, else ((va - d) / c, 1)
	local P
	if math.abs(b) > EPS then
		P = matrix:new({ { 1, 1 }, { (va1 - a) / b, (va2 - a) / b } })
	else
		P = matrix:new({ { (va1 - d) / c, (va2 - d) / c }, { 1, 1 } })
	end
	return matrix:new({ { va1, 0 }, { 0, va2 } }), P
end

function is_diagonal_matrix_(A)
	if not A:is_square() then
		return false
	end
	for i = 1, A.rows do
		for j = 1, A.cols do
			if i ~= j and entry_abs(A.set[i][j]) > tkz.epsilon then
				return false
			end
		end
	end
	return true
end

-- A is orthogonal if A^T A = I
function is_orthogonal_matrix_(A)
	if not A:is_square() then
		return false
	end
	return mul_matrix_(transposeMatrix(A), A) == id_matrix_(A.rows)
end

-- ----------------------------------------------------------------------------
-- homogeneous transformation matrices
-- ----------------------------------------------------------------------------
function homogenization_(A)
	local m = (A.type == "matrix" and A.set or A)
	if A.cols ~= 1 or A.rows ~= 2 then
		error("tkz-elements: matrix:homogenization: a 2x1 column matrix is expected", 3)
	end
	return matrix:new({ { m[1][1] }, { m[2][1] }, { 1 } })
end

function get_element_(A, i, j)
	local m = (A.type == "matrix" and A.set or A)
	if m[i] and m[i][j] then
		return m[i][j]
	end
end

function get_htm_point(A)
	local m = (A.type == "matrix" and A.set or A)
	return point:new(m[1][1], m[2][1])
end

-- Affine map defined by a 3x3 homogeneous matrix.
-- Returns f (point -> point), similar (f is a similarity) and degenerate
-- (f is not invertible); used by matrix:htm_apply.
function htm_map_(M)
	local name = "matrix:htm_apply"
	if M.rows ~= 3 or M.cols ~= 3 then
		error("tkz-elements: " .. name .. ": a 3x3 matrix is expected", 3)
	end
	local m = M.set
	local EPS = tkz.epsilon
	if math.abs(m[3][1]) > EPS or math.abs(m[3][2]) > EPS or math.abs(m[3][3] - 1) > EPS then
		error("tkz-elements: " .. name .. ": the last row must be (0, 0, 1)", 3)
	end
	local a, b, tx = m[1][1], m[1][2], m[1][3]
	local c, d, ty = m[2][1], m[2][2], m[2][3]
	local f = function(p)
		return point(a * p.re + b * p.im + tx, c * p.re + d * p.im + ty)
	end
	-- similarity: the columns of the linear part are orthogonal and of the
	-- same norm
	local n1, n2 = a * a + c * c, b * b + d * d
	local scale = math.max(n1, n2, 1)
	local similar = math.abs(n1 - n2) <= EPS * scale and math.abs(a * b + c * d) <= EPS * scale
	local degenerate = math.abs(a * d - b * c) <= EPS * scale
	return f, similar, degenerate
end
