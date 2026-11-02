-- tkz_elements_3d.lua
-- Prototype of a 3D extension of tkz-elements (not part of the package).
-- Loaded after tkz-elements with:  require("tkz_elements_3d")
-- 3D points live in the table z3; the camera projects them to 2D points of
-- tkz-elements (class point), stored in z and drawn with tkz-euclide.

local EPS = 1e-9

z3 = z3 or {}   -- 3D points (like z for the 2D points)
TD = TD or {}   -- counts of visible/hidden pieces, per group

---------------------------------------------------------------------------
-- point3
---------------------------------------------------------------------------
point3 = {}
point3.__index = point3

function point3:new(x, y, zz)
  if type(x) ~= "number" or type(y) ~= "number" or type(zz) ~= "number" then
    error("tkz-elements 3d: point3: three numbers are expected", 2)
  end
  return setmetatable({ x = x, y = y, z = zz, type = "point3" }, point3)
end
setmetatable(point3, { __call = function(cls, ...) return cls:new(...) end })

local P3 = function(x, y, zz) return point3:new(x, y, zz) end

local function is_point3(p) return getmetatable(p) == point3 end

function point3.__add(a, b) return P3(a.x + b.x, a.y + b.y, a.z + b.z) end
function point3.__sub(a, b) return P3(a.x - b.x, a.y - b.y, a.z - b.z) end
function point3.__unm(a) return P3(-a.x, -a.y, -a.z) end
function point3.__mul(a, b)
  if type(a) == "number" then return P3(a * b.x, a * b.y, a * b.z) end
  if type(b) == "number" then return P3(a.x * b, a.y * b, a.z * b) end
  error("tkz-elements 3d: point3 * point3: use dot or cross", 2)
end
function point3.__div(a, k) return P3(a.x / k, a.y / k, a.z / k) end
function point3.__tostring(a)
  return string.format("point3(%.5f, %.5f, %.5f)", a.x, a.y, a.z)
end

function point3.dot(a, b) return a.x * b.x + a.y * b.y + a.z * b.z end
function point3.cross(a, b)
  return P3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
end
function point3.norm(a) return math.sqrt(a:dot(a)) end
function point3.unit(a)
  local n = a:norm()
  if n < EPS then error("tkz-elements 3d: unit of a null vector", 2) end
  return a / n
end

function midpoint3(a, b) return (a + b) / 2 end
function barycenter3(list)
  local s = P3(0, 0, 0)
  for _, p in ipairs(list) do s = s + p end
  return s / #list
end

-- two unit vectors e1, e2 such that (e1, e2, n) is a direct orthonormal basis
local function basis_of(n)
  n = n:unit()
  local a = math.abs(n.x) < 0.9 and P3(1, 0, 0) or P3(0, 1, 0)
  local e1 = a:cross(n):unit()
  local e2 = n:cross(e1)
  return e1, e2, n
end

---------------------------------------------------------------------------
-- camera: orthographic (default), perspective or cavalier
--   camera{azimuth = 30, elevation = 20}                 (degrees)
--   camera{azimuth = 30, elevation = 20, perspective = true, distance = 15}
--   camera{cavalier = true, angle = 30, ratio = 0.5}
-- Cavalier (oblique parallel projection): the plane yOz is drawn in true
-- size (y to the right, z upwards); the receding axis x is drawn at the
-- given angle below the horizontal, toward the left, with the given ratio.
---------------------------------------------------------------------------
camera = {}
camera.__index = camera

function camera:new(t)
  t = t or {}
  local th = math.rad(t.azimuth or 30)
  local ph = math.rad(t.elevation or 20)
  local c = {
    target = t.target or P3(0, 0, 0),
    perspective = t.perspective or false,
    distance = t.distance or 15,
    scale = t.scale or 1,
    type = "camera",
  }
  -- d: unit vector toward the viewer; (u, v): screen axes; u x v = d
  c.d = P3(math.cos(ph) * math.cos(th), math.cos(ph) * math.sin(th), math.sin(ph))
  c.u = P3(-math.sin(th), math.cos(th), 0)
  c.v = c.d:cross(c.u)
  c.eye = c.target + c.distance * c.d
  if t.cavalier then
    c.cavalier, c.perspective = true, false
    c.angle = math.rad(t.angle or 30)
    c.ratio = t.ratio or 0.5
    c.kx = c.ratio * math.cos(c.angle)
    c.kz = c.ratio * math.sin(c.angle)
    -- direction of projection (toward the viewer): its image is a point
    c.d = P3(1, c.kx, c.kz):unit()
    c.u, c.v = P3(0, 1, 0), P3(0, 0, 1)
  end
  return setmetatable(c, camera)
end
setmetatable(camera, { __call = function(cls, ...) return cls:new(...) end })

-- 2D point (class point of tkz-elements)
function camera:project(p)
  if not is_point3(p) then error("tkz-elements 3d: project: a point3 is expected", 2) end
  local q = p - self.target
  if self.cavalier then
    return point(self.scale * (q.y - self.kx * q.x), self.scale * (q.z - self.kz * q.x))
  end
  local x, y = q:dot(self.u), q:dot(self.v)
  if self.perspective then
    local depth = (self.eye - p):dot(self.d)
    if depth <= EPS then error("tkz-elements 3d: project: point behind the camera", 2) end
    local f = self.distance / depth
    x, y = x * f, y * f
  end
  return point(self.scale * x, self.scale * y)
end

-- unit vector from p toward the camera
function camera:toward(p)
  if self.perspective then return (self.eye - p):unit() end
  return self.d
end

-- stores the projections in z: cam:to_z{A = z3.A, B = z3.B} or cam:to_z("A", z3.A)
function camera:to_z(name, p)
  if type(name) == "table" then
    for k, q in pairs(name) do z[k] = self:project(q) end
  else
    z[name] = self:project(p)
  end
end

---------------------------------------------------------------------------
-- line3, plane
---------------------------------------------------------------------------
line3 = {}
line3.__index = line3
function line3:new(a, b)
  return setmetatable({ pa = a, pb = b, mid = midpoint3(a, b), type = "line3" }, line3)
end
setmetatable(line3, { __call = function(cls, ...) return cls:new(...) end })
function line3:point(t) return self.pa + t * (self.pb - self.pa) end

plane = {}
plane.__index = plane
-- plane(A, B, C) or plane(P, normal)
function plane:new(a, b, c)
  local n = c and (b - a):cross(c - a) or b
  return setmetatable({ point = a, normal = n:unit(), type = "plane" }, plane)
end
setmetatable(plane, { __call = function(cls, ...) return cls:new(...) end })

---------------------------------------------------------------------------
-- geometry of space: projections, intersections (lines, planes, spheres)
---------------------------------------------------------------------------
local GEPS = 1e-9

function line3:dir() return (self.pb - self.pa):unit() end

-- orthogonal projection of p on the line
function line3:project(p)
  local u = self:dir()
  return self.pa + u:dot(p - self.pa) * u
end
function line3:distance(p) return (p - self:project(p)):norm() end

-- intersection with another line (nil if parallel or not coplanar)
function line3:intersect_line(L)
  local u, v, w = self.pb - self.pa, L.pb - L.pa, L.pa - self.pa
  local n = u:cross(v)
  local nn = n:dot(n)
  if nn < GEPS * u:dot(u) * v:dot(v) then return nil end           -- parallel
  if math.abs(w:dot(n)) > 1e-7 * math.sqrt(nn) * (w:norm() + 1) then
    return nil                                                      -- skew lines
  end
  local t = w:cross(v):dot(n) / nn
  return self.pa + t * u
end

-- intersection with a plane (nil if parallel)
function line3:intersect_plane(P) return P:intersect_line(self) end

-- signed distance (positive on the side of the normal)
function plane:distance(p) return self.normal:dot(p - self.point) end
function plane:contains(p, eps) return math.abs(self:distance(p)) <= (eps or 1e-9) end
function plane:project(p) return p - self:distance(p) * self.normal end

function plane:intersect_line(L)
  local u = L.pb - L.pa
  local den = self.normal:dot(u)
  if math.abs(den) < GEPS * u:norm() then return nil end
  return L.pa + (-self:distance(L.pa) / den) * u
end

-- line of intersection of two planes (nil if parallel)
function plane:intersect_plane(Q)
  local u = self.normal:cross(Q.normal)
  local nu = u:norm()
  if nu < 1e-9 then return nil end
  -- point of both planes: solve with the third direction u
  local n1, n2 = self.normal, Q.normal
  local d1, d2 = n1:dot(self.point), n2:dot(Q.point)
  local p = (d1 * n2:cross(u) + d2 * u:cross(n1)) / (nu * nu)
  return line3:new(p, p + u / nu)
end

-- circle of intersection with a sphere: center, radius (nil if none)
function plane:intersect_sphere(S)
  local d = self:distance(S.center)
  if math.abs(d) > S.radius then return nil end
  return self:project(S.center), math.sqrt(S.radius ^ 2 - d * d)
end

-- a rectangle of the plane, to draw it: center c (default: projection of
-- the origin), half-sides a and b, rotation angle (degrees) in the plane
function plane:patch(a, b, c, angle, opts)
  c = c and self:project(c) or self:project(P3(0, 0, 0))
  local e1, e2 = basis_of(self.normal)
  local t = math.rad(angle or 0)
  local u = math.cos(t) * e1 + math.sin(t) * e2
  local v = self.normal:cross(u)
  return polygon3:new({ c - a * u - b * v, c + a * u - b * v, c + a * u + b * v, c - a * u + b * v }, opts)
end

-- cube with the names used in French textbooks: ABCD bottom face (A front
-- left, B front right, C back right, D back left), EFGH above (E over A...).
-- Front face ABFE in the plane x = o.x + a (cavalier view: x toward the viewer)
function named_cube(a, o, opaque)
  o = o or P3(0, 0, 0)
  local V = {
    A = o + P3(a, 0, 0), B = o + P3(a, a, 0), C = o + P3(0, a, 0), D = o,
    E = o + P3(a, 0, a), F = o + P3(a, a, a), G = o + P3(0, a, a), H = o + P3(0, 0, a),
  }
  local P = polyhedron:new({ V.A, V.B, V.C, V.D, V.E, V.F, V.G, V.H },
    { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 1, 2, 6, 5 }, { 2, 3, 7, 6 }, { 3, 4, 8, 7 }, { 4, 1, 5, 8 } },
    opaque)
  P.V = V
  return P
end

---------------------------------------------------------------------------
-- sphere (opaque by default)
---------------------------------------------------------------------------
sphere = {}
sphere.__index = sphere
function sphere:new(c, r, opaque)
  if opaque == nil then opaque = true end
  return setmetatable({ center = c, radius = r, opaque = opaque, type = "sphere" }, sphere)
end
setmetatable(sphere, { __call = function(cls, ...) return cls:new(...) end })

-- true if the ray x + t w (t > 0) goes through the sphere
function sphere:blocks(x, w)
  local q = x - self.center
  local b = w:dot(q)
  local c = q:dot(q) - self.radius ^ 2
  local disc = b * b - c
  if disc <= EPS * self.radius ^ 2 then return false end
  local s = math.sqrt(disc)
  local t1, t2 = -b - s, -b + s
  local tol = 1e-7 * self.radius
  return t2 - math.max(t1, tol) > tol
end

-- apparent contour: circle (center, normal, radius) on the sphere
function sphere:contour(cam)
  if not cam.perspective then
    return self.center, cam.d, self.radius
  end
  local de = cam.eye - self.center
  local d = de:norm()
  local r = self.radius
  if d <= r then error("tkz-elements 3d: the camera is inside the sphere", 2) end
  local n = de / d
  return self.center + (r * r / d) * n, n, r * math.sqrt(1 - (r / d) ^ 2)
end

---------------------------------------------------------------------------
-- convex polyhedra
---------------------------------------------------------------------------
polyhedron = {}
polyhedron.__index = polyhedron

-- vertices: list of point3; faces: lists of indices
function polyhedron:new(vertices, faces, opaque)
  if opaque == nil then opaque = true end
  local P = { vertices = vertices, faces = {}, edges = {}, opaque = opaque, type = "polyhedron" }
  P.centroid = barycenter3(vertices)
  local index = {}
  for k, f in ipairs(faces) do
    local v0, v1, v2 = vertices[f[1]], vertices[f[2]], vertices[f[3]]
    local n = (v1 - v0):cross(v2 - v0):unit()
    if n:dot(v0 - P.centroid) < 0 then n = -n end
    local pts = {}
    for i, j in ipairs(f) do pts[i] = vertices[j] end
    P.faces[k] = { idx = f, normal = n, h = n:dot(v0), center = barycenter3(pts) }
    for i = 1, #f do
      local a, b = f[i], f[i % #f + 1]
      local key = math.min(a, b) .. "-" .. math.max(a, b)
      if not index[key] then
        index[key] = { a = math.min(a, b), b = math.max(a, b), faces = {} }
        table.insert(P.edges, index[key])
      end
      table.insert(index[key].faces, k)
    end
  end
  return setmetatable(P, polyhedron)
end
setmetatable(polyhedron, { __call = function(cls, ...) return cls:new(...) end })

-- level 1: a face is seen if its outward normal points toward the camera
function polyhedron:face_visible(k, cam)
  local f = self.faces[k]
  return f.normal:dot(cam:toward(f.center)) > EPS
end

-- true if the ray x + t w (t > 0) goes through the (convex) solid
function polyhedron:blocks(x, w)
  local tmin, tmax = 1e-7, math.huge
  for _, f in ipairs(self.faces) do
    local den = f.normal:dot(w)
    local num = f.h - f.normal:dot(x)
    if math.abs(den) < EPS then
      if num < -1e-9 then return false end
    else
      local t = num / den
      if den > 0 then tmax = math.min(tmax, t) else tmin = math.max(tmin, t) end
    end
  end
  return tmax - tmin > 1e-7
end

function cube(c, a, opaque)
  local h = a / 2
  local V = {
    c + P3(-h, -h, -h), c + P3(h, -h, -h), c + P3(h, h, -h), c + P3(-h, h, -h),
    c + P3(-h, -h, h),  c + P3(h, -h, h),  c + P3(h, h, h),  c + P3(-h, h, h),
  }
  local F = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 1, 2, 6, 5 },
              { 2, 3, 7, 6 }, { 3, 4, 8, 7 }, { 4, 1, 5, 8 } }
  return polyhedron:new(V, F, opaque)
end

function tetrahedron(a, b, c, d, opaque)
  return polyhedron:new({ a, b, c, d }, { { 1, 2, 3 }, { 1, 2, 4 }, { 1, 3, 4 }, { 2, 3, 4 } }, opaque)
end

-- pyramid with base polygon (list of point3, convex) and apex s
function pyramid(base, s, opaque)
  local V, F, n = {}, {}, #base
  local bf = {}
  for i, p in ipairs(base) do V[i] = p; bf[i] = i end
  V[n + 1] = s
  F[1] = bf
  for i = 1, n do F[#F + 1] = { i, i % n + 1, n + 1 } end
  local P = polyhedron:new(V, F, opaque)
  P.apex, P.base_center = s, barycenter3(base)
  return P
end

-- prism with base polygon (list of point3, convex) and translation vector v
function prism(base, v, opaque)
  local V, F, n = {}, {}, #base
  local b1, b2 = {}, {}
  for i, p in ipairs(base) do
    V[i], V[n + i] = p, p + v
    b1[i], b2[i] = i, n + i
  end
  F[1], F[2] = b1, b2
  for i = 1, n do
    local j = i % n + 1
    F[#F + 1] = { i, j, n + j, n + i }
  end
  return polyhedron:new(V, F, opaque)
end

-- regular polygon of n vertices (circle of center c, normal nrm, radius r)
function regular_polygon3(c, nrm, r, n, phase)
  local e1, e2 = basis_of(nrm)
  local L = {}
  for k = 0, n - 1 do
    local a = 2 * math.pi * k / n + (phase or 0)
    L[#L + 1] = c + (r * math.cos(a)) * e1 + (r * math.sin(a)) * e2
  end
  return L
end

---------------------------------------------------------------------------
-- cylinder and cone (right circular). For the hidden parts they are
-- replaced by an inscribed prism / pyramid with many faces ("hull").
---------------------------------------------------------------------------
local HULL = 256

cylinder = {}
cylinder.__index = cylinder
-- cylinder(B, T, r): bases centered at B and T, radius r
function cylinder:new(b, t, r, opaque)
  if opaque == nil then opaque = true end
  local C = { base = b, top = t, radius = r, opaque = opaque, type = "cylinder" }
  C.axis = (t - b):unit()
  C.height = (t - b):norm()
  C.centroid = midpoint3(b, t)
  C.hull = prism(regular_polygon3(b, C.axis, r, HULL), t - b, true)
  return setmetatable(C, cylinder)
end
setmetatable(cylinder, { __call = function(cls, ...) return cls:new(...) end })
function cylinder:blocks(x, w) return self.hull:blocks(x, w) end

cone = {}
cone.__index = cone
-- cone(B, S, r): base centered at B, radius r, apex S
function cone:new(b, s, r, opaque)
  if opaque == nil then opaque = true end
  local C = { base = b, apex = s, radius = r, opaque = opaque, type = "cone" }
  C.axis = (s - b):unit()
  C.height = (s - b):norm()
  C.centroid = b + 0.25 * (s - b)
  C.hull = pyramid(regular_polygon3(b, C.axis, r, HULL), s, true)
  return setmetatable(C, cone)
end
setmetatable(cone, { __call = function(cls, ...) return cls:new(...) end })
function cone:blocks(x, w) return self.hull:blocks(x, w) end

-- unit radial vectors n with cos(angle(n, q_perp)) = k  (0, 1 or 2 solutions)
local function radial_dirs(axis, q, k)
  local qp = q - q:dot(axis) * axis
  local rho = qp:norm()
  if rho < EPS then return {} end
  local c = k / rho
  if c >= 1 - 1e-12 or c <= -1 + 1e-12 then return {} end
  local u = qp / rho
  local v = axis:cross(u)
  local sn = math.sqrt(1 - c * c)
  return { c * u + sn * v, c * u - sn * v }
end

-- radial directions of the contour generators
function cylinder:contour_dirs(cam)
  if cam.perspective then
    -- n . (E - B) = r
    return radial_dirs(self.axis, cam.eye - self.base, self.radius)
  end
  -- n . d = 0
  return radial_dirs(self.axis, cam.d, 0)
end

function cone:contour_dirs(cam)
  -- tangent plane along a generator: normal h n + r a ; it contains q = E - S
  -- (perspective) or d (orthographic):  h (n . q) + r (a . q) = 0
  local q = cam.perspective and (cam.eye - self.apex) or cam.d
  local qp = q - q:dot(self.axis) * self.axis
  local rho = qp:norm()
  if rho < EPS then return {} end
  return radial_dirs(self.axis, q, -self.radius * q:dot(self.axis) / self.height)
end

-- stores the projected vertices in z: P:vertices_to_z(cam, {"A","B",...})
function polyhedron:vertices_to_z(cam, names)
  for i, name in ipairs(names) do z[name] = cam:project(self.vertices[i]) end
end

---------------------------------------------------------------------------
-- polygon3: planar convex polygon (a face alone: box without lid, flap,
-- plane z = z0 ...). opts: color (fill), opaque (default true)
---------------------------------------------------------------------------
polygon3 = {}
polygon3.__index = polygon3
function polygon3:new(pts, opts)
  opts = opts or {}
  if #pts < 3 then error("tkz-elements 3d: polygon3: three points at least", 2) end
  local n = P3(0, 0, 0)
  for i = 1, #pts do                       -- Newell's normal
    local a, b = pts[i], pts[i % #pts + 1]
    n = n + P3((a.y - b.y) * (a.z + b.z), (a.z - b.z) * (a.x + b.x), (a.x - b.x) * (a.y + b.y))
  end
  n = n:unit()
  local F = { pts = pts, normal = n, h = n:dot(pts[1]), center = barycenter3(pts),
              color = opts.color, fill = opts.fill, type = "polygon3" }
  if opts.opaque == nil then F.opaque = true else F.opaque = opts.opaque end
  return setmetatable(F, polygon3)
end
setmetatable(polygon3, { __call = function(cls, ...) return cls:new(...) end })

-- true if the ray x + t w (t > 0) crosses the polygon
function polygon3:blocks(x, w)
  local den = self.normal:dot(w)
  if math.abs(den) < EPS then return false end
  local t = (self.h - self.normal:dot(x)) / den
  if t <= 1e-7 then return false end
  local p = x + t * w
  local pts, n = self.pts, self.normal
  for i = 1, #pts do
    local a, b = pts[i], pts[i % #pts + 1]
    if (b - a):cross(p - a):dot(n) < -1e-9 then return false end
  end
  return true
end

-- section of a convex polyhedron by a plane: list of point3 (convex
-- polygon, ordered), or nil
function polyhedron:section(PL)
  local pts = {}
  local function add(p)
    for _, q in ipairs(pts) do if (p - q):norm() < 1e-7 then return end end
    pts[#pts + 1] = p
  end
  for _, e in ipairs(self.edges) do
    local a, b = self.vertices[e.a], self.vertices[e.b]
    local da, db = PL:distance(a), PL:distance(b)
    if math.abs(da) < 1e-9 then add(a) end
    if math.abs(db) < 1e-9 then add(b) end
    if da * db < 0 then add(a + (da / (da - db)) * (b - a)) end
  end
  if #pts < 3 then return nil end
  local c = barycenter3(pts)
  local e1, e2 = basis_of(PL.normal)
  table.sort(pts, function(p, q)
    return math.atan((p - c):dot(e2), (p - c):dot(e1)) < math.atan((q - c):dot(e2), (q - c):dot(e1))
  end)
  return pts
end

-- the two parts of a convex polygon3 on each side of a plane
-- (the polygon itself if it does not cross the plane)
function polygon3:split(PL)
  local A, B = {}, {}
  local n = #self.pts
  for i = 1, n do
    local p, q = self.pts[i], self.pts[i % n + 1]
    local dp, dq = PL:distance(p), PL:distance(q)
    if dp >= 0 then A[#A + 1] = p end
    if dp <= 0 then B[#B + 1] = p end
    if dp * dq < 0 then
      local m = p + (dp / (dp - dq)) * (q - p)
      A[#A + 1] = m; B[#B + 1] = m
    end
  end
  local opts = { color = self.color, fill = self.fill, opaque = self.opaque }
  if #A < 3 or #B < 3 then return { self } end
  return { polygon3:new(A, opts), polygon3:new(B, opts) }
end

-- the faces of a polyhedron as polygon3 (to fill them)
function polyhedron:polygons(opts)
  local L = {}
  for _, f in ipairs(self.faces) do
    local pts = {}
    for i, j in ipairs(f.idx) do pts[i] = self.vertices[j] end
    L[#L + 1] = polygon3:new(pts, opts)
  end
  return L
end

-- open box (carton): corner o, length L (x), width W (y), height H (z);
-- opts.flaps = {front, right, back, left}: opening angles of the flaps in
-- degrees from the vertical (nil: no flap); opts.color.
-- Returns the list of polygon3 and the table of the vertices A ... H
function open_box(o, L, W, H, opts)
  opts = opts or {}
  local c = opts.color or "orange"
  local ex, ey, ez = P3(L, 0, 0), P3(0, W, 0), P3(0, 0, H)
  local V = { A = o, B = o + ex, C = o + ex + ey, D = o + ey }
  V.E, V.F, V.G, V.H = V.A + ez, V.B + ez, V.C + ez, V.D + ez
  local F = {
    polygon3({ V.A, V.B, V.C, V.D }, { color = c }),
    polygon3({ V.A, V.B, V.F, V.E }, { color = c }),
    polygon3({ V.B, V.C, V.G, V.F }, { color = c }),
    polygon3({ V.C, V.D, V.H, V.G }, { color = c }),
    polygon3({ V.D, V.A, V.E, V.H }, { color = c }),
  }
  local fl = opts.flaps or {}
  local hinges = { { V.E, V.F, P3(0, -1, 0) }, { V.F, V.G, P3(1, 0, 0) },
                   { V.G, V.H, P3(0, 1, 0) },  { V.H, V.E, P3(-1, 0, 0) } }
  local len = (opts.flap or W / 2)
  for i, h in ipairs(hinges) do
    if fl[i] then
      local phi = math.rad(fl[i])
      local d = len * (math.cos(phi) * P3(0, 0, 1) + math.sin(phi) * h[3])
      F[#F + 1] = polygon3({ h[1], h[2], h[2] + d, h[1] + d }, { color = c .. "!60!white" })
    end
  end
  return F, V
end

---------------------------------------------------------------------------
-- transformations of space: functions point3 -> point3
---------------------------------------------------------------------------
function translation3(v) return function(p) return p + v end end

-- scaling of center c (default O) with ratios kx, ky, kz
function scale3(kx, ky, kz, c)
  c = c or P3(0, 0, 0)
  return function(p)
    local q = p - c
    return c + P3(kx * q.x, ky * q.y, kz * q.z)
  end
end

-- rotation around the axis (c, u), angle in radians (Rodrigues' formula)
function rotation3(c, u, angle)
  local k = u:unit()
  local co, si = math.cos(angle), math.sin(angle)
  return function(p)
    local v = p - c
    return c + co * v + si * k:cross(v) + ((1 - co) * k:dot(v)) * k
  end
end

-- composition: compose3(f, g)(p) = f(g(p))
function compose3(f, g) return function(p) return f(g(p)) end end

---------------------------------------------------------------------------
-- graph3: vertices (named point3) and edges, in space
-- (for drawings in the style of tkz-graph)
---------------------------------------------------------------------------
graph3 = {}
graph3.__index = graph3
function graph3:new()
  return setmetatable({ orig = {}, names = {}, edges = {}, f = nil, type = "graph3" }, graph3)
end
setmetatable(graph3, { __call = function(cls, ...) return cls:new(...) end })

function graph3:vertex(name, p)
  if not self.orig[name] then table.insert(self.names, name) end
  self.orig[name] = p
end
function graph3:edge(a, b, tag) table.insert(self.edges, { a = a, b = b, tag = tag }) end

-- position of a vertex (after the transformation, if any)
function graph3:pos(name)
  local p = self.orig[name]
  return self.f and self.f(p) or p
end

-- image of the graph by a transformation f: the vertices AND the edges
-- are transformed (a non-linear f bends the edges)
function graph3:map(f)
  local G = graph3:new()
  G.orig, G.names, G.edges = self.orig, self.names, self.edges
  G.f = self.f and compose3(f, self.f) or f
  return G
end

-- grid of nx x ny x nz vertices, step h, corner o; names v<i><j><k>;
-- edges tagged "frame" (on an edge of the box) or "inner"
function grid3(nx, ny, nz, h, o)
  h, o = h or 1, o or P3(0, 0, 0)
  local G = graph3:new()
  local function name(i, j, k) return "v" .. i .. j .. k end
  local function ext(i, n) return (i == 0 or i == n - 1) and 1 or 0 end
  for i = 0, nx - 1 do for j = 0, ny - 1 do for k = 0, nz - 1 do
    G:vertex(name(i, j, k), o + P3(i * h, j * h, k * h))
  end end end
  local function add(i, j, k, i2, j2, k2, e)
    G:edge(name(i, j, k), name(i2, j2, k2), e >= 2 and "frame" or "inner")
  end
  for i = 0, nx - 1 do for j = 0, ny - 1 do for k = 0, nz - 1 do
    if i < nx - 1 then add(i, j, k, i + 1, j, k, ext(j, ny) + ext(k, nz)) end
    if j < ny - 1 then add(i, j, k, i, j + 1, k, ext(i, nx) + ext(k, nz)) end
    if k < nz - 1 then add(i, j, k, i, j, k + 1, ext(i, nx) + ext(j, ny)) end
  end end end
  return G
end

-- the vertices in z (names of the graph)
function graph3:vertices_to_z(cam)
  for _, n in ipairs(self.names) do z[n] = cam:project(self:pos(n)) end
end

---------------------------------------------------------------------------
-- scene: visible / hidden pieces
---------------------------------------------------------------------------
scene3 = {}
scene3.__index = scene3

function scene3:new(cam)
  return setmetatable({ cam = cam, objects = {}, groups = {}, order = {} }, scene3)
end
setmetatable(scene3, { __call = function(cls, ...) return cls:new(...) end })

-- objects that can hide the others (opaque ones)
function scene3:add(obj) table.insert(self.objects, obj); return obj end

-- level 3: is x hidden by an opaque object of the scene (other than exclude)?
function scene3:occluded(x, exclude)
  local w = self.cam:toward(x)
  for _, o in ipairs(self.objects) do
    if o ~= exclude and o.opaque and o:blocks(x, w) then return true end
  end
  return false
end

-- f(t), t in [0, 1]; hidden(p) -> boolean; the curve is cut where the state
-- changes (bisection), giving pieces {hidden = bool, pts = {point3...}}
local function split(f, n, hidden, closed)
  local pieces = {}
  local t0, s0 = 0, hidden(f(0))
  local cur = { f(0) }
  for k = 1, n do
    local t = k / n
    local p = f(t)
    local s = hidden(p)
    if s ~= s0 then
      local a, b = t0, t
      for _ = 1, 40 do
        local m = (a + b) / 2
        if hidden(f(m)) == s0 then a = m else b = m end
      end
      local q = f((a + b) / 2)
      cur[#cur + 1] = q
      pieces[#pieces + 1] = { hidden = s0, pts = cur }
      cur = { q }
    end
    cur[#cur + 1] = p
    t0, s0 = t, s
  end
  pieces[#pieces + 1] = { hidden = s0, pts = cur }
  -- closed curve: join the last piece and the first one if same state
  if closed and #pieces > 1 and pieces[1].hidden == pieces[#pieces].hidden then
    local last = table.remove(pieces)
    for i = 2, #pieces[1].pts do last.pts[#last.pts + 1] = pieces[1].pts[i] end
    pieces[1] = last
  end
  return pieces
end

function scene3:store(group, pieces, straight)
  local g = self.groups[group]
  if not g then
    g = { v = {}, h = {} }
    self.groups[group] = g
    table.insert(self.order, group)
  end
  for _, pc in ipairs(pieces) do
    local pts = pc.pts
    if straight then pts = { pts[1], pts[#pts] } end
    table.insert(pc.hidden and g.h or g.v, pts)
  end
end

-- segment [a, b]; opts.exclude: object to ignore; opts.hidden: forced hidden
function scene3:segment(group, a, b, opts)
  opts = opts or {}
  local pieces
  if opts.hidden then
    pieces = { { hidden = true, pts = { a, b } } }
  else
    pieces = split(function(t) return a + t * (b - a) end, opts.n or 200,
      function(p) return self:occluded(p, opts.exclude) end)
  end
  self:store(group, pieces, true)
end

-- circle of center c, normal nrm, radius r
function scene3:circle(group, c, nrm, r, opts)
  opts = opts or {}
  local e1, e2 = basis_of(nrm)
  local f = function(t)
    local a = 2 * math.pi * t
    return c + (r * math.cos(a)) * e1 + (r * math.sin(a)) * e2
  end
  self:store(group, split(f, opts.n or 360,
    function(p) return self:occluded(p, opts.exclude) end, true))
end

-- any curve f(t), t in [0, 1]; opts.closed, opts.n, opts.exclude;
-- opts.nohide = true: everything visible (transparent mesh, ...)
function scene3:curve(group, f, opts)
  opts = opts or {}
  local n = opts.n or 360
  if opts.nohide then
    local pts = {}
    for k = 0, n do pts[#pts + 1] = f(k / n) end
    self:store(group, { { hidden = false, pts = pts } })
    return
  end
  self:store(group, split(f, n,
    function(p) return self:occluded(p, opts.exclude) end, opts.closed))
end

-- closed path PA.<name>: visible part of the zone of the sphere S between
-- the planes orthogonal to axis at the heights h2 < h1 (from the center).
-- Boundary: visible arc of the upper circle, contour, visible arc of the
-- lower circle, contour. Used to fill or to clip the zone.
function scene3:sphere_zone(name, S, axis, h1, h2, n)
  n = n or 120
  local cam, C, r = self.cam, S.center, S.radius
  local e1, e2, a = basis_of(axis)
  local q = cam.perspective and (cam.eye - C) or cam.d
  local qa, qx, qy = q:dot(a), q:dot(e1), q:dot(e2)
  local m = math.sqrt(qx * qx + qy * qy)
  local alpha = math.atan(qy, qx)
  -- visible arc of the circle at height h: |theta - alpha| < beta(h)
  local function beta(h)
    local rho = math.sqrt(math.max(r * r - h * h, 0))
    local A = rho * m
    local B = cam.perspective and (h * qa - r * r) or h * qa
    if A < EPS then return (B > 0) and math.pi or 0 end
    local c = -B / A
    if c <= -1 then return math.pi elseif c >= 1 then return 0 end
    return math.acos(c)
  end
  local function P(th, h)
    local rho = math.sqrt(math.max(r * r - h * h, 0))
    return C + (rho * math.cos(th)) * e1 + (rho * math.sin(th)) * e2 + h * a
  end
  local pts = {}
  local b1, b2 = beta(h1), beta(h2)
  for k = 0, n do pts[#pts + 1] = P(alpha - b1 + 2 * b1 * k / n, h1) end
  for k = 1, n do local h = h1 + (h2 - h1) * k / n; pts[#pts + 1] = P(alpha + beta(h), h) end
  for k = 1, n do pts[#pts + 1] = P(alpha + b2 - 2 * b2 * k / n, h2) end
  for k = 1, n - 1 do local h = h2 + (h1 - h2) * k / n; pts[#pts + 1] = P(alpha - beta(h), h) end
  local pa = path()
  for _, p in ipairs(pts) do pa:add_point(cam:project(p), 6) end
  PA[name] = pa
end

-- edges of a polyhedron: level 1 (own faces), then level 3 (other objects)
function scene3:polyhedron(group, P)
  for _, e in ipairs(P.edges) do
    local seen = false
    for _, k in ipairs(e.faces) do
      if P:face_visible(k, self.cam) then seen = true end
    end
    self:segment(group, P.vertices[e.a], P.vertices[e.b],
      { exclude = P, hidden = not seen })
  end
end

-- a point of the boundary of obj: hidden by obj itself or by the others
function scene3:hidden_on(p, obj)
  return obj:blocks(p, self.cam:toward(p)) or self:occluded(p, obj)
end

-- circle of the boundary of obj (rim of a cylinder or of a cone)
function scene3:rim(group, obj, c, r)
  local e1, e2 = basis_of(obj.axis)
  local f = function(t)
    local a = 2 * math.pi * t
    return c + (r * math.cos(a)) * e1 + (r * math.sin(a)) * e2
  end
  self:store(group, split(f, 360, function(p) return self:hidden_on(p, obj) end, true))
end

-- cylinder: two rims + contour generators (level 2)
function scene3:cylinder(group, CY)
  self:rim(group, CY, CY.base, CY.radius)
  self:rim(group, CY, CY.top, CY.radius)
  for _, n in ipairs(CY:contour_dirs(self.cam)) do
    self:segment(group, CY.base + CY.radius * n, CY.top + CY.radius * n, { exclude = CY })
  end
end

-- cone: rim + contour generators (level 2)
function scene3:cone(group, CO)
  self:rim(group, CO, CO.base, CO.radius)
  for _, n in ipairs(CO:contour_dirs(self.cam)) do
    self:segment(group, CO.base + CO.radius * n, CO.apex, { exclude = CO })
  end
end

-- apparent contour of a sphere (level 2 for the circles drawn on it:
-- use scene3:circle, the sphere itself hides their back part)
function scene3:sphere(group, S)
  local c, n, r = S:contour(self.cam)
  self:circle(group, c, n, r, { exclude = S })
end

-- closed polygonal line through a list of point3 (section, ...)
function scene3:polygon(group, pts)
  for i = 1, #pts do self:segment(group, pts[i], pts[i % #pts + 1]) end
end

-- segments drawn as they are (no hidden part): construction lines
function scene3:plain(group, a, b)
  self:store(group, { { hidden = false, pts = { a, b } } }, true)
end

-- edges of a list of polygons (each edge once), visible / hidden
function scene3:polygon_edges(group, polys)
  local seen = {}
  local function key(p) return string.format("%.6f,%.6f,%.6f", p.x, p.y, p.z) end
  for _, F in ipairs(polys) do
    for i = 1, #F.pts do
      local a, b = F.pts[i], F.pts[i % #F.pts + 1]
      local ka, kb = key(a), key(b)
      local k = ka < kb and (ka .. "|" .. kb) or (kb .. "|" .. ka)
      if not seen[k] then
        seen[k] = true
        self:segment(group, a, b)
      end
    end
  end
end

-- filled faces, from the farthest to the nearest (painter's algorithm).
-- color of a face: F.fill (TikZ options) or F.color shaded by orientation
local LIGHT = nil
function scene3:faces(group, polys)
  local cam = self.cam
  LIGHT = LIGHT or (0.3 * cam.u + 0.6 * cam.v + 0.75 * cam.d):unit()
  local L = {}
  for _, F in ipairs(polys) do
    local depth = cam:toward(F.center):dot(F.center - cam.target)
    if cam.perspective then depth = -(cam.eye - F.center):norm() end
    L[#L + 1] = { F = F, depth = depth }
  end
  table.sort(L, function(a, b) return a.depth < b.depth end)
  local g = { n = #L, opts = {} }
  for i, e in ipairs(L) do
    local F = e.F
    local pa = path()
    for _, p in ipairs(F.pts) do pa:add_point(cam:project(p), 6) end
    PA[group .. "f" .. i] = pa
    if F.fill then
      g.opts[i] = F.fill
    else
      local k = math.floor(55 + 45 * math.abs(F.normal:dot(LIGHT)) + 0.5)
      g.opts[i] = (F.color or "gray") .. "!" .. k .. "!black"
    end
  end
  TD[group] = g
end

-- 2D convex hull of the projections of a list of point3 -> PA.<name>
-- (outline of a convex surface: paraboloid, disk seen obliquely, ...)
function scene3:hull_region(name, pts3)
  local P = {}
  for _, p in ipairs(pts3) do
    local q = self.cam:project(p)
    P[#P + 1] = { q.re, q.im }
  end
  table.sort(P, function(a, b) return a[1] < b[1] or (a[1] == b[1] and a[2] < b[2]) end)
  local function cross(o, a, b)
    return (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  end
  local H = {}
  for _, p in ipairs(P) do
    while #H >= 2 and cross(H[#H - 1], H[#H], p) <= 0 do table.remove(H) end
    H[#H + 1] = p
  end
  local t = #H + 1
  for i = #P - 1, 1, -1 do
    local p = P[i]
    while #H >= t and cross(H[#H - 1], H[#H], p) <= 0 do table.remove(H) end
    H[#H + 1] = p
  end
  table.remove(H)
  local pa = path()
  for _, p in ipairs(H) do pa:add_point(point(p[1], p[2]), 6) end
  PA[name] = pa
end

-- closed region PA.<name> from a closed curve f(t), t in [0, 1]
function scene3:curve_region(name, f, n)
  n = n or 180
  local pa = path()
  for k = 0, n - 1 do pa:add_point(self.cam:project(f(k / n)), 6) end
  PA[name] = pa
end

-- edges of a graph: group .. tag (ex. "cubeframe", "cubeinner");
-- each edge is the image of the segment by the transformation of the graph;
-- vertices sorted from the farthest to the nearest: TD[group .. "nodes"]
function scene3:graph(group, G, opts)
  opts = opts or {}
  for _, e in ipairs(G.edges) do
    local a, b = G.orig[e.a], G.orig[e.b]
    local f = function(t)
      local p = a + t * (b - a)
      return G.f and G.f(p) or p
    end
    local n = G.f and (opts.n or 40) or 1
    self:curve(group .. (e.tag or ""), f, { n = n, nohide = not opts.hide })
  end
  local L = {}
  for _, name in ipairs(G.names) do
    local p = G:pos(name)
    L[#L + 1] = { name = name, depth = self.cam:toward(p):dot(p - self.cam.target) }
  end
  table.sort(L, function(x, y) return x.depth < y.depth end)
  local names = {}
  for i, e in ipairs(L) do names[i] = e.name end
  TD[group .. "nodes"] = names
  G:vertices_to_z(self.cam)
end

-- creates the paths PA.<group>v<i> and PA.<group>h<i>, and TD[group]
function scene3:export()
  for _, name in ipairs(self.order) do
    local g = self.groups[name]
    TD[name] = { nv = #g.v, nh = #g.h }
    for kind, list in pairs({ v = g.v, h = g.h }) do
      for i, pts in ipairs(list) do
        local pa = path()
        for _, p in ipairs(pts) do pa:add_point(self.cam:project(p), 6) end
        PA[name .. kind .. i] = pa
      end
    end
  end
end

-- used by \tkzFillRegion / \tkzClipRegion (closed path PA.<name>)
function tkz3d_region(name, cmd, opts)
  if not PA[name] then error("tkz-elements 3d: unknown region " .. name, 2) end
  tex.print("\\" .. cmd .. "[" .. (opts or "") .. "] plot coordinates {"
    .. PA[name]:concat(" ") .. "} -- cycle;")
end

-- used by \tkzFillFaces (faces from the farthest to the nearest)
function tkz3d_faces(group, extra)
  local g = TD[group]
  if not g or not g.opts then error("tkz-elements 3d: unknown faces " .. group, 2) end
  for i = 1, g.n do
    tex.print("\\fill[" .. g.opts[i] .. (extra ~= "" and ("," .. extra) or "")
      .. "] plot coordinates {" .. PA[group .. "f" .. i]:concat(" ") .. "} -- cycle;")
  end
end

-- used by \tkzDrawBalls: shaded balls at the vertices (farthest first)
function tkz3d_balls(group, color, radius)
  local L = TD[group .. "nodes"]
  if not L then error("tkz-elements 3d: unknown graph " .. group, 2) end
  for _, n in ipairs(L) do
    tex.print("\\shade[ball color=" .. color .. "] (" .. n .. ") circle (" .. radius .. ");")
  end
end

-- used by \tkzDrawVisible / \tkzDrawHidden (prototype of the .sty macros)
function tkz3d_draw(kind, group, opts)
  local g = TD[group]
  if not g then error("tkz-elements 3d: unknown group " .. group, 2) end
  for i = 1, (kind == "v" and g.nv or g.nh) do
    tex.print("\\tkzDrawCoordinates[" .. opts .. "](PA." .. group .. kind .. i .. ")")
  end
end
