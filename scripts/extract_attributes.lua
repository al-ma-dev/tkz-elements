-- extract_class_attrs.lua
-- Usage :
--   lua extract_class_attrs.lua path/to/tkz_elements_circle.lua
--   lua extract_class_attrs.lua path/to/file.lua ClassName
-- Option :
--   --verbose

local VERBOSE = false
local argv = {}
for _, a in ipairs(arg) do
  if a == "--verbose" or a == "-v" then VERBOSE = true else argv[#argv+1] = a end
end
if #argv == 0 then
  io.stderr:write("Usage: lua extract_instance_attrs.lua file.lua [ClassName]\n")
  os.exit(1)
end
local SRC_PATH   = argv[1]
local FORCED_CLS = argv[2]
local function log(...) if VERBOSE then print("[attrs]", ...) end end

-- helpers
local function split_path(p) local d,f=p:match("^(.*)/([^/]+)$"); if not d then return ".",p end; return d,f end
local function drop_lua_ext(s) return s:gsub("%.lua$","") end
local function read_all(p) local f=assert(io.open(p,"r")); local s=f:read("*a"); f:close(); return s end
local function esc(s) return (s:gsub("([%%%[%]%^%-$().*+?-])","%%%1")) end
local function infer_class_from_filename(file)
  local base=drop_lua_ext(file)
  local name=base:gsub("^tkz[_%-]elements[_%-]",""):gsub("^elements[_%-]",""):gsub("^tkz[_%-]","")
  if name:match("^functions[_%-]") then return nil end
  return name
end
local function out_path_for(src_path, class)
  local dir, file = split_path(src_path)
  local base = drop_lua_ext(file)
  local suffix = class and (class .. "_ATTRS") or "ATTRS"
  return string.format("%s/EXTRACTION/%s_%s.txt", dir, base, suffix)
end

-- collecte des noms de méthodes pour filtrage
local function collect_method_names(src, class)
  local m = {}
  if class and #class > 0 then
    -- function Class:foo(...)
    for name in src:gmatch("function%s+" .. esc(class) .. ":([%w_]+)%s*%b()") do m[name]=true end
    -- function Class.foo(...)
    for name in src:gmatch("function%s+" .. esc(class) .. "%.([%w_]+)%s*%b()") do m[name]=true end
    -- Class.foo = function(...)
    for name in src:gmatch(esc(class) .. "%.([%w_]+)%s*=%s*function%s*%b()") do m[name]=true end
  end
  return m
end

local function attrs_from_literal_block(block, acc)
  for key in block:gmatch("([%w_]+)%s*=") do
    if key:sub(1,2) ~= "__" then acc[key]=true end
  end
end

local function extract_instance_attrs(src, class)
  local attrs, cand = {}, {}          -- cand = candidats bruts avant filtrage
  local methods = collect_method_names(src, class)

  -- 1) self.xxx = ...
  for name in src:gmatch("self%.([%w_]+)%s*=") do cand[name] = true end

  -- 2) Class.xxx = ...  (attributs de classe → optionnel; on peut ne PAS les inclure)
  -- Si tu veux les inclure, décommente les deux lignes suivantes :
  -- if class and #class>0 then
  --   for name in src:gmatch(esc(class).."%.([%w_]+)%s*=") do cand[name]=true end
  -- end

  -- 3) setmetatable({ ... }, self)
  for block in src:gmatch("setmetatable%s*%(%s*{(.-)}%s*,%s*self%s*%)") do
    for key in block:gmatch("([%w_]+)%s*=") do cand[key]=true end
  end

  -- 4) local o = { ... }; setmetatable(o, self)
  for obj, block, obj2 in src:gmatch("local%s+([%w_]+)%s*=%s*{(.-)}%s*;?%s*setmetatable%s*%(%s*([%w_]+)%s*,%s*self%s*%)") do
    if obj == obj2 then
      for key in block:gmatch("([%w_]+)%s*=") do cand[key]=true end
    end
  end

  -- 5) o.xxx = ... ; setmetatable(o, self)
  local owns = {}
  for obj in src:gmatch("setmetatable%s*%(%s*([%w_]+)%s*,%s*self%s*%)") do owns[obj]=true end
  for obj in pairs(owns) do
    local pat = esc(obj) .. "%.([%w_]+)%s*="
    for name in src:gmatch(pat) do cand[name]=true end
  end

  -- 6) self.xxx en lecture (pour capter un champ utilisé mais pas initialisé ici)
  for name, after in src:gmatch("self%.([%w_]+)%s*([%(%w_])?") do
    if after ~= "(" then cand[name]=true end
  end

  -- *** Filtrage des méthodes ***
  -- a) retirer les noms vus comme appels self.xxx(
  for name in pairs(cand) do
    local pat = "self%." .. esc(name) .. "%s*%("
    if src:match(pat) then cand[name] = nil end
  end
  -- b) retirer les noms qui sont des méthodes connues
  for name in pairs(methods) do cand[name] = nil end

  -- c) petits nettoyages
  for name in pairs(cand) do
    if name:sub(1,2) == "__" then cand[name] = nil end
  end

  -- résultat trié
  for k in pairs(cand) do attrs[#attrs+1] = k end
  table.sort(attrs)
  return attrs
end

-- run
local dir, file = split_path(SRC_PATH)
local CLASS = FORCED_CLS or infer_class_from_filename(file)
log("Fichier:", SRC_PATH, "Classe déduite:", tostring(CLASS))

local SRC = read_all(SRC_PATH)
local list = extract_instance_attrs(SRC, CLASS)

local outp = out_path_for(SRC_PATH, CLASS)
log("Écriture:", outp, "count="..tostring(#list))
local f = assert(io.open(outp, "w"))
for _, k in ipairs(list) do f:write(k, "\n") end
f:close()
if CLASS then
  print(("→ %s (%d attributs pour %s)"):format(outp, #list, CLASS))
else
  print(("→ %s (%d attributs)"):format(outp, #list))
end
