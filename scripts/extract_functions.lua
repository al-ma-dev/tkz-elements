-- list_standalone_funcs.lua
-- Usage :
--   lua list_standalone_funcs.lua path1.lua [path2.lua ...]
-- Options :
--   --verbose | -v

local VERBOSE = false
local paths = {}
for _, a in ipairs(arg) do
  if a == "--verbose" or a == "-v" then VERBOSE = true else paths[#paths+1] = a end
end
if #paths == 0 then
  io.stderr:write("Usage: lua list_standalone_funcs.lua file1.lua [file2.lua ...]\n")
  os.exit(1)
end
local function log(...) if VERBOSE then print("[standalone]", ...) end end

-- helpers
local function split_path(p) local d,f=p:match("^(.*)/([^/]+)$"); if not d then return ".",p end; return d,f end
local function drop_lua_ext(s) return s:gsub("%.lua$","") end
local function read_all(p) local f=assert(io.open(p,"r")); local s=f:read("*a"); f:close(); return s end
local function norm_spaces(s) return (s:gsub("%s+"," "):gsub("^%s+",""):gsub("%s+$","")) end
local function out_path_for(src_path)
  local dir, file = split_path(src_path)
  local base = drop_lua_ext(file)
  return string.format("%s/EXTRACTION/%s_STANDALONE_FUNCS.txt", dir, base)
end

local function list_funcs_for_src(src)
  local out, seen = {}, {}

  -- 1) local function name(args)
  for name, par in src:gmatch("local%s+function%s+([%w_]+)%s*(%b())") do
    if not name:find("[%.:]") then
      local sig = string.format("local %s(%s)", name, norm_spaces(par:sub(2,-2)))
      if not seen[sig] then out[#out+1]=sig; seen[sig]=true end
    end
  end

  -- 2) function name(args)   (global)
  for name, par in src:gmatch("[^\n]-function%s+([%w_]+)%s*(%b())") do
    if not name:find("[%.:]") then
      local sig = string.format("%s(%s)", name, norm_spaces(par:sub(2,-2)))
      if not seen[sig] then out[#out+1]=sig; seen[sig]=true end
    end
  end

  -- 3) local name = function(args)
  for name, par in src:gmatch("local%s+([%w_]+)%s*=%s*function%s*(%b())") do
    if not name:find("[%.:]") then
      local sig = string.format("local %s(%s)", name, norm_spaces(par:sub(2,-2)))
      if not seen[sig] then out[#out+1]=sig; seen[sig]=true end
    end
  end

  -- 4) name = function(args)   (global assign)
  for prefix, name, par in src:gmatch("([^\n]-)([%w_]+)%s*=%s*function%s*(%b())") do
    -- ignorer les cas précédents où 'local' était déjà capturé
    if not prefix:match("local%s+$") and not name:find("[%.:]") then
      local sig = string.format("%s(%s)", name, norm_spaces(par:sub(2,-2)))
      if not seen[sig] then out[#out+1]=sig; seen[sig]=true end
    end
  end

  table.sort(out)
  return out
end

for _, path in ipairs(paths) do
  log("Reading:", path)
  local src = read_all(path)
  local list = list_funcs_for_src(src)
  local outp = out_path_for(path)
  log("Writing:", outp, "count=", #list)
  local f = assert(io.open(outp, "w"))
  for _, sig in ipairs(list) do f:write(sig, "\n") end
  f:close()
  print(("→ %s (%d fonctions)"):format(outp, #list))
end
