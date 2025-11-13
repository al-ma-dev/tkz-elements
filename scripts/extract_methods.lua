-- list_methods.lua
-- Usage:
--   lua list_methods.lua ../latex/tkz_elements_circle.lua
--   lua list_methods.lua --verbose ../latex/tkz_elements_circle.lua

local VERBOSE = false
local paths = {}

-- --- args parsing (optional --verbose) ----------------------------
for _, a in ipairs(arg) do
  if a == "--verbose" or a == "-v" then
    VERBOSE = true
  else
    paths[#paths+1] = a
  end
end
if #paths == 0 then
  io.stderr:write("Usage: lua list_methods.lua [--verbose] file1.lua [file2.lua ...]\n")
  os.exit(1)
end

local function log(...)
  if VERBOSE then print("[list_methods]", ...) end
end

-- --- tiny helpers -------------------------------------------------
local function dirname_and_basename(p)
  local dir, file = p:match("^(.*)/([^/]+)$")
  if not dir then return ".", p end
  return dir, file
end

local function without_lua_ext(name)
  return name:gsub("%.lua$", "")
end

local function norm_spaces(s)
  return (s:gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", ""))
end

local function ensure_dir_p(dir)
  -- always succeed on macOS/Linux
  local cmd = string.format('mkdir -p "%s"', dir)
  local ok = os.execute(cmd)
  log("mkdir -p:", dir, "->", tostring(ok))
end

-- --- core ---------------------------------------------------------
local function list_methods_for_file(path)
  log("Reading:", path)
  local f, err = io.open(path, "r")
  if not f then
    error("Impossible d'ouvrir le fichier source: "..tostring(err))
  end
  local src = f:read("*a"); f:close()

  local out = {}
  local count = 0
  for prefix, name, parens in src:gmatch("([^\n]-)function%s+([%w_%.:]+)%s*(%b())") do
    local is_local = prefix:find("local%s*$") ~= nil
    if not is_local then
      local args = norm_spaces(parens:sub(2, -2))
      out[#out+1] = string.format("%s(%s)", name, args)
      count = count + 1
    end
  end
  log("Found methods:", count)
  table.sort(out)
  return out
end

local function write_list(src_path, list)
  local dir, file = dirname_and_basename(src_path)
  local out_dir = dir .. "/EXTRACTION"
  local base = without_lua_ext(file)
  local out_path = string.format("%s/%s_METHODS.txt", out_dir, base)

  ensure_dir_p(out_dir)

  log("Writing to:", out_path)
  local f, err = io.open(out_path, "w")
  if not f then
    error("Impossible de créer le fichier de sortie: "..tostring(err).."\n→ Vérifie les droits d’écriture sur: "..out_dir)
  end
  for _, sig in ipairs(list) do f:write(sig, "\n") end
  f:close()

  print(("→ %s (%d méthodes)"):format(out_path, #list))
end

-- --- run ----------------------------------------------------------
for _, p in ipairs(paths) do
  local list = list_methods_for_file(p)
  write_list(p, list)
end
