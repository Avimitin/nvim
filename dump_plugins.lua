
-- Ensure we can load modules from current directory
vim.opt.rtp:prepend(vim.fn.getcwd())

-- Mock libs.notify to avoid errors if it relies on plugins
package.loaded["libs.notify"] = {
  error = function(...)
    local args = {...}
    print("Error:", table.concat(args, " "))
  end,
  info = function(...)
    local args = {...}
    print("Info:", table.concat(args, " "))
  end,
}

-- Require the pack module to get access to registry
local pack = require("pack")

-- Manually load the configuration modules that register plugins
-- These mirror the list in lua/pack.lua's collect_plugins
local modules = {
  "completion",
  "git",
  "lang",
  "tools",
  "treesitter",
  "ui",
}

for _, mod in ipairs(modules) do
  local ok, err = pcall(require, mod)
  if not ok then
    print("Failed to load " .. mod .. ": " .. tostring(err))
  end
end

-- Collect specs
local specs = pack.specs
local output = {}

for _, spec in ipairs(specs) do
  table.insert(output, {
    name = spec.name,
    src = spec.src,
    version = spec.version, -- Can be nil, string, or table (not handled by json encode usually if it's a function/userdata, but version strings/tables should be ok if simple)
    rev = spec.rev,
    sha256 = spec.sha256,
  })
end

-- Write to JSON
local f = io.open("plugins.json", "w")
if f then
  -- Pretty print
  f:write(vim.json.encode(output))
  f:close()
  print("Successfully dumped " .. #output .. " plugins to plugins.json")
else
  print("Error opening plugins.json for writing")
end
