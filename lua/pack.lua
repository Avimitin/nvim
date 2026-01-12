local notify = require("libs.notify")

local M = {
  specs = {},
  configs = {},
}

local function collect_plugins()
  local overlays = {
    "completion",
    "git",
    "lang",
    "tools",
    "treesitter",
    "ui",
  }

  for _, component in ipairs(overlays) do
    local ok, maybe_error = pcall(require, component)
    if not ok then
      notify.error("fail to load component " .. component, maybe_error)
    end
  end
end

function M.setup()
  collect_plugins()

  if vim.env.NEOVIM_EXTERNAL_PLUGIN_MANAGEMENT then
    -- Skip vim.pack.add as plugins are managed externally
  elseif vim.pack then
    vim.pack.add(M.specs)
  else
    notify.error("vim.pack not available (requires Neovim 0.12+)")
  end

  for _, spec in ipairs(M.specs) do
    local name = spec.name
    if M.configs[name] then
      local ok, err = pcall(M.configs[name])
      if not ok then
        notify.error("Error running config for " .. name, err)
      end
    end
  end
end

---@param repo_path string URL to the plugin repositories
---@param config table? Plugin config
function M.register(repo_path, config)
  local package = config or {}
  local name = repo_path:match(".*/(.*)") or repo_path

  local spec = {
    src = "https://github.com/" .. repo_path,
    name = name,
  }

  if package.branch then
    spec.version = package.branch
  elseif package.tag then
    spec.version = package.tag
  elseif package.commit then
    spec.version = package.commit
  end

  if package.init then
    package.init()
  end

  M.configs[name] = package.config

  table.insert(M.specs, spec)
end

function M.dump()
  if not vim.pack then
    return {}
  end
  return vim.pack.get(nil, { info = true })
end

return M
