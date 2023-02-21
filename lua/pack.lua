local notify = require("libs.notify")

local M = {
  repositories = {},
}

local function collect_plugins()
  local overlays = {
    "completion",
    "git",
    "lsp",
    "markdown",
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
  local lazy_nvim_install_path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazy_nvim_install_path) then
    notify.info("Package manager not found, installing...")
    local success, maybe_error = pcall(vim.fn.system, {
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazy_nvim_install_path,
    })

    if not success then
      notify.error("ERROR: Fail to install plugin manager lazy.nvim", maybe_error)
      notify.info("You can cleanup the " .. lazy_nvim_install_path .. " and start again")
      return
    end
  end

  vim.opt.rtp:prepend(lazy_nvim_install_path)

  collect_plugins()

  require("lazy").setup(M.repositories, {
    install = {
      colorscheme = { "kanagawa" },
    },
  })
end

---@param repo_path string URL to the plugin repositories
---@param config table? Lazy.nvim plugin spec
function M.register(repo_path, config)
  local package = config or {}

  local user_config = vim.cfg.plugins[repo_path]
  if user_config then
    if user_config.disable then
      return
    end

    package = vim.tbl_deep_extend("force", config or {}, user_config)
  end

  package[1] = repo_path

  table.insert(M.repositories, package)
end

return M
