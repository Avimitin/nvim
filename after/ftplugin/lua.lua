if require("libs.cache")["lua_lsp"] then
  return
end

local user_config = vim.cfg.lua
if not user_config.enable then
  return
end

local config = require("lang.config")

local is_nvim_config_dir = (vim.fn.getcwd()):find("nvim")

local settings = user_config.settings

if is_nvim_config_dir then
  local default = {
    diagnostics = {
      enable = true,
      globals = { "vim" },
    },
    runtime = {
      version = "LuaJIT",
      path = vim.split(package.path, ";"),
    },
    workspace = {
      library = {
        vim.env.VIMRUNTIME,
        vim.fn.stdpath("data") .. "/lazy/emmylua-nvim",
      },
      checkThirdParty = false,
    },
    completion = {
      callSnippet = "Replace",
    },
  }

  -- respect user config
  settings = vim.tbl_deep_extend("force", default, settings)
end

config.settings = {
  Lua = settings,
}

require("lang").run_lsp(user_config.server, config)

-- End of LSP configuration --
