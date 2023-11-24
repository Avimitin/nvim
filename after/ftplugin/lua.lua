if require("libs.cache")["lua_lsp"] then
  return
end

local config = require("lang.config")

local is_nvim_config_dir = (vim.fn.getcwd()):find("nvim")

if is_nvim_config_dir then
  local neovim_setting = {
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
  config.settings = {
    Lua = neovim_setting,
  }
end

require("lang").run_lsp("lua_ls", config)
