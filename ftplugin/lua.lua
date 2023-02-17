if vim.b.did_load_lspconfig then
  return
end

vim.b.did_load_lspconfig = true

local lspconfig = require("lspconfig")
local config = require("lsp.config")
local user_config = vim.cfg.lua
local apply_keymap = require("lsp.keymaps")

local is_nvim_config_dir = (vim.fn.getcwd()):find(vim.fn.stdpath("config"))

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
        vim.env.HOME .. "/.local/share/nvim/lazy/emmylua-nvim",
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

config.on_attach = apply_keymap
config.settings = {
  Lua = settings,
}

lspconfig[user_config.server].setup(config)
