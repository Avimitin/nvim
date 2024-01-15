if require("libs.cache")["lua_lsp"] then
  return
end

local config = {}

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
        vim.api.nvim_get_runtime_file("", true),
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

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "lua_ls", config)
