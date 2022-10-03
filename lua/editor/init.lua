local nvim = {}

-- default config for the editor
nvim.config = {
  theme = "kanagawa",

  -- Use `set` data structure to filter redundant
  treesitter_ft = {
    -- enable lua, vim, rust by default only
    lua = 1,
    vim = 1,
    rust = 1,
  },

  lspconfig = {},

  null_ls = {
    enable_stylua_fmt = false,
  },

  autocmd_enable = {
    fcitx5 = false,
    lastline = false,
    diff_on_commit = false,
  },

  markdown = {
    -- must be executable
    preview_browser = "firefox",
  },
}

-- Expand the "{ "filetype", ... }" style table
local function expand_single_ft_table(ft, lsp_server, lsp_settings)
  if nvim.config.treesitter_ft[ft] == nil then
    nvim.config.treesitter_ft[ft] = 1
  end

  if lsp_server == nil then
    return
  end

  if nvim.config.lspconfig[ft] ~= nil then
    return
  end

  nvim.config.lspconfig[ft] = {
    name = lsp_server,
    -- this value can be nil, just like it doesn't exist
    settings = lsp_settings,
  }
end

-- Expand the "{{ft, ft, ft}, ...}" style table
local function expand_multi_ft_table(langs, lsp, cfg)
  for _, ft in ipairs(langs) do
    expand_single_ft_table(ft, lsp, cfg)
  end
end

-- Expand the custom.langs field
local function expand_lang(languages)
  for _, v in ipairs(languages) do
    -- "filetype" string only
    if type(v) == "string" and nvim.config.treesitter_ft[v] == nil then
      nvim.config.treesitter_ft[v] = 1
    end

    -- { "filetype", ... } table
    if type(v) == "table" and type(v[1]) == "string" then
      expand_single_ft_table(v[1], v[2], v[3])
    end

    -- { {"ft1", "ft2", "ft3"}, ... } table
    if type(v) == "table" and type(v[1]) == "table" then
      expand_multi_ft_table(v[1], v[2], v[3])
    end
  end
end

local function expand_config()
  local present, custom = pcall(require, "custom")
  -- avoid some idiot return nil to the config
  if not present or not custom then
    return
  end

  -- merge lspconfig and treesitter config by hand
  if custom.langs then
    expand_lang(custom.langs)
  end
  custom.langs = nil

  nvim.config = vim.tbl_deep_extend("force", nvim.config, custom)
end

-- The entry point of the whole configuration
nvim.setup = function()
  -- expand the custom.lua file
  expand_config()

  -- load basic configuration
  local utils = require("editor.utils")

  -- load configuration from lua/editor/<module>
  for _, module_name in ipairs({
    "editor.options",
    "editor.auto_commands",
    "editor.keymap",
  }) do
    local ok, err = pcall(require, module_name)
    if not ok then
      local msg = "calling module: " .. module_name .. " fail: " .. err
      utils.errorL(msg)
    end
  end

  -- since we have packer compiled, we don't need to load this immediately
  vim.defer_fn(function()
    require("plugins").init()
  end, 0)
end

return nvim
