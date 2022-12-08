---@class CoreCfg
---@field ui CoreCfgUI The UI customization
---@field coding CoreCfgCoding The coding customization
---@field markdown MarkdownOpts Option for markdown
---@field autocmds { [string]: boolean } Option for auto commands

---@class MarkdownOpts
---@field previewer string Executable for preview the markdown

---@class CoreCfgUI
---@field theme string The colorscheme name
---@field darker boolean Use darker background
---@field darkmode DarkMode

---@class DarkMode
---@field enable boolean
---@field day string The colorscheme name for day time
---@field night string The colorscheme name for night time
---@field night_time { begin: string, ending: string } Schedule the night time

---@class CoreCfgCoding
---@field langs table The treesitter and LSP config customization
---@field opts { [string]: boolean } Optional injection for null-ls
---@field rust table Configuration that will be pass into rust-analyzer

---@class ExpandedCoreCfg The final representation
---@field ui CoreCfgUI
---@field lspconfig table Name-value pair configuration that will be pass into nvim-lspconfig
---@field lspconfig_fts string[] List of filetypes for nvim-lspconfig to activate
---@field treesitter_fts string[] List of filetypes for nvim-treesitter to activate
---@field rust_config table Configuration that will passed into rust-analyzer

---@param tbl table
---@return any[]
local function get_tbl_key(tbl)
  local arr = {}
  for k, _ in pairs(tbl) do
    table.insert(arr, k)
  end

  return arr
end

-- Expand the custom.langs field
local function expand_lang(languages)
  local treesitter_ft_set = {}
  local lspconfig_set = {}

  -- Expand the "{ "filetype", ... }" style table
  local function expand_single_ft_table(ft, lsp_server, lsp_settings)
    if treesitter_ft_set[ft] == nil then
      treesitter_ft_set[ft] = 1
    end

    if lsp_server == nil then
      return
    end

    if lspconfig_set[ft] ~= nil then
      return
    end

    lspconfig_set[ft] = {
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

  for _, v in ipairs(languages) do
    -- "filetype" string only. Use set datastructure to exclude redundant
    if type(v) == "string" and treesitter_ft_set[v] == nil then
      treesitter_ft_set[v] = 1
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

  return {
    treesitter = treesitter_ft_set,
    lspconfig = lspconfig_set,
  }
end

---@param props CoreCfgCoding
local function process_coding_props(props)
  local null_ls_sources = get_tbl_key(props.opts)

  local expanded = nil
  if props.langs then
    expanded = expand_lang(props.langs)
  end

  return {
    null_ls_sources = null_ls_sources,
    expand_result = expanded,
  }
end

-- This function will parse our string time to a table to a unix timestamp
local function parse_time(str)
  if str then
    local hour, min = str:match("(%d+):(%d+)")
    return os.time({
      hour = hour,
      min = min,
      day = 1,
      month = 1,
      year = 1970,
    })
  end
end

---@param props DarkMode
local function choose_darkmode_theme(props)
  -- if user setup the darkmode related fields
  if
    not props.day
    or not props.night
    or not props.night_time
    or not props.night_time.begin
    or not props.night_time.ending
    or not props.enable
  then
    return nil
  end
  local begin = parse_time(props.night_time.begin)
  local ending = parse_time(props.night_time.ending)
  local now = parse_time(os.date("%H:%M"))

  -- we might want the 7:00 in next day
  if ending < begin then
    -- add 24 hour
    ending = ending + 72000
  end

  -- if the night has come
  if (now >= begin) and (now <= ending) then
    return props.night
  else
    return props.day
  end
end

-- This function is used for finding the root directory of the current file.
-- It will first try to find it by LSP client if it is available. Then it will
-- use the rooter API to find root by patterns. This patterns can be configured
-- in lua/overlays/rc/rooter.lua file. If none of this mechanism works, then
-- the current path will be return.
---@return string The root directory of the current project
local function find_root_dir()
  local rooter = require("libs.rooter")

  return rooter.get_root() or vim.fn.getcwd()
end

local function find_local()
  local root_dir = find_root_dir()
  local expect_cfg = root_dir .. "/.neovim.lua"
  local ok, mod = pcall(dofile, expect_cfg)
  if not ok then
    return nil
  end

  return mod
end

---@param orig CoreCfg|nil The original user config
return function(orig)
  if orig == nil then
    return
  end

  local extend = vim.tbl_deep_extend

  local custom = find_local()
  if custom then
    orig = extend("force", orig, custom)
  end

  ---@type ExpandedCoreCfg
  local final = {
    ui = {
      theme = "kanagawa",
      darker = false,
    },
    lspconfig = {},
    lspconfig_fts = {},
    treesitter_fts = {},
    null_ls_sources = {},
    markdown = {},
    autocmds = {},
  }

  local extended = extend("force", final.ui, orig.ui)
  if extended ~= nil then
    final.ui = extended
  end

  if orig.coding then
    local result = process_coding_props(orig.coding)
    if result.null_ls_sources then
      final.null_ls_sources = result.null_ls_sources
    end

    if result.expand_result then
      final.lspconfig = result.expand_result.lspconfig
      final.lspconfig_fts = get_tbl_key(final.lspconfig)
      final.treesitter_fts = get_tbl_key(result.expand_result.treesitter)
    end

    if orig.coding.rust then
      final.rust_config = orig.coding.rust
    end
  end

  if orig.markdown then
    final.markdown = extend("force", final.markdown, orig.markdown)
  end

  if orig.autocmds then
    final.autocmds = extend("force", final.autocmds, orig.autocmds)
  end

  if orig.ui.darkmode then
    final.ui.theme = choose_darkmode_theme(orig.ui.darkmode) or orig.ui.theme
  end
  -- Make it as a global access variable
  vim.g.nvcfg = final
end
