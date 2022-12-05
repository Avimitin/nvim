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

---@class CoreCfgCoding
---@field langs table The treesitter and LSP config customization
---@field opts { [string]: boolean } Optional injection for null-ls

---@class ExpandedCoreCfg The final representation
---@field ui CoreCfgUI
---@field lspconfig table Name-value pair configuration that will be pass into nvim-lspconfig
---@field lspconfig_fts string[] List of filetypes for nvim-lspconfig to activate
---@field treesitter_fts string[] List of filetypes for nvim-treesitter to activate

local function extend_tb(...)
  vim.tbl_deep_extend("force", ...)
end

---@param tbl table
---@return any[]
local function get_tbl_key(tbl)
  local arr = {}
  for k, _ in pairs(tbl) do
    arr:insert(k)
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

---@param props table
---@return string[] | nil
local function insert_null_ls_source(props)
  local sources = {}

  for k, v in pairs(props.opts) do
    if type(k) == "string" and type(v) == "boolean" then
      table.insert(sources, k)
    end
  end

  if #sources == 0 then
    return nil
  end

  return sources
end

---@param props CoreCfgCoding
local function process_coding_props(props)
  local null_ls_sources = nil
  -- assert the input type, and insert only when type is expected
  if props.opts and type(props.opts) == "table" then
    null_ls_sources = insert_null_ls_source(props.opts)
  end

  local expanded = nil
  if props.langs then
    expanded = expand_lang(props.langs)
  end

  return {
    null_ls_sources = null_ls_sources,
    expand_result = expanded,
  }
end

---@param orig CoreCfg|nil The original user config
return function(orig)
  if orig == nil then
    return
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

  local extended = extend_tb(final.ui, orig.ui)
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
  end

  if orig.markdown then
    final.markdown = extend_tb(final.markdown, orig.markdown)
  end

  if orig.autocmds then
    final.autocmds = extend_tb(final.autocmds, orig.autocmds)
  end

  -- Make it as a global access variable
  vim.g.nvcfg = final
end
