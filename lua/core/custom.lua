local default = {
  ui = {
    theme = "kanagawa",
    darker_background = true,
  },
  core = {
    -- Override options
    options = {},

    disable_builtin_plugins = {},
    disable_builtin_provider = {},

    autocmd = {
      relative_number = true,
      terminal_auto_insert = true,
      highlight_yanked = true,
      copy_yanked_to_clipboard = true,
      find_project_root = true,
    },
  },
  -- *Append* key mappings
  --
  -- Spec:
  --    [1]: left key mapping
  --    [2]: right key mapping
  --    mode?: optional, default "n"
  --    option: see :h nvim_set_keymap
  --
  -- Example:
  --
  -- ```lua
  -- keymaps = {
  --    { "jk", "<ESC>", mode = "i", desc = "Exit insert mode" },
  -- }
  -- ```
  keymaps = {},
  -- Override plugin spec, support extra key `disable` to remove plugin
  --
  -- Example:
  --
  -- ```lua
  -- plugins = {
  --    { "tpope/dot-repeat" },
  --    { "neovim/nvim-lspconfig", disable = true }, -- then this plugin will never be installed
  -- }
  -- ```
  plugins = {},
  -- Nerd font icon list
  icons = {
    -- Completion menu, Symbol outlines...
    Text = "",
    Method = "",
    Function = "",
    Constructor = "",
    Field = "",
    Variable = "",
    Class = "ﴯ",
    Interface = "",
    Module = "",
    Property = "ﰠ",
    Unit = "",
    Value = "",
    Enum = "",
    Keyword = "",
    Snippet = "",
    Color = "",
    File = "",
    Reference = "",
    Folder = "",
    EnumMember = "",
    Constant = "",
    Struct = "",
    Event = "",
    Operator = "",
    TypeParameter = "",
    Vim = "",

    -- lsp diagnostic (Notes: case sensitive)
    ERROR = "",
    WARN = "",
    HINT = "",
    INFO = "",
  },
  -- Override completion configuration
  completion = {
    keymap = {
      scroll_up = "<C-u>",
      scroll_down = "<C-d>",
      abort = "<C-c>",
      confirm = "<CR>",
      select_next = "<Tab>",
      select_prev = "<S-Tab>",
    },
  },
  markdown = {
    previewer = "firefox",
  },
  treesitter = {
    -- require gcc
    ensure_installed = {
      "lua",
      "rust",
      "bash",
      "vim",
      "toml",
      "json",
      "html",
      "comment",
      "javascript",
      "typescript",
      "tsx",
    },
  },
  -- override lua lsp settings
  lua = {
    server = "lua_ls",
    settings = {},
    stylua = true,
  },
  -- override Rust Lsp settings
  rust = {
    settings = {
      cargo = {
        autoreload = true,
      },
      -- I would prefer to use cargo clippy to whip me more
      checkOnSave = {
        command = "clippy",
      },
    },
  },
}

--- Path to custom file in neovim root dir, return nil if it is not exist
---@return string|nil path
local function get_root_custom()
  local filepath = vim.fn.stdpath("config") .. "/.neovim.lua"
  if vim.loop.fs_stat(filepath) then
    return filepath
  end
end

--- Path to custom file in project root dir, return nil if it is not exist
---@return string|nil
local function get_project_custom()
  local root_dir_pattern = {
    "Cargo.toml",
    ".git",
    "package.json",
    "yarn.lock",
    ".hg",
    ".svn",
  }
  local root_dir = require("libs.find_root").get_root(root_dir_pattern)
  if not root_dir then
    return nil
  end
  local filepath = root_dir .. "/.neovim.lua"
  if vim.loop.fs_stat(filepath) then
    return filepath
  end
end

local function new_cfg()
  local final_config = default
  local extend = function(ext)
    final_config = vim.tbl_deep_extend("force", final_config, ext)
  end

  local root_file = get_root_custom()
  local ok, mod = pcall(dofile, root_file)
  if ok then
    extend(mod)
  end

  local project_file = get_project_custom()
  ok, mod = pcall(dofile, project_file)
  if ok then
    extend(mod)
  end

  return final_config
end

vim.cfg = new_cfg()
