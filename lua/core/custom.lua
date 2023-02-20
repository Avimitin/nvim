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
  -- Append key mappings
  keymap = {
    n = {},
    i = {},
    x = {},
    t = {},
  },
  -- Control plugin spec
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
  lsp = {},
  treesitter = {
    ensure_installed = {},
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

vim.cfg = default
