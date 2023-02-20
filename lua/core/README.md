## Customize

There are three ways to modify the configuration

### Fork it

You can use this project as a kick start template and develop your neovim configuration.
Just click the `fork` button and do whatever you like.

### Root local config

You can create a `.neovim.lua` file inside `~/.config/nvim/`.
This script should return a Lua table that following the ["custom spec"](#custom-spec).

### Project local config

You can also create a `.neovim.lua` file inside your project root(identified by `.git`, `Cargo.toml`, `package.json`...etc).
This script file should retuan a Lua table that following the ["custom spec"](#custom-spec).

### Custom config priority

`default <--merge-- root local <--merge-- project local`

### Custom Spec

```lua
local default = {
  ui = {
    -- colorscheme name, there are "kanagawa" only now
    theme = "kanagawa",
    -- Use custom background color. This can also be a string value that contains color hex code.
    --
    -- Example:
    --
    -- darker_background = "#010203",
    --
    darker_background = true,
  },
  core = {
    -- Override options
    --
    -- Example, to `set textwidth=80`
    --
    -- options = {
    --   textwidth = 80,
    -- }
    options = {},

    -- Override default disabled plugins.
    disable_builtin_plugins = {},
    -- Override default disabled providers.
    disable_builtin_provider = {},

    -- default enabled auto commands
    autocmd = {
      relative_number = true,
      terminal_auto_insert = true,
      highlight_yanked = true,
      copy_yanked_to_clipboard = true,
      find_project_root = true,
      jump_lastline = {
        enable = true,
        ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit", "Dashboard" },
        ignore_buffer_type = { "quickfix", "nofile", "help" },
      },
      -- require fcitx5-remote executable
      toggle_fcitx5 = false,
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
  -- keymaps = {
  --    { "jk", "<ESC>", mode = "i", desc = "Exit insert mode" },
  -- }
  keymaps = {},
  -- **Override** plugin spec, support extra key `disable` to remove plugin
  --
  -- Example:
  --
  -- plugins = {
  --    { "tpope/dot-repeat" },
  --    { "neovim/nvim-lspconfig", disable = true }, -- then this plugin will never be installed
  --    {
  --      "projekt0n/github-nvim-theme"
  --      config = function()
  --        require("github-theme").setup({})
  --      end
  --    },
  -- }
  plugins = {},
  -- Nerd font icon list, you can change them to Emoji or Pure text if you don't like nerd font
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
  -- Override lua lsp settings
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
      checkOnSave = {
        command = "clippy",
      },
    },
  },
  javascript = {
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  javascriptreact = {
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  -- You can also use "deno_ls" and disable prettier & eslint
  typescript = {
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  typescriptreact = {
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
}
```
