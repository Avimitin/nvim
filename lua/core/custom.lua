local default = {
  ui = {
    theme = "kanagawa",
    darker_background = true,
  },
  core = {
    -- Override options
    options = {},

    disable_builtin_plugins = {
      "gzip",
      "zip",
      "zipPlugin",
      "tar",
      "tarPlugin",
      "getscript",
      "getscriptPlugin",
      "vimball",
      "vimballPlugin",
      "2html_plugin",
      "matchit",
      "matchparen",
      "logiPat",
      "rust_vim",
      "rust_vim_plugin_cargo",
      "rrhelper",
      "netrw",
      "netrwPlugin",
      "netrwSettings",
      "netrwFileHandlers",
    },
    disable_builtin_provider = {
      "perl",
      "node",
      "ruby",
      "python",
      "python3",
    },

    autocmd = {
      relative_number = false,
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
      "bash",
      "gitcommit",
      "gitignore",
      "html",
      "javascript",
      "json",
      "lua",
      "markdown",
      "markdown_inline",
      "rust",
      "toml",
      "tsx",
      "typescript",
      "vim",
    },
  },
  neovide = {
    font = { "agave Nerd Font Mono:h11" },
    transparency = 0.9,
    vfx_mode = "sonicboom",
  },
  -- override lua lsp settings
  lua = {
    enable = true,
    server = "lua_ls",
    settings = {},
    stylua = true,
  },
  -- override Rust Lsp settings
  rust = {
    enable = true,
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
    enable = true,
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  javascriptreact = {
    enable = true,
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  typescript = {
    enable = true,
    server = "tsserver",
    prettier = true,
    eslint = true,
  },
  typescriptreact = {
    enable = true,
    server = "tsserver",
    prettier = true,
    eslint = true,
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
--- It will also change current working directory to the project root.
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

  local finder = require("libs.find_root")
  local root_dir = finder.get_root(root_dir_pattern)
  if not root_dir then
    return nil
  end

  vim.api.nvim_set_current_dir(root_dir)

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

  local is_valid = function(tbl)
    return tbl ~= nil and type(tbl) == "table"
  end

  local root_file = get_root_custom()
  local ok, mod = pcall(dofile, root_file)
  if ok and is_valid(mod) then
    extend(mod)
  end

  local project_file = get_project_custom()
  ok, mod = pcall(dofile, project_file)
  if ok and is_valid(mod) then
    extend(mod)
  end

  return final_config
end

vim.cfg = new_cfg()
