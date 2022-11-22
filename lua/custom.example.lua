-- rename this file to custom.lua to configure the nvim

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

  -- Use darker background? Can be a boolean value *OR* a strings with hex-color.
  use_darker_background = true,
  -- use_darker_background = "#0d1117"

  -- Treesitter and Lspconfig settings
  langs = {
    --
    -- Single string means that you only need the treesitter plugin on the filetype
    --

    -- Enable treesitter only
    -- Supported language: https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
    "bash",
    "comment",
    "html",
    "json",
    "toml",
    "vim",

    -- Don't configure Rust here, rust-tools.nvim is already configured it
    "rust",
    -- Lua is also pre-configured
    "lua",

    --
    -- Array with multiple items means you want treesitter and lspconfig on the filetype
    --
    -- Lsp server configuration:
    --   https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
    --
    -- Belows are example config, you can remove them if you don't need them.
    -- Or uncomment them to enable.

    -- Enable treesitter for *.go, and use gopls as lsp server
    -- { "go", "gopls" },

    -- Enable treesitter for *.c, *.cpp, and use clangd for them
    -- { { "c", "cpp" }, "clangd" },

    -- use eslint for .js, .ts, .tsx, .jsx with treesitter enable
    -- Install it via your system package manager or `npm install -g tsserver`
    -- { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "tsserver" },
  },

  -- configuration for null-ls lsp injection
  null_ls = {
    enable_stylua_fmt = false, -- require stylua executable
    enable_eslint = false, -- require eslint, useful to combine use with tsserver
    enable_prettier = false, -- require prettier, useful when you want format in js/ts{x}
  },

  autocmd_enable = {
    fcitx5 = false, -- require fcitx5-remote
    lastline = true, -- always jump to last edit line of the current opened file
    diff_on_commit = false, -- show diff in vertical splited window when committing
  },

  markdown = {
    -- must be executable name that can be found by $PATH or a full path to executable
    preview_browser = "chrome",
  },
}

return my_config
