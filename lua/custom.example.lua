-- rename this file to custom.lua to configure the nvim

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

  -- Use darker background? Can be a boolean value *OR* a strings with hex-color.
  use_darker_background = true,
  -- use_darker_background = "#0d1117"

  -- Treesitter and Lspconfig settings
  -- HINT: Don't configure Lua and Rust here, they are pre-configured.
  langs = {
    --
    -- Single string means that you only need the treesitter plugin on the filetype
    --

    -- Enable treesitter only
    -- Supported language: https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
    "bash",
    "comment",
    "fish",
    "html",
    "json",
    "nix",
    "rust",
    "toml",
    "vim",

    --
    -- Array with multiple items means you want treesitter and lspconfig on the filetype
    --
    -- Lsp server configuration:
    --   https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
    --
    -- Belows are example config, you can remove them if you don't need them

    -- Enable treesitter for *.go, and use gopls as lsp server
    { "go", "gopls" },

    -- Enable treesitter for *.c, *.cpp, and use clangd for them
    { { "c", "cpp" }, "clangd" },

    -- use eslint for .js, .ts, .tsx, .jsx with treesitter enable
    -- Install it via your system package manager or `npm install -g tsserver`
    { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "tsserver" },

    -- Enable treesitter for *.lua, use sumneko_lua as lsp server, and add neovim runtime library path
    -- into server search path. You can delete the settings if you are not developing
    -- neovim configuration or plugins.
    {
      "lua",
      "sumneko_lua",
      { -- <- do not miss the brace, we need to pass a Key-Value data structure to the server
        Lua = {
          runtime = {
            version = "LuaJIT",
          },
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
          },
          telemetry = {
            enable = false,
          },
        },
      }, -- <- same as above
    },
  },

  -- configuration for null-ls lsp injection
  null_ls = {
    enable_stylua_fmt = false, -- require stylua executable
    enable_eslint = false, -- require eslint, useful to combine use with tsserver
    enable_prettier = false, -- require prettier, useful when you want format in js/ts{x}
  },

  autocmd_enable = {
    fcitx5 = false, -- require fcitx5-remote
    lastline = true,
    diff_on_commit = false, -- might mess up your window
  },

  markdown = {
    -- must be executable
    preview_browser = "chrome",
  },
}

return my_config
