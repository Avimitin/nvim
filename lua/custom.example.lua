-- rename this file to custom.lua to configure the nvim

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

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

    -- Enable treesitter for *.go, and use gopls as lsp server
    { "go", "gopls" },

    -- Enable treesitter for *.c, *.cpp, and use clangd for them
    { { "c", "cpp" }, "clangd" },

    -- use eslint for .js, .ts, .tsx, .jsx with treesitter enable
    { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "eslint" },

    -- Enable treesitter for *.py, use pyright as lsp server, and pass some
    -- custom settings to the lsp server.
    {
      "python",
      "pyright",
      { -- <- do not miss the brace, we need to pass a Key-Value data structure to the server
        python = {
          analysis = {
            autoSearchPaths = true,
            diagnosticMode = "workspace",
            useLibraryCodeForTypes = true,
          },
        },
      }, -- <- same as above
    },
  },

  -- configuration for null-ls lsp injection
  null_ls = {
    enable_stylua_fmt = false, -- require stylua executable
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
