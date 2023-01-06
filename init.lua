--[[
███╗   ██╗██╗   ██╗██╗███╗   ███╗
████╗  ██║██║   ██║██║████╗ ████║
██╔██╗ ██║██║   ██║██║██╔████╔██║
██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝

Author: Avimitin
Source: https://github.com/Avimitin/nvim
License: Apache-2.0 License
--]]

-- You can modify the value below to change configuration. Or, create a `.neovim.lua` file
-- in your project root to have different configuration between projects.
require("editor").setup({
  ui = {
    theme = "kanagawa",
    darker = true,
    -- optional
    darkmode = {
      enable = false,
      day = "github_light",
      night = "kanagawa",
      night_time = {
        begin = "19:00",
        ending = "7:00",
      },
    },
  },

  coding = {
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
      "zsh",

      --
      -- Array with multiple items means you want treesitter and lspconfig on the filetype
      --
      -- Lsp server configuration:
      --   https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
      --
      -- Belows are example config, you can remove them if you don't need them.
      -- Or uncomment them to enable.

      -- Enable treesitter for lua, and use `sumneko_lua` as LSP server
      { "lua", "sumneko_lua" },

      -- use tsserver for .js, .ts, .tsx, .jsx with treesitter enable
      -- Install it via your system package manager or `npm install -g tsserver`
      { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "tsserver" },
    },

    opts = {
      -- Use stylua as formatter for Lua, require stylua executable
      stylua = false,
      -- Inject eslint code action and diagnostic into tsserver, require eslint executable
      eslint = false,
      -- Use prettier to format javascript
      prettier = false,
    },

    rust = {
      cargo = {
        autoreload = true,
      },
      -- I would prefer to use cargo clippy to whip me more
      checkOnSave = {
        command = "clippy",
      },
      inlayHints = {
        -- Wait for https://github.com/simrat39/rust-tools.nvim/pull/307 merged
        locationLinks = false,
      },
    },

    nvim_dev_mode = false,
  },

  markdown = {
    previewer = "firefox",
  },

  autocmds = {
    fcitx5 = false, -- require fcitx5-remote
    lastline = true, -- always jump to last edit line of the current opened file
    diff_on_commit = false, -- show diff in vertical splited window when committing
  },
})
