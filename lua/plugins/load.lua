local autoload = {
  -- speed up neovim!
  {
    "nathom/filetype.nvim",
    config = function()
      require("filetype").setup({
        -- overrides the filetype or function for filetype
        -- See https://github.com/nathom/filetype.nvim#customization
        overrides = {},
      })
    end,
  },

  -- adjust the shiftwidth and expandtab settins
  {
    "tpope/vim-sleuth",
  },

  -- add notify window
  {
    "rcarriga/nvim-notify",
  },
}

local markdown_plugins = {
  -- markdown toc
  {
    "mzlogin/vim-markdown-toc",
    cmd = {
      "GenTocGFM",
    },
  },

  -- markdown preview
  {
    "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
    config = function()
      require("config.mkdp")
    end,
    ft = {
      "markdown",
    },
  },

  -- markdown editing enhancement
  {
    "plasticboy/vim-markdown",
    ft = {
      "markdown",
    },
  },

  -- table editing enhancement
  {
    "dhruvasagar/vim-table-mode",
    cmd = "TableModeToggle",
  },
}

local git_tools = {
  -- A git tool like magit in Emacs
  {
    "tpope/vim-fugitive",
    cmd = {
      "G",
      "Git",
      "Ggrep",
      "Gdiffsplit",
      "GBrowse",
    },
  },

  -- Call the lazygit inside neovim, relies on lazygit executable
  {
    "kdheepak/lazygit.nvim",
    setup = function()
      vim.g.lazygit_floating_window_winblend = 0
      vim.g.lazygit_floating_window_scaling_factor = 1
      vim.g.lazygit_floating_window_corner_chars = {
        "╭",
        "╮",
        "╰",
        "╯",
      }
      vim.g.lazygit_floating_window_use_plenary = 0
      vim.g.lazygit_use_neovim_remote = 1
      if vim.g.lazygit_use_neovim_remote == 1 and vim.fn.executable("nvr") then
        vim.env.GIT_EDITOR = "nvr -cc split --remote-wait +'set bufhidden=wipe'"
      end
    end,
    cmd = "LazyGit",
  },

  -- Show git information in neovim
  {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    event = "BufRead",
    config = function()
      require("config.gitsign")
    end,
  },
  {
    "sindrets/diffview.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("diffview").setup({
        -- see configuration in
        -- https://github.com/sindrets/diffview.nvim#configuration
      })
    end,
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
  },
  {
    "rbong/vim-flog",
    -- run command after the vim fugitive
    cmd = {
      "Flog",
      "Flogsplit",
    },
  },
}

local editor_enhance = {
  -- list of nerdfont icons
  {
    "kyazdani42/nvim-web-devicons",
    event = "BufRead",
  },

  -- fancy status line
  {
    "Avimitin/galaxyline.nvim",
    branch = "main",
    after = "nvim-web-devicons",
    config = function()
      require("config.statusline")
    end,
  },

  -- buffer manager
  {
    "akinsho/nvim-bufferline.lua",
    config = function()
      require("config.bufferline_config")
    end,
    event = "BufRead",
  },

  -- tree style file manager
  {
    "kyazdani42/nvim-tree.lua",
    config = function()
      require("config.nvimtree")
    end,
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle",
    },
  },

  -- open a popup terminal
  {
    "numtostr/FTerm.nvim",
    config = function()
      require("config.fterm")
    end,
    cmd = {
      "FTermToggle",
    },
  },

  -- generate color from hex/rgb code
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup({
        "*", -- Highlight all files, but customize some others.
        css = {
          rgb_fn = true,
        }, -- Enable parsing rgb(...) functions in css.
      })
    end,
    cmd = {
      "ColorizerToggle",
      -- this help generate color for no filetype file
      "ColorizerAttachToBuffer",
    },
  },

  -- editing with multiple cursor
  {
    "mg979/vim-visual-multi",
    event = "InsertEnter",
    branch = "master",
  },

  -- Linux coreutil in vim
  {
    "tpope/vim-eunuch",
    cmd = {
      -- Sudo needs you to configured the /etc/sudo.conf file to set the
      -- correct askpass executable.
      "SudoWrite",
      "SudoEdit",
      "Delete",
      "Unlink",
      "Move",
      "Rename",
      "Chmod",
      "Mkdir",
    },
  },

  -- a dashboard that useless but beautiful
  {
    "glepnir/dashboard-nvim",
    cmd = {
      "Dashboard",
    },
    config = function()
      require("config.dashboard")
    end,
  },

  -- cd into the root directory
  {
    "airblade/vim-rooter",
    cmd = "Rooter",
    setup = function()
      vim.g.rooter_manual_only = 1
      vim.g.rooter_change_directory_for_non_project_files = "current"
      vim.g.rooter_patterns = {
        ".git",
        "Cargo.toml",
      }
    end,
  },

  -- telescope: extensible fuzzy file finder
  {
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("config.telescope_config")
    end,
    module = "telescope",
  },

  -- record and manage your paste history
  {
    "AckslD/nvim-neoclip.lua",
    cmd = {
      "ClipRec",
      "ClipView",
    },
    config = function()
      require("neoclip").setup()
      require("telescope").load_extension("neoclip")
    end,
  },

  -- Press enter to select text object
  {
    "gcmt/wildfire.vim",
    keys = {
      {
        "n",
        "<Enter>",
      },
      {
        "n",
        "<leader><Enter>",
      },
    },
  },

  -- surrounding select text with given signs
  {
    "tpope/vim-surround",
    event = "BufRead",
    config = function()
      -- release the S key to the lightspeed
      require("utils").map("x", "S", "<Plug>Lightspeed_S", {
        noremap = false,
      })
      -- and remap it to gs
      require("utils").map("x", "gs", "<Plug>VSurround", {
        noremap = false,
      })
    end,
  },

  -- a swiss knife for aligning text
  {
    "junegunn/vim-easy-align",
    cmd = "EasyAlign",
  },

  -- Fix the CursorHold performance bug
  {
    "antoinemadec/FixCursorHold.nvim",
  },

  -- Move cursor by text search
  {
    "ggandor/lightspeed.nvim",
    keys = {
      { "n", "s" },
      { "v", "s" },
      { "n", "S" },
      { "v", "S" },
      { "n", "f" },
      { "n", "F" },
      { "n", "t" },
      { "n", "T" },
      { "v", "f" },
      { "v", "F" },
      { "v", "t" },
      { "v", "T" },
    },
  },

  -- Enhanced the `%` keymap
  {
    "andymass/vim-matchup",
    keys = {
      { "n", "%" },
      { "v", "%" },
    },
  },

  -- automatically pairs the bracket
  {
    "windwp/nvim-autopairs",
    config = function()
      require("config.autopairs")
    end,
    after = "nvim-cmp",
  },

  -- close buffer and tab gracefully
  {
    "mhinz/vim-sayonara",
    setup = function()
      vim.g.sayonara_confirm_quit = 1
    end,
    cmd = "Sayonara",
  },

  -- file manager without any dependency
  {
    "obaland/vfiler.vim",
    cmd = "VFiler",
    requires = {
      "obaland/vfiler-column-devicons",
    },
    config = function()
      require("vfiler/config").setup({
        options = {
          columns = "indent,devicons,name,mode,size,time",
        },
      })
    end,
  },

  -- split lines and join lines, useful for closing bracket
  {
    "AndrewRadev/splitjoin.vim",
    keys = {
      { "n", "gJ" },
      { "n", "gS" },
    },
  },

  -- generate line for indent
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("config.indent")
    end,
    event = "BufRead",
  },
  {
    "tpope/vim-repeat",
    keys = {
      { "n", "." },
    },
  },
  {
    "NTBBloodbath/rest.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("rest-nvim").setup({
        -- Open request results in a horizontal split
        result_split_horizontal = false,
        -- Skip SSL verification, useful for unknown certificates
        skip_ssl_verification = false,
        -- Highlight request on run
        highlight = {
          enabled = true,
          timeout = 150,
        },
        result = {
          -- toggle showing URL, HTTP info, headers at top the of result window
          show_url = true,
          show_http_info = true,
          show_headers = true,
        },
        -- Jump to request line on run
        jump_to_request = false,
        env_file = ".env",
        custom_dynamic_variables = {},
        yank_dry_run = true,
      })

      local bufmap = vim.api.nvim_buf_set_keymap
      local opts = {
        noremap = true,
        expr = false,
      }
      bufmap(0, "n", "<Leader>rn", ":lua require('rest-nvim').run()<CR>", opts)
      bufmap(0, "n", "<Leader>rp", ":lua require('rest-nvim').run(true)<CR>", opts)
      bufmap(0, "n", "<Leader>rl", ":lua require('rest-nvim').last()<CR>", opts)
    end,
    ft = "http",
  },
}

local colorscheme = {
  {
    "Avimitin/neovim-deus",
    cond = function()
      return require("colors").theme == "deus"
    end,
    config = function()
      require("colors").deus_setup()
    end,
  },
  {
    "Shatur/neovim-ayu",
    cond = function()
      return require("colors").theme == "ayu"
    end,
    config = function()
      require("colors").ayu_setup()
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    cond = function()
      return require("colors").theme == "kanagawa"
    end,
    config = function()
      require("colors").kanagawa_setup()
    end,
  },
  {
    "sainnhe/everforest",
    cond = function()
      return require("colors").theme == "everforest"
    end,
    config = function()
      require("colors").everforest_setup()
    end,
  },
  {
    "morhetz/gruvbox",
    cond = function()
      return require("colors").theme == "gruvbox"
    end,
    config = function()
      require("colors").gruvbox_setup()
    end,
  },
}

local coding_enhance = {
  {
    "williamboman/nvim-lsp-installer",
    ft = {
      "bash",
      "sh",
      "c",
      "cpp",
      "lua",
      "go",
      "html",
      "toml",
      "json",
      "python",
      "javascript",
    },
    config = function()
      require("lspconfig")
    end,
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("config.lsp")
    end,
    module = "lspconfig",
  },
  {
    "tami5/lspsaga.nvim",
    after = "nvim-lspconfig",
    config = function()
      require("config.lspsaga_setting")
    end,
  },
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    config = function()
      require("config.rust")
    end,
  },
  {
    "saecki/crates.nvim",
    event = {
      "BufRead Cargo.toml",
    },
    requires = {
      {
        "nvim-lua/plenary.nvim",
      },
    },
    config = function()
      require("crates").setup({
        popup = {
          autofocus = true,
          border = "single",
        },
      })
    end,
  }, -- }}}
  {
    "mfussenegger/nvim-dap",
    module = "dap",
    config = function()
      require("config.dap_config")
    end,
  },
  {
    "rcarriga/nvim-dap-ui",
    module = "dapui",
  },
  {
    "simrat39/symbols-outline.nvim",
    config = function()
      require("config.symbols")
    end,
    cmd = "SymbolsOutline",
  },
  {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = function()
      require("config.treesitter")
    end,
  },
  {
    "fatih/vim-go",
    config = function()
      require("config.vim-go")
    end,
    ft = { "go" },
  },
  {
    "rhysd/vim-clang-format",
    ft = {
      "cpp",
      "c",
      "h",
      "hpp",
    },
  },

  -- use `gcc` `gbc` to comment
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
    keys = {
      { "n", "gcc" },
      { "n", "gbc" },
      { "v", "gc" },
      { "v", "gb" },
    },
  },
  {
    "andrejlevkovitch/vim-lua-format",
    ft = "lua",
  },
  {
    "pechorin/any-jump.vim",
    setup = function()
      vim.g.any_jump_window_width_ratio = 0.8
      vim.g.any_jump_window_height_ratio = 0.9
      vim.g.any_jump_disable_default_keybindings = 1
    end,
    cmd = {
      "AnyJump",
      "AnyJumpBack",
    },
  },
  {
    "tpope/vim-dispatch",
    cmd = "Dispatch",
  },
  {
    "j-hui/fidget.nvim",
    after = "nvim-lspconfig",
    config = function()
      require("fidget").setup({})
    end,
  },
}

local completion = {
  -- lot's of pre-set snippets
  {
    "rafamadriz/friendly-snippets",
    event = "InsertEnter",
  },

  -- the completion core
  {
    "hrsh7th/nvim-cmp",
    after = "friendly-snippets",
    config = function()
      require("config.completion")
    end,
    requires = {
      "onsails/lspkind-nvim",
    },
  },

  -- completion source for system path
  {
    "hrsh7th/cmp-path",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for lspconfig
  {
    "hrsh7th/cmp-nvim-lsp",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for word in current buffer
  {
    "hrsh7th/cmp-buffer",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for vsnip snippet plugin
  {
    "hrsh7th/cmp-vsnip",
    after = {
      "nvim-cmp",
    },
  },

  -- the snippet core
  {
    "hrsh7th/vim-vsnip",
    after = {
      "nvim-cmp",
    },
  },
}

return {
  autoload,
  markdown_plugins,
  git_tools,
  editor_enhance,
  colorscheme,
  completion,
  coding_enhance,
}
