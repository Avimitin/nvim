require('packer').init {
  display = {
    open_fn = function()
      return require("packer.util").float {
        border = "single"
      }
    end
  },
  git = {
    clone_timeout = 60 -- Timeout, in seconds, for git clones
  }
}

local map = require('utils').map

return require('packer').startup(function(use)
  -- Packer can manage itself
  use {
    "wbthomason/packer.nvim",
    event = "VimEnter"
  }

  -- ======================= EDITOR SETUP ==============================
  -- Beautify{{{
  use {
    'kyazdani42/nvim-web-devicons',
    event = "BufRead"
  }

  use {
    'Avimitin/galaxyline.nvim',
    branch = 'main',
    after = "nvim-web-devicons",
    config = function()
      require("config.statusline")
    end
  }

  use {
    'kyazdani42/nvim-tree.lua',
    config = function()
      require("config.nvimtree")
    end,
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle"
    }
  }

  use {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      require("config.indent")
    end,
    event = 'BufRead'
  }

  -- nvim-bufferline: better buffer line--
  use {
    'akinsho/nvim-bufferline.lua',
    config = function()
      require("config.bufferline_config")
    end,
    event = "BufRead"
  }

  -- neovim color theme
  use {
    'Avimitin/neovim-deus',
    'Shatur/neovim-ayu',
    'rebelot/kanagawa.nvim'
  }
  -- }}}

  -- ==================== CODING ==================================

  -- Completion {{{
  use {
    'rafamadriz/friendly-snippets',
    event = "InsertEnter"
  }

  -- ======= Completion =========
  use {
    'hrsh7th/nvim-cmp',
    after = "friendly-snippets",
    config = function()
      require("config.completion")
    end,
    requires = {
      'onsails/lspkind-nvim'
    }
  }

  use {
    'hrsh7th/cmp-path',
    after = {
      'nvim-cmp'
    }
  }

  use {
    'hrsh7th/cmp-nvim-lsp',
    after = {
      'nvim-cmp'
    }
  }

  use {
    'hrsh7th/cmp-buffer',
    after = {
      'nvim-cmp'
    }
  }

  use {
    'hrsh7th/cmp-vsnip',
    after = {
      'nvim-cmp'
    }
  }

  use {
    'hrsh7th/vim-vsnip',
    after = {
      'nvim-cmp'
    }
  }

  use {
    "lukas-reineke/cmp-rg",
    after = {
      'nvim-cmp'
    }
  }
  -- }}}

  -- ========= LSP ============{{{
  use {
    'williamboman/nvim-lsp-installer',
    ft = {
      "bash", "sh",
      "c", "cpp",
      "lua", "go",
      "html",
      "toml",
      "json",
      "python",
      "javascript"
    },
    config = function()
      require("lspconfig")
    end
  }

  use {
    'neovim/nvim-lspconfig',
    config = function()
      require("config.lsp")
    end,
    module = "lspconfig"
  }

  use {
    'tami5/lspsaga.nvim',
    after = "nvim-lspconfig",
    config = function()
      local lspsaga = require 'lspsaga'
      lspsaga.setup { -- defaults ...
        debug = false,
        use_saga_diagnostic_sign = true,
        -- diagnostic sign
        error_sign = "",
        warn_sign = "",
        hint_sign = "",
        infor_sign = "",
        diagnostic_header_icon = "   ",
        -- code action title icon
        code_action_icon = " ",
        code_action_prompt = {
          enable = true,
          sign = true,
          sign_priority = 40,
          virtual_text = true
        },
        finder_definition_icon = "  ",
        finder_reference_icon = "  ",
        max_preview_lines = 10,
        finder_action_keys = {
          open = "o",
          vsplit = "s",
          split = "i",
          quit = "q",
          scroll_down = "<C-f>",
          scroll_up = "<C-b>"
        },
        code_action_keys = {
          quit = "q",
          exec = "<CR>"
        },
        rename_action_keys = {
          quit = "<C-c>",
          exec = "<CR>"
        },
        definition_preview_icon = "  ",
        border_style = "single",
        rename_prompt_prefix = "➤",
        server_filetype_map = {},
        diagnostic_prefix_format = "%d. "
      }
    end
  }

  -- RUST
  use {
    'simrat39/rust-tools.nvim',
    ft = "rust",
    config = function()
      require("config.rust")
    end
  }

  use {
    'mfussenegger/nvim-dap',
    module = "dap",
    config = function()
      require("config.dap_config")
    end
  }

  use {
    'rcarriga/nvim-dap-ui',
    module = "dapui"
  }
  -- }}}

  use {
    'rust-lang/rust.vim',
    after = "rust-tools.nvim",
    setup = function()
      vim.g.rust_clip_command = 'xclip -selection clipboard'
    end
  }

  use {
    'simrat39/symbols-outline.nvim',
    config = function()
      require("config.symbols")
    end,
    cmd = "SymbolsOutline"
  }

  -- treesitter: support more colorful highlighting
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    ft = {
      -- This are some filetype I used common
      -- Add yours below.
      -- {{{
      -- programming language
      "c", "cpp",
      "rust", "go",
      "json", "lua",
      "lua",
      "fish", "c",
      "javascript",
      "python",
      "ruby",
      -- shell
      "zsh",
      "bash", "sh",
      -- configuration format
      "json",
      "toml",
      "yaml",
      -- frontend
      "css", "html"
      -- }}}
    },
    config = function()
      require('config.treesitter')
    end
  }

  -- git information
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    event = "BufRead",
    config = function()
      require("config.gitsign")
    end
  }

  -- Golang
  use {
    'fatih/vim-go',
    config = function()
      require("config.vim-go")
    end,
    ft = {"go"}
  }

  -- CPP
  use {
    'rhysd/vim-clang-format',
    ft = {
      'cpp', 'c',
      'h', 'hpp'
    }
  }

  -- =================== Utilities =========================

  -- markdown toc
  use {
    'mzlogin/vim-markdown-toc',
    cmd = {
      'GenTocGFM'
    }
  }

  -- markdown preview
  use {
    'iamcco/markdown-preview.nvim',
    run = function()
      vim.fn['mkdp#util#install']()
    end,
    config = function()
      require("config.mkdp")
    end,
    ft = {
      "markdown"
    }
  }

  use {
    'plasticboy/vim-markdown',
    ft = {
      "markdown"
    }
  }

  use {
    'dhruvasagar/vim-table-mode',
    cmd = 'TableModeToggle'
  }

  -- open a big terminal
  use {
    'numtostr/FTerm.nvim',
    config = function()
      require("config.fterm")
    end,
    cmd = {
      'FTermToggle'
    }
  }

  use {
    'kassio/neoterm',
    cmd = {
      'T', "Tkill",
      "Tclose",
      "Tmap"
    }
  }

  -- show color at words
  use {
    'RRethy/vim-hexokinase',
    run = 'make',
    cmd = "HexokinaseToggle",
    setup = function()
      vim.g.Hexokinase_highlighters = {
        'backgroundfull'
      }
      vim.g.Hexokinase_optInPatterns = {
        'full_hex',
        'rgb',
        'rgba',
        'hsl',
        'hsla'
      }
    end
  }

  -- vim-commentary: for quickly commenting--
  use {
    'tpope/vim-commentary',
    event = "BufRead"
  }

  -- mulit cursor
  use {
    'mg979/vim-visual-multi',
    event = "InsertEnter",
    branch = 'master'
  }

  -- open file when forget sudo
  use {
    'lambdalisue/suda.vim',
    cmd = {
      'SudaWrite',
      'SudaRead'
    }
  }

  use {
    "andrejlevkovitch/vim-lua-format",
    ft = "lua"
  }

  use {
    'glepnir/dashboard-nvim',
    cmd = {
      "Dashboard"
    },
    config = function()
      require("config.dashboard")
    end
  }

  use {
    'airblade/vim-rooter',
    cmd = "Rooter",
    setup = function()
      vim.g.rooter_manual_only = 1
      vim.g.rooter_change_directory_for_non_project_files = 'current'
      vim.g.rooter_patterns = {
        '.git',
        'Cargo.toml'
      }
    end
  }

  use {
    'pechorin/any-jump.vim',
    setup = function()
      vim.g.any_jump_window_width_ratio = 0.8
      vim.g.any_jump_window_height_ratio = 0.9
      vim.g.any_jump_disable_default_keybindings = 1
    end,
    cmd = {
      'AnyJump',
      'AnyJumpBack'
    }
  }

  -- telescope: extensible fuzzy file finder--
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require("config.telescope_config")
    end,
    module = 'telescope'
  }

  use {
    "AckslD/nvim-neoclip.lua",
    cmd = {
      'ClipRec',
      'ClipView'
    },
    config = function()
      require('neoclip').setup()
      require('telescope').load_extension('neoclip')
    end
  }

  -- Enhancement{{{
  -- Select text object
  use {
    'gcmt/wildfire.vim',
    event = "BufRead"
  }

  -- surrounding select text with given text
  use {
    "tpope/vim-surround",
    after = "wildfire.vim"
  }

  -- align
  use {
    'junegunn/vim-easy-align',
    cmd = 'EasyAlign'
  }

  -- speed up neovim!
  use {
    'nathom/filetype.nvim',
    config = function()
      require("filetype").setup({
        -- overrides the filetype or function for filetype
        -- See https://github.com/nathom/filetype.nvim#customization
        overrides = {}
      })
    end
  }

  use {
    'beauwilliams/focus.nvim',
    cmd = {
      "FocusSplitNicely",
      "FocusSplitCycle"
    },
    module = "focus",
    config = function()
      require("focus").setup({
        hybridnumber = true,
        bufnew = true
      })
      map("n", "<C-W>h", ":FocusSplitLeft<CR>")
      map("n", "<C-W>l", ":FocusSplitRight<CR>")
      map("n", "<C-W>j", ":FocusSplitDown<CR>")
      map("n", "<C-W>k", ":FocusSplitUp<CR>")
      map("n", "<C-W>s", ":FocusSplitNicely<CR>")
    end
  }

  use {
    "ggandor/lightspeed.nvim"
  }

  use {
    -- Enhanced the `%` keymap
    'andymass/vim-matchup',
    event = "BufRead"
  }
  -- }}}

  -- Git tools{{{
  use {
    'windwp/nvim-autopairs',
    config = function()
      require('config.autopairs')
    end,
    after = 'nvim-cmp'
  }

  use {
    'tpope/vim-fugitive',
    cmd = {
      'G', 'Git',
      'Ggrep',
      'Gdiffsplit',
      'GBrowse'
    }
  }

  use {
    'Avimitin/lazygit.nvim',
    setup = function()
      vim.g.lazygit_floating_window_winblend = 0
      vim.g.lazygit_floating_window_scaling_factor = 1
      vim.g.lazygit_floating_window_corner_chars = {
        '╭',
        '╮',
        '╰', '╯'
      }
      vim.g.lazygit_floating_window_use_plenary = 0
      vim.g.lazygit_use_neovim_remote = 1
      if vim.g.lazygit_use_neovim_remote == 1 and vim.fn.executable('nvr') then
        vim.env.GIT_EDITOR = "nvr -cc split --remote-wait +'set bufhidden=wipe'"
      end
    end,
    cmd = "LazyGit"
  }
  -- }}}

  use {
    'mhinz/vim-sayonara',
    setup = function()
      vim.g.sayonara_confirm_quit = 1
    end,
    cmd = 'Sayonara'
  }

  use {
    'obaland/vfiler.vim',
    cmd = 'VFiler',
    requires = {
      'obaland/vfiler-column-devicons'
    },
    config = function()
      require'vfiler/config'.setup {
        options = {
          columns = 'indent,devicons,name,mode,size,time'
        }
      }
    end
  }

  use {
    'AndrewRadev/splitjoin.vim',
    after = "nvim-treesitter"
  }

  use {
    "wakatime/vim-wakatime"
  }

  use {
    'AndrewRadev/splitjoin.vim',
    after = "nvim-treesitter"
  }
end)
-- vim: foldmethod=marker
