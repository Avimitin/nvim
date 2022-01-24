return {
  -- list of nerdfont icons
  {
    'kyazdani42/nvim-web-devicons',
    event = "BufRead"
  },

  -- fancy status line
  {
    'Avimitin/galaxyline.nvim',
    branch = 'main',
    after = "nvim-web-devicons",
    config = function()
      require("config.statusline")
    end
  },

  -- buffer manager
  {
    'akinsho/nvim-bufferline.lua',
    config = function()
      require("config.bufferline_config")
    end,
    event = "BufRead"
  },

  -- tree style file manager
  {
    'kyazdani42/nvim-tree.lua',
    config = function()
      require("config.nvimtree")
    end,
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle"
    }
  },

  -- open a popup terminal
  {
    'numtostr/FTerm.nvim',
    config = function()
      require("config.fterm")
    end,
    cmd = {
      'FTermToggle'
    }
  },

  -- generate color from hex/rgb code
  {
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
  },

  -- editing with multiple cursor
  {
    'mg979/vim-visual-multi',
    event = "InsertEnter",
    branch = 'master'
  },

  -- Linux coreutil in vim
  {
    'tpope/vim-eunuch',
    cmd = {
      -- Sudo needs you to configured the /etc/sudo.conf file to set the
      -- correct askpass executable.
      'SudoWrite',
      'SudoEdit',
      'Delete',
      'Unlink',
      'Move',
      'Rename',
      'Chmod',
      'Mkdir'
    }
  },

  -- a dashboard that useless but beautiful
  {
    'glepnir/dashboard-nvim',
    cmd = {
      "Dashboard"
    },
    config = function()
      require("config.dashboard")
    end
  },

  -- cd into the root directory
  {
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
  },

  -- telescope: extensible fuzzy file finder
  {
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require("config.telescope_config")
    end,
    module = 'telescope'
  },

  -- record and manage your paste history
  {
    "AckslD/nvim-neoclip.lua",
    cmd = {
      'ClipRec',
      'ClipView'
    },
    config = function()
      require('neoclip').setup()
      require('telescope').load_extension('neoclip')
    end
  },

  -- Press enter to select text object
  {
    'gcmt/wildfire.vim',
    keys = {
      {"n", "<Enter>"},
      {"n", "<leader><Enter>"},
    },
  },

  -- surrounding select text with given signs
  {
    "tpope/vim-surround",
    event = "BufRead",
  },

  -- a swiss knife for aligning text
  {
    'junegunn/vim-easy-align',
    cmd = 'EasyAlign'
  },

  -- speed up neovim!
  {
    'nathom/filetype.nvim',
    config = function()
      require("filetype").setup({
        -- overrides the filetype or function for filetype
        -- See https://github.com/nathom/filetype.nvim#customization
        overrides = {}
      })
    end
  },

  -- Fix the CursorHold performance bug
  {
    'antoinemadec/FixCursorHold.nvim'
  },

  -- Move cursor by text search
  {
    "ggandor/lightspeed.nvim",
    keys = {
      {'n', 's'},
      {'v', 's'},
      {'n', 'S'},
      {'n', 'f'},
      {'n', 'F'},
      {'n', 't'},
      {'n', 'T'},
      {'v', 'f'},
      {'v', 'F'},
      {'v', 't'},
      {'v', 'T'}
    }
  },

  -- Enhanced the `%` keymap
  {
    'andymass/vim-matchup',
    keys = {
      {'n', '%'},
      {'v', '%'}
    }
  },

  -- automatically pairs the bracket
  {
    'windwp/nvim-autopairs',
    config = function()
      require('config.autopairs')
    end,
    after = 'nvim-cmp'
  },

  -- close buffer and tab gracefully
  {
    'mhinz/vim-sayonara',
    setup = function()
      vim.g.sayonara_confirm_quit = 1
    end,
    cmd = 'Sayonara'
  },

  -- file manager without any dependency
  {
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
  },

  -- split lines and join lines, useful for closing bracket
  {
    'AndrewRadev/splitjoin.vim',
    after = "nvim-treesitter",
  },

  -- generate line for indent
  {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      require("config.indent")
    end,
    event = 'BufRead'
  },

  -- manage your life with neovim!
  {
    'nvim-orgmode/orgmode',
    config = function()
      require('orgmode').setup {
        org_agenda_files = {'~/Documents/orgfiles/**/*'},
        org_default_notes_file = '~/Documents/orgfiles/inbox.org',
      }
    end
  }
}
