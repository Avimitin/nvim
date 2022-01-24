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

  local component = {'colors', 'cmp', 'coding', 'mkd', 'git_tools'}
  for _, compo in ipairs(component) do
    for _, plugin in ipairs(require("partial."..compo)) do
      use(plugin)
    end
  end

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

  -- mulit cursor
  use {
    'mg979/vim-visual-multi',
    event = "InsertEnter",
    branch = 'master'
  }

  -- open file when forget sudo
  use {
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
    keys = {
      {"n", "<Enter>"},
      {"n", "<leader><Enter>"},
    },
  }

  -- surrounding select text with given text
  use {
    "tpope/vim-surround",
    event = "BufRead",
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

  -- Fix the CursorHold performance bug
  use {
    'antoinemadec/FixCursorHold.nvim'
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
  }

  use {
    -- Enhanced the `%` keymap
    'andymass/vim-matchup',
    keys = {
      {'n', '%'},
      {'v', '%'}
    }
  }
  -- }}}

  use {
    'windwp/nvim-autopairs',
    config = function()
      require('config.autopairs')
    end,
    after = 'nvim-cmp'
  }

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
    'dstein64/vim-startuptime',
    cmd = "StartupTime"
  }

  use {
    'nvim-orgmode/orgmode',
    config = function()
      require('orgmode').setup {
        org_agenda_files = {'~/Documents/orgfiles/**/*'},
        org_default_notes_file = '~/Documents/orgfiles/inbox.org',
      }
    end
  }
end)

-- vim: foldmethod=marker
