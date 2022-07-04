local register = require("plugins").register
local repos = {
  -- prompt up panel to give a key mapping hint
  {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup({
        triggers = "auto",
        triggers_blacklist = {
          -- list of mode / prefixes that should never be hooked by WhichKey
          -- this is mostly relevant for key maps that start with a native binding
          -- most people should not need to change this
          n = { "/", ":", "<", ">" },
          i = { "j", "k" },
          v = { "j", "k" },
        },
      })
      require("mappings.whichkey")
    end,
    -- load this plugin after Neovim UI is already rendered
    event = "VimEnter",
  },

  -- manage undo history
  {
    "simnalamburt/vim-mundo",
    cmd = {
      "MundoToggle",
    },
  },

  -- manage windows
  {
    "sindrets/winshift.nvim",
    cmd = {
      "WinShift",
    },
    config = function()
      require("winshift").setup({})
    end,
  },

  -- list of nerdfont icons
  {
    "kyazdani42/nvim-web-devicons",
    event = "BufRead",
  },

  -- fancy status line
  {
    "glepnir/galaxyline.nvim",
    branch = "main",
    after = "nvim-web-devicons",
    config = function()
      require("plugins.config.galaxyline_cfg")
    end,
  },

  -- buffer manager
  {
    "akinsho/nvim-bufferline.lua",
    config = function()
      require("plugins.config.bufferline_cfg")
    end,
    event = "BufRead",
  },

  -- tree style file manager
  {
    "kyazdani42/nvim-tree.lua",
    config = function()
      require("plugins.config.nvimtree_cfg")
    end,
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle",
    },
  },

  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("plugins.config.toggleterm_cfg")
    end,
    cmd = "ToggleTerm",
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
      require("plugins.config.dashboard_cfg")
    end,
  },

  -- cd into the root directory
  {
    "airblade/vim-rooter",
    event = "BufReadPost",
    config = function()
      vim.cmd("Rooter")
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
      require("plugins.config.telescope_cfg")
    end,
    module = "telescope",
  },

  -- record and manage your paste history
  {
    "AckslD/nvim-neoclip.lua",
    event = "TextYankPost",
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
    },
  },

  -- surrounding select text with given signs
  {
    "tpope/vim-surround",
    event = "BufRead",
    config = function()
      local map = require("mappings.utils").map
      -- release the S key to the lightspeed
      map("x", "S", "<Plug>Lightspeed_S", {
        noremap = false,
      })
      -- and remap it to gs
      map("x", "gs", "<Plug>VSurround", {
        noremap = false,
      })
    end,
  },

  -- a swiss knife for aligning text
  {
    "junegunn/vim-easy-align",
    cmd = "EasyAlign",
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
    config = function()
      require("lightspeed").setup({
        substitute_chars = {
          ["\r"] = "",
          [" "] = "␣",
        },
      })
    end,
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
      require("plugins.config.autopairs_cfg")
    end,
    after = "nvim-cmp",
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

  -- split single line and join multiple lines, useful for closing bracket
  {
    "AndrewRadev/splitjoin.vim",
    keys = {
      { "n", "gJ" },
      { "n", "gS" },
    },
  },

  -- generate line for guiding indent
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("plugins.config.indent_cfg")
    end,
    event = "BufRead",
  },

  -- a curl wrapper in neovim
  {
    "NTBBloodbath/rest.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("plugins.config.rest_nvim_cfg")
    end,
    ft = "http",
  },

  -- sort the number or text
  {
    "sQVe/sort.nvim",
    config = function()
      require("sort").setup({})
    end,
    cmd = "Sort",
  },

  -- scroll smoothly
  {
    "karb94/neoscroll.nvim",
    config = function()
      require("plugins.config.neoscroll_cfg")
    end,
    keys = {
      { "n", "<C-e>" },
      { "n", "<C-y>" },
      { "n", "<C-f>" },
      { "n", "<C-b>" },
      { "n", "<C-j>" },
      { "n", "<C-k>" },
    },
  },

  -- search and replace with a panel
  {
    "windwp/nvim-spectre",
    requires = { "nvim-lua/plenary.nvim" },
    module = "spectre",
  },

  {
    "beauwilliams/focus.nvim",
    event = "WinEnter",
    config = function()
      require("focus").setup({
        excluded_filetypes = { "fterm", "term", "toggleterm", "Mundo", "MundoDiff" },
        signcolumn = false,
      })
    end,
  },

  {
    "stevearc/dressing.nvim",
    module = "vim.ui",
    config = function()
      require("dressing").setup({})
    end,
  },

  -- add notify window
  {
    "rcarriga/nvim-notify",
    module = "vim",
    config = function()
      vim.notify = require("notify")
    end,
  },
}


local colorscheme = {
  -- dark green color scheme
  {
    "Avimitin/neovim-deus",
    cond = function()
      return require("core.colors").theme == "deus"
    end,
    config = function()
      require("core.colors").deus_setup()
    end,
  },

  -- dark purple color scheme
  {
    "rebelot/kanagawa.nvim",
    cond = function()
      return require("core.colors").theme == "kanagawa"
    end,
    config = function()
      require("core.colors").kanagawa_setup()
    end,
  },

  -- GitHub light and dark colorscheme
  {
    "projekt0n/github-nvim-theme",
    cond = function()
      local select = require("core.colors").theme
      for _, avail in ipairs({
        "github_dark",
        "github_dimmed",
        "github_light",
        "github_light_default",
      }) do
        if select == avail then
          return true
        end
      end
      return false
    end,
    config = function()
      require("core.colors").github_setup()
    end,
  },
}

register(repos)
register(colorscheme)
