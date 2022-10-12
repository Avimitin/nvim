local config = require("plugins.enhance.config")

local repos = {
  -- prompt up panel to give a key mapping hint
  {
    "folke/which-key.nvim",
    config = config.whichkey_config,
    -- load this plugin after Neovim UI is already rendered
    keys = {
      "g",
      "v",
      "c",
      "d",
      "z",
      ";",
    },
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
    module = "nvim-web-devicons",
  },

  -- fancy status line
  {
    "glepnir/galaxyline.nvim",
    branch = "main",
    event = "UIEnter",
    config = function()
      require("plugins.enhance.config").galaxyline_config()
    end,
  },

  -- buffer manager
  {
    "akinsho/nvim-bufferline.lua",
    config = config.bufferline_cfg,
    event = "BufRead",
  },

  -- tree style file manager
  {
    "kyazdani42/nvim-tree.lua",
    config = config.nvim_tree_config,
    module = "nvim-tree",
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle",
    },
  },

  {
    "akinsho/toggleterm.nvim",
    config = config.toggleterm_config,
    cmd = "ToggleTerm",
  },

  -- Preview and pick color inside neovim
  {
    "uga-rosa/ccc.nvim",
    config = function()
      require("ccc").setup()
    end,
    cmd = {
      "CccPick",
      "CccHighlighterEnable",
    },
  },

  -- editing with multiple cursor
  {
    "mg979/vim-visual-multi",
    event = "InsertEnter",
    keys = "u",
    branch = "master",
  },

  -- a dashboard that useless but beautiful
  {
    "glepnir/dashboard-nvim",
    event = "VimEnter",
    config = function()
      local config = require("plugins.enhance.config")
      config.dashboard_cfg()
    end,
  },

  -- Try to find project root and cd into it
  {
    "notjedi/nvim-rooter.lua",
    module = "nvim-rooter",
  },

  -- telescope: extensible fuzzy file finder
  {
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
    },
    config = config.telescope_config,
    module = "telescope",
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
      local map = require("editor.utils").map
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
    after = "nvim-treesitter",
  },

  -- automatically pairs the bracket
  {
    "windwp/nvim-autopairs",
    config = config.autopairs_config,
    after = "nvim-cmp",
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
    config = config.indent_config,
    event = "BufRead",
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
    config = config.neoscroll_config,
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
    event = "UIEnter",
    config = function()
      vim.notify = require("notify")
    end,
  },

  -- repeat motion by key .
  {
    "tpope/vim-repeat",
    keys = {
      { "n", "." },
    },
  },

  {
    "monaqa/dial.nvim",
    module = "dial",
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        -- default augends used when no group name is specified
        default = {
          augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
          augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
          augend.date.alias["%Y/%m/%d"], -- date (2022/02/19, etc.)
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%m/%d"],
          augend.date.alias["%H:%M"],
          augend.constant.alias.bool, -- boolean value (true <-> false)
          -- switch between and/or &&/||
          augend.constant.new({
            elements = { "and", "or" },
            word = true,
            cyclic = true,
          }),
          augend.constant.new({
            elements = { "&&", "||" },
            word = false,
            cyclic = true,
          }),
          augend.semver.alias.semver,
        },
      })
    end,
  },

  --
  --
  -- Auto Load
  --
  --
  -- adjust the shiftwidth and expandtab settins
  {
    "tpope/vim-sleuth",
  },

  -- Fix the CursorHold performance bug
  {
    "antoinemadec/FixCursorHold.nvim",
  },

  -- cache everything!
  {
    "lewis6991/impatient.nvim",
  },
}

return repos
