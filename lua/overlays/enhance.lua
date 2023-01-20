return {
  {
    "nvim-lua/plenary.nvim",
    module = "plenary",
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
    rc = "galaxyline",
  },

  -- buffer manager
  {
    "akinsho/nvim-bufferline.lua",
    rc = "bufferline",
    event = "BufRead",
  },

  -- tree style file manager
  {
    "kyazdani42/nvim-tree.lua",
    rc = "nvim_tree",
    module = "nvim-tree",
    cmd = {
      "NvimTreeRefresh",
      "NvimTreeToggle",
    },
  },

  {
    "akinsho/toggleterm.nvim",
    rc = "toggleterm",
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
    keys = {
      { "n", "un" },
      { "n", "ux" },
      { "n", "<C-down>" },
      { "n", "<C-up>" },
      { "x", "ua" },
      { "x", "uf" },
    },
    branch = "master",
  },

  -- a dashboard that useless but beautiful
  {
    "glepnir/dashboard-nvim",
    opt = true,
    rc = "dashboard",
  },

  -- telescope: extensible fuzzy file finder
  {
    "nvim-telescope/telescope.nvim",
    rc = "telescope",
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
    "kylechui/nvim-surround",
    keys = {
      { "n", "ys" },
      { "n", "yS" },
      { "n", "cs" },
      { "n", "cS" },
      { "n", "ds" },
      { "n", "dS" },
      { "x", "gs" },
      { "x", "gS" },
      { "i", "<C-g>" },
    },
    config = function()
      require("nvim-surround").setup({
        keymaps = {
          visual = "gs",
        },
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
    "ggandor/leap.nvim",
    keys = {
      { "n", "s" },
      { "n", "S" },
      { "n", "gs" },
    },
    config = function()
      require("leap").add_default_mappings()
      vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
    end,
  },

  {
    "ggandor/flit.nvim",
    keys = {
      { "n", "f" },
      { "n", "F" },
      { "n", "t" },
      { "n", "T" },
    },
    config = function()
      require("packer").loader("leap.nvim")
      require("flit").setup()
    end,
  },

  -- Enhanced the `%` keymap
  {
    "andymass/vim-matchup",
    keys = {
      { "n", "<Plug>(matchup-%)" },
      { "x", "<Plug>(matchup-%)" },
      { "o", "<Plug>(matchup-%)" },
    },
  },

  -- automatically pairs the bracket
  {
    "windwp/nvim-autopairs",
    rc = "autopairs",
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
    rc = "indent",
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
    "declancm/cinnamon.nvim",
    rc = "cinnamon",
    module = "cinnamon",
  },

  -- search and replace with a panel
  {
    "windwp/nvim-spectre",
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
      vim.notify = function(msg, level, opts)
        local function split_length(text, length)
          local lines = {}
          local next_line
          while true do
            if #text == 0 then
              return lines
            end
            next_line, text = text:sub(1, length), text:sub(length)
            lines[#lines + 1] = next_line
          end
        end

        if type(msg) == "string" then
          if msg:len() < 72 then
            return require("notify")(msg, level, opts)
          end
          msg = vim.split(msg, "\n")
        end
        local truncated = {}
        for _, line in ipairs(msg) do
          local new_lines = split_length(line, 72)
          for _, l in ipairs(new_lines) do
            truncated[#truncated + 1] = l
          end
        end
        return require("notify")(truncated, level, opts)
      end
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
    rc = "dial",
  },

  {
    "petertriho/nvim-scrollbar",
    module = "scrollbar",
    event = "VimEnter",
    rc = "scrollbar",
  },

  {
    "kevinhwang91/nvim-hlslens",
    rc = "hlslens",
    keys = {
      { "n", "n" },
      { "n", "N" },
      { "n", "*" },
      { "n", "#" },
      { "n", "g*" },
      { "n", "g#" },
    },
  },

  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    config = function()
      require("todo-comments").setup({ signs = false })
    end,
  },

  {
    "anuvyklack/hydra.nvim",
    rc = "hydra",
    module = "hydra",
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

  -- cache everything!
  {
    "lewis6991/impatient.nvim",
  },
}
