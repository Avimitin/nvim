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
    keys = { { "n", "u" } },
    branch = "master",
    rc = "multi_cursor",
  },

  -- a dashboard that useless but beautiful
  {
    "glepnir/dashboard-nvim",
    opt = true,
    rc = "dashboard",
  },

  -- Try to find project root and cd into it
  {
    "notjedi/nvim-rooter.lua",
    module = "nvim-rooter",
    rc = "rooter",
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
    rc = "wildfire",
  },

  -- surrounding select text with given signs
  {
    "tpope/vim-surround",
    keys = {
      { "n", "ys" },
      { "n", "yS" },
      { "n", "cs" },
      { "n", "cS" },
      { "n", "ds" },
      { "n", "dS" },
      { "x", "gs" },
      { "x", "gS" },
    },
    config = function()
      local map = require("editor.keymap").map
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
      { "n", "," },
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
    "karb94/neoscroll.nvim",
    rc = "neoscroll",
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

  {
    "petertriho/nvim-scrollbar",
    config = function()
      require("scrollbar").setup({
        marks = {
          Error = { text = { "" } },
          Warn = { text = { "" } },
          Hint = { text = { "" } },
          Info = { text = { "" } },
          GitAdd = { text = "▕" },
          GitChange = { text = "▕" },
        },
        excluded_buftypes = {
          "terminal",
        },
        excluded_filetypes = {
          "prompt",
          "TelescopePrompt",
          "noice",
          "Git",
        },
        handlers = {
          cursor = false,
        },
      })
    end,
    module = "scrollbar",
    event = "VimEnter",
  },

  {
    "kevinhwang91/nvim-hlslens",
    config = function()
      local nmap = require("libs.keymaps").nmap
      nmap(
        "n",
        [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      )
      nmap(
        "N",
        [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]]
      )
      nmap("*", [[*<Cmd>lua require('hlslens').start()<CR>]])
      nmap("#", [[#<Cmd>lua require('hlslens').start()<CR>]])
      nmap("g*", [[g*<Cmd>lua require('hlslens').start()<CR>]])
      nmap("g#", [[g#<Cmd>lua require('hlslens').start()<CR>]])

      require("scrollbar.handlers.search").setup()
    end,
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

  -- Fix the CursorHold performance bug
  {
    "antoinemadec/FixCursorHold.nvim",
  },

  -- cache everything!
  {
    "lewis6991/impatient.nvim",
  },
}
