return {
  -- Auto-pairs key mappings
  {
    "hrsh7th/nvim-insx",
    event = "InsertEnter",
    branch = "main",
    config = function()
      require("insx.preset.standard").setup()
    end,
  },

  -- Neovim API completion sources
  {
    "ii14/emmylua-nvim",
    lazy = true,
  },

  -- Neovim Library wrapper
  {
    "nvim-lua/plenary.nvim",
    lazy = true,
  },

  -- UI Library
  {
    "MunifTanjim/nui.nvim",
    lazy = true,
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    config = function()
      vim.g.neo_tree_remove_legacy_commands = 1

      require("neo-tree").setup({
        close_if_last_window = true,
        sources = {
          "filesystem",
          "buffers",
          "git_status",
          "document_symbols",
        },
        open_files_do_not_replace_types = {
          "terminal",
          "trouble",
          "qf",
          "diff",
          "fugitive",
          "fugitiveblame",
          "notify",
        },
        window = {
          width = 30,
          mappings = {
            ["<Tab>"] = "next_source",
            ["<S-Tab>"] = "prev_source",
          },
        },
        source_selector = {
          winbar = true,
          statusline = false,
          sources = {
            { source = "filesystem" },
            { source = "git_status" },
            { source = "document_symbols" },
          },
        },
      })
    end,
    -- End of config
    keys = {
      { "<leader>t", "<CMD>Neotree action=focus toggle=true reveal=true position=left<CR>" },
      { "<leader>fl", "<CMD>Neotree source=filesystem reveal=true position=float<CR>" },
    },
    cmd = {
      "Neotree",
    },
  },

  -- Fuzzy Picker
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    config = function()
      require("plugins.tools.telescope")
    end,
    keys = {
      {
        "<leader>ff",
        function()
          require("telescope.builtin").find_files(
            require("telescope.themes").get_ivy({ hidden = true })
          )
        end,
        desc = "Find file",
      },
      {
        "<leader>fd",
        function()
          require("telescope.builtin").lsp_document_symbols(require("telescope.themes").get_ivy())
        end,
        desc = "Find symbol",
      },
      {
        "<leader>fs",
        function()
          require("telescope.builtin").live_grep(require("telescope.themes").get_ivy())
        end,
        desc = "Find keyword",
      },
    },
  },

  -- Quick select for text objects
  {
    "gcmt/wildfire.vim",
    keys = "<Enter>",
  },

  -- Surround operation
  {
    "kylechui/nvim-surround",
    keys = {
      "ys",
      "yS",
      "cs",
      "cS",
      "ds",
      "dS",
      { "gs", mode = { "x", "n" } },
      { "gS", mode = { "x", "n" } },
      { "<C-g>", mode = "i" },
    },
    config = function()
      require("nvim-surround").setup({
        keymaps = {
          visual = "gs",
        },
        -- add new object "y" for operating on Types. For example: `Vector<String>`
        surrounds = {
          ["y"] = {
            add = function()
              local result = require("nvim-surround.config").get_input("Enter the type name: ")
              if result then
                return { { result .. "<" }, { ">" } }
              end
            end,
            find = function()
              return require("nvim-surround.config").get_selection({
                pattern = "[^=%s%(%)]+%b<>",
              })
            end,
            delete = "^(.-<)().-(>)()$",
            change = {
              target = "^.-([%w_]+)()<.->()()$",
              replacement = function()
                local result =
                  require("nvim-surround.config").get_input("Enter new type replacement: ")
                if result then
                  return { { result }, { "" } }
                end
              end,
            },
          },
        },
      })
    end,
  },

  -- Quick moving by two character searching
  {
    "ggandor/leap.nvim",
    keys = {
      "s",
      "S",
      { "s", mode = "x" },
      { "S", mode = "x" },
      { "gj", require("plugins.tools.line_leap").leap_to_line, desc = "Leap to line" },
    },
    config = function()
      require("leap").add_default_mappings()
      require("leap").opts.safe_labels = {}
    end,
  },

  -- Quick moving by one character searching
  {
    "ggandor/flit.nvim",
    keys = {
      "f",
      "F",
      "t",
      "T",
    },
    config = function()
      require("flit").setup()
    end,
  },

  -- sort the number or text
  {
    "sQVe/sort.nvim",
    config = function()
      require("sort").setup({})
    end,
    cmd = "Sort",
  },

  -- Highlight search matches
  {
    "kevinhwang91/nvim-hlslens",
    keys = {
      {
        "n",
        [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
      },
      {
        "N",
        [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
      },
      { "*", [[*<Cmd>lua require('hlslens').start()<CR>]] },
      { "#", [[#<Cmd>lua require('hlslens').start()<CR>]] },
      { "g*", [[g*<Cmd>lua require('hlslens').start()<CR>]] },
      { "g#", [[g#<Cmd>lua require('hlslens').start()<CR>]] },
    },
    config = function()
      require("hlslens").setup()
      require("scrollbar.handlers.search").setup()
    end,
  },

  -- Auto matically setting tab width by projects
  { "tpope/vim-sleuth" },

  -- Cache everything
  { "lewis6991/impatient.nvim" },

  --- Color utils
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

  -- Multiple Cursor
  {
    "mg979/vim-visual-multi",
    keys = {
      "un",
      "<C-down>",
      "<C-up>",
      "uA",
      "ux",
      { mode = "x", "ux" },
      { mode = "x", "uA" },
      { mode = "x", "ua" },
      { mode = "x", "uf" },
      { mode = "x", "uc" },
    },
    init = function()
      -- remove the original key mappings. It is mapped to <C-z> already
      vim.keymap.set("n", "u", "<nop>")
      vim.keymap.set("x", "u", "<nop>")

      -- then, update the visual multi key mappings
      vim.g.VM_maps = {
        ["Find Under"] = "un",
        ["Find Subword Under"] = "un",
        ["Select Cursor Down"] = "<C-down>",
        ["Select Cursor Up"] = "<C-up>",
        ["Select All"] = "uA",
        ["Undo"] = "<C-z>",
        ["Redo"] = "<C-r>",
        ["Start Regex Search"] = "ux",
        ["Visual Regex"] = "ux",
        ["Visual All"] = "uA",
        ["Visual Add"] = "ua",
        ["Visual Find"] = "uf",
        ["Visual Cursors"] = "uc",
      }
    end,
  },

  -- Easy aligning text
  {
    "junegunn/vim-easy-align",
    cmd = "EasyAlign",
    keys = { { "<space>e", ":EasyAlign<CR>", mode = "x" } },
  },

  -- Show key stroke
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = function()
      local whichkey = require("which-key")
      whichkey.setup({})

      local ngrp = {
        mode = "n",
        ["g"] = { name = "+LSP" },
        ["<leader>g"] = { name = "+Git" },
        ["<leader>f"] = { name = "+Telescope" },
      }
      whichkey.register(ngrp)
    end,
  },

  -- Split and Join
  {
    "Wansmer/treesj",
    keys = {
      {
        "<leader>sj",
        function()
          require("treesj").toggle()
        end,
        desc = "Split or Join multiple line",
      },
    },
    config = function()
      require("treesj").setup({
        use_default_keymaps = false,
      })
    end,
  },

  {
    "chrisgrieser/nvim-spider",
    keys = vim
      .iter({ "w", "e", "b" })
      :map(function(key)
        return {
          key,
          function()
            require("spider").motion(key)
          end,
          desc = "Spider motion" .. key,
        }
      end)
      :totable(),

    lazy = true,
  },
}
