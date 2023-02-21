local register = require("pack").register

-- Auto-pairs key mappings
register("hrsh7th/nvim-insx", {
  event = "InsertEnter",
  branch = "main",
  config = function()
    require("insx.preset.standard").setup()
  end,
})

-- Neovim API completion sources
register("ii14/emmylua-nvim", {
  lazy = true,
})

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {
  lazy = true,
})

-- Tree-like file browser
register("kyazdani42/nvim-tree.lua", {
  lazy = true,
  keys = {
    {
      "<leader>t",
      function()
        if vim.bo.filetype == "NvimTree" then
          require("nvim-tree").toggle()
        else
          require("nvim-tree").focus()
        end
      end,
    },
  },
  --
  init = function()
    -- Auto load nvim-tree when neovim is started with directory
    vim.api.nvim_create_autocmd("UIEnter", {
      pattern = "*",
      callback = function()
        if vim.fn.argc() == 0 then
          return
        end
        local first_arg = vim.fn.argv(0)
        if not first_arg or #first_arg == 0 then
          return
        end

        vim.loop.fs_stat(
          first_arg,
          vim.schedule_wrap(function(err, stat)
            if err then
              return
            end

            if stat.type ~= "directory" then
              return
            end

            require("nvim-tree").open(first_arg)
          end)
        )
      end,
    })
  end,
  --
  config = function()
    require("tools.nvim_tree")
  end,
}) -- End of nvim-tree

-- Enhanced neovim terminal
register("akinsho/toggleterm.nvim", {
  keys = {
    { "_", "<CMD>ToggleTerm direction=horizontal<CR>" },
    -- moving cursor between terminal
    { "<A-;>", [[<C-\><C-n>]], mode = "t" },
    { "<C-k>", [[<C-\><C-n><C-w>k]], mode = "t" },
    { "<C-l>", [[<C-\><C-n><C-w>l]], mode = "t" },
    { "<C-h>", [[<C-\><C-n><C-w>h]], mode = "t" },
  },
  --
  config = function()
    require("toggleterm").setup({
      -- size can be a number or function which is passed the current terminal
      size = function(term)
        if term.direction == "horizontal" then
          return 15
        elseif term.direction == "vertical" then
          return vim.o.columns * 0.4
        end
      end,
      hide_numbers = true,
      open_mapping = "_",
      insert_mappings = false,
      start_in_insert = true,
      terminal_mappings = false,
      direction = "horizontal", -- 'window' | 'float' | 'vertical' ,
      close_on_exit = true, -- close the terminal window when the process exits
      shade_terminals = false,
      shell = vim.o.shell, -- change the default shell
      float_opts = {
        border = "single",
        winblend = 3,
        highlights = {
          border = "Normal",
          background = "Normal",
        },
      },
      winbar = {
        enabled = true,
        name_formatter = function(term)
          local _, _, name, id = term.name:find([[#(%w+)#(%d)]])
          return string.format("%s (%s)", name, id)
        end,
      },
    })
  end,
}) -- end of toggle term

-- Fuzzy Picker
register("nvim-telescope/telescope.nvim", {
  lazy = true,
  config = function()
    require("tools.telescope")
  end,
  keys = {
    {
      "<leader>f",
      function()
        require("telescope.builtin").find_files(require("telescope.themes").get_ivy())
      end,
    },
    {
      "<leader>@",
      function()
        require("telescope.builtin").lsp_document_symbols(require("telescope.themes").get_ivy())
      end,
    },
    {
      "<leader>s",
      function()
        require("telescope.builtin").live_grep(require("telescope.themes").get_ivy())
      end,
    },
  },
})

-- Quick select for text objects
register("gcmt/wildfire.vim", {
  keys = "<Enter>",
})

-- Surround operation
register("kylechui/nvim-surround", {
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
})

-- Quick moving by two character searching
register("ggandor/leap.nvim", {
  keys = {
    "s",
    "S",
  },
  config = function()
    require("leap").add_default_mappings()
    vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
  end,
})

-- Quick moving by one character searching
register("ggandor/flit.nvim", {
  keys = {
    "f",
    "F",
    "t",
    "T",
  },
  config = function()
    require("flit").setup()
  end,
})

-- Enhanced the `%` keymap
register("andymass/vim-matchup", {
  keys = {
    { mode = "n", "<Plug>(matchup-%)" },
    { mode = "x", "<Plug>(matchup-%)" },
    { mode = "o", "<Plug>(matchup-%)" },
  },
})

-- sort the number or text
register("sQVe/sort.nvim", {
  config = function()
    require("sort").setup({})
  end,
  cmd = "Sort",
})

-- Better search and replace (With Rust regex)
-- Load it with command `:Sed`
register("windwp/nvim-spectre", {
  lazy = true,
  init = function()
    vim.api.nvim_create_user_command("Sed", function()
      require("spectre").open()
    end, {})
  end,
})

-- Highlight search matches
register("kevinhwang91/nvim-hlslens", {
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
    require("scollbar.handlers.search").setup()
  end,
})

-- Auto matically setting tab width by projects
register("tpope/vim-sleuth")

-- Cache everything
register("lewis6991/impatient.nvim")

--- Color utils
register("uga-rosa/ccc.nvim", {
  config = function()
    require("ccc").setup()
  end,
  cmd = {
    "CccPick",
    "CccHighlighterEnable",
  },
})

-- Multiple Cursor
register("mg979/vim-visual-multi", {
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
})

-- Easy aligning text
register("junegunn/vim-easy-align", {
  cmd = "EasyAlign",
  keys = { { "<space>e", ":EasyAlign<CR>", mode = "x" } },
})
