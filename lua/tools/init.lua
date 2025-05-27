local register = require("pack").register

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {
  lazy = true,
})

-- UI Library
register("MunifTanjim/nui.nvim", {
  lazy = true,
})

-- Use oil for main file management, use neo-tree for treestyle display only.
register("stevearc/oil.nvim", {
  cmd = { "Oil" },
  lazy = false,
  keys = {
    {
      "_",
      function()
        require("oil").open(vim.fn.getcwd())
      end,
      desc = "Open cwd",
    },
    { "-", "<CMD>Oil<CR>", { desc = "Open parent directory" } },
  },
  config = function()
    require("oil").setup({
      float = {
        border = "solid",
        max_width = 200,
        max_height = 50,
      },
      keymaps = {
        ["q"] = "actions.close",
      },
      view_options = {
        show_hidden = true,
      },
    })
  end,
})

local function my_ivy(opt)
  local my_opt = {
    borderchars = {
      prompt = { " " },
      results = { " " },
      preview = { " " },
    },
  }
  local exted = vim.tbl_deep_extend("force", my_opt, opt or {})
  return require("telescope.themes").get_ivy(exted)
end

-- Fuzzy Picker
register("nvim-telescope/telescope.nvim", {
  lazy = true,
  config = function()
    require("telescope").setup({
      defaults = {
        prompt_prefix = "  ",
        entry_prefix = "  ",
        defaults = {
          vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--trim", -- add this value
          },
        },
      },
      pickers = {
        find_files = {
          -- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
          find_command = { "rg", "--files", "--hidden", "--glob", "!**/.git/*" },
        },
      },
    })
  end,
  keys = {
    {
      "<leader>ff",
      function()
        local function my_find_file(opt)
          if not vim.b._is_inside_git_worktree then
            vim.fn.system("git rev-parse --is-inside-work-tree")
            vim.b._is_inside_git_worktree = vim.v.shell_error == 0
          end
          if vim.b._is_inside_git_worktree then
            return require("telescope.builtin").git_files(opt)
          else
            return require("telescope.builtin").find_files(opt)
          end
        end

        my_find_file(my_ivy({ hidden = true }))
      end,
      desc = "Find file",
    },
    {
      "<leader>fs",
      function()
        require("telescope.builtin").lsp_dynamic_workspace_symbols(my_ivy())
      end,
      desc = "Find symbol",
    },
    {
      "<leader>fg",
      function()
        require("telescope.builtin").live_grep(my_ivy())
      end,
      desc = "Find keyword",
    },
  },
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
    { "gs", mode = { "x" } },
    { "gS", mode = { "x" } },
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
    { "s", "<Plug>(leap-forward-to)", mode = { "n", "x", "o" }, desc = "Leap forward to" },
    { "S", "<Plug>(leap-backward-to)", mode = { "n", "x", "o" }, desc = "Leap backward to" },
    { "x", "<Plug>(leap-forward-till)", mode = { "x", "o" }, desc = "Leap forward until" },
    { "X", "<Plug>(leap-backward-till)", mode = { "x", "o" }, desc = "Leap backward until" },
    { "gw", "<Plug>(leap-from-window)", mode = { "n" }, desc = "Leap from window" },
    { "gW", "<Plug>(leap-cross-window)", mode = { "n" }, desc = "Leap cross window" },
  },
})

-- sort the number or text
register("sQVe/sort.nvim", {
  config = function()
    require("sort").setup({})
  end,
  cmd = "Sort",
})

-- Highlight search matches
register("kevinhwang91/nvim-hlslens", {
  keys = {
    {
      "n",
      [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>zz]],
    },
    {
      "N",
      [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>zz]],
    },
    { "*", [[*<Cmd>lua require('hlslens').start()<CR>]] },
    { "#", [[#<Cmd>lua require('hlslens').start()<CR>]] },
    { "g*", [[g*<Cmd>lua require('hlslens').start()<CR>]] },
    { "g#", [[g#<Cmd>lua require('hlslens').start()<CR>]] },
    { "<ESC>", [[<cmd>noh<CR><cmd>lua require('hlslens').stop()<CR>]] },
  },
  config = function()
    require("scrollbar.handlers.search").setup()
  end,
})

-- Auto matically setting tab width by projects
register("tpope/vim-sleuth")

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

-- Easy aligning text
register("junegunn/vim-easy-align", {
  cmd = "EasyAlign",
  keys = { { "<space>e", ":EasyAlign<CR>", mode = "x" } },
})

-- Show key stroke
register("folke/which-key.nvim", {
  event = "VeryLazy",
  config = function()
    local whichkey = require("which-key")
    whichkey.setup({
      plugins = {
        mark = false,
        register = false,
      },
      layout = {
        align = "center",
      },
      icons = { mappings = false },
    })
  end,
})

register("stevearc/quicker.nvim", {
  ft = "qf",
  config = function()
    require("quicker").setup()
    require("galaxyline").load_galaxyline()
  end,
})

-- Open buffer manager
register("j-morano/buffer_manager.nvim", {
  lazy = true,
  keys = {
    -- tools
    {
      "<leader>b",
      function()
        require("buffer_manager.ui").toggle_quick_menu()
      end,
      desc = "Toggle buffer manager",
    },
    {
      "<Tab>",
      function()
        require("cybu").autocmd()
        require("buffer_manager.ui").nav_next()
      end,
      desc = "Next buffer",
    },
    {
      "<S-Tab>",
      function()
        require("cybu").autocmd()
        require("buffer_manager.ui").nav_prev()
      end,
      desc = "Prev buffer",
    },
  },
  config = function()
    require("buffer_manager").setup({
      -- Defined in kanagawa.nvim
      highlight = "Normal:BufferManagerBorder",
    })
  end,
})

-- Cycle through buffers
register("ghillb/cybu.nvim", {
  lazy = true,
  config = function()
    require("cybu").setup({
      style = {
        border = "none",
        padding = 5,
        hide_buffer_id = true,
      },
      display_time = 1500,
      exclude = {
        "neo-tree",
        "qf",
        "neo-term",
      },
    })
  end,
})

register("dhruvasagar/vim-table-mode", {
  cmd = "TableModeToggle",
})
