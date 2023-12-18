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

-- UI Library
register("MunifTanjim/nui.nvim", {
  lazy = true,
})

register("nvim-neo-tree/neo-tree.nvim", {
  branch = "v3.x",
  init = function()
    vim.api.nvim_create_autocmd("VimEnter", {
      pattern = "*",
      callback = function()
        -- User might using stdin
        if vim.fn.argc() == 0 then
          return
        end
        local first_arg = vim.fn.argv(0)
        if not first_arg or #first_arg == 0 or (first_arg[1] == "-" or first_arg[1] == "+") then
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

            require("neo-tree.setup.netrw").hijack()
          end)
        )
      end,
    })
  end,
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
      filesystem = {
        hijack_netrw_behavior = "open_current",
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
        width = 28,
      },
      default_component_configs = {
        diagnostics = {
          symbols = {
            error = "",
            warn = "",
            hint = "",
            info = "",
          },
        },
        git_status = {
          symbols = {
            -- I don't need change type
            added = "",
            deleted = "",
            modified = "",
            renamed = "",
            -- Status type
            untracked = "",
            ignored = "",
            unstaged = "",
            staged = "",
            conflict = "",
          },
        },
      },
    })
  end,
  -- End of config
  keys = {
    {
      "<leader>t",
      "<CMD>Neotree action=focus toggle=true reveal=true position=left<CR>",
      desc = "Open file tree",
    },
    {
      "gO",
      "<CMD>Neotree action=focus toggle=true source=document_symbols position=float<CR>",
      desc = "View document symbols",
    },
  },
  cmd = {
    "Neotree",
  },
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

-- Quick select for text objects
register("sustech-data/wildfire.nvim", {
  keys = "<Enter>",
  config = function()
    require("wildfire").setup()
  end,
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
    {
      "J",
      require("tools.line_leap").leap_to_line,
      mode = { "n", "o" },
      desc = "Leap to line",
    },
    {
      "K",
      require("tools.line_leap").leap_to_line,
      mode = { "n", "o" },
      desc = "Leap to line",
    },
    {
      "gj",
      require("tools.line_leap").leap_to_line,
      mode = { "x" },
      desc = "Leap to line",
    },
    {
      "gk",
      require("tools.line_leap").leap_to_line,
      mode = { "x" },
      desc = "Leap to line",
    },
    { "s", "<Plug>(leap-forward-to)", mode = { "n", "x", "o" }, desc = "Leap forward to" },
    { "S", "<Plug>(leap-backward-to)", mode = { "n", "x", "o" }, desc = "Leap backward to" },
    { "x", "<Plug>(leap-forward-till)", mode = { "x", "o" }, desc = "Leap forward until" },
    { "X", "<Plug>(leap-backward-till)", mode = { "x", "o" }, desc = "Leap backward until" },
    { "gw", "<Plug>(leap-from-window)", mode = { "n", "x", "o" }, desc = "Leap from window" },
    { "gW", "<Plug>(leap-cross-window)", mode = { "n", "x", "o" }, desc = "Leap cross window" },
  },
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
    require("hlslens").setup()
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

-- Multiple Cursor
register("mg979/vim-visual-multi", {
  keys = {
    "<leader>mn",
    "<C-down>",
    "<C-up>",
    "<leader>mA",
    "<leader>mx",
    { mode = "x", "<leader>mx" },
    { mode = "x", "<leader>mA" },
    { mode = "x", "<leader>ma" },
    { mode = "x", "<leader>mf" },
    { mode = "x", "<leader>mc" },
  },
  init = function()
    -- then, update the visual multi key mappings
    vim.g.VM_maps = {
      ["Find Under"] = "<leader>mn",
      ["Find Subword Under"] = "<leader>mn",
      ["Select Cursor Down"] = "<C-down>",
      ["Select Cursor Up"] = "<C-up>",
      ["Undo"] = "<C-z>",
      ["Redo"] = "<C-r>",
      ["Select All"] = "<leader>mA",
      ["Start Regex Search"] = "<leader>mx",
      ["Visual Regex"] = "<leader>mx",
      ["Visual All"] = "<leader>mA",
      ["Visual Add"] = "<leader>ma",
      ["Visual Find"] = "<leader>mf",
      ["Visual Cursors"] = "<leader>mc",
    }
  end,
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
    whichkey.setup({})

    local ngrp = {
      mode = "n",
      ["g"] = { name = "+LSP" },
      ["<leader>g"] = { name = "+Git" },
      ["<leader>f"] = { name = "+Telescope" },
    }
    whichkey.register(ngrp)
  end,
})

-- Split and Join
register("Wansmer/treesj", {
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
})

local function gen_spider_keys()
  local keys = { "w", "e", "b" }
  local final = {}
  for _, k in ipairs(keys) do
    table.insert(final, {
      k,
      function()
        require("spider").motion(k)
      end,
      desc = "Spider motion " .. k,
    })
  end
  return final
end
register("chrisgrieser/nvim-spider", {
  keys = gen_spider_keys(),
  lazy = true,
})

register("nyngwang/NeoTerm.lua", {
  lazy = true,
  keys = {
    { "<C-`>", vim.cmd.NeoTermToggle },
    { "<C-`>", vim.cmd.NeoTermToggle, mode = "t" },
    { "<A-;>", vim.cmd.NeoTermEnterNormal, mode = "t" },
  },
  config = function()
    require("neo-term").setup({
      term_mode_hl = "Normal",
    })
  end,
})

register("willothy/flatten.nvim", {
  config = function()
    require("flatten").setup({
      callbacks = {
        should_block = function(argv)
          return vim.tbl_contains(argv, "lazygit") or vim.tbl_contains(argv, "-b")
        end,
        pre_open = function()
          vim.cmd.NeoTermToggle()
        end,
        post_open = function(bufnr, winnr, ft, is_blocking)
          if is_blocking then
            -- Hide the terminal while it's blocking
            vim.cmd.NeoTermToggle()
          else
            vim.api.nvim_set_current_win(winnr)
          end

          if ft == "gitcommit" or ft == "gitrebase" then
            vim.api.nvim_create_autocmd("BufWritePost", {
              buffer = bufnr,
              once = true,
              callback = vim.schedule_wrap(function()
                vim.api.nvim_buf_delete(bufnr, {})
              end),
            })
          end
        end,
        block_end = function()
          -- After blocking ends (for a git commit, etc), reopen the terminal
          vim.schedule(function()
            vim.cmd.NeoTermToggle()
          end)
        end,
      },
    })
  end,
  -- This plugin only read `vim.env.NVIM` on start up, there is no overhead.
  -- But lazy loading it will instead causing multiple issues, so just don't be lazy here.
  lazy = false,
  priority = 1001,
})
