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
  lazy = true,
  event = "VeryLazy",
  branch = "v3.x",
  config = function()
    require("neo-tree").setup({
      close_if_last_window = true,
      -- improve speed in LLVM, Linux kernel
      enable_git_status = false,
      -- No need for this, use quickfix list (w/ key gO)
      enable_diagnostics = false,
      sources = { "filesystem", "document_symbols" },
    })

    -- statuscol.nvim is also started with "VeryLazy" event. And it will mess up the neo-tree interface
    -- So here I add a 50ms delay to ensure neo-tree buffer is opened after statuscol set up.
    vim.defer_fn(function()
      require("neo-tree.command").execute({
        source = "filesystem",
        reveal = true,
        action = "show",
      })
    end, 50)
  end,
  keys = {
    { "<leader>tt", "<CMD>Neotree reveal=true toggle=true<CR>", desc = "Toggle Tree file manager" },
    {
      "<leader>td",
      "<CMD>Neotree source=document_symbols toggle=true<CR>",
      desc = "Toggle document symbols",
    },
  },
})

-- Use oil for main file management, use neo-tree for treestyle display only.
register("stevearc/oil.nvim", {
  config = function()
    require("oil").setup()
    vim.keymap.set("n", "-", "<CMD>Oil --float<CR>", { desc = "Open parent directory" })
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
      window = {
        winblend = 10,
      },
      layout = {
        align = "center",
      },
    })

    local ngrp = {
      mode = "n",
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
      "J",
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

register("kevinhwang91/nvim-bqf", {
  ft = "qf",
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
    require("buffer_manager").setup({})
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
      display_time = 1000,
      exclude = {
        "neo-tree",
        "qf",
        "neo-term",
      },
    })
  end,
})
