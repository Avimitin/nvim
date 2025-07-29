local register = require("pack").register

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {
  lazy = true,
})

-- Use oil for main file management
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
        require("buffer_manager.ui").nav_next()
      end,
      desc = "Next buffer",
    },
    {
      "<S-Tab>",
      function()
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

register("chomosuke/typst-preview.nvim", {
  ft = "typst",
  version = "1.*",
  config = function()
    require("typst-preview").setup({
      open_cmd = "/usr/bin/env firefox --new-window %s",
      dependencies_bin = {
        ["tinymist"] = "tinymist",
        ["websocat"] = "websocat",
      },
    })
  end,
})

register("windwp/nvim-autopairs", {
  event = "InsertEnter",
  config = function()
    require("nvim-autopairs").setup({})
  end,
})

register("echasnovski/mini.pick", {
  version = "*",
  config = function()
    require("mini.pick").setup()
  end,
  keys = {
    {
      "<leader>fr",
      function()
        require("mini.pick").builtin.resume()
      end,
    },
    {
      "<leader>ff",
      function()
        require("mini.pick").builtin.files({ tool = "rg" })
      end,
    },
    {
      "<leader>fg",
      function()
        require("mini.pick").builtin.grep_live({ tool = "rg" })
      end,
    },
    {
      "<leader>fb",
      function()
        require("mini.pick").builtin.buffers()
      end,
    },
  },
})
