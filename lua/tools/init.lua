local register = require("pack").register

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {})

-- Use oil for main file management
register("stevearc/oil.nvim", {
  config = function()
    require("oil").setup({
      float = {
        border = "solid",
        max_width = 200,
        max_height = 50,
      },
      keymaps = {
        ["q"] = "actions.close",
        ["g!"] = { "actions.open_cmdline" },
        ["gy"] = "actions.copy_to_system_clipboard",
      },
      view_options = {
        show_hidden = true,
      },
    })
    require("keys").map("n", {
      {
        "_",
        function()
          require("oil").open(vim.fn.getcwd())
        end,
        desc = "Open cwd",
      },
      { "-", "<CMD>Oil<CR>", desc = "Open parent directory" },
    })
  end,
})

-- Surround operation
register("kylechui/nvim-surround", {
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
  config = function()
    require("leap").opts.preview = function(ch0, ch1, ch2)
      return not (ch1:match("%s") or (ch0:match("%a") and ch1:match("%a") and ch2:match("%a")))
    end

    -- Define equivalence classes for brackets and quotes, in addition to
    -- the default whitespace group:
    require("leap").opts.equivalence_classes = {
      " \t\r\n",
      "([{",
      ")]}",
      "'\"`",
    }

    -- Use the traversal keys to repeat the previous motion without
    -- explicitly invoking Leap:
    require("leap.user").set_repeat_keys("<enter>", "<backspace>")

    require("keys").map({ "n", "x", "o" }, {
      { "s", "<Plug>(leap)", desc = "Leap" },
      { "S", "<Plug>(leap-from-window)", desc = "Leap backward to" },
    })
  end,
})

-- Auto matically setting tab width by projects
register("tpope/vim-sleuth")

--- Color utils
register("uga-rosa/ccc.nvim", {
  config = function()
    require("ccc").setup()
  end,
})

-- Easy aligning text
register("junegunn/vim-easy-align", {
  config = function()
    require("keys").map("x", { "<space>e", ":EasyAlign<CR>" })
  end,
})

register("stevearc/quicker.nvim", {
  config = function()
    require("quicker").setup()
    require("galaxyline").load_galaxyline()
  end,
})

-- Open buffer manager
register("j-morano/buffer_manager.nvim", {
  config = function()
    require("buffer_manager").setup({
      -- Defined in kanagawa.nvim
      highlight = "Normal:BufferManagerBorder",
    })

    require("keys").map("n", {
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
    })
  end,
})

register("chomosuke/typst-preview.nvim", {
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
  config = function()
    require("nvim-autopairs").setup({})
  end,
})

register("echasnovski/mini.pick", {
  version = "*",
  config = function()
    require("mini.pick").setup()
    require("keys").map("n", {
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
    })
  end,
})
