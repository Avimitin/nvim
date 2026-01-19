local register = require("pack").register

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {
  rev = "b9fd5226c2f76c951fc8ed5923d85e4de065e509",
  sha256 = "sha256-9Un7ekhBxcnmFE1xjCCFTZ7eqIbmXvQexpnhduAg4M0=",
})

-- Use oil for main file management
register("stevearc/oil.nvim", {
  rev = "f55b25e493a7df76371cfadd0ded5004cb9cd48a",
  sha256 = "sha256-eMruZSMi72A74LqtSsyJSI3WY595aKO9n2XxJ3Du/9Y=",
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
  rev = "1098d7b3c34adcfa7feb3289ee434529abd4afd1",
  sha256 = "sha256-IzJ9PWUeEw52Gs5nCH3zAUBbar3Wq6PCN+Rt6HKC1pc=",
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
register("https://codeberg.org/andyg/leap.nvim.git", {
  rev = "819388c5546548016b0dab9a72a42aa5a40bf35d",
  sha256 = "sha256-XPlcmTzWZA70yzXAPT9wnjpDoHLfYK2u4YXfn3C2tcY=",
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

    vim.keymap.set({ "n", "x", "o" }, "s", "<Plug>(leap)")
    vim.keymap.set("n", "S", "<Plug>(leap-from-window)")

    vim.keymap.set({ "x", "o" }, "R", function()
      require("leap.treesitter").select({
        opts = require("leap.user").with_traversal_keys("R", "r"),
      })
    end)

    -- Return an argument table for `leap()`, tailored for f/t-motions.
    local function as_ft(key_specific_args)
      local common_args = {
        inputlen = 1,
        inclusive = true,
        -- To limit search scope to the current line:
        -- pattern = function (pat) return '\\%.l'..pat end,
        opts = {
          labels = "", -- force autojump
          safe_labels = vim.fn.mode(1):match("[no]") and "" or nil, -- [1]
        },
      }
      return vim.tbl_deep_extend("keep", common_args, key_specific_args)
    end

    local clever = require("leap.user").with_traversal_keys -- [2]
    local clever_f = clever("f", "F")
    local clever_t = clever("t", "T")

    for key, key_specific_args in pairs({
      f = { opts = clever_f },
      F = { backward = true, opts = clever_f },
      t = { offset = -1, opts = clever_t },
      T = { backward = true, offset = 1, opts = clever_t },
    }) do
      vim.keymap.set({ "n", "x", "o" }, key, function()
        require("leap").leap(as_ft(key_specific_args))
      end)
    end
  end,
})

-- Auto matically setting tab width by projects
register("tpope/vim-sleuth", {
  rev = "be69bff86754b1aa5adcbb527d7fcd1635a84080",
  sha256 = "sha256-uHVHAp3tJDcpnV8Un75I03lpFS1r1mbwWSkjuhLwhkg=",
})

--- Color utils
register("uga-rosa/ccc.nvim", {
  rev = "9d1a256e006decc574789dfc7d628ca11644d4c2",
  sha256 = "sha256-3TZ8VmvdgQ9n63m78C3r4OIUkVQHTHBvC24ixBdhTig=",
  config = function()
    require("ccc").setup()
  end,
})

-- Easy aligning text
register("junegunn/vim-easy-align", {
  rev = "9815a55dbcd817784458df7a18acacc6f82b1241",
  sha256 = "sha256-EARuJTE5VQVF9KbX6TOtxaoHiem49R468h9IfUj32Kg=",
  config = function()
    require("keys").map("x", { "<space>e", ":EasyAlign<CR>" })
  end,
})

register("stevearc/quicker.nvim", {
  rev = "fc041830fa7cf093786b0d5990d99cf3c7b0c129",
  sha256 = "sha256-cL/BxgmqWE28UVRwoGXz0pSlJREjrQw5/st15SITYeA=",
  config = function()
    require("quicker").setup()
    require("galaxyline").load_galaxyline()
  end,
})

-- Open buffer manager
register("j-morano/buffer_manager.nvim", {
  rev = "d62dc479603459cf6664abd468761ae78316e803",
  sha256 = "sha256-eOWgLq1eTHutV3jZzVRPtgwVIm25dcZ31ayXLbfPH4M=",
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
  rev = "bf5d5eaf23bbfcca9f98a24ed29bd084abf89bf2",
  sha256 = "sha256-2IGGLRLVf1Or6nu3y07HQZ2fvcOzWOUNZ4B6eMUeugc=",
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
  rev = "c2a0dd0d931d0fb07665e1fedb1ea688da3b80b4",
  sha256 = "sha256-8k/vGYktoFBX3kjOyn+VR49xmhFhNZbkaI8UmbPSYF4=",
  config = function()
    require("nvim-autopairs").setup({})
  end,
})

register("echasnovski/mini.pick", {
  rev = "66148e0e4e2d087c86326c6b58d663da4dde8fc5",
  sha256 = "sha256-t7+goJcRfwIO+MyZN7TvpzA4V61Fce5pyiMVRmi8Vvg=",
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
