local register = require("pack").register

-- Deep dark purple colorscheme
register("rebelot/kanagawa.nvim", {
  cond = vim.cfg.ui.theme == "kanagawa",
  config = function()
    require("ui.kanagawa")
  end,
})

--- List of nerd-font icons
register("kyazdani42/nvim-web-devicons", {
  lazy = true,
})

-- Status line
register("glepnir/galaxyline.nvim", {
  event = "UIEnter",
  config = function()
    require("ui.statusline")
  end,
})

-- tab line
register("akinsho/nvim-bufferline.lua", {
  event = "BufRead",
  config = function()
    require("ui.bufferline")
  end,
})

-- Indent guide line
register("lukas-reineke/indent-blankline.nvim", {
  event = "BufRead",
  config = function()
    require("ui.indent")
  end,
})

-- Notification UI
register("rcarriga/nvim-notify", {
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
})

-- Scrollbar UI
register("petertriho/nvim-scrollbar", {
  lazy = true,
  event = "BufReadPost",
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
        "cmp_menu",
        "cmp_docs",
      },
      handlers = {
        cursor = false,
      },
    })
  end,
})

-- Spinner for LSP server setup progress
register("j-hui/fidget.nvim", {
  event = "User LspProgressUpdate",
  config = function()
    require("fidget").setup({
      text = {
        spinner = "dots",
      },
      source = {
        ["null-ls"] = {
          ignore = true,
        },
      },
    })
  end,
})

-- Dim the inactive variable/function
register("zbirenbaum/neodim", {
  event = "LspAttach",
  config = function()
    require("neodim").setup({
      alpha = 0.7,
      blend_color = "#000000",
      update_in_insert = {
        enable = false,
        delay = 100,
      },
      hide = {
        virtual_text = true,
        signs = true,
        underline = true,
      },
    })
  end,
})

-- Display diagnostic inline
register("https://git.sr.ht/~whynothugo/lsp_lines.nvim", {
  lazy = true,
  config = function()
    require("lsp_lines").setup()
    require("lsp_lines").toggle()
  end,
})

register("folke/todo-comments.nvim", {
  event = "LspAttach",
  config = function()
    require("todo-comments").setup({
      signs = false,
    })
  end,
})

-- prettify the input and select ui
register("stevearc/dressing.nvim", {
  lazy = true,
  init = function()
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.ui.select = function(...)
      require("lazy").load({ plugins = { "dressing.nvim" } })
      return vim.ui.select(...)
    end
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.ui.input = function(...)
      require("lazy").load({ plugins = { "dressing.nvim" } })
      return vim.ui.input(...)
    end
  end,
})
