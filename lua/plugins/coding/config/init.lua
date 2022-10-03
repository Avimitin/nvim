local config = {}

--
-- Config that should be set before plugin loaded
config.pre = function()
  -- close vim matchup's notice because we have lspsaga's winbar
  if require("editor.utils").vhas("nvim-0.8.0") then
    vim.g.matchup_matchparen_offscreen = {}
  end
end

--
-- SymbolsOutline configuration
--
config.symbols_outline_config = function()
  vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = true,
    position = "right",
    show_numbers = false,
    show_relative_numbers = false,
    show_symbol_details = true,
    keymaps = {
      close = "<Esc>",
      goto_location = "<Cr>",
      focus_location = "o",
      hover_symbol = "<C-space>",
      rename_symbol = "r",
      code_actions = "a",
    },
    lsp_blacklist = {},
    symbol_blacklist = {},
    symbols = {
      File = { icon = "", hl = "TSURI" },
      Module = { icon = "", hl = "TSNamespace" },
      Namespace = { icon = "", hl = "TSNamespace" },
      Package = { icon = "", hl = "TSNamespace" },
      Class = { icon = "ﴯ", hl = "TSType" },
      Method = { icon = "", hl = "TSMethod" },
      Property = { icon = "", hl = "TSMethod" },
      Field = { icon = "", hl = "TSField" },
      Constructor = { icon = "", hl = "TSConstructor" },
      Enum = { icon = "", hl = "TSType" },
      Interface = { icon = "", hl = "TSType" },
      Function = { icon = "", hl = "TSFunction" },
      Variable = { icon = "", hl = "TSConstant" },
      Constant = { icon = "", hl = "TSConstant" },
      String = { icon = "ﮜ", hl = "TSString" },
      Number = { icon = "", hl = "TSNumber" },
      Boolean = { icon = "ﮒ", hl = "TSBoolean" },
      Array = { icon = "", hl = "TSConstant" },
      Object = { icon = "⦿", hl = "TSType" },
      Key = { icon = "", hl = "TSType" },
      Null = { icon = "ﳠ", hl = "TSType" },
      EnumMember = { icon = "", hl = "TSField" },
      Struct = { icon = "ﴯ", hl = "TSType" },
      Event = { icon = "🗲", hl = "TSType" },
      Operator = { icon = "+", hl = "TSOperator" },
      TypeParameter = { icon = "𝙏", hl = "TSParameter" },
    },
  }
end

--
-- null-ls
--
config.null_ls_config = function()
  local attachment = require("plugins.coding.keymap")
  local null_ls = require("null-ls")

  local sources = {}
  local presented, null_ls_settings = pcall(require, "custom")
  if not presented then
    return
  end

  if not null_ls then
    return
  end

  if null_ls_settings.enable_stylua_fmt then
    table.insert(sources, null_ls.builtins.formatting.stylua)
  end

  if null_ls_settings.enable_eslint then
    table.insert(sources, null_ls.builtins.code_actions.eslint)
  end

  if null_ls_settings.enable_prettier then
    table.insert(sources, null_ls.builtins.formatting.prettier)
  end

  null_ls.setup({
    sources = sources,
    on_attach = attachment.lsp_keymap,
  })
end

--
-- treesitter
--
config.treesitter_config = function()
  local ft = require("plugins.coding.config").treesitter_ft

  -- treesitter don't know what is {type,java}scriptreact,
  -- let's filter it out
  local ensure_installed = {}

  local idx = 0

  -- a f*cking disgusting hack for the messy treesitter naming convention
  for _, val in ipairs(ft) do
    if val ~= "javascriptreact" and val ~= "typescriptreact" then
      ensure_installed[idx] = val
      idx = idx + 1
    end

    if val == "typescriptreact" then
      ensure_installed[idx] = "tsx"
      idx = idx + 1
    end
  end

  require("nvim-treesitter.configs").setup({
    -- packer compile is compiled without runtime context, so here we must give it
    -- the full path to the treesitter ft function for evaluating the filetype
    ensure_installed = ensure_installed,
    highlight = {
      enable = true,
    },
    indent = {
      enable = true,
    },
    matchup = {
      enable = true,
    },
    autotag = {
      enable = true,
    },
    textobjects = {
      select = {
        enable = true,

        -- Automatically jump forward to textobj, similar to targets.vim
        lookahead = true,

        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
          ["ab"] = "@block.outer",
          ["ib"] = "@block.inner",
          ["al"] = "@call.outer",
          ["il"] = "@call.inner",
          ["ap"] = "@parameter.outer",
          ["ip"] = "@parameter.inner",
          ["ao"] = "@condition.outer",
          ["io"] = "@condition.inner",
          ["as"] = "@statement.outer",
        },
      },
    },
  })

  vim.api.nvim_command("set foldmethod=expr")
  vim.api.nvim_command("set foldexpr=nvim_treesitter#foldexpr()")
end

local function treesitter_ft()
  local fts = {}

  for ft, _ in pairs(require("editor").config.treesitter_ft) do
    table.insert(fts, ft)
  end

  return fts
end

config.treesitter_ft = treesitter_ft()

--
-- lspconfig
--
config.lspconfig_config = function()
  require("plugins.coding.config.lspconfig")
end

-- Generate lspconfig load filetype from langs table
local function lspconfig_ft()
  -- Rust is configured by rust-tools.nvim plugin
  local filetype = {
    "rust",
  }

  for ft, _ in pairs(require("editor").config.lspconfig) do
    table.insert(filetype, ft)
  end

  return filetype
end

config.lspconfig_ft = lspconfig_ft()

--
-- lspsaga
--
config.lspsaga_config = function()
  local enable_winbar = require("editor.utils").vhas("nvim-0.8.0")
  local saga = require("lspsaga")
  local themes = require("lspsaga.lspkind")
  themes[12][2] = " "

  -- use custom config
  saga.init_lsp_saga({
    -- when cursor in saga window you config these to move
    move_in_saga = { prev = "k", next = "j" },
    diagnostic_header = { " ", " ", " ", " " },
    scroll_in_preview = {
      scroll_down = "<C-d>",
      scroll_up = "<C-u>",
    },
    code_action_icon = "ﯦ ",
    -- same as nvim-lightbulb but async
    code_action_lightbulb = {
      sign = false,
      virtual_text = true,
    },
    finder_icons = {
      def = "  ",
      ref = "  ",
      link = "  ",
    },
    finder_action_keys = {
      open = "<CR>",
      vsplit = "s",
      split = "i",
      tabe = "t",
      quit = "q",
      scroll_down = "<C-f>",
      scroll_up = "<C-b>", -- quit can be a table
    },
    -- show symbols in winbar must be neovim 0.8.0,
    -- close it until neovim 0.8.0 become stable
    symbol_in_winbar = {
      in_custom = false,
      enable = enable_winbar,
      separator = "  ",
      show_file = true,
      click_support = false,
    },
  })

  -- set lightbulb as comment like
  vim.api.nvim_set_hl(0, "LspSagaLightBulb", { fg = "#E8C266" })
end

--
-- rust-tools.nvim
--
config.rust_tools_config = function()
  require("plugins.coding.config.rust_tools")
end

--
-- crates.nvim
--
config.crates_nvim_config = function()
  require("crates").setup({
    popup = {
      autofocus = true,
      border = "single",
    },
  })
  require("packer").loader("nvim-cmp")
  require("cmp").setup.buffer({ sources = { { name = "crates" } } })
end

--
-- dap
--
config.dap_config = function()
  local dap = require("dap")

  dap.adapters.lldb = {
    type = "executable",
    command = "/usr/bin/lldb-vscode", -- adjust as needed
    name = "lldb",
  }

  dap.configurations.cpp = {
    {
      name = "Launch",
      type = "lldb",
      request = "launch",
      program = function()
        return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
      end,
      cwd = "${workspaceFolder}",
      stopOnEntry = false,
      args = {},
      runInTerminal = true,
    },
  }

  dap.configurations.c = dap.configurations.cpp
  dap.configurations.rust = dap.configurations.cpp

  require("dapui").setup()
end

return config
