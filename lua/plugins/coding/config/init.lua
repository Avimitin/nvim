local present, custom = pcall(require, "custom")

local config = {}

--
-- Config that should be set before plugin loaded
config.pre = function()
  config.vim_go_config()
end

--
-- vim-go configuration (Should be set before plugin load)
--
config.vim_go_config = function()
  vim.g.go_echo_go_info = 0
  vim.g.go_doc_popup_window = 1
  vim.g.go_def_mapping_enabled = 0
  vim.g.go_template_autocreate = 0
  vim.g.go_textobj_enabled = 0
  vim.g.go_auto_type_info = 1
  vim.g.go_def_mapping_enabled = 0
  vim.g.go_highlight_array_whitespace_error = 1
  vim.g.go_highlight_build_constraints = 1
  vim.g.go_highlight_chan_whitespace_error = 1
  vim.g.go_highlight_extra_types = 1
  vim.g.go_highlight_fields = 1
  vim.g.go_highlight_format_strings = 1
  vim.g.go_highlight_function_calls = 1
  vim.g.go_highlight_function_parameters = 1
  vim.g.go_highlight_functions = 1
  vim.g.go_highlight_generate_tags = 1
  vim.g.go_highlight_methods = 1
  vim.g.go_highlight_operators = 1
  vim.g.go_highlight_space_tab_error = 1
  vim.g.go_highlight_string_spellcheck = 1
  vim.g.go_highlight_structs = 1
  vim.g.go_highlight_trailing_whitespace_error = 1
  vim.g.go_highlight_types = 1
  vim.g.go_highlight_variable_assignments = 0
  vim.g.go_highlight_variable_declarations = 0
  vim.g.go_doc_keywordprg_enabled = 0
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
  local null_ls_settings = require("custom").null_ls

  if not null_ls then
    return
  end

  if null_ls_settings.enable_stylua_fmt then
    table.insert(sources, null_ls.builtins.formatting.stylua)
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
  require("nvim-treesitter.configs").setup({
    -- packer compile is compiled without runtime context, so here we must give it
    -- the full path to the treesitter ft function for evaluating the filetype
    ensure_installed = require("plugins.coding.config").treesitter_ft(),
    highlight = {
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
end

config.treesitter_ft = function()
  -- enable treesitter for what filetype?
  local ft = {
    "json",
    "lua",
    "vim",
  }

  local alias = {
    ["typescriptreact"] = "typescript",
    ["javascriptreact"] = "javascript",
  }

  if not present or not custom.langs then
    return ft
  end

  -- append some customize treesitter filetype
  for _, v in ipairs(custom.langs) do
    local la = nil
    if type(v) == "string" then
      la = v
    elseif type(v) == "table" and #v > 0 then
      la = v[1]
    end

    if la ~= nil then
      if alias[la] then
        la = alias[la]
      end

      table.insert(ft, la)
    end
  end

  return ft
end

--
-- lspconfig
--
config.lspconfig_config = function()
  require("plugins.coding.config.lspconfig")
end

config.lspconfig_ft = {
  "bash",
  "c",
  "cpp",
  "go",
  "html",
  "javascript",
  "json",
  "lua",
  "python",
  "rust",
  "sh",
  "toml",
}

-- do a unique insertion into lspconfig
function update_lspconfig_ft()
  local set = {}
  for _, v in ipairs(config.lspconfig_ft) do
    set[v] = 0
  end

  if present and custom.langs then
    for _, lang in ipairs(custom.langs) do
      if type(lang) == "table" and #lang > 1 and set[lang[1]] == nil then
        table.insert(config.lspconfig_ft, lang[1])
      end
    end
  end
end

-- do once, to reduce complexity
update_lspconfig_ft()

--
-- lspsaga
--
config.lspsaga_config = function()
  local saga = require("lspsaga")

  -- use custom config
  saga.init_lsp_saga({
    -- "single" | "double" | "rounded" | "bold" | "plus"
    border_style = "single",
    -- when cursor in saga window you config these to move
    move_in_saga = { prev = "k", next = "j" },
    diagnostic_header = { " ", " ", " ", " " },
    -- show diagnostic source
    show_diagnostic_source = true,
    -- add bracket or something with diagnostic source, just have 2 elements
    diagnostic_source_bracket = { "戀", ":" },
    -- use emoji lightbulb in default
    code_action_icon = " ",
    -- if true can press number to execute the codeaction in codeaction window
    code_action_num_shortcut = true,
    -- same as nvim-lightbulb but async
    code_action_lightbulb = {
      enable = true,
      sign = true,
      sign_priority = 40,
      virtual_text = false,
    },
    -- preview lines of lsp_finder and definition preview
    max_preview_lines = 10,
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
    code_action_keys = {
      quit = "q",
      exec = "<CR>",
    },
    rename_action_quit = "<C-c>",
    definition_preview_icon = "  ",
    -- show symbols in winbar must be neovim 0.8.0,
    -- close it until neovim 0.8.0 become stable
    symbol_in_winbar = {
      in_custom = false,
      enable = false,
      separator = " ",
      show_file = true,
      click_support = false,
    },
  })
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
