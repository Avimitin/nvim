local utils = {}

utils.setup_keymaps = function(_, bufnr)
  local bnmap = function(mappings)
    require("libs.keymap").buf_map(bufnr, "n", mappings)
  end

  local lspsaga = function(action)
    return require("libs.keymap").wrap_cmd("Lspsaga " .. action)
  end

  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

  bnmap({
    -- gf: Format code, define in conform module at lang/init.lua
    { "gd", lspsaga("finder"), desc = "Find symbol" },
    { "gp", lspsaga("peek_definition"), desc = "Peek definition" },
    { "gh", lspsaga("hover_doc"), desc = "Open document" },
    { "gr", lspsaga("rename"), desc = "Rename symbol" },
    { "ga", lspsaga("code_action"), desc = "Open code action" },
    -- gO: Open Symbols, define in neotree module at tools/init.lua
    { "gt", lspsaga("peek_type_definition"), desc = "Peek type definition" },
    { "[d", lspsaga("diagnostic_jump_prev"), desc = "Jump to previous error" },
    { "]d", lspsaga("diagnostic_jump_next"), desc = "Jump to next error" },
  })
end

utils.setup_icons = function()
  local icons = {
    -- lsp diagnostic (Notes: case sensitive)
    ERROR = "",
    WARN = "",
    HINT = "",
    INFO = "",
  }
  -- Setup diagnostic icons and signs
  vim.diagnostic.config({
    virtual_text = {
      prefix = "",
      spacing = 6,
      format = function(diagnostic)
        return string.format(
          "%s  %s",
          icons[vim.diagnostic.severity[diagnostic.severity]],
          diagnostic.message
        )
      end,
    },
    signs = true,
    underline = true,
    -- update diagnostic in insert mode will be annoying when the output is too verbose
    update_in_insert = false,
  })

  local types = {
    "Error",
    "Warn",
    "Hint",
    "Info",
  }

  for _, diag_type in ipairs(types) do
    local hl = "DiagnosticSign" .. diag_type
    vim.fn.sign_define(hl, {
      text = "",
      linehl = hl,
    })
  end
end

utils.setup_inlay_hint = function(bufnr)
  -- Enable inlay hint by default when using neovim > v0.10.0
  if vim.version().minor >= 10 then
    -- Must be delayed after lsp attach: when setting up inlay_hint,
    -- neovim will check if the LSP server implemented inlay_hint protocol, which required a connection.
    vim.api.nvim_create_autocmd("LspAttach", {
      desc = "Enable inlay hint",
      callback = function()
        vim.lsp.inlay_hint.enable(bufnr, true)
      end,
    })
  end
end

-- LSP's on_attach interface accept two arguments client and bufnr. But we don't use client for now, so it is okay to pass nil here.
utils.setup_all = function(client, bufnr)
  utils.setup_icons()
  utils.setup_keymaps(client, bufnr)
  utils.setup_inlay_hint(bufnr)
end

return utils
