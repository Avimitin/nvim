local utils = {}

utils.setup_keymaps = function(_, bufnr)
  local bnmap = function(mappings)
    require("libs.keymap").buf_map(bufnr, "n", mappings)
  end

  local telescope = function(action, args)
    return function()
      local theme = require("telescope.themes").get_dropdown(args or {})
      local builtins = require("telescope.builtin")
      builtins[action](theme)
    end
  end

  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

  bnmap({
    -- gf: Format code, define in conform module at lang/init.lua

    { "gsd", telescope("lsp_definitions"), desc = "[LSP] Search and goto `definitions`" },
    { "gsr", telescope("lsp_references"), desc = "[LSP] Search and goto `references`" },
    { "gsi", telescope("lsp_implementations"), desc = "[LSP] Search and goto `implementations`" },
    { "gst", telescope("lsp_type_definitions"), desc = "[LSP] Search and goto `type definition`" },

    { "ga", vim.lsp.buf.code_action, desc = "[LSP] Open code actions" },
    { "K", vim.lsp.buf.hover, desc = "[LSP] Open document" },
    { "gh", vim.lsp.buf.hover, desc = "[LSP] Open document" },
    { "R", vim.lsp.buf.rename, desc = "[LSP] Rename symbol" },
    { "gR", vim.lsp.buf.rename, desc = "[LSP] Rename symbol" },
    { "go", vim.diagnostic.open_float, desc = "[LSP] Open floating list" },
    { "gO", vim.diagnostic.setqflist, desc = "[LSP] Open quickfix list" },
    { "gr", vim.lsp.codelens.run, desc = "[LSP] Run codelens at current line" },

    -- gl: Open Symbols, define in neotree module at tools/init.lua

    { "[d", vim.diagnostic.goto_prev, desc = "[LSP] Jump to previous error" },
    { "]d", vim.diagnostic.goto_next, desc = "[LSP] Jump to next error" },
  })
end

utils.setup_icons = function()
  local icons = {
    "",
    "",
    "",
    "",
  }

  -- Setup diagnostic icons and signs
  vim.diagnostic.config({
    virtual_text = {
      prefix = function(diag)
        return icons[diag.severity]
      end,
      spacing = 4,
    },
    float = {
      prefix = function(diag)
        return icons[diag.severity] .. " "
      end,
      border = "rounded",
    },
    signs = false,
    underline = true,
    -- update diagnostic in insert mode will be annoying when the output is too verbose
    update_in_insert = false,
  })
end

utils.setup_inlay_hint = function(bufnr)
  -- Enable inlay hint by default when using neovim > v0.10.0
  if vim.version().minor >= 10 then
    -- Must be delayed after lsp attach: when setting up inlay_hint,
    -- neovim will check if the LSP server implemented inlay_hint protocol, which required a connection.
    vim.api.nvim_create_autocmd("LspAttach", {
      desc = "Enable inlay hint",
      callback = function()
        vim.lsp.inlay_hint.enable(true, { bnfnr = bufnr })
      end,
    })
  end
end

utils.setup_document_highlight_on_cursor = function(client, bufnr)
  if client.server_capabilities.documentHighlightProvider then
    local id = vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
    vim.api.nvim_clear_autocmds({ buffer = bufnr, group = "lsp_document_highlight" })
    vim.api.nvim_create_autocmd("CursorHold", {
      callback = vim.lsp.buf.document_highlight,
      buffer = bufnr,
      group = id,
      desc = "Document Highlight",
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      callback = vim.lsp.buf.clear_references,
      buffer = bufnr,
      group = id,
      desc = "Clear All the References",
    })
  end
end

utils.setup_codelens_refresher = function(client, bufnr)
  if client and client.server_capabilities and client.server_capabilities.codeLensProvider then
    vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
      buffer = bufnr,
      callback = function()
        vim.lsp.codelens.refresh()
      end,
    })
  end
end

-- LSP's on_attach interface accept two arguments client and bufnr. But we don't use client for now, so it is okay to pass nil here.
utils.setup_all = function(client, bufnr)
  utils.setup_icons()
  utils.setup_keymaps(client, bufnr)
  utils.setup_inlay_hint(bufnr)
  utils.setup_document_highlight_on_cursor(client, bufnr)
  utils.setup_codelens_refresher(client, bufnr)
end

return utils
