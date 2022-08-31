local lsp_keymap = function(client, bufnr)
  local function bmap(mode, lhs, rhs)
    mode = mode or "n"
    local opts = {
      noremap = true,
      silent = true,
    }

    if type(rhs) == "function" then
      opts.callback = rhs
      rhs = ""
    end

    vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
  end

  local function bnmap(...)
    bmap("n", ...)
  end

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  bnmap("gd", "<Cmd>Lspsaga lsp_finder<CR>")
  bnmap("gp", "<Cmd>Lspsaga preview_definition<CR>")
  bnmap("gh", "<Cmd>Lspsaga hover_doc<CR>")
  bnmap("gs", "<cmd>Lspsaga signature_help<CR>")
  bnmap("go", "<cmd>Lspsaga show_line_diagnostics<CR>")
  bnmap("gj", "<cmd>Lspsaga diagnostic_jump_next<CR>")
  bnmap("gk", "<cmd>Lspsaga diagnostic_jump_prev<CR>")
  bnmap("gr", "<cmd>Lspsaga rename<CR>")
  bnmap("ga", "<cmd>Lspsaga code_action<CR>")

  -- most of the lsp server don't implement textDocument/Declaration, so gD is useless for now.
  bnmap("gD", function()
    vim.lsp.buf.declaration()
  end)
  bnmap("gm", function()
    vim.lsp.buf.implementation()
  end)
  bnmap("gt", function()
    vim.lsp.buf.type_definition()
  end)
  bnmap("gq", "<cmd>TroubleToggle<CR>")

  -- add rust specific keymappings
  if client.name == "rust_analyzer" then
    bnmap("<leader>rr", "<cmd>RustRunnables<CR>")
    bnmap("<leader>ra", "<cmd>RustHoverAction<CR>")
  end

  -- Set some keybinds conditional on server capabilities
  -- 0.8.0
  if vim.fn.has("nvim-0.8.0") == 1 then
    if client.server_capabilities.documentFormattingProvider then
      bnmap("gf", function()
        vim.lsp.buf.format({ async = true })
      end)
    elseif client.server_capabilities.documentRangeFormattingProvider then
      bmap("x", "gf", function()
        vim.lsp.buf.range_formatting()
      end)
    end

    -- 0.6.0 - 0.7.0
  else
    if client.resolved_capabilities.document_formatting then
      bnmap("gf", function()
        vim.lsp.buf.formatting()
      end)
    elseif client.resolved_capabilities.document_range_formatting then
      bmap("x", "gf", function()
        vim.lsp.buf.range_formatting()
      end)
    end
  end

  bnmap("gl", function()
    local current = vim.diagnostic.config().virtual_text
    if current ~= false then
      vim.g.diagnostic_virtual_text_config = current
      vim.diagnostic.config({ virtual_text = false })
    else
      vim.diagnostic.config({ virtual_text = vim.g.diagnostic_virtual_text_config })
    end
    require("lsp_lines").toggle()
  end)
end

return {
  lsp_keymap = lsp_keymap,
}
