local lsp_keymap = function(client, bufnr)
  local function bmap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = {
    noremap = true,
    silent = true,
  }
  bmap("n", "gd", "<Cmd>Lspsaga lsp_finder<CR>", opts)
  bmap("n", "gp", "<Cmd>Lspsaga preview_definition<CR>", opts)
  bmap("n", "gh", "<Cmd>Lspsaga hover_doc<CR>", opts)
  bmap("n", "<C-u>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>", opts)
  bmap("n", "<C-d>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", opts)
  bmap("n", "gs", "<cmd>Lspsaga signature_help<CR>", opts)
  bmap("n", "go", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
  bmap("n", "gj", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
  bmap("n", "gk", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
  bmap("n", "gr", "<cmd>Lspsaga rename<CR>", opts)
  bmap("n", "ga", "<cmd>Lspsaga code_action<CR>", opts)

  -- most of the lsp server don't implement textDocument/Declaration, so gD is useless for now.
  bmap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  bmap("n", "gm", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  bmap("n", "gt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  bmap("n", "gq", "<cmd>lua vim.diagnostic.set_loclist()<CR>", opts)

  -- add rust specific keymappings
  if client.name == "rust_analyzer" then
    bmap("n", "<leader>rr", "<cmd>RustRunnables<CR>", opts)
    bmap("n", "<leader>ra", "<cmd>RustHoverAction<CR>", opts)
  end

  -- Set some keybinds conditional on server capabilities
  -- 0.8.0
  if vim.fn.has("nvim-0.8.0") then
    if client.server_capabilities.documentFormattingProvider then
      bmap("n", "gf", "<cmd>lua vim.lsp.buf.format({ async = true })<CR>", opts)
    elseif client.server_capabilities.documentRangeFormattingProvider then
      bmap("x", "gf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end

    -- 0.6.0 - 0.7.0
  else
    if client.resolved_capabilities.document_formatting then
      bmap("n", "gf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    elseif client.resolved_capabilities.document_range_formatting then
      bmap("x", "gf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end
  end
end

return {
  lsp_keymap = lsp_keymap
}
