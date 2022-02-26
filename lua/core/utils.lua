local M = {}
local ntf_loaded, notify = pcall(require, "notify")
if ntf_loaded then
  vim.notify = notify
end

-- errorL notify a message in error level
-- @msg: logging message
-- @title: the logging title
M.errorL = function(msg, title)
  vim.notify(msg, vim.log.levels.ERROR, {
    title = title,
  })
end

-- infoL notify a message in info level
-- @msg: logging message
-- @title: the logging title
M.infoL = function(msg, title)
  vim.notify(msg, vim.log.levels.INFO, {
    title = title,
  })
end

-- this function serve the LSP
M.lsp_attach = function(client, bufnr)
  local function buf_set_keymap(...)
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
  buf_set_keymap("n", "gd", "<Cmd>Lspsaga preview_definition<CR>", opts)
  buf_set_keymap("n", "gh", "<Cmd>Lspsaga hover_doc<CR>", opts)
  buf_set_keymap(
    "n",
    "<C-u>",
    "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>",
    opts
  )
  buf_set_keymap(
    "n",
    "<C-d>",
    "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>",
    opts
  )
  buf_set_keymap("n", "gs", "<cmd>Lspsaga signature_help<CR>", opts)
  buf_set_keymap("n", "go", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
  buf_set_keymap("n", "gj", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
  buf_set_keymap("n", "gk", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>Lspsaga rename<CR>", opts)
  buf_set_keymap("n", "gx", "<cmd>Lspsaga code_action<CR>", opts)

  buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gm", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap("n", "gt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  buf_set_keymap("n", "gq", "<cmd>lua vim.diagnostic.set_loclist()<CR>", opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "gf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("x", "gf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end
end

M.fek = function(key, mode)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode)
end

return M
