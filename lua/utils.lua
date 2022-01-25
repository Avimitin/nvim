local M = {}

M.map = function(mode, lhs, rhs, opts)
  local options = {
    noremap = true,
    silent = true
  }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  local stat, error = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
  if not stat then
    vim.notify(error, vim.log.levels.ERROR, {
      title = 'keymap'
    })
  end
end

M.new_cmd = function(cmd, repl, force)
  local command
  if force then
    command = "command! " .. cmd .. " " .. repl
  else
    command = "command " .. cmd .. " " .. repl
  end
  local ok, err = pcall(vim.cmd, command)
  if not ok then
    vim.notify("setting cmd: " .. cmd .. " " .. err, vim.log.levels.ERROR,
               {
      title = 'command'
    })
  end
end

M.log_err = function(msg, title)
  vim.notify(msg, vim.log.levels.ERROR, {
    title = title
  })
end

M.log_info = function(msg, title)
  vim.notify(msg, vim.log.levels.INFO, {
    title = title
  })
end

M.lsp_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = {
    noremap = true,
    silent = true
  }
  buf_set_keymap('n', 'gd', '<Cmd>Lspsaga preview_definition<CR>', opts)
  buf_set_keymap('n', 'gh', '<Cmd>Lspsaga hover_doc<CR>', opts)
  buf_set_keymap("n", "<C-u>",
                 "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>",
                 opts)
  buf_set_keymap("n", "<C-d>",
                 "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>",
                 opts)
  buf_set_keymap('n', 'gs', '<cmd>Lspsaga signature_help<CR>', opts)
  buf_set_keymap('n', 'go', '<cmd>Lspsaga show_line_diagnostics<CR>', opts)
  buf_set_keymap('n', 'gj', '<cmd>Lspsaga diagnostic_jump_next<CR>', opts)
  buf_set_keymap('n', 'gk', '<cmd>Lspsaga diagnostic_jump_prev<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>Lspsaga rename<CR>', opts)
  buf_set_keymap('n', 'gx', '<cmd>Lspsaga code_action<CR>', opts)

  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gm', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'gt', '<cmd>lua vim.lsp.buf.type_definition()<CR>',
                 opts)
  buf_set_keymap('n', 'gq', '<cmd>lua vim.diagnostic.set_loclist()<CR>',
                 opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "gf", "<cmd>lua vim.lsp.buf.formatting()<CR>",
                   opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("x", "gf",
                   "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end
end

M.fek = function(key, mode)
  vim.fn.feedkeys(
    vim.api.nvim_replace_termcodes(key, true, true, true),
    mode
  )
end

return M
