local M = {}

M.map = function (mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
      options = vim.tbl_extend("force", options, opts)
    end
    local stat, error = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
    if not stat then
      vim.notify(error, vim.log.levels.ERROR, {title='keymap'})
    end
end

M.new_cmd = function (cmd, repl, force)
    local command
    if force then
      command = "command! "..cmd.." "..repl
    else
      command = "command "..cmd.." "..repl
    end
    local ok, err = pcall(vim.cmd, command)
    if not ok then
      vim.notify("setting cmd: "..cmd.." "..err, vim.log.levels.ERROR, {title='command'})
    end
end

M.log_err = function (msg, title)
  vim.notify(msg, vim.log.levels.ERROR, {title=title})
end

M.log_info = function (msg, title)
  vim.notify(msg, vim.log.levels.INFO, {title=title})
end

-- This is a hook, to setup for lazy loaded plugins
local function setup_plugins_after_loaded()
    -- Run rooter when it is the first time enter the neovim
    vim.cmd[[autocmd VimEnter * Rooter]]
    require("colors")
end

local function setup_plugins_before_loaded()
  if not vim.fn.has("nvim-0.6") then
    -- for filetype.nvim
    -- If using a Neovim version earlier than 0.6.0
    vim.g.did_load_filetypes = 1
  end
end

M.load_plugins = function()
  -- detecting plugin manager
  local no_packer = false
  local fn = vim.fn
  local install_path = fn.stdpath("data") ..
                           "/site/pack/packer/opt/packer.nvim"

  if fn.empty(fn.glob(install_path)) > 0 then
      M.log_info("Installing packer to " .. install_path)
      no_packer = fn.system({
          'git', 'clone', '--depth', '1',
          'https://github.com/wbthomason/packer.nvim', install_path
      })
  end

  local packer_call, error_msg = pcall(vim.cmd, [[packadd packer.nvim]])
  if not packer_call then
      M.log_err(error_msg, "load plugin")
      return
  end

  -- add a hook
  setup_plugins_before_loaded()

  -- Reading plugins configuration
  local ok, error = pcall(require, 'plug')
  if not ok then
    M.log_err("Load plugins: "..error, "load plugins")
  end

  vim.cmd([[autocmd BufWritePost plug.lua source <afile> | PackerCompile]])

  if no_packer then
    require('packer').sync()
    return
  end

  -- add a hook
  setup_plugins_after_loaded()
end

M.lsp_attach = function (client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    -- Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = {noremap = true, silent = true}
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', '<space>h', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>',
                   opts)
    buf_set_keymap('n', '<space>wa',
                   '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wr',
                   '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wl',
                   '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
                   opts)
    buf_set_keymap('n', '<space>D',
                   '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>',
                   opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<space>e',
                   '<cmd>lua vim.diagnostic.show_line_diagnostics()<CR>',
                   opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>',
                   opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>',
                   opts)
    buf_set_keymap('n', '<space>q',
                   '<cmd>lua vim.diagnostic.set_loclist()<CR>', opts)

    -- Set some keybinds conditional on server capabilities
    if client.resolved_capabilities.document_formatting then
        buf_set_keymap("n", "<space>m", "<cmd>lua vim.lsp.buf.formatting()<CR>",
                       opts)
    elseif client.resolved_capabilities.document_range_formatting then
        buf_set_keymap("n", "<space>m",
                       "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end
end

return M
