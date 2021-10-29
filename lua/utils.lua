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

  -- Reading plugins configuration
  local ok, error = pcall(require, 'plug')
  if not ok then
    M.log_err("Load plugins: "..error, "load plugins")
  end

  vim.cmd([[autocmd BufWritePost plug.lua source <afile> | PackerCompile]])

  if no_packer then require('packer').sync() end
end

return M
