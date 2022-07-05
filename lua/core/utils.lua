local M = {}

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

M.fek = function(key, mode)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode)
end

-- Create a new user command that can omit option
-- @params cmd: user command
-- @params exec: the final invoked command or lua function
-- @params opt: can be nil, or a table with commands defined options
M.alias = function(cmd, exec, opt)
  local o = opt or {}
  vim.api.nvim_create_user_command(cmd, exec, o)
end

return M
