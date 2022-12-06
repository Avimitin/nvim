local M = {}

-- Create a new user command that can omit option
-- @params cmd: user command
-- @params exec: the final invoked command or lua function
-- @params opt: can be nil, or a table with commands defined options
M.alias = function(cmd, exec, opt)
  local o = opt or {}
  vim.api.nvim_create_user_command(cmd, exec, o)
end

-- Return true if the variable is presenting in the vim global variable
-- @param var: vim variable
M.vhas = function(var)
  return vim.fn.has(var) == 1
end

return M
