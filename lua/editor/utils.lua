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

-- map create a new mapping
-- @param mode specify vim mode
-- @param lhs specify the new keymap
-- @param rhs specify the keymap or commands
-- @param opts setting options. Default: { noremap = true, silent = true, expr = false }
local function map(mode, lhs, rhs, opts)
  local options = {
    noremap = true,
    silent = true,
    expr = false,
  }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  if type(rhs) == "function" then
    options.callback = rhs
    rhs = ""
  end
  local stat, error = pcall(vim.keymap.set, mode, lhs, rhs, options)
  if not stat then
    vim.notify(error, vim.log.levels.ERROR, {
      title = "keymap",
    })
  end
end

-- nmap create a new mapping in normal mode
-- @param lhs specify the new keymap
-- @param rhs specify the keymap or commands
-- @param opts setting options. Default: { noremap = true, silent = true, eval = false }
local function nmap(lhs, rhs, opts)
  map("n", lhs, rhs, opts)
end

-- xmap create a new mapping in selection mode
-- @param lhs specify the new keymap
-- @param rhs specify the keymap or commands
-- @param opts setting options. Default: { noremap = true, silent = true, eval = false }
local function xmap(lhs, rhs, opts)
  map("x", lhs, rhs, opts)
end

-- fmap create a new mapping for lua function
local function fmap(mode, key, func)
  map(mode, key, "", { callback = func })
end

local function new_desc(d)
  return { desc = d }
end

M.map = map
M.nmap = nmap
M.xmap = xmap
M.fmap = fmap
M.new_desc = new_desc

-- Return true if the variable is presenting in the vim global variable
-- @param var: vim variable
M.vhas = function(var)
  return vim.fn.has(var) == 1
end

return M
