-- map create a new mapping
---@param mode string|table specify vim mode
---@param lhs string specify the new keymap
---@param rhs string|function specify the keymap or commands
---@param opts table|nil setting options. Default: { noremap = true, silent = true, expr = false }
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

local function nmap(...)
  map("n", ...)
end

local function xmap(...)
  map("x", ...)
end

local function imap(...)
  map("i", ...)
end

local d = function(s)
  return { desc = s }
end

return {
  map = map,
  nmap = nmap,
  xmap = xmap,
  imap = imap,
  d = d,
}
