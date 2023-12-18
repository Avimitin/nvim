local M = {
  __cache = {},
}

-- function loaded will insert the given module name into cache when it is not exist.
---@param mod string A unique module name
---@return boolean true if the given mod is loaded, false if the given mod is not exist.
function M.loaded(mod)
  local key = string.format("%d%s", vim.api.nvim_get_current_buf(), mod)
  if M.__cache[key] then
    return true
  end

  M.__cache[key] = 1

  return false
end

setmetatable(M, {
  __call = function(self, a)
    return self.loaded(a)
  end,
  __index = function(self, a)
    return self.loaded(a)
  end,
})

return M
