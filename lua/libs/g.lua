--
-- Maintain a self global module load cache to avoid conflict with vim.g.*
-- User can use the below mechanism to get cache status
--
--   * require("libs.g")["index"]
--   * require("libs.g").index
--   * require("libs.g")("index")
--   * require("libs.g").loaded("index")
--

local M = {
  __cache = {},
}

-- function loaded will insert the given module name into cache when it is not exist.
---@param mod string A unique module name
---@return boolean true if the given mod is loaded, false if the given mod is not exist.
function M.loaded(mod)
  if vim.g.packer_bootstraping and vim.g.packer_bootstraping == 1 then
    return true
  end

  if M.__cache[mod] then
    return true
  end

  M.__cache[mod] = 1
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
