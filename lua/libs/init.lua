local M = {}

local pub = function(module)
  M = vim.tbl_deep_extend("force", M, require(module))
end

pub("libs.trivial")

return M
