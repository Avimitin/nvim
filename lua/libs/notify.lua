local M = {}

function M.info(msg)
  vim.notify(" => INFO: " .. msg, vim.log.levels.INFO)
end

function M.error(title, msg)
  vim.api.nvim_echo({ { " => ERROR: " .. title, "ErrorMsg" } }, true, {})
  vim.notify(msg, vim.log.levels.ERROR)
end

return M
