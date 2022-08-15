local M = {}

local function setup_auto_diff()
  local au = vim.api.nvim_create_autocmd
  au("FileType", {
    pattern = "gitcommit",
    callback = function(opts)
      if opts == nil then
        return
      end

      local win_width = vim.fn.winwidth("%")
      if win_width < 160 then
        return
      end

      vim.cmd("vertical Git diff --staged")
      vim.cmd("setlocal filetype=git")
      vim.cmd("wincmd h")
    end,
  })
end

M.setup = function()
  if require("editor").config.autocmd_enable.diff_on_commit then
    setup_auto_diff()
  end
end

return M
