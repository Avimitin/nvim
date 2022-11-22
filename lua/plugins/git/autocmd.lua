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
      local win_id = vim.api.nvim_get_current_win()
      local buf_id = vim.api.nvim_win_get_buf(win_id)
      vim.api.nvim_create_autocmd("BufWinLeave", {
        buffer = opts.buf,
        callback = function()
          local id = win_id
          vim.api.nvim_buf_delete(buf_id, { force = true })
          vim.api.nvim_win_close(id, true)
        end,
      })
      vim.api.nvim_set_current_win(win_id)
    end,
  })
end

M.setup = function()
  if require("editor").config.autocmd_enable.diff_on_commit then
    setup_auto_diff()
  end
end

return M
