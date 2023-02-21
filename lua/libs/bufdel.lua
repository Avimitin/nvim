-- nvim-bufdel
-- By Olivier Roques
-- github.com/ojroques

local M = {}

-- Switch to buffer 'buf' on each window from list 'windows'
local function switch_buffer(windows, buf)
  local cur_win = vim.fn.winnr()
  for _, winid in ipairs(windows) do
    vim.cmd(string.format("%d wincmd w", vim.fn.win_id2win(winid)))
    vim.cmd(string.format("buffer %d", buf))
  end
  vim.cmd(string.format("%d wincmd w", cur_win))
end

-- Select the next buffer to display
local function get_next_buf(buf)
  -- build table mapping buffers to their actual position
  local buffers, buf_index = {}, 1
  for i, bufinfo in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    if buf == bufinfo.bufnr then
      buf_index = i
    end
    table.insert(buffers, bufinfo.bufnr)
  end
  -- select next buffer according to user choice
  if buf_index == #buffers and #buffers > 1 then
    return buffers[#buffers - 1]
  end
  return buffers[buf_index % #buffers + 1]
end

-- Delete a buffer, ignoring changes if 'force' is set
local function delete_buffer(buf, force)
  if vim.fn.buflisted(buf) == 0 then
    return
  end
  -- retrieve buffer and delete it while preserving window layout
  local next_buf = get_next_buf(buf)
  local windows = vim.fn.getbufinfo(buf)[1].windows
  switch_buffer(windows, next_buf)
  -- force deletion of terminal buffers
  if force or vim.fn.getbufvar(buf, "&buftype") == "terminal" then
    vim.cmd(string.format("bd! %d", buf))
  else
    vim.cmd(string.format("silent! confirm bd %d", buf))
  end
  -- revert buffer switches if deletion was cancelled
  if vim.fn.buflisted(buf) == 1 then
    switch_buffer(windows, buf)
  end
end

-- Delete a given buffer, ignoring changes if 'force' is set
function M.delete_buffer_expr(bufexpr, force)
  if #vim.fn.getbufinfo({ buflisted = 1 }) < 2 then
    -- exit when there is only one buffer left
    if force then
      vim.cmd("qall!")
    else
      vim.cmd("confirm qall")
    end
    return
  end
  -- retrieve buffer number from buffer expression
  if not bufexpr then
    delete_buffer(vim.fn.bufnr(), force)
  end
  if tonumber(bufexpr) then
    delete_buffer(tonumber(bufexpr), force)
  end
  bufexpr = string.gsub(bufexpr, [[^['"]+]], "") -- escape any start quote
  bufexpr = string.gsub(bufexpr, [[['"]+$]], "") -- escape any end quote
  delete_buffer(vim.fn.bufnr(bufexpr), force)
end

-- Delete all listed buffers except current, ignoring changes if 'force' is set
function M.delete_buffer_others(force)
  for _, bufinfo in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    if bufinfo.bufnr ~= vim.fn.bufnr() then
      delete_buffer(bufinfo.bufnr, force)
    end
  end
end

return M
