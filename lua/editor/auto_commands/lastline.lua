--
--
-- Setup lastline auto commands. This auto command can help us quickly resume
-- the cursor to the last edit position.
--
-- Git commit/rebase, svn, Dashboard are ignored by default. Append the filetype
-- that you want to ignore into the ignore_filetype array.
--
--
--      This is a lightweight Lua rewrite for the vim last line plugin
--         * Credit: https://github.com/farmergreg/vim-lastplace
--
--

local ignore_buftype = { "quickfix", "nofile", "help" }
local ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit", "Dashboard" }

local function can_jump()
  local contains = vim.tbl_contains
  local current_buftype = vim.api.nvim_buf_get_option(0, "buftype")
  -- return when current buftype is ignored
  if contains(ignore_buftype, current_buftype) then
    return false
  end

  -- jump to the beginning of the file and return when current filetype is ignored
  local current_filetype = vim.api.nvim_buf_get_option(0, "filetype")
  if contains(ignore_filetype, current_filetype) then
    vim.cmd("normal! gg")
    return false
  end

  -- return if the line is already specified by other stuff or command line argument
  if vim.fn.line(".") > 1 then
    return false
  end

  return true
end

local function set_cursor()
  local last_focus_line = vim.fn.line([['"]])
  -- total line in current file buffer
  local total_line_in_buf = vim.fn.line("$")
  -- first line visible in current window
  local first_line_visible = vim.fn.line("w0")
  -- last line visible in current window
  local last_line_visible = vim.fn.line("w$")
  local cmd = vim.cmd

  -- if last focus line is the first line
  if last_focus_line == 0 then
    return
  end

  -- if the last focus line is not in current buffer
  -- or was exist but isn't exist for now
  if last_focus_line > total_line_in_buf then
    return
  end

  if last_line_visible == total_line_in_buf then
    cmd([[normal! g`"]])
    -- if the last focus line is in the upper part of the window
    -- focus it to the center.
  elseif
    total_line_in_buf - last_focus_line > ((last_line_visible - first_line_visible) / 2) - 1
  then
    cmd([[normal! g`"zz]])
  else
    cmd([[normal! G'"<C-e>]])
  end
end

local group_id = vim.api.nvim_create_augroup("AutoJumpLastPlace", { clear = true })
vim.api.nvim_create_autocmd({ "BufWinEnter", "FileType" }, {
  callback = function()
    if not can_jump() then
      return
    end
    set_cursor()
  end,
  group = group_id,
})
