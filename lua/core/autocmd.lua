local au = vim.api.nvim_create_autocmd

-- Copy data to system clipboard only when we are pressing 'y'. 'd', 'x' will be filtered out.
--
-- Credit: https://github.com/ibhagwan/smartyank.nvim
local smart_yank_gid = vim.api.nvim_create_augroup("SmartYank", { clear = true })
au("TextYankPost", {
  group = smart_yank_gid,
  desc = "Copy and highlight yanked text to system clipboard",
  callback = function()
    vim.highlight.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })

    if not vim.fn.has("clipboard") == 1 then
      return
    end

    local copy_key_is_y = vim.v.operator == "y"
    if not copy_key_is_y then
      return
    end

    local copy = function(str)
      local ok, error = pcall(vim.fn.setreg, "+", str)
      if not ok then
        vim.notify("fail to copy to clipboard: " .. error, vim.log.levels.ERROR)
        return
      end
    end

    local present, yank_data = pcall(vim.fn.getreg, "0")
    if not present then
      vim.notify("fail to get content from reg 0: " .. yank_data, vim.log.levels.ERROR)
      return
    end
    if #yank_data < 1 then
      return
    end

    copy(yank_data)
  end,
})

--
-- Automatically set PWD to buffer
--
au({ "BufEnter" }, {
  pattern = { "*" },
  callback = function()
    local finder = require("libs.find_root")

    local old_cwd = vim.loop.cwd()
    finder.set_root()
    local new_cwd = vim.loop.cwd()

    if not vim.b.current_buf_root_dir and new_cwd ~= old_cwd then
      vim.notify("Dir changed to: " .. new_cwd)
    end
  end,
})

--
-- Automatically set cursor to last editing line
--
local ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit", "Dashboard" }
local ignore_buftype = { "quickfix", "nofile", "help" }

local function can_jump()
  local contains = vim.tbl_contains
  local current_buftype = vim.api.nvim_get_option_value("buftype", {})
  -- return when current buftype is ignored
  if contains(ignore_buftype, current_buftype) then
    return false
  end

  -- jump to the beginning of the file and return when current filetype is ignored
  local current_filetype = vim.api.nvim_get_option_value("filetype", {})
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

local auto_jump_gid = vim.api.nvim_create_augroup("AutoJumpLastPlace", { clear = true })
vim.api.nvim_create_autocmd({ "BufWinEnter", "FileType" }, {
  callback = function()
    if not can_jump() then
      return
    end
    set_cursor()
  end,
  group = auto_jump_gid,
})
