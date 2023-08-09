local M = {}

local au = vim.api.nvim_create_autocmd

function M.change_rnu()
  -- use relativenumber when editing
  local rnu_group = vim.api.nvim_create_augroup("RNUGroup", { clear = true })
  au({ "InsertEnter" }, {
    group = rnu_group,
    pattern = { "*" },
    callback = function()
      vim.opt.rnu = false
    end,
  })
  au({ "InsertLeave" }, {
    group = rnu_group,
    pattern = { "*" },
    callback = function()
      if vim.fn.mode() ~= "i" then
        vim.opt.rnu = true
      end
    end,
  })
end

function M.insert_mode_in_term()
  au({ "TermOpen", "TermEnter" }, { pattern = { "*" }, command = "startinsert" })
  au({ "WinEnter" }, { pattern = { "term://*toggleterm#*" }, command = "startinsert" })
end

function M.smart_yank(opts)
  local smart_yank_gid = vim.api.nvim_create_augroup("SmartYank", { clear = true })
  au("TextYankPost", {
    group = smart_yank_gid,
    desc = "Copy and highlight yanked text to system clipboard",
    callback = function()
      if opts.highlight.enable then
        vim.highlight.on_yank({
          higroup = "HighLightLineMatches",
          timeout = opts.highlight.duration or 200,
        })
      end

      if not vim.fn.has("clipboard") == 1 then
        return
      end

      local copy_key_is_y = vim.v.operator == "y"
      if not copy_key_is_y then
        return
      end

      local copy = function(str)
        pcall(vim.fn.setreg, "+", str)
      end

      local present, yank_data = pcall(vim.fn.getreg, "0")
      if not present or #yank_data < 1 then
        return
      end

      copy(yank_data)
    end,
  })
end

function M.cd_project_root(opts)
  au({ "VimEnter" }, {
    pattern = { "*" },
    callback = function()
      require("libs.find_root").goto_root(vim.tbl_extend("force", {
        ".git",
        ".hg",
        ".svn",
      }, opts.root_patterns or {}))
    end,
  })
end

function M.fcitx5_toggle()
  au({ "InsertLeave" }, {
    callback = function()
      vim.system({ "fcitx5-remote" }, nil, function(fd)
        if fd.code ~= 0 then
          vim.notify("fail to call fcitx5-remote: " .. fd.stderr, vim.log.levels.ERROR)
        end
        local is_active = fd.stdout:gsub("%s+", "") == "2"
        if is_active then
          vim.fn.system("fcitx5-remote -c")
          vim.g.fcitx5_should_reactivate = true
        end
      end)
    end,
  })

  au({ "InsertEnter" }, {
    callback = function()
      if not vim.g.fcitx5_should_reactivate then
        return
      end

      -- use neovim API as a fallback solution
      vim.fn.system("fcitx5-remote -o")
      vim.g.fcitx5_should_reactivate = false
    end,
  })
end

-- jump to lastline on enter
function M.lastline(opts)
  local ignore_buftype = opts.ignore_buffer_type
  local ignore_filetype = opts.ignore_file_type

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
end

-- automatically switch on/off cursor line
function M.cursor_line()
  au({ "VimEnter", "WinEnter", "InsertLeave" }, {
    callback = function()
      vim.wo.cursorline = true
    end,
  })
  au({ "WinLeave", "InsertEnter" }, {
    callback = function()
      vim.wo.cursorline = false
    end,
  })
end

return M
