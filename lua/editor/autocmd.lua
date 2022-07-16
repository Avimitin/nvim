local au = vim.api.nvim_create_autocmd

-- use relativenumber when editing
au({ "InsertEnter" }, { pattern = { "*" }, command = "set nornu" })
au({ "InsertLeave" }, { pattern = { "*" }, command = "set rnu" })

-- highlight yanked text
au("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })
  end,
})

local has_custom_config, custom = pcall(require, "custom")

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

local function setup_auto_jump_last_edit_line()
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
end

if has_custom_config and custom.enable_lastline then
  setup_auto_jump_last_edit_line()
end

-- Uncomment this if you want the galaxyline to auto reload after resizing the window.
-- Side effect: this autocommand can be invoked by scrolling window.
--
--[[ au("WinScrolled", {
  callback = function()
    require('galaxyline').load_galaxyline()
  end,
}) ]]

--
-- [fcitx5 auto toggle]
--
-- In archlinux, fcitx5-remote is already contained in fcitx5 packages.
-- If fcitx5-remote executable is not found on your machine, try install one.
--
-- This autocommand will not always activate fcitx5 in insert mode.
-- My approach is to activate fcitx5 only when fcitx5 is manually triggered last
-- time in insert mode.
--
-- You can disable this feature in `~/.config/lua/custom.lua` file
--
if not has_custom_config or not custom.auto_toggle_fcitx5 then
  return
end

local function async_close_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      args = { "-c" },
      on_exit = function(_, ret)
        if ret ~= 0 then
          vim.notify("fail to close fcitx5")
          return
        end

        vim.g.fcitx5_should_reactivate = true
      end,
    })
    :start()
end

local function async_toggle_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      on_exit = function(j, ret)
        if ret ~= 0 then
          vim.notify("Fail to execute fcitx5-remote command")
          return
        end

        local stdout = j:result()
        -- Grievance: why Lua array index start with 1?
        if #stdout > 0 and stdout[1] == "2" then
          async_close_fcitx5()
        end
      end,
    })
    :start()
end

local function async_open_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      args = { "-o" },
      on_exit = function(_, ret)
        if ret ~= 0 then
          vim.notify("fail to open fcitx5")
          return
        end

        vim.g.fcitx5_should_reactivate = false
      end,
    })
    :start()
end

au({ "InsertLeave" }, {
  callback = function()
    -- plenary is a wrapper for libuv API, it can help us asynchronously execute commands.
    -- But it is not included in Neovim API, so we have to make sure that it **does** exist.
    local has_plenary, _ = pcall(require, "plenary")

    if has_plenary then
      async_toggle_fcitx5()
      return
    end

    --
    -- use neovim API as a fallback solution
    --
    -- If the fcitx5 is manually activated in insert mode
    local is_active = vim.fn.system("fcitx5-remote"):gsub("%s+", "") == "2"

    if is_active then
      vim.fn.system("fcitx5-remote -c")
      vim.g.fcitx5_should_reactivate = true
    end
  end,
})

au({ "InsertEnter" }, {
  callback = function()
    if not vim.g.fcitx5_should_reactivate then
      return
    end

    -- use plenary as primary solution
    local has_plenary, _ = pcall(require, "plenary")
    if has_plenary then
      async_open_fcitx5()
      return
    end

    -- use neovim API as a fallback solution
    vim.fn.system("fcitx5-remote -o")
    vim.g.fcitx5_should_reactivate = false
  end,
})
