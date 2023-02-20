local au = vim.api.nvim_create_autocmd
local option = vim.cfg.core.autocmd

if option.relative_number then
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

if option.auto_insert then
  au({ "TermOpen", "TermEnter" }, { pattern = { "*" }, command = "startinsert" })
  au({ "WinEnter" }, { pattern = { "term://*toggleterm#*" }, command = "startinsert" })
end

-- Copy data to system clipboard only when we are pressing 'y'. 'd', 'x' will be filtered out.
--
-- Credit: https://github.com/ibhagwan/smartyank.nvim
if option.highlight_yanked or option.copy_yanked_to_clipboard then
  local smart_yank_gid = vim.api.nvim_create_augroup("SmartYank", { clear = true })
  au("TextYankPost", {
    group = smart_yank_gid,
    desc = "Copy and highlight yanked text to system clipboard",
    callback = function()
      if option.highlight_yanked then
        vim.highlight.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })
      end

      if not option.copy_yanked_to_clipboard then
        return
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

if option.find_project_root then
  au({ "VimEnter" }, {
    pattern = { "*" },
    callback = function()
      -- FIXME: install rooter plugin
      if true then
        return
      end

      local rooter = require("libs.rooter")
      local opts = {
        rooter_patterns = { ".git", ".hg", ".svn", "Cargo.toml", "go.mod", "package.json" },
        exclude_filetypes = { "gitcommit" },
        manual = false,
      }

      local old_cwd = vim.loop.cwd()

      rooter.setup(opts)
      rooter.rooter()

      local new_cwd = vim.loop.cwd()

      if new_cwd ~= old_cwd then
        vim.notify("Dir changed: " .. new_cwd)
      end
    end,
  })
end

local user_hide_commandline = vim.opt.ch == 0
if user_hide_commandline then
  au("CmdlineEnter", {
    pattern = "*",
    callback = function()
      vim.opt.ch = 1
    end,
  })

  au("CmdlineLeave", {
    pattern = "*",
    callback = function()
      vim.opt.ch = 0
    end,
  })
end

if option.toggle_fcitx5 then
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
end
-- End of fcitx5 --

if option.jump_lastline.enable then
  local ignore_buftype = option.jump_lastline.ignore_buffer_type
  local ignore_filetype = option.jump_lastline.ignore_filetype

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
