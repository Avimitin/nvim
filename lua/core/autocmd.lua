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
