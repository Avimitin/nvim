local au = vim.api.nvim_create_autocmd

-- use relativenumber when editing
au({ "InsertEnter" }, { pattern = { "*" }, command = "set nornu" })
au({ "InsertLeave" }, { pattern = { "*" }, command = "set rnu" })

au("FileType", {
  pattern = "markdown",
  callback = function()
    vim.api.nvim_set_keymap("n", "ge", "", {
      noremap = true,
      silent = false,
      callback = function()
        -- TODO: pass user configuration into it
        require("plugins.libs.markdown-openfile").edit_url()
      end,
      desc = "Edit the file under cursor",
    })
  end,
})

au({ "TermOpen", "TermEnter" }, { pattern = { "*" }, command = "startinsert" })
au({ "WinEnter" }, { pattern = { "term://*toggleterm#*" }, command = "startinsert" })

-- Copy data to system clipboard only when we are pressing 'y'. 'd', 'x' will be filtered out.
--
-- Credit: https://github.com/ibhagwan/smartyank.nvim
local smart_yank_gid = vim.api.nvim_create_augroup("SmartYank", { clear = true })
au("TextYankPost", {
  group = smart_yank_gid,
  desc = "Copy and highlight yanked text to system clipboard",
  callback = function()
    -- first highlight it
    vim.highlight.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })

    -- if user doesn't have clipboard
    if not vim.fn.has("clipboard") == 1 then
      return
    end

    -- if user are not intended to copy something, abort the action
    if vim.v.operator ~= "y" then
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

au({ "VimEnter" }, {
  pattern = { "*" },
  callback = function()
    local rooter = require("libs.rooter")
    local opts = {
      rooter_patterns = { ".git", ".hg", ".svn", "Cargo.toml", "go.mod", "package.json" },
      exclude_filetypes = { "gitcommit" },
      -- Trigger manually
      manual = false,
    }

    local old_cwd = vim.loop.cwd()

    rooter.setup(opts)
    -- trigger it only once
    rooter.rooter()

    local new_cwd = vim.loop.cwd()

    if new_cwd ~= old_cwd then
      vim.notify("Dir changed: " .. new_cwd)
    end
  end,
})

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
