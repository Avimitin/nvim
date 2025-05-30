local au = vim.api.nvim_create_autocmd

-- Copy data to system clipboard only when we are pressing 'y'. 'd', 'x' will be filtered out.
--
-- Credit: https://github.com/ibhagwan/smartyank.nvim
local smart_yank_gid = vim.api.nvim_create_augroup("SmartYank", { clear = true })
au("TextYankPost", {
  group = smart_yank_gid,
  desc = "Copy and highlight yanked text to system clipboard",
  callback = function()
    vim.hl.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })

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
au({ "VimEnter" }, {
  pattern = { "*" },
  callback = function()
    local finder = require("libs.find_root")

    local old_cwd = vim.loop.cwd()
    finder.set_root({ patterns = { "Cargo.toml", ".git", "flake.nix" } })
    local new_cwd = vim.loop.cwd()

    if not vim.b.current_buf_root_dir and new_cwd ~= old_cwd then
      vim.notify("Dir changed to: " .. new_cwd)
    end
  end,
})

au({ "BufWinEnter" }, {
  desc = "return cursor to where it was last time closing the file",
  group = vim.api.nvim_create_augroup("AutoJump", { clear = true }),
  callback = function()
    if not vim.tbl_contains({ "", "acwrite" }, vim.bo.buftype) then
      return
    end
    if vim.tbl_contains({ "gitcommit" }, vim.bo.filetype) then
      return
    end

    vim.cmd('silent! normal! g`"zv')
  end,
})

-- only highlight when searching
au("CmdlineEnter", {
  callback = function()
    local cmd = vim.v.event.cmdtype
    if cmd == "/" or cmd == "?" then
      vim.opt.hlsearch = true
    end
  end,
})
au("CmdlineLeave", {
  callback = function()
    local cmd = vim.v.event.cmdtype
    if cmd == "/" or cmd == "?" then
      vim.opt.hlsearch = false
    end
  end,
})
