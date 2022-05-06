local au = vim.api.nvim_create_autocmd

-- use relativenumber when editing
au({ "InsertEnter" }, { pattern = { "*" }, command = "set nornu" })
au({ "InsertLeave" }, { pattern = { "*" }, command = "set rnu" })

-- auto compile when editing the load.lua file
au({ "BufWritePost" }, { pattern = "load.lua", command = "source <afile> | PackerCompile" })

-- start insert when enter the terminal
au({ "TermOpen" }, { pattern = "term://*", command = "startinsert" })

-- highlight yanked text
au("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ higroup = "HighLightLineMatches", timeout = 200 })
  end,
})

-- Automatically close and activate fcitx5
-- Enable this feature in `~/.config/lua/custom.lua` file
-- If you don't have one, just create it.
local ok, custom = pcall(require, "custom")
if not ok or not custom.has_fcitx5 then
  return
end

-- In archlinux, fcitx5-remote is contained in fcitx5 packages.
-- If fcitx5-remote executable is not found on your machine, remove this autocommand.
--
-- This autocommand will not always activate fcitx5 in insert mode.
-- My approach is to activate fcitx5 only when fcitx5 is manually triggered last
-- time in insert mode.

au({ "InsertLeave" }, {
  callback = function()
    -- If the fcitx5 is manually activated in insert mode
    vim.g.fcitx5_is_active = vim.fn.system("fcitx5-remote"):gsub("%s+", "") == "2"

    -- if fcitx5 is active, we close it when we leave insert mode
    if vim.g.fcitx5_is_active then
      vim.fn.system("fcitx5-remote -c")

      -- when we inactive the fcitx5 in autocommand, we might want to reactive it
      -- in insert mode. This can help InsertEnter autocommand know the fcitx5 is
      -- close by neovim, not user.
      vim.g.fcitx5_should_reactivate = true
      vim.g.fcitx5_is_active = false
    end
  end,
})

au({ "InsertEnter" }, {
  callback = function()
    if vim.g.fcitx5_should_reactivate then
      vim.fn.system("fcitx5-remote -o")

      vim.g.fcitx5_should_reactivate = false
    end
  end,
})
