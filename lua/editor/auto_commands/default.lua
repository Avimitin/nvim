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
