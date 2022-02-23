if not pcall(require, "FTerm") then
  vim.notify("Fail to load FTerm", vim.log.levels.ERROR, { title = "plugins" })
  return
end

require("FTerm").setup({
  dimensions = { height = 1, width = 1 },
  border = "rounded",
  hl = "FTermBackground",
})

vim.cmd('command! FTermToggle lua require("FTerm").toggle()')
