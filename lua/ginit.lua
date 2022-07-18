vim.opt.mouse = "a"
vim.opt.guifont = [[FiraCode\ Nerd\ Font\ Mono:h18]]
vim.opt.title = true

vim.api.nvim_set_keymap(
  "n",
  "<RightMouse>",
  "call GuiShowContextMenu()<CR>",
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "i",
  "<RightMouse>",
  "<ESC>call GuiShowContextMenu()<CR>",
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "v",
  "<RightMouse>",
  "call GuiShowContextMenu()<CR>gv",
  { silent = true, noremap = true }
)
