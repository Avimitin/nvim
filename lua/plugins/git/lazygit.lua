local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({
  cmd = "lazygit",
  hidden = true,
  direction = "float",
  on_open = function(term)
    vim.cmd("startinsert!")
    vim.api.nvim_buf_set_keymap(
      term.bufnr,
      "t",
      ";a",
      "<cmd>close<CR>",
      { noremap = true, silent = true }
    )
  end,
  env = {
    GIT_EDITOR = "nvr -cc split --remote-wait +'set bufhidden=wipe'",
  },
})

return lazygit
