local utils = require("mappings.utils")
local map = utils.map
local nmap = utils.nmap
local xmap = utils.xmap
local d = utils.new_desc

vim.g.mapleader = " "

-- load plugin's keymapping
require("mappings.other")

-- quicker motion
nmap("J", "5j", d("Jump 5 lines down"))
xmap("J", "5j", d("Jump 5 lines down"))

nmap("K", "5k", d("Jump 5 lines up"))
xmap("K", "5k", d("Jump 5 lines up"))

nmap("L", "g_", d("Jump to the end of the character"))
nmap("H", "^", d("Jump to the beginning of the character"))

xmap("L", "g_", d("Jump to the end of the character"))
xmap("H", "^", d("Jump to the beginning of the character"))

nmap("W", "5w", d("Jump 5 word forward"))
nmap("B", "5b", d("Jump 5 word backward"))

-- no more background key
nmap("<C-z>", "u", d("Revert change"))

-- move block easily
nmap("<", "<<", d("Decrease indent"))
nmap(">", ">>", d("Increase indent"))
xmap("<", "<gv", d("Increase indent"))
xmap(">", ">gv", d("Decrease indent"))

-- create tab like window
nmap("<C-T>h", ":tabprevious<CR>", d("Goto previous tab"))
nmap("<C-T>l", ":tabnext<CR>", d("Goto next tab"))
nmap("<C-T>n", ":tabnew<CR>", d("Create a new tab"))

-- save quickly
nmap(";w", ":w<CR>", d("Save buffer"))

-- do thing like ctrl c and ctrl v
xmap("<C-y>", [["+y]], d("Copy to system clipboard"))
nmap("<C-p>", [["+p]], d("Paste from system clipboard"))
map("i", "<C-p>", [[<ESC>"+pa]], d("Paste from system clipboard"))

-- shut down the search high light
nmap("<ESC>", ":nohlsearch<CR>", d("Close search highlight"))
-- no more finger expansion
map("i", "<A-;>", "<ESC>", d("Exit the insert mode"))

-- move around the window
nmap(";k", "<C-w>k", d("Jump to window above"))
nmap(";j", "<C-w>j", d("Jump to window below"))
nmap(";l", "<C-w>l", d("Jump to the left window"))
nmap(";h", "<C-w>h", d("Jump to the righ window"))

-- resize the window
nmap("<C-S-up>", ":res +5<CR>", d("Extend the upper boundary of the current window"))
nmap("<C-S-down>", ":res -5<CR>", d("Extend the lower boundary of the current window"))
nmap("<C-S-right>", ":vertical resize-5<CR>", d("Extend the right boundary of the current window"))
nmap("<C-S-left>", ":vertical resize+5<CR>", d("Extend the right boundary of the current window"))

-- center editing line
map("i", "<C-c>", "<ESC>zzi", d("Place the current line to the middle of the screen"))

-- After plugin loaded
vim.defer_fn(function()
  local wk = require("which-key")

  wk.register({
    gi = {
      name = "+Git",
      B = "Toggle git blame (Popup Panel)",
      b = "Toggle inline git blame (Virtual text)",
      c = "Commit staged changes",
      D = "Toggle diff panel",
      d = "Toggle diff panel",
      h = "Toggle deleted",
      j = "Jump to next hunk",
      k = "Jump to previouse hunk",
      P = "Create a git push prompt",
      p = "Preview current diff",
      r = "Reset current hunk",
      R = "Reset whole buffer",
      S = "Stage current buffer",
      s = "Stage current hunk",
      u = "Undo staged hunk",
    },
  })

  wk.register({
    [";"] = {
      name = "+Quick Operation",
      d = "Open cmdline to dispatch commands",
      f = "Open quick find file panel",
      g = "Open git status panel",
      h = "Jump to left window",
      j = "Jump to below window",
      k = "Jump to upper window",
      l = "Jump to right window",
      p = "Enter buffer pick mode",
      q = "Quit current buffer",
      s = "Open live grep panel",
      t = "Open tree file manager",
      w = "Save buffer to file",
    },
  })

  wk.register({
    ["<leader>"] = {
      t = "Open telescope",
      l = "Open lazygit",
    },
  })

  wk.register({
    c = {
      name = "+Comments",
      c = { "Comment current line" },
    },
  }, { prefix = "g" })
end, 10)
