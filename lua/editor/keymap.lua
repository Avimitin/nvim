-- map create a new mapping
---@param mode string|table specify vim mode
---@param lhs string specify the new keymap
---@param rhs string|function specify the keymap or commands
---@param opts table|nil setting options. Default: { noremap = true, silent = true, expr = false }
local function map(mode, lhs, rhs, opts)
  local options = {
    noremap = true,
    silent = true,
    expr = false,
  }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  if type(rhs) == "function" then
    options.callback = rhs
    rhs = ""
  end
  local stat, error = pcall(vim.keymap.set, mode, lhs, rhs, options)
  if not stat then
    vim.notify(error, vim.log.levels.ERROR, {
      title = "keymap",
    })
  end
end

local function nmap(...)
  map("n", ...)
end

local function xmap(...)
  map("x", ...)
end

local function imap(...)
  map("i", ...)
end

local d = function(s)
  return { desc = s }
end

vim.g.mapleader = ";"

-- quicker motion
nmap("J", "5j", d("Jump 5 lines down"))
xmap("J", "5j", d("Jump 5 lines down"))

nmap("K", "5k", d("Jump 5 lines up"))
xmap("K", "5k", d("Jump 5 lines up"))

-- Emacs key mapping in insert mode
imap("<C-a>", "<Home>")
imap("<C-e>", "<End>")
imap("<C-b>", "<ESC>bi")
imap("<C-f>", "<ESC>wa")
imap("<C-n>", "<ESC>ja")
imap("<C-p>", "<ESC>ka")

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

-- shut down the search high light
nmap("<ESC>", ":nohlsearch<CR>", d("Close search highlight"))
-- no more finger expansion
map("i", "<A-;>", "<ESC>", d("Exit the insert mode"))

-- kill buffer with ;q , quit window with :q.
nmap(";q", require("plugins.libs.bufdel").delete_buffer)

-- Write and quit. Alias for :wq<CR>
nmap(";x", ":x<CR>")

-- % is so hard to reach...
map({ "n", "x", "o" }, ",", "%", { noremap = false, silent = false })

-- paste from system clipboard
nmap("<C-p>", [["+p]])

return {
  map = map,
  nmap = nmap,
  imap = imap,
  xmap = xmap,
}
