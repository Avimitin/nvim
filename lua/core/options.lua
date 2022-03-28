local opt = vim.opt

opt.encoding = "utf-8"
-- When file encoding forcely set to UTF-8, some file with non-Unicode
-- encoding will lose information during the encoding conversion.
-- If you have problem with this encoding, set value to empty string "".
opt.fileencoding = "utf-8"

-- enable number and relative line number
opt.number = true
opt.rnu = true

-- highlight the current line
opt.cursorline = true

-- TAB SETTING
-- Use 2 spaces forcely. But don't worry, vim-sleuth will handle the indent
-- gracefully. See <https://github.com/tpope/vim-sleuth> for more.
--
-- Use the appropriate number of spaces to insert a <Tab>.
opt.expandtab = true
-- Number of spaces that a <Tab> in the file counts for.
opt.tabstop = 2
opt.shiftwidth = 2
opt.softtabstop = 2

-- Copy indent from current line when starting a new line
opt.autoindent = true

-- A List is an ordered sequence of items.
opt.list = true
opt.listchars = "tab:> ,trail:·"

-- Minimal number of screen lines to keep above and below the cursor.
opt.scrolloff = 5

-- Time in milliseconds to wait for a key code sequence to complete
opt.timeoutlen = 200
opt.ttimeoutlen = 0
-- no waiting for key combination
opt.timeout = false

-- remember where to recover cursor
opt.viewoptions = "cursor,folds,slash,unix"

-- lines longer than the width of the window will wrap and displaying continues
-- on the next line.
opt.wrap = true

-- set text width to zero to use the wrap functionality
opt.tw = 0

opt.cindent = true

-- set windows split at bottom-right by default
opt.splitright = true
opt.splitbelow = true

-- don't show the "--VISUAL--" "--INSERT--" text
opt.showmode = false

-- show chars, selected block in visual mode
opt.showcmd = true

-- auto completion on command
opt.wildmenu = true

-- ignore case when searching and only on searching
opt.ignorecase = true
opt.smartcase = true

vim.cmd("set shortmess+=cwm")
opt.inccommand = "split"
opt.completeopt = { "menuone", "noselect", "menu" }
opt.ttyfast = true
opt.lazyredraw = true
opt.visualbell = true
opt.updatetime = 100
opt.virtualedit = "block"

-- highlight a column at the 100 chars
opt.colorcolumn = "100"

-- screen will not redraw when exec marcro, register
opt.lazyredraw = true

-- always draw signcolumn, with 1 fixed space to show 2 icon at the same time
opt.signcolumn = "yes:1"

-- enable all the mouse functionality
opt.mouse = "a"

-- use indent as the fold method
opt.foldmethod = "indent"
opt.foldlevel = 99
opt.foldenable = true
opt.formatoptions = "qj"

opt.hidden = true

-- conceal the text
opt.conceallevel = 2
-- show hiding character at cursor line
opt.concealcursor = ""

-- Changed home directory here
local backup_dir = vim.fn.stdpath("cache") .. "/backup"
local backup_stat = pcall(os.execute, "mkdir -p " .. backup_dir)
if backup_stat then
  opt.backupdir = backup_dir
  opt.directory = backup_dir
end

local undo_dir = vim.fn.stdpath("cache") .. "/undo"
local undo_stat = pcall(os.execute, "mkdir -p " .. undo_dir)
local has_persist = vim.fn.has("persistent_undo")
if undo_stat and has_persist == 1 then
  opt.undofile = true
  opt.undodir = undo_dir
end

-- nvui specific settings
if vim.g.nvui then
  vim.opt.guifont = [[Cascadia Code:h12,FiraCode\ Nerd\ Font\ Mono:h12]]
  vim.cmd([[NvuiCmdFontFamily FiraCode Nerd Font Mono]])
  vim.cmd([[NvuiCmdFontSize 12]])
  vim.cmd([[NvuiAnimationsEnabled 1]])
  vim.cmd([[NvuiCmdCenterXPos 0.5]])
  vim.cmd([[NvuiCmdCenterYPos 0.2]])
  vim.cmd([[NvuiCmdBorderWidth 3]])
  vim.cmd([[NvuiCmdBorderColor #6E6C6A]])
  vim.cmd([[NvuiCmdBigFontScaleFactor 1.3]])
  vim.cmd([[NvuiCmdPadding 13]])
  vim.cmd([[NvuiPopupMenuBorderWidth 4]])
  vim.cmd([[NvuiPopupMenuBorderColor #6E6C6A]])
  -- nvui g3486971 feature
  vim.cmd([[autocmd InsertEnter * NvuiIMEEnable]])
  vim.cmd([[autocmd InsertLeave * NvuiIMEDisable]])
  -- nvui g87f61c0 feature
  vim.cmd([[hi Normal guisp=#6899B8]])
end

if vim.g.neovide then
  -- neovide specific settings
  vim.g.neovide_cursor_vfx_mode = "sonicboom"
  vim.opt.guifont = [[monospace:h11]]
end
