local opt = vim.opt

opt.encoding='utf-8'
opt.termguicolors=true
opt.background="dark"

-- enable number and relative line number
opt.number=true

-- line behind cursor
opt.cursorline=true

-- [[
-- TAB SETTING
-- Use a mix of tab and space to avoid mix up file
-- ]]
-- Use the appropriate number of spaces to insert a <Tab>.
opt.expandtab=false
-- Number of spaces that a <Tab> in the file counts for.
opt.tabstop=2
opt.shiftwidth=2
opt.softtabstop=2

-- Copy indent from current line when starting a new line
--opt.autoindent=true

-- A List is an ordered sequence of items.
opt.list=true
opt.listchars="tab:<->,trail:·"

-- Minimal number of screen lines to keep above and below the cursor.
opt.scrolloff=5

-- Time in milliseconds to wait for a key code sequence to complete
opt.ttimeoutlen=0
-- no waiting for key combination
opt.timeout=false

-- remember where to recover cursor
opt.viewoptions='cursor,folds,slash,unix'

-- [[
-- lines longer than the width of the window will wrap and displaying continues
-- on the next line.
-- ]]
opt.wrap=true
opt.tw=0
opt.cindent=true
opt.splitright=true
opt.splitbelow=true
opt.showmode=false
opt.showcmd=true

-- auto completion on command
opt.wildmenu=true

-- ignore case when searching and only on searching
opt.ignorecase=true
opt.smartcase=true

vim.cmd("set shortmess+=c")
opt.inccommand='split'
opt.completeopt={"menuone", "noselect", "noinsert", "preview"}
opt.ttyfast=true
opt.lazyredraw=true
opt.visualbell=true
opt.updatetime=100
opt.virtualedit='block'
opt.colorcolumn='100'
opt.lazyredraw=true
opt.signcolumn="yes:1"
opt.mouse='a'

opt.foldmethod='indent'
opt.foldlevel=99
opt.foldenable=true
opt.formatoptions='qj'

opt.hidden=true

-- Changed home directory here
local backup_dir = vim.fn.stdpath("cache").."/backup"
local backup_stat = pcall(os.execute, "mkdir -p "..backup_dir)
if backup_stat then
  opt.backupdir=backup_dir
  opt.directory=backup_dir
end

local undo_dir = vim.fn.stdpath("cache").."/undo"
local undo_stat = pcall(os.execute, "mkdir -p "..undo_dir)
local has_persist = vim.fn.has("persistent_undo")
if undo_stat and has_persist == 1 then
  opt.undofile=true
  opt.undodir=undo_dir
end

vim.api.nvim_command('autocmd TermOpen term://* startinsert')
vim.cmd[[set guifont=JetbrainsMono\ Nerd\ Font\ Mono:h10]]
