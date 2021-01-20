" Author: @avimitin

let &t_ut='' "adjust terminal color

"vim setting
set exrc "exec command in init.vim
set secure "safely do command above
set autochdir "auto change directory
set number
set relativenumber
set cursorline "set line below cursor
set noexpandtab "use only '\t' as tab
set tabstop=2 "show how many space for a '\t'
set shiftwidth=2 "use how many space for >> or <<
set softtabstop=2 "use how many space when pressing tab
set autoindent
set list "show hiding char
set listchars=tab:\|\ ,trail:· "define tab and space show
set scrolloff=4 "least amount line below and above the cursor
set ttimeoutlen=0 "set never wait for key
set notimeout
set viewoptions=cursor,folds,slash,unix "remember where to recover cursor
set wrap "auto line feed
set tw=0 "text width for automatically wrapping
set indentexpr=
set splitright
set splitbelow
set noshowmode "not showing current mode
set showcmd "show cmd inputing like key combine
set wildmenu "auto finish vim command
set ignorecase "ignore case when searching
set smartcase "ignore case only on searching
set shortmess+=c "don't show useless msg
set inccommand=split "show substitution automatically
set completeopt=longest,noinsert,menuone,noselect,preview "complete opject with a menue
set ttyfast "make scrolling faster
set lazyredraw
set visualbell "flash screen to notify error
set updatetime=100
set virtualedit=block
set colorcolumn=100

"set the rule of folding line
set foldmethod=indent
set foldlevel=99
set foldenable
set formatoptions-=tc

"keep undo history
set hidden
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/backup,.
if has('persistent_undo')
	set undofile
	set undodir=~/.config/nvim/tmp/undo,.
endif

"cursor movement
noremap <silent> u k
noremap <silent> U 5k
noremap <silent> H 0
noremap <silent> J 5j
noremap <silent> k l
noremap <silent> K $
noremap W 5w
noremap B 5b
inoremap <C-a> <ESC>A
inoremap <C-i> <ESC>I

"delete a line
noremap D dd

"clip a line
noremap X Vx

"force quit
noremap <C-A-S-q> :q!<CR>

"undo
noremap <C-z> u

"add indent
nnoremap < <<
nnoremap > >>

"disable key 's'
noremap s <nop>

"up and down in search mode
noremap - N
noremap = n

"set leader
let mapleader=" "

"save
noremap <LEADER>s :w<CR>

"save and quit
noremap <LEADER>q :wq<CR>

"copy select chars
vnoremap <LEADER>y "+y

"close highlight
noremap <LEADER><CR> :nohlsearch<CR>

"fold para
noremap <silent> <LEADER>f za "folding

"jump to next 'todo' and edit it
noremap <LEADER><LEADER> <ESC>/TODO<CR>:nohlsearch<CR>c4l

"split screen
noremap su :set nosplitbelow<CR>:split<CR>:set splitbelow<CR>
noremap sj :set splitbelow<CR>:split<CR>
noremap sh :set nosplitright<CR>:vsplit<CR>:set splitright<CR>
noremap sk :set splitright<CR>:vsplit<CR>

"place screen up and down
noremap sw <C-w>t<C-w>K
"place screen side by side
noremap sb <C-w>t<C-w>H

"rotate screens
noremap srk <C-w>b<C-w>K
noremap srh <C-w>b<C-w>H

"resize split windows
nnoremap <up> :res +5<CR>
nnoremap <down> :res -5<CR>
nnoremap <left> :vertical resize-5<CR>
nnoremap <right> :vertical resize+5<CR>

"tab
noremap tu :tabe<CR>
noremap th :-tabnext<CR>
noremap tk :+tabnext<CR>
noremap tmh :-tabmove<CR>
noremap tmk :+tabmove<CR>

"focus on center
noremap <C-c> <ESC>zz
