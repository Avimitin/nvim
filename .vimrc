"===== settings =====
filetype plugin on
filetype indent on
syntax on
set mouse=a
set number
set rnu
set incsearch
set hlsearch
set autoread
set wildmenu
set laststatus=2
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set lbr
set tw=500
set ai
set si
set wrap
set nobackup
set nowb
set noswapfile
set encoding=utf8
set ruler
set cmdheight=1
set hid
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set ignorecase
set smartcase
set list
set listchars=tab:>\ ,trail:-

"===== keymaps =====
nnoremap J 5j
xnoremap J 5j
nnoremap K 5k
xnoremap K 5k

nnoremap L g_
xnoremap L g_
nnoremap H ^
xnoremap H ^

nnoremap W 5w
xnoremap W 5w
nnoremap B 5b
xnoremap B 5b

nnoremap <C-z> u

nnoremap > >>
xnoremap > >gv
nnoremap < <<
xnoremap < <gv

nnoremap ; :
nnoremap ;w :w<CR>

nnoremap <C-t>n :tabnew<CR>
nnoremap <C-t>h :tabprevious<CR>
nnoremap <C-t>l :tabnext<CR>

nnoremap <up> :res +5<CR>
nnoremap <down> :res -5<CR>
nnoremap <left> :vertical resize-5<CR>
nnoremap <right> :vertical resize+5<CR>

inoremap jj <ESC>

"===== colors =====
set background=dark
colorscheme peachpuff
