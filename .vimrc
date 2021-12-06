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
set listchars=tab:\|\ ,trail:-
" statusline settings
let g:currentmode={
       \ 'n'  : 'NORMAL ',
       \ 'v'  : 'VISUAL ',
       \ 'V'  : 'V·Line ',
       \ "\<C-V>" : 'V·Block ',
       \ 'i'  : 'INSERT ',
       \ 'R'  : 'R ',
       \ 'Rv' : 'V·Replace ',
       \ 'c'  : 'Command ',
       \}

set statusline=
set statusline+=%1*
set statusline+=\ %{toupper(g:currentmode[mode()])}
set statusline+=%{&spell?'[SPELL]':''}
set statusline+=%#WarningMsg#
set statusline+=%{&paste?'[PASTE]':''}
set statusline+=%2*
set statusline+=\ %F
set statusline+=%{&modified?'\ [+]':''}
set statusline+=%{&readonly?'\ []':''}
set statusline+=%<
set statusline+=%=
set statusline+=%{&filetype!=#''?&filetype.'\ ':'none\ '}
set statusline+=%#WarningMsg#
set statusline+=%{&fileencoding!='utf-8'?'['.&fileencoding.']':''}
set statusline+=%2*
set statusline+=%-7([%{&fileformat}]%)
set statusline+=%#WarningMsg#
set statusline+=%{&bomb?'[BOM]':''}
set statusline+=%1*
set statusline+=[%l/%L]
set statusline+=\ col:%2c

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

nnoremap - N
nnoremap = n
nnoremap ; :
nnoremap ;w :w<CR>

nnoremap <C-t>n :tabnew<CR>
nnoremap <C-t>h :tabprevious<CR>
nnoremap <C-t>l :tabnext<CR>

nnoremap <up> :res +5<CR>
nnoremap <down> :res -5<CR>
nnoremap <left> :vertical resize-5<CR>
nnoremap <right> :vertical resize+5<CR>

nnoremap <esc> :nohlsearch<CR>

xnoremap <C-y> "+y
nnoremap <C-p> "+p
inoremap <C-p> "+p

inoremap jj <ESC>

"===== colors =====
set background=dark
colorscheme peachpuff
