noremap u k
noremap U 5k
noremap k l
noremap H 0
noremap J 5j
noremap K $
noremap <C-u> 5<C-y>
noremap <C-j> 5<C-e>
noremap W 5w
noremap B 5b
noremap l w
noremap L b
inoremap <C-a> <ESC>A

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

"edit file at new tab
nnoremap <LEADER>e :tabe<CR>:edit 

"edit neovim file at any time
nnoremap <LEADER>n :tabe<CR>:edit ~/AppData/Local/nvim/init.vim<CR>

"reboot vim
noremap <LEADER>rb :so ~/AppData/Local/nvim/init.vim<CR>

"save
noremap <silent> <LEADER>s :w<CR>

"save and quit
noremap <LEADER>q :wq<CR>

"normal quit
nnoremap Q :q<CR>

"copy select chars
vnoremap <LEADER>y "+y

"paste from system clipboard
noremap <LEADER>p "+p

"close highlight
nnoremap <silent> <LEADER><CR> :nohlsearch<CR>

"escape
inoremap <silent> jj <ESC>

"fold para
noremap <silent> <LEADER>o za "folding

"jump to next '<++>' and edit it
noremap <LEADER><LEADER> <ESC>/<++><CR>:nohlsearch<CR>c4l

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

"move cursor between windows
noremap su <C-w>k
noremap sj <C-w>j
noremap sh <C-w>h
noremap sk <C-w>l

"tab
noremap tu :tabe<CR>
noremap th :-tabnext<CR>
noremap tk :+tabnext<CR>
noremap tmh :-tabmove<CR>
noremap tmk :+tabmove<CR>

"focus on center
inoremap <C-c> <ESC>zzi

"terminal
nnoremap <C-\> :tabe<CR>:term<CR>
tnoremap <C-q> <C-\><C-n>:q<CR>
tnoremap <C-n> <C-\><C-n>

nnoremap r R
