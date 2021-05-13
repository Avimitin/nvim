noremap i k
noremap j h
noremap k j

noremap I 5k
noremap K 5j
noremap L $
noremap J 0

noremap <C-k> 5<C-y>
noremap <C-j> 5<C-e>
inoremap <C-a> <ESC>A

"delete a line
noremap D J

"clip a line
noremap X Vx

"force quit
noremap <C-A-S-q> :q!<CR>

noremap u <nop>

"undo
noremap <C-z> u

noremap ; i
noremap <C-i> I

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
nnoremap <LEADER>n :tabe<CR>:edit ~/.config/nvim/init.vim<CR>

"reboot vim
noremap <LEADER>rb :so ~/.config/nvim/init.vim<CR>

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
inoremap <silent> ;; <ESC>

"fold para
noremap <silent> <LEADER>o za "folding

"jump to next '<++>' and edit it
noremap <LEADER><LEADER> <ESC>/<++><CR>:nohlsearch<CR>c4l

"place screen up and down
noremap sw <C-w>t<C-w>K
"place screen side by side
noremap sb <C-w>t<C-w>H

"rotate screens
noremap srj <C-w>b<C-w>K
noremap srl <C-w>b<C-w>H

"resize split windows
nnoremap <up> :res +5<CR>
nnoremap <down> :res -5<CR>
nnoremap <left> :vertical resize-5<CR>
nnoremap <right> :vertical resize+5<CR>

"move cursor between windows
noremap si <C-w>k
noremap sk <C-w>j
noremap sj <C-w>h
noremap sl <C-w>l

"tab
noremap ta :tabe<CR>
noremap tj :-tabnext<CR>
noremap tl :+tabnext<CR>
noremap tmj :-tabmove<CR>
noremap tml :+tabmove<CR>

"focus on center
inoremap <C-c> <ESC>zzi

"terminal
nnoremap <C-\> :tabe<CR>:term<CR>
tnoremap <C-q> <C-\><C-n>:q<CR>
tnoremap <C-n> <C-\><C-n>
