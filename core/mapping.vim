noremap K 5k
noremap J 5j
noremap L $
noremap H 0

"delete a line
noremap D J

"clip a line
noremap X Vx

"force quit
noremap <C-A-S-q> :q!<CR>

noremap W 5w
noremap B 5b
noremap vw viw

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

"save (with normal and leader habit)
nnoremap <silent> <LEADER>s :w<CR>
nnoremap <silent> <C-s> :w<CR>

"save and quit
nnoremap <LEADER>q :wq<CR>

"only quit
nnoremap <C-q> :q<CR>

"copy select chars
vnoremap <LEADER>y "+y

"paste from system clipboard
noremap <LEADER>p "+p

"close highlight
nnoremap <silent> <ESC> :nohlsearch<CR>

"escape
inoremap <silent> jj <ESC>
vnoremap <silent> jj <ESC>

"place screen horizol
noremap sph <C-w>t<C-w>K
"place screen vertically
noremap spv <C-w>t<C-w>H

"rotate screens
noremap srr <C-w>b<C-w>K
"rotate screen reversely
noremap srv <C-w>b<C-w>H

"resize split windows
nnoremap <up> :res +5<CR>
nnoremap <down> :res -5<CR>
nnoremap <left> :vertical resize-5<CR>
nnoremap <right> :vertical resize+5<CR>

"move cursor between windows
noremap sk <C-w>k
noremap sj <C-w>j
noremap sh <C-w>h
noremap sl <C-w>l

"focus on center
inoremap <C-c> <ESC>zzi
