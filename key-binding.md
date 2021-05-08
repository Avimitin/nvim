## a

## b

## c

## d
```vimscript
noremap D dd
```

## e

## f

## g
```vimscript
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)
autocmd FileType go nnoremap <silent> gt :GoTestFunc<CR>
autocmd FileType go nnoremap <silent> gr :GoRun<CR>
```

## h

## i
```vimscript
noremap i k
noremap I 5k
```

## j
```vimscript
noremap j h
noremap J 0
inoremap <silent> jj <ESC>
```

## k
```vimscript
noremap k j
noremap K 5j
```

## l
```vimscript
noremap L $
```

## m

## n

## o
```vimscript
noremap o w
noremap O 5w
```

## p

## q
```vimscript
nnoremap Q :q<CR>
```

## r
```vimscript
nnoremap r R
nnoremap <silent> R :RnvimrToggle<CR><C-\><C-n>:RnvimrResize 0<CR>
```

## s
```vimscript
"place screen up and down
noremap sw <C-w>t<C-w>K
"place screen side by side
noremap sb <C-w>t<C-w>H

"rotate screens
noremap srk <C-w>b<C-w>K
noremap srh <C-w>b<C-w>H

noremap su <C-w>k
noremap sj <C-w>j
noremap sh <C-w>h
noremap sk <C-w>l
```

## t
```vimscript
"tab
noremap ta :tabe<CR>
noremap tj :-tabnext<CR>
noremap tl :+tabnext<CR>
noremap tmj :-tabmove<CR>
noremap tml :+tabmove<CR>
```

## u
```vimscript
noremap u b
noremap U 5b
```

## v

## w

## x
```vimscript
noremap X Vx
```

## y

## z

## '
```vimscript
noremap ' i
noremap " I
```

## ctrl
```vimscript
inoremap <C-a> <ESC>A
map <C-b> <nop>
let g:AutoPairsShortcutBackInsert = '<C-b>'
nnoremap <c-c> :CocCommand<CR>
inoremap <C-c> <ESC>zzi
vmap <C-e> <Plug>(coc-snippets-select)
imap <C-e> <Plug>(coc-snippets-expand-jump)
noremap <C-j> 5<C-e>
imap <C-l> <Plug>(coc-snippets-expand)
tnoremap <C-n> <C-\><C-n>
tnoremap <C-q> <C-\><C-n>:q<CR>
noremap <C-u> 5<C-y>
noremap <C-A-S-q> :q!<CR>
noremap <C-z> u
nnoremap <C-\> :tabe<CR>:term<CR>
```

## leader
```vimscript
nnoremap <silent><nowait> <LEADER>d :CocList diagnostics<cr>

nnoremap <LEADER>e :tabe<CR>:edit 

nnoremap <LEADER>f :Leaderf file<CR>

nnoremap <LEADER>gu :GitGutterUndoHunk<CR>
nnoremap <LEADER>gs :GitGutterStageHunk<CR>
nnoremap <LEADER>gp :GitGutterPreviewHunk<CR>
nnoremap <LEADER>g= :GitGutterNextHunk<CR>
nnoremap <LEADER>g- :GitGutterPrevHunk<CR>

nnoremap <LEADER>h :call Show_documentation()<CR>

nnoremap <LEADER>n :tabe<CR>:edit ~/.config/nvim/init.vim<CR>

noremap <LEADER>p "+p

noremap <LEADER>q :wq<CR>

noremap <LEADER>rb :so ~/.config/nvim/init.vim<CR>

noremap <silent> <LEADER>s :w<CR>

noremap <LEADER>v :Vista!!<CR>

vnoremap <LEADER>y "+y

nnoremap <silent> <LEADER><CR> :nohlsearch<CR>
```
