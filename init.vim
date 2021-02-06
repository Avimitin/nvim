" Author: @avimitin

if empty(glob('~/.config/nvim/autoload/plug.vim'))
	!curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
			\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

let &t_ut='' "adjust terminal color

" vim setting
set exrc                                                  " exec command in init.vim
set secure                                                " safely do command above
set autochdir                                             " auto change directory
set number
set relativenumber
set cursorline                                            " set line below cursor
set noexpandtab                                           " use only '\t' as tab
set tabstop=2                                             " show how many space for a '\t'
set shiftwidth=2                                          " use how many space for >> or <<
set softtabstop=2                                         " use how many space when pressing tab
set autoindent
set list                                                  " show hiding char
set listchars=tab:\|\ ,trail:·                            " define tab and space show
set scrolloff=4                                           " least amount line below and above the cursor
set ttimeoutlen=0                                         " set never wait for key
set notimeout
set viewoptions=cursor,folds,slash,unix                   " remember where to recover cursor
set wrap                                                  " auto line feed
set tw=0                                                  " text width for automatically wrapping
set indentexpr=
set splitright
set splitbelow
set noshowmode                                            " not showing current mode
set showcmd                                               " show cmd inputing like key combine
set wildmenu                                              " auto finish vim command
set ignorecase                                            " ignore case when searching
set smartcase                                             " ignore case only on searching
set shortmess+=c                                          " don't show useless msg
set inccommand=split                                      " show substitution automatically
set completeopt=longest,noinsert,menuone,noselect,preview " complete opject with a menue
set ttyfast                                               " make scrolling faster
set lazyredraw
set visualbell                                            " flash screen to notify error
set updatetime=100
set virtualedit=block
set colorcolumn=100
set lazyredraw

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

"reboot vim
noremap <LEADER>rb :so%<CR>

"save
noremap <silent> <LEADER>s :w<CR>

"save and quit
noremap <LEADER>q :wq<CR>

"normal quit
nnoremap Q :q<CR>

"copy select chars
vnoremap <LEADER>y "+y

"close highlight
nnoremap <silent> <LEADER><CR> :nohlsearch<CR>

"escape
inoremap <silent> <LEADER><CR> <ESC>

"fold para
noremap <silent> <LEADER>o za "folding

"jump to next '<++>' and edit it
noremap <LEADER><LEADER> <ESC>/<++><CR>:nohlsearch<CR>c4l

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

"move cursor between windows
noremap <LEADER><up> <C-w>k
noremap <LEADER><down> <C-w>j
noremap <LEADER><left> <C-w>h
noremap <LEADER><right> <C-w>l

"tab
noremap tu :tabe<CR>
noremap th :-tabnext<CR>
noremap tk :+tabnext<CR>
noremap tmh :-tabmove<CR>
noremap tmk :+tabmove<CR>

"focus on center
inoremap <C-c> <ESC>zzi

"terminal
nnoremap <C-\> :tabe<CR>:term<CR>a
tnoremap <C-q> <C-\><C-n>:q<CR>
tnoremap <C-n> <C-\><C-n>

"===plugin===

call plug#begin('~/.config/nvim/plugged')

"treesitter: support more colorful highlighting
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

"vim-bolt: vim syntax highlighting
Plug 'bpietravalle/vim-bolt'

"nvim-deus: neovim color theme
"Plug 'theniceboy/nvim-deus'
Plug 'rakr/vim-one'
"Plug 'rakr/vim-two-firewatch'

"status bar
Plug 'theniceboy/eleline.vim'

"progress bar
Plug 'ojroques/vim-scrollstatus'

"convert RGB... color value to actual color
Plug 'RRethy/vim-hexokinase', { 'do': 'make hexokinase' }

"highlight all the word below the cursor
Plug 'RRethy/vim-illuminate'

"file navigation
Plug 'kevinhwang91/rnvimr'
Plug 'airblade/vim-rooter'
Plug 'pechorin/any-jump.vim'
Plug 'Yggdroot/LeaderF', { 'do': ':LeaderfInstallCExtension'  }

"list function/module/struct tag
Plug 'liuchengxu/vista.vim'

"auto complete
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"snippets
Plug 'theniceboy/vim-snippets'

"provide syntax highlighting for gitignore file
Plug 'theniceboy/vim-gitignore', { 'for': ['gitignore', 'vim-plug'] }

"show git diff in sign column
Plug 'airblade/vim-gitgutter'

"Golang support
Plug 'fatih/vim-go' , { 'for': ['go', 'vim-plug'], 'tag': '*' }

"Select text object
Plug 'gcmt/wildfire.vim'

"surrounding select text with given text
Plug 'tpope/vim-surround'

"auto pair bracket
Plug 'jiangmiao/auto-pairs'

"amazing icon
Plug 'ryanoasis/vim-devicons'

"align
Plug 'junegunn/vim-easy-align'

"find and replace
Plug 'brooth/far.vim'

"lazygit
Plug 'kdheepak/lazygit.nvim'

call plug#end()
set re=0

"set color theme
color one
set termguicolors " enable true colors support
hi NonText ctermfg=gray guifg=grey10

"===plugin setting===
" eleline.vim
let g:airline_powerline_fonts = 0

" GitGutter
let g:gitgutter_sign_allow_clobber = 0
let g:gitgutter_map_keys = 0
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_preview_win_floating = 1
let g:gitgutter_sign_added = '▎'
let g:gitgutter_sign_modified = '░'
let g:gitgutter_sign_removed = '▏'
let g:gitgutter_sign_removed_first_line = '▔'
let g:gitgutter_sign_modified_removed = '▒'

"coc.nvim
let g:coc_global_extensions = [
	\ 'coc-diagnostic',
	\ 'coc-explorer',
	\ 'coc-gitignore',
	\ 'coc-html',
	\ 'coc-json',
	\ 'coc-lists',
	\ 'coc-prettier',
	\ 'coc-snippets',
	\ 'coc-syntax',
	\ 'coc-tasks',
	\ 'coc-translator',
	\ 'coc-vimlsp',
	\ 'coc-yaml',
	\ 'coc-yank']

inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! Show_documentation()
	call CocActionAsync('highlight')
	if (index(['vim','help'], &filetype) >= 0)
		execute 'h '.expand('<cword>')
	else
		call CocAction('doHover')
	endif
endfunction

inoremap <silent><expr> <c-o> coc#refresh()
nnoremap <LEADER>h :call Show_documentation()<CR>
nnoremap <silent><nowait> <LEADER>d :CocList diagnostics<cr>
nmap <silent> <LEADER>- <Plug>(coc-diagnostic-prev)
nmap <silent> <LEADER>= <Plug>(coc-diagnostic-next)
nnoremap <c-c> :CocCommand<CR>
nnoremap <silent> <space>y :<C-u>CocList -A --normal yank<cr>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)
nmap tt :CocCommand explorer<CR>
nmap ts <Plug>(coc-translator-p)
imap <C-l> <Plug>(coc-snippets-expand)
vmap <C-e> <Plug>(coc-snippets-select)
let g:coc_snippet_next = '<c-e>'
let g:coc_snippet_prev = '<c-n>'
imap <C-e> <Plug>(coc-snippets-expand-jump)
let g:snips_author = 'Avimitin'
autocmd BufRead,BufNewFile tsconfig.json set filetype=jsonc

function! s:cocActionsOpenFromSelected(type) abort
	execute 'CocCommand actions.open ' . a:type
endfunction
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>aw  <Plug>(coc-codeaction-selected)

"vista
noremap <LEADER>v :Vista!!<CR>
noremap <c-t> :silent! Vista finder coc<CR>
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista_default_executive = 'coc'
let g:vista_fzf_preview = ['right:50%']
let g:vista#renderer#enable_icon = 1
let g:vista#renderer#icons = {
\   "function": "\uf794",
\   "variable": "\uf71b",
\  }

" vim-go
autocmd FileType go nnoremap <silent> <LEADER>gi :GoImports<CR>
autocmd FileType go nnoremap <silent> <LEADER>gt :GoTestFunc<CR>
autocmd FileType go nnoremap <silent> <LEADER>gr :GoRun<CR>
let g:go_echo_go_info = 0
let g:go_doc_popup_window = 1
let g:go_def_mapping_enabled = 0
let g:go_template_autocreate = 0
let g:go_textobj_enabled = 0
let g:go_auto_type_info = 1
let g:go_def_mapping_enabled = 0
let g:go_highlight_array_whitespace_error = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_chan_whitespace_error = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_function_parameters = 1
let g:go_highlight_functions = 1
let g:go_highlight_generate_tags = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_space_tab_error = 1
let g:go_highlight_string_spellcheck = 1
let g:go_highlight_structs = 1
let g:go_highlight_trailing_whitespace_error = 1
let g:go_highlight_types = 1
let g:go_highlight_variable_assignments = 0
let g:go_highlight_variable_declarations = 0
let g:go_doc_keywordprg_enabled = 0

"markdown key map
autocmd Filetype markdown inoremap <buffer> ,f <Esc>/<++><CR>:nohlsearch<CR>"_c4l
autocmd Filetype markdown inoremap <buffer> ,w <Esc>/ <++><CR>:nohlsearch<CR>"_c5l<CR>
autocmd Filetype markdown inoremap <buffer> ,n ---<Enter><Enter>
autocmd Filetype markdown inoremap <buffer> ,b **** <++><Esc>F*hi
autocmd Filetype markdown inoremap <buffer> ,s ~~~~ <++><Esc>F~hi
autocmd Filetype markdown inoremap <buffer> ,i ** <++><Esc>F*i
autocmd Filetype markdown inoremap <buffer> ,d `` <++><Esc>F`i
autocmd Filetype markdown inoremap <buffer> ,c ```<Enter><++><Enter>```<Enter><Enter><++><Esc>4kA
autocmd Filetype markdown inoremap <buffer> ,m - [ ] 
autocmd Filetype markdown inoremap <buffer> ,p ![](<++>) <++><Esc>F[a
autocmd Filetype markdown inoremap <buffer> ,a [](<++>) <++><Esc>F[a
autocmd Filetype markdown inoremap <buffer> ,1 #<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <buffer> ,2 ##<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <buffer> ,3 ###<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <buffer> ,4 ####<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <buffer> ,l --------<Enter>

"LeaderF
nnoremap <LEADER>f :Leaderf file<CR>
let g:Lf_PreviewInPopup = 1
let g:Lf_PreviewCode = 1
let g:Lf_ShowHidden = 1
let g:Lf_ShowDevIcons = 1
let g:Lf_UseVersionControlTool = 0
let g:Lf_WindowPosition = 'popup'
let g:Lf_IgnoreCurrentBufferName = 1
let g:Lf_WildIgnore = {
        \ 'dir': ['.git', 'vendor', 'node_modules'],
        \ 'file': ['__vim_project_root']
        \}
let g:Lf_UseMemoryCache = 0
let g:Lf_UseCache = 0
let g:Lf_CommandMap = {
\ '<C-u>': ['<C-k>'],
\}


"git gutter
nnoremap <LEADER>gu :GitGutterUndoHunk<CR>
nnoremap <LEADER>gs :GitGutterStageHunk<CR>
nnoremap <LEADER>gp :GitGutterPreviewHunk<CR>
nnoremap <LEADER>g- :GitGutterNextHunk<CR>
nnoremap <LEADER>g= :GitGutterPrevHunk<CR>

"far.vim
nnoremap <silent> <C-f> :Farf<cr>

"auto-pairs
let g:AutoPairsFlyMode = 1
map <C-b> <nop>
let g:AutoPairsShortcutBackInsert = '<C-b>'

"lazygit
nnoremap <silent> <c-g> :LazyGit<CR>
let g:lazygit_floating_window_winblend = 0 " transparency of floating window
let g:lazygit_floating_window_scaling_factor = 0.9 " scaling factor for floating window
let g:lazygit_floating_window_corner_chars = ['╭', '╮', '╰', '╯'] " customize lazygit popup window corner characters
let g:lazygit_use_neovim_remote = 1 " fallback to 0 if neovim-remote is not installed

"anyjump
let g:any_jump_window_width_ratio  = 0.8
let g:any_jump_window_height_ratio = 0.9
