" change user home path in windows before you use it
if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs 
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" plugin

call plug#begin()

"markdown preview
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug'] }

"mulit cursor
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

"open file when forget sudo
Plug 'lambdalisue/suda.vim'

"tabline
Plug 'mg979/vim-xtabline'

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

"jenkinsfile
Plug 'martinda/Jenkinsfile-vim-syntax'

"modify text after object
Plug 'junegunn/vim-after-object'

call plug#end()
set re=0

"set color theme
color one
set termguicolors " enable true colors support
hi NonText ctermfg=gray guifg=grey10

" plugin setting

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
autocmd FileType go nnoremap <silent> gi :GoImports<CR>
autocmd FileType go nnoremap <silent> gt :GoTestFunc<CR>
autocmd FileType go nnoremap <silent> gr :GoRun<CR>
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
" Normal mode: open previous opened file (after jump)
nnoremap <leader>b :AnyJumpBack<CR>

"vim-after-project
autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')

"vim-visual-multi
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<C-k>'
let g:VM_maps['Find Subword Under'] = '<C-k>'
let g:VM_maps["Undo"] = '<C-z>'
let g:VM_custom_motions = {'k': 'u', 'l': 'k'}

" rnvimr
let g:rnvimr_ex_enable = 1
let g:rnvimr_pick_enable = 1
let g:rnvimr_draw_border = 1
highlight link RnvimrNormal CursorLine
nnoremap <silent> R :RnvimrToggle<CR><C-\><C-n>:RnvimrResize 0<CR>
let g:rnvimr_action = {
            \ '<C-t>': 'NvimEdit tabedit',
            \ '<C-x>': 'NvimEdit split',
            \ '<C-v>': 'NvimEdit vsplit',
            \ 'gw': 'JumpNvimCwd',
            \ 'yw': 'EmitRangerCwd'
            \ }
let g:rnvimr_layout = { 'relative': 'editor',
            \ 'width': &columns,
            \ 'height': &lines,
            \ 'col': 0,
            \ 'row': 0,
            \ 'style': 'minimal' }
let g:rnvimr_presets = [{'width': 1.0, 'height': 1.0}]
