" change user home path in windows before you use it
if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs 
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" plugin

call plug#begin()
"""TrueZen.nvim: zen mode in neovim"""
Plug 'Pocco81/TrueZen.nvim'

"""asyncrun.vim: run command in background"""
Plug 'skywind3000/asyncrun.vim'

"""vim-commentary: for quickly commenting"""
Plug 'tpope/vim-commentary'

"""vim-smoothie: smooth scrolling"""
Plug 'psliwka/vim-smoothie'

"""barbar.nvim: bufferline bar"""
Plug 'kyazdani42/nvim-web-devicons'
Plug 'romgrk/barbar.nvim'

"""fancy start page"""
Plug 'mhinz/vim-startify'

"markdown preview
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug'] }

"mulit cursor
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

"open file when forget sudo
Plug 'lambdalisue/suda.vim'

"tabline
"Plug 'mg979/vim-xtabline'

"treesitter: support more colorful highlighting
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

"vim-bolt: vim syntax highlighting
Plug 'bpietravalle/vim-bolt'

"neovim color theme
Plug 'Avimitin/neovim-deus'
Plug 'morhetz/gruvbox'

"status bar
Plug 'glepnir/galaxyline.nvim', { 'branch': 'main' }
Plug 'Avimitin/nerd-galaxyline'

"progress bar
Plug 'ojroques/vim-scrollstatus'

"convert RGB... color value to actual color
Plug 'RRethy/vim-hexokinase', { 'do': 'make hexokinase' }

"highlight all the word below the cursor
Plug 'RRethy/vim-illuminate'

"file navigation
Plug 'mcchrish/nnn.vim'
Plug 'airblade/vim-rooter'
Plug 'pechorin/any-jump.vim'
Plug 'Yggdroot/LeaderF', { 'do': ':LeaderfInstallCExtension'  }

"list function/module/struct tag
Plug 'liuchengxu/vista.vim'

"auto complete
Plug 'neoclide/coc.nvim', {'branch': 'release'}

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

"modify text after object
Plug 'junegunn/vim-after-object'

"markdown toc
Plug 'mzlogin/vim-markdown-toc'

"clang-format
Plug 'rhysd/vim-clang-format'

"rust
Plug 'rust-lang/rust.vim'

"easy motion
Plug 'easymotion/vim-easymotion'

" Plugin for cmake
Plug 'cdelledonne/vim-cmake'

" open a big terminal
Plug 'numtostr/FTerm.nvim'

call plug#end()
set re=0

"set color theme
set termguicolors "enable true color support

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

set background=dark    " Setting dark mode
colorscheme deus
let g:deus_termcolors=256
hi NonText ctermfg=gray guifg=grey10

" plugin setting

""""""""""""""""""barbar.nvim""""""""""""""""""
" Move to previous/next
nnoremap <silent>    ,       :BufferPrevious<CR>
nnoremap <silent>    .       :BufferNext<CR>
" Re-order to previous/next
nnoremap <silent>    <A-<>   :BufferMovePrevious<CR>
nnoremap <silent>    <A->>   :BufferMoveNext<CR>
" Close buffer and quit
nnoremap <A-q>               :BufferClose<CR>
nnoremap <silent>    <A-S-q> :BufferCloseAllButCurrent<CR>
" Magic buffer-picking mode
nnoremap <silent>    <A-S-p>   :BufferLinePick<CR>
let bufferline = get(g:, 'bufferline', {})
let bufferline.icon_close_tab_modified = ''
let bufferline.auto_hide = v:true
let bufferline.tabpages = v:true
let bufferline.closable = v:true
let bufferline.clickable = v:true

"""""""""""""""""""""""""""vim-gitgutter"""""""""""""""""""""""""""
let g:gitgutter_sign_allow_clobber = 0
let g:gitgutter_map_keys = 0
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_preview_win_floating = 1
let g:gitgutter_sign_added = '▎'
let g:gitgutter_sign_modified = '░'
let g:gitgutter_sign_removed = '▏'
let g:gitgutter_sign_removed_first_line = '▔'
let g:gitgutter_sign_modified_removed = '▒'

nnoremap giu :GitGutterUndoHunk<CR>
nnoremap gis :GitGutterStageHunk<CR>
nnoremap gip :GitGutterPreviewHunk<CR>
nnoremap gi= :GitGutterNextHunk<CR>
nnoremap gi- :GitGutterPrevHunk<CR>

""""""""""""""""coc.nvim""""""""""""""""
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

"""press tab to downward waiting list"""
inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"""press shift tab to downward waiting list"""
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

"""press <enter> to select the complete"""
inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

"""pop up new windows and show document in it"""
nnoremap <LEADER>h :call Show_documentation()<CR>

function! Show_documentation()
	call CocActionAsync('highlight')
	if (index(['vim','help'], &filetype) >= 0)
		execute 'h '.expand('<cword>')
	else
		call CocAction('doHover')
	endif
endfunction

"""press ctrl+o at insert mode to trigger complete"""
inoremap <silent><expr> <c-o> coc#refresh()

"""press space+ -/= to navigate between diagnostic"""
nmap <silent> <LEADER>- <Plug>(coc-diagnostic-prev)
nmap <silent> <LEADER>= <Plug>(coc-diagnostic-next)

"""press ctrl+c at normal mode to open coc command list"""
nnoremap <c-c> :CocCommand<CR>

"""press <space>+y to yank select test into coc list"""
nnoremap <silent> <space>y :<C-u>CocList -A --normal yank<cr>

"""GoTo action"""
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)

"""rename word under cursor"""
nmap <leader>rn <Plug>(coc-rename)

"""press tt to open tree explorer"""
nmap tt :CocCommand explorer<CR>

"""press ts to translate word under cursor"""
nmap ts <Plug>(coc-translator-p)

"""coc-snippet"""
"""Use <C-e> for trigger snippet expand."""
imap <C-e> <Plug>(coc-snippets-expand)
"""Use <C-j> for select text for visual placeholder of snippet."""
vmap <C-j> <Plug>(coc-snippets-select)"
let g:coc_snippet_next = '<tab>'
let g:coc_snippet_prev = '<c-k>'
" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

autocmd BufRead,BufNewFile tsconfig.json set filetype=jsonc

" Remap <C-f> and <C-b> for scroll float windows/popups.
"
if has('nvim-0.4.0') || has('patch-8.2.0750')
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

""""""""""""""""vista.vim""""""""""""""""""
noremap <LEADER>va :Vista!!<CR>
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista_default_executive = 'coc'
let g:vista_fzf_preview = ['right:50%']
let g:vista#renderer#enable_icon = 1
let g:vista#renderer#icons = {
\   "function": "\uf794",
\   "variable": "\uf71b",
\  }

"""""""""""""""""""""vim-go""""""""""""""""""""""
autocmd BufWrite *.go GoImports
autocmd FileType go nmap <silent> got :GoTestFunc<CR>
autocmd FileType go nmap <silent> gor :GoRun<CR>
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

""""""""""""""""""LeaderF""""""""""""""""""
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

"""""""""""""""far.vim"""""""""""""""
nmap <C-f> :Farf --source=ag<cr>

"""""""""""""auto-pairs"""""""""""""
let g:AutoPairsFlyMode = 1
map <C-b> <nop>
let g:AutoPairsShortcutBackInsert = '<C-b>'

"""""""""""""""""lazygit.nvim"""""""""""""""""
nnoremap <silent> <c-g> :LazyGit<CR>
let g:lazygit_floating_window_winblend = 0 " transparency of floating window
let g:lazygit_floating_window_scaling_factor = 0.9 " scaling factor for floating window
let g:lazygit_floating_window_corner_chars = ['╭', '╮', '╰', '╯'] " customize lazygit popup window corner characters
let g:lazygit_use_neovim_remote = 1 " fallback to 0 if neovim-remote is not installed

"""""""""""""""""""""any-jump"""""""""""""""""""""
let g:any_jump_window_width_ratio  = 0.8
let g:any_jump_window_height_ratio = 0.9
let g:any_jump_disable_default_keybindings = 1
" Normal mode: open previous opened file (after jump)
nmap <leader>aj :AnyJump<CR>
nmap <leader>ab :AnyJumpBack<CR>

"""""""""""""""""""""vim-after-object"""""""""""""""""""""
autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')

"""""""""""""vim-visual-multi"""""""""""""""
let g:VM_maps = {}
let g:VM_maps["Undo"] = '<C-z>'

""""""""""""""""markdown-preview-nvim""""""""""""""""
let g:mkdp_browser = 'firefox'
let g:mkdp_open_to_the_world = 1
let g:mkdp_port = '57843'

""""""""""""""""""vim-clang-format"""""""""""""""""" 
let g:clang_format#detect_style_file=1
autocmd BufWritePre *.c,*.h,*.cpp,*.hpp,*.cc ClangFormat

""""""""""""""rust.vim""""""""""""""
autocmd BufWrite *.rs RustFmt

"""""""""""""""""nnn.vim""""""""""""""""" 
" nnn windows size
let g:nnn#layout = { 'left': '~20%' }
" Floating window setting
let g:nnn#layout = { 'window': { 'width': 0.8, 'height': 0.6, 'highlight': 'Debug' } }
" nnn keymap
let g:nnn#set_default_mappings = 0
nnoremap <silent> <leader>n :NnnPicker %:p:h<CR>
nnoremap <leader>o :tabe<CR>:NnnPicker %:p:h<CR>
let g:nnn#action = {
      \ '<c-t>': 'tab split',
      \ '<c-x>': 'split',
      \ '<c-v>': 'vsplit' }
let g:nnn#command = 'nnn -d -e -H'

""""""""""""""vim-easymotion""""""""""""""
let g:EasyMotion_do_mapping = 0
nmap u <Plug>(easymotion-overwin-f2)
nmap <Leader>j <Plug>(easymotion-j)
nmap <Leader>k <Plug>(easymotion-k)

"""""""""""""""""FTerm.nvim""""""""""
nmap <C-\> <CMD>lua require("FTerm").open()<CR>
