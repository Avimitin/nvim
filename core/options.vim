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
set signcolumn=yes:1

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

" auto insert when terminal open
autocmd TermOpen term://* startinsert
