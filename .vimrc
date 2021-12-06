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
set listchars=tab:\|\ ,trail:·
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
set background=dark    " Setting dark mode
set t_Co=256
set termguicolors

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" setup palette dictionary
let s:ds = {}

" fill it with absolute colors
let s:ds.dark0       = ['#1A1B26', 235]     " 40-40-40 Background
let s:ds.dark1       = ['#3c3836', 237]     " 60-56-54
let s:ds.dark2       = ['#242a32', 239]     " 80-73-69
let s:ds.dark3       = ['#665c54', 241]     " 102-92-84
let s:ds.dark4       = ['#7c6f64', 243]     " 124-111-100
let s:ds.dark4_256   = ['#7c6f64', 243]     " 124-111-100

let s:ds.gray_245    = ['#928374', 245]     " 146-131-116
let s:ds.gray_244    = ['#928374', 244]     " 146-131-116

let s:ds.light0      = ['#d2d2d2', 229]     " 253-244-193
let s:ds.light1      = ['#ebdbb2', 223]     " 235-219-178
let s:ds.light2      = ['#d5c4a1', 250]     " 213-196-161
let s:ds.light3      = ['#bdae93', 248]     " 189-174-147
let s:ds.light4      = ['#a89984', 246]     " 168-153-132
let s:ds.light4_256  = ['#a89984', 246]     " 168-153-132

let s:ds.bright_red     = ['#fb4934', 167]     " 251-73-52
let s:ds.bright_green   = ['#98C379', 142]     " 184-187-38
let s:ds.bright_yellow  = ['#fabd2f', 214]     " 250-189-47
let s:ds.bright_blue    = ['#83a598', 109]     " 131-165-152
let s:ds.bright_purple  = ['#C678DD', 175]     " 211-134-155
let s:ds.bright_aqua    = ['#8ec07c', 108]     " 142-192-124
let s:ds.bright_orange  = ['#fe8019', 208]     " 254-128-25

let s:bold = 'bold,'
let s:italic = 'italic,'
let s:underline = 'underline,'
let s:undercurl = 'undercurl,'
let s:inverse = 'inverse,'

let s:vim_bg = ['bg', 'bg']
let s:vim_fg = ['fg', 'fg']
let s:none = ['NONE', 'NONE']

" determine relative colors
let s:bg0  = s:ds.dark0
let s:bg1  = s:ds.dark1
let s:bg2  = s:ds.dark2
let s:bg3  = s:ds.dark3
let s:bg4  = s:ds.dark4

let s:gray = s:ds.gray_245

let s:fg0 = s:ds.light0
let s:fg1 = s:ds.light1
let s:fg2 = s:ds.light2
let s:fg3 = s:ds.light3
let s:fg4 = s:ds.light4

let s:fg4_256 = s:ds.light4_256

let s:red    = s:ds.bright_red
let s:green  = s:ds.bright_green
let s:yellow = s:ds.bright_yellow
let s:blue   = s:ds.bright_blue
let s:purple = s:ds.bright_purple
let s:aqua   = s:ds.bright_aqua
let s:orange = s:ds.bright_orange

" save current relative colors back to palette dictionary
let s:ds.bg0 = s:bg0
let s:ds.bg1 = s:bg1
let s:ds.bg2 = s:bg2
let s:ds.bg3 = s:bg3
let s:ds.bg4 = s:bg4

let s:ds.gray = s:gray

let s:ds.fg0 = s:fg0
let s:ds.fg1 = s:fg1
let s:ds.fg2 = s:fg2
let s:ds.fg3 = s:fg3
let s:ds.fg4 = s:fg4

let s:ds.fg4_256 = s:fg4_256

let s:ds.red    = s:red
let s:ds.green  = s:green
let s:ds.yellow = s:yellow
let s:ds.blue   = s:blue
let s:ds.purple = s:purple
let s:ds.aqua   = s:aqua
let s:ds.orange = s:orange

" Overload Setting: {{{
let s:hls_cursor = s:orange
let s:number_column = s:bg4
let s:sign_column = s:bg2
let s:color_column = s:bg1
let s:vert_split = s:bg2
let s:invert_signs = ''
let s:invert_selection = s:inverse
let s:invert_tabline = ''
let s:italicize_comments = s:italic
let s:italicize_strings = ''

" Highlighting Function:
function! s:HL(group, fg, ...)
  " Arguments: group, guifg, guibg, gui, guisp

  " foreground
  let fg = a:fg

  " background
  if a:0 >= 1
    let bg = a:1
  else
    let bg = s:none
  endif

  " emphasis
  if a:0 >= 2 && strlen(a:2)
    let emstr = a:2
  else
    let emstr = 'NONE,'
  endif

  " special fallback
  if a:0 >= 3
    if g:deus_guisp_fallback != 'NONE'
      let fg = a:3
    endif

    " bg fallback mode should invert higlighting
    if g:deus_guisp_fallback == 'bg'
      let emstr .= 'inverse,'
    endif
  endif

  let histring = [ 'hi', a:group,
        \ 'guifg=' . fg[0], 'ctermfg=' . fg[1],
        \ 'guibg=' . bg[0], 'ctermbg=' . bg[1],
        \ 'gui=' . emstr[:-2], 'cterm=' . emstr[:-2]
        \ ]

  " special
  if a:0 >= 3
    call add(histring, 'guisp=' . a:3[0])
  endif

  execute join(histring, ' ')
endfunction

" memoize common hi groups
call s:HL('deusFg0', s:fg0)
call s:HL('deusFg1', s:fg1)
call s:HL('deusFg2', s:fg2)
call s:HL('deusFg3', s:fg3)
call s:HL('deusFg4', s:fg4)
call s:HL('deusGray', s:gray)
call s:HL('deusBg0', s:bg0)
call s:HL('deusBg1', s:bg1)
call s:HL('deusBg2', s:bg2)
call s:HL('deusBg3', s:bg3)
call s:HL('deusBg4', s:bg4)

call s:HL('deusRed', s:red)
call s:HL('deusRedBold', s:red, s:none, s:bold)
call s:HL('deusGreen', s:green)
call s:HL('deusGreenBold', s:green, s:none, s:bold)
call s:HL('deusYellow', s:yellow)
call s:HL('deusYellowBold', s:yellow, s:none, s:bold)
call s:HL('deusBlue', s:blue)
call s:HL('deusBlueBold', s:blue, s:none, s:bold)
call s:HL('deusPurple', s:purple)
call s:HL('deusPurpleBold', s:purple, s:none, s:bold)
call s:HL('deusAqua', s:aqua)
call s:HL('deusAquaBold', s:aqua, s:none, s:bold)
call s:HL('deusOrange', s:orange)
call s:HL('deusOrangeBold', s:orange, s:none, s:bold)

call s:HL('deusRedSign', s:red, s:sign_column, s:invert_signs)
call s:HL('deusGreenSign', s:green, s:sign_column, s:invert_signs)
call s:HL('deusYellowSign', s:yellow, s:sign_column, s:invert_signs)
call s:HL('deusBlueSign', s:blue, s:sign_column, s:invert_signs)
call s:HL('deusPurpleSign', s:purple, s:sign_column, s:invert_signs)
call s:HL('deusAquaSign', s:aqua, s:sign_column, s:invert_signs)

" General UI:
call s:HL('Normal', s:fg1, s:bg0)

if version >= 700
  call s:HL('CursorLine',   s:none, s:bg2)
  hi! link CursorColumn CursorLine

  call s:HL('TabLineFill', s:bg4, s:vim_bg, s:invert_tabline)
  call s:HL('TabLineSel', s:vim_bg, s:bg4, s:bold . s:invert_tabline)

  hi! link TabLine TabLineFill

  call s:HL('MatchParen', s:none, s:bg3, s:bold)
endif

if version >= 703
  call s:HL('ColorColumn',  s:none, s:color_column)
  call s:HL('Conceal', s:blue, s:none)
  call s:HL('CursorLineNr', s:fg1, s:bg2)
endif

hi! link NonText deusGreen
hi! link Ignore deusPurple
hi! link SpecialKey deusOrange

call s:HL('Visual',    s:none,  s:bg3, s:invert_selection)
hi! link VisualNOS Visual

call s:HL('Search',    s:bg0, s:yellow)
call s:HL('IncSearch', s:bg0, s:hls_cursor)
call s:HL('Underlined', s:blue, s:none, s:underline)
call s:HL('StatusLine',   s:bg2, s:fg1, s:bold . s:inverse)
call s:HL('StatusLineNC', s:bg2, s:fg1, s:bold . s:inverse)
call s:HL('VertSplit', s:fg4, s:vert_split)
call s:HL('WildMenu', s:blue, s:bg2, s:bold)
hi! link Directory deusGreenBold
hi! link Title deusGreenBold

call s:HL('ErrorMsg',   s:red, s:bg1, s:bold)
hi! link MoreMsg deusYellowBold
hi! link ModeMsg deusYellowBold
hi! link Question deusOrangeBold
hi! link WarningMsg deusRedBold

call s:HL('LineNr', s:number_column)
call s:HL('SignColumn', s:none, s:sign_column)
call s:HL('Folded', s:gray, s:bg2, s:italic)
call s:HL('FoldColumn', s:gray, s:bg2)

" Cursor:
hi! link vCursor Cursor
hi! link iCursor Cursor
hi! link lCursor Cursor

" Syntax Highlighting:
call s:HL('Comment', s:gray, s:none, s:italicize_comments)
call s:HL('Todo', s:vim_fg, s:vim_bg, s:bold . s:italic)
call s:HL('Error', s:red, s:vim_bg, s:bold . s:inverse)

" Completion Menu:
if version >= 700
  " Popup menu: normal item
  call s:HL('Pmenu', s:fg1, s:bg2)
  " Popup menu: selected item
  call s:HL('PmenuSel', s:bg2, s:blue, s:bold)
  " Popup menu: scrollbar
  call s:HL('PmenuSbar', s:none, s:bg2)
  " Popup menu: scrollbar thumb
  call s:HL('PmenuThumb', s:none, s:bg4)
endif

hi StatusLine ctermbg=10 ctermfg=10 cterm=bold guibg=NONE guifg=NONE gui=NONE
hi StatusLineNC ctermbg=10 ctermfg=10 cterm=NONE guibg=NONE guifg=NONE gui=NONE

" Diff:
hi! link diffAdded deusGreen
hi! link diffRemoved deusRed
hi! link diffChanged deusAqua
hi! link diffFile deusOrange
hi! link diffNewFile deusYellow
hi! link diffLine deusBlue
call s:HL('DiffDelete', s:red, s:bg0, s:inverse)
call s:HL('DiffAdd',    s:green, s:bg0, s:inverse)
call s:HL('DiffChange', s:aqua, s:bg0, s:inverse)
call s:HL('DiffText',   s:yellow, s:bg0, s:inverse)

" Vim:
call s:HL('vimCommentTitle', s:fg4_256, s:none, s:bold . s:italicize_comments)
hi! link vimNotation deusOrange
hi! link vimMapModKey deusOrange
hi! link vimFuncSID deusFg3
hi! link vimSetSep deusFg3
hi! link vimSep deusFg3
hi! link vimContinue deusFg3

" C:
hi! link Type deusYellow
hi! link StorageClass deusOrange
hi! link Structure deusAqua
hi! link Typedef deusYellow
hi! link Constant deusPurple
hi! link Character deusPurple
call s:HL('String', s:green, s:none, s:italicize_strings)
hi! link Boolean deusPurple
hi! link Number deusPurple
hi! link Float deusPurple
hi! link Statement deusRed
hi! link Conditional deusRed
hi! link Repeat deusRed
hi! link Label deusRed
hi! link Exception deusRed
hi! link Operator Normal
hi! link Keyword deusRed
hi! link Identifier deusBlue
hi! link Function deusGreenBold
hi! link PreProc deusAqua
hi! link Include deusAqua
hi! link Define deusAqua
hi! link PreCondit deusAqua
hi! link cOperator deusPurple
hi! link cStructure deusOrange
hi! link NamespaceTag deusPurpleBold
hi! link NamespaceAliasTag deusPurpleBold
hi! link ClassTag duesOrange
hi! link StructureTag deusOrange
hi! link EnumTag deusAqua
hi! link EnumValueTag deusAqua
hi! link UnionTag deusPurple
hi! link FieldTag deusPurple
hi! link LocalVariableTag deusOrange
hi! link FunctionTag deusGreen
hi! link MethodTag deusGreen
hi! link FunctionParameterTag deusPurpleBold
hi! link TemplateTypeParameterTag deusPurple
hi! link TemplateNonTypeParameterTag deusRedBold
hi! link TemplateTemplateParameterTag deusRed
hi! link MacroDefinitionTag deusBlue
hi! link MacroInstantiationTag deusBlue
hi! link TypedefTag deusPurple
hi! link UsingDirectiveTag deusOrange
hi! link UsingDeclarationTag deusOrange
