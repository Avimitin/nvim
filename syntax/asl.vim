" Vim syntax file
" Language: asl
" Maintainer:   Alastair Reid <alastair.reid@intel.com>
" Last Change:  Thu Sep 8 10:17 GMT 2022
" Filenames:    *.asl

" Comments:
" Make sure to create a file name .vim/ftdetect/asl.vim containing this line:
" au BufRead,BufNewFile *.asl set filetype=asl

if exists("b:current_syntax")
    finish
endif

syn keyword aslType        array bit bits boolean enumeration integer real record signal string
syn keyword aslType        __mask __RAM
syn keyword aslExpr        AND DIV DIVRM EOR IN MOD NOT OR QUOT REM UNKNOWN XOR as typeof
syn keyword aslStmt        assert begin case catch do downto else elsif end for if of otherwise repeat return then throw to try until when where while break
syn keyword aslDecl        config constant func getter let setter type var
syn keyword aslDecl        __builtin __operator1 __operator2
syn keyword aslConstant    FALSE TRUE HIGH LOW

syn match   aslIdentifier  "\<[A-Za-z_][A-Za-z0-9_]*\>"
syn match   aslNumber      "\<\d\+\>"
syn match   aslNumber      "\<\d\+\.\d+\>"
syn match   aslNumber      "\<0[xX][0-9A-F_]\+\>"
syn match   aslNumber      "\<'[01_]\+'\>"
syn match   aslDelimiter   "\[\|\]\|(\|)\|{\|}"
syn match   aslOperator    "!=\|==\|+\|-\|*\|/\|&&\|||\|^\|!\|==>\|<==>\|<\|<=\|>\|>="
syn match   aslAssign      "="
syn match   aslReturns     "=>"
syn match   aslCoCo        "::"

syn region  aslMultilineComment start="/\*" end="\*/"
syn region  aslTrailingComment  start="//" end="$"
syn region  aslString           start=/"/ skip=/\\./ end=/"/

hi def link aslMultilineComment comment
hi def link aslTrailingComment comment
hi def link aslIdentifier      Normal
hi def link aslDecl            Keyword
hi def link aslType            Type
hi def link aslReturns         Keyword
hi def link aslCoCo            Keyword
hi def link aslStmt            Conditional
hi def link aslAssign          Conditional
hi def link aslExpr            StorageClass
hi def link aslConstant        Constant
hi def link aslOperator        Special
hi def link aslDelimiter       Normal
hi def link aslNumber          Number
hi def link aslString          String

let b:current_syntax = "asl"
