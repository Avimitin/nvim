set conceallevel=2

"
" Utils for open relative file path in neovim
" Credit: vim-markdown https://github.com/preservim/vim-markdown
"
"    Default keymap: ge
"
" TODO: Rewrite this in Lua

function! s:FindCornerOfSyntax(lnum, col, step)
    let l:col = a:col
    let l:syn = synIDattr(synID(a:lnum, l:col, 1), 'name')
    while synIDattr(synID(a:lnum, l:col, 1), 'name') ==# l:syn
        let l:col += a:step
    endwhile
    return l:col - a:step
endfunction

function! s:FindNextSyntax(lnum, col, name)
    let l:col = a:col
    let l:step = 1
    while synIDattr(synID(a:lnum, l:col, 1), 'name') !=# a:name
        let l:col += l:step
    endwhile
    return [a:lnum, l:col]
endfunction

function! s:FindCornersOfSyntax(lnum, col)
    return [<sid>FindLeftOfSyntax(a:lnum, a:col), <sid>FindRightOfSyntax(a:lnum, a:col)]
endfunction

function! s:FindRightOfSyntax(lnum, col)
    return <sid>FindCornerOfSyntax(a:lnum, a:col, 1)
endfunction

function! s:FindLeftOfSyntax(lnum, col)
    return <sid>FindCornerOfSyntax(a:lnum, a:col, -1)
endfunction

function! s:Markdown_GetUrlForPosition(lnum, col)
    let l:lnum = a:lnum
    let l:col = a:col
    let l:syn = synIDattr(synID(l:lnum, l:col, 1), 'name')

    if l:syn ==# 'mkdInlineURL' || l:syn ==# 'markdownUrl' || l:syn ==# 'mkdLinkDefTarget'
        " Do nothing.
    elseif l:syn ==# 'markdownLink'
        let [l:lnum, l:col] = <sid>FindNextSyntax(l:lnum, l:col, 'markdownUrl')
        let l:syn = 'markdownURL'
    elseif l:syn ==# 'markdownDelimiter'
        let l:line = getline(l:lnum)
        let l:char = l:line[col - 1]
        if l:char ==# '<'
            let l:col += 1
        elseif l:char ==# '>' || l:char ==# ')'
            let l:col -= 1
        elseif l:char ==# '[' || l:char ==# ']' || l:char ==# '('
            let [l:lnum, l:col] = <sid>FindNextSyntax(l:lnum, l:col, 'markdownUrl')
        else
            return ''
        endif
    else
        return ''
    endif

    let [l:left, l:right] = <sid>FindCornersOfSyntax(l:lnum, l:col)
    return getline(l:lnum)[l:left - 1 : l:right - 1]
endfunction

" We need a definition guard because we invoke 'edit' which will reload this
" script while this function is running. We must not replace it.
if !exists('*s:EditUrlUnderCursor')
    function s:EditUrlUnderCursor()
        let l:editmethod = 'edit'
        " determine how to open the linked file (split, tab, etc)
        let l:url = s:Markdown_GetUrlForPosition(line('.'), col('.'))
        if l:url !=# ''
            let l:anchor = ''
            let l:parts = split(l:url, '#', 1)
            if len(l:parts) == 2
                let [l:url, l:anchor] = parts
                let l:anchorexpr = get(g:, 'vim_markdown_anchorexpr', '')
                if l:anchorexpr !=# ''
                    let l:anchor = eval(substitute(
                        \ l:anchorexpr, 'v:anchor',
                        \ escape('"'.l:anchor.'"', '"'), ''))
                endif
            endif
            echom l:url
            if l:url !=# ''
                let l:ext = ''
                if get(g:, 'vim_markdown_no_extensions_file_link', 0)
                    " use another file extension if preferred
                    if exists('g:vim_markdown_custom_ext')
                        let l:ext = '.'.g:vim_markdown_custom_ext
                    else
                        let l:ext = '.md'
                    endif
                endif
                let l:url = fnameescape(fnamemodify(expand('%:h').'/'.l:url.l:ext, ':.'))
                execute l:editmethod l:url
            endif
            if l:anchor !=# ''
                silent! execute '/'.l:anchor
            endif
        else
            execute l:editmethod . ' <cfile>'
        endif
    endfunction
endif

nnoremap <silent> <buffer> ge :call <sid>EditUrlUnderCursor()<CR>
