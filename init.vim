""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  _   _                 _              _____             __ _       "
" | \ | |               (_)            / ____|           / _(_)      "
" |  \| | ___  _____   ___ _ __ ___   | |     ___  _ __ | |_ _  __ _ "
" | . ` |/ _ \/ _ \ \ / / | '_ ` _ \  | |    / _ \| '_ \|  _| |/ _` |"
" | |\  |  __/ (_) \ V /| | | | | | | | |___| (_) | | | | | | | (_| |"
" |_| \_|\___|\___/ \_/ |_|_| |_| |_|  \_____\___/|_| |_|_| |_|\__, |"
"                                                               __/ |"
"                                                              |___/ "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Author: @avimitin

" INTRODUCTION
" ============
" This neovim configuration is inspired by jdhao/nvim-config
" and theniceboy/nvim.
" 
" Please read thought this configuration file to familiarise
" yourself with it before you use it.
"
" Run :checkhealth to make sure you installed all the dependence.
"
" All the configuration file store in the core/ directory. With 
" this file stucture:
"   core/
"     - autocmd.vim (store markdown macro)
"     - mapping.vim (store basic mapping)
"     - options.vim (store neovim settings)
"     - plugins.vim (store all the plugins and their settings)
"
" LICENSE
" =======
" License: MIT License
"
" Copyright (c) 2018-2021 avimitin
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to
" deal in the Software without restriction, including without limitation the
" rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
" sell copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
" FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
" IN THE SOFTWARE.

let g:file_list = [
	\ 'mapping.vim',
	\ 'options.vim',
	\ 'plugins.vim']

for s:fname in g:file_list
	execute printf('source ~/.config/nvim/core/%s', s:fname)
endfor

lua require('plugins')
