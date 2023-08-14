(let [vimopt (let [lib (require :lib)
                   undodir (lib.make_cache :undo)
                   backupdir (lib.make_cache :backup)]
               {:timeout true
                :shiftwidth 2
                :visualbell true
                :number true
                :softtabstop 2
                :ttyfast true
                :signcolumn "yes:1"
                :listchars {:tab "> " :trail "·"}
                :ignorecase true
                :foldlevel 99
                : undodir
                :tw 0
                :showcmd true
                :list true
                :directory backupdir
                :undofile true
                :showmode false
                :rnu false
                :shortmess :aTWF
                :fileencoding :utf-8
                :completeopt [:menuone :noselect :menu]
                :expandtab true
                :wrap true
                :ch 0
                :virtualedit :block
                :formatoptions :qj
                :inccommand :split
                :foldmethod :indent
                :mouse :a
                :hidden true
                :updatetime 100
                :foldenable true
                :smartcase true
                :splitright true
                :wildmenu true
                :splitbelow true
                :cindent true
                :viewoptions [:cursor :folds :slash :unix]
                :ttimeoutlen 200
                : backupdir
                :scrolloff 5
                :timeoutlen 800
                :autoindent true
                :tabstop 2
                :termguicolors true
                :encoding :utf-8})
      disable_builtins {:plugins [:gzip
                                  :zip
                                  :zipPlugin
                                  :tar
                                  :tarPlugin
                                  :getscript
                                  :getscriptPlugin
                                  :vimball
                                  :vimballPlugin
                                  :2html_plugin
                                  :matchit
                                  :matchparen
                                  :logiPat
                                  :rust_vim
                                  :rust_vim_plugin_cargo
                                  :rrhelper
                                  :netrw
                                  :netrwPlugin
                                  :netrwSettings
                                  :netrwFileHandlers]
                        :providers [:perl :node :ruby :python :python3]}]
  {: vimopt : disable_builtins})
