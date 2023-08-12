(local keymapper (require :lib.keymapper))

(lambda setup_with [option]
  ;; setup key mapping
  (keymapper.setup_keymap option.keymap)
  ;; setup vim option & disable built-in
  (let [lib (require :lib)]
    (lib.set_vim_opts (require :opt))
    (lib.disable_builtin {:plugins [:gzip
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
                          :providers [:perl :node :ruby :python :python3]})
    (values true))
  nil)

{: setup_with}
