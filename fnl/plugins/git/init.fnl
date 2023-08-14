[{:repo :lewis6991/gitsigns.nvim
  :lazy true
  :init (fn []
          (let [au vim.api.nvim_create_autocmd
                init_gitsigns #(do
                                 (require :plugins.git.gitsigns)
                                 ((. (require :scrollbar.handlers.gitsigns)
                                     :setup))
                                 nil)
                callback #(if (= 0 $1)
                              (vim.schedule init_gitsigns))
                current_file (vim.fn.expand "%")]
            (au [:BufAdd :VimEnter]
                {:callback #(vim.system [:git
                                         :--ls-files
                                         :--error-unmatch
                                         current_file]
                                        nil callback)})))}]
