(let [augroup vim.api.nvim_create_augroup]
  [;; Relative number trigger
   (let [rnu_id (augroup :RnuGroup {:clear true})]
     (values {:event [:InsertEnter]
              :option {:group rnu_id
                       :pattern ["*"]
                       :callback #(tset vim.opt :rnu false)}}
             {:event [:InsertLeave]
              :option {:group rnu_id
                       :pattern ["*"]
                       :callback #(when (not= (vim.fn.mode) :i)
                                    (tset vim.opt :rnu true))}}))
   ;; Copy and highlight yanked text to system clipboard
   (let [augid (augroup :SmartYank {:clear true})
         desc "Copy and highlight yanked text to system clipboard"
         has_clipboard (= (vim.fn.has :clipboard) 1)
         copy_to_system #(pcall vim.fn.setreg "+" $1)
         get_from_current #(pcall vim.fn.getreg :0)]
     {:event [:TextYankPost]
      :option {:group augid
               : desc
               :callback #(when has_clipboard
                            (vim.highlight.onyank {:higroup :DiagnosticWarn
                                                   :timeout 200})
                            (when (= vim.v.operator :y)
                              (let [(ok? data) (get_from_current)]
                                (when (and ok? (> (length data) 1))
                                  (copy_to_system data)
                                  nil))))}})
   ;; Go to project root
   (let [rooter (require :lib.rooter)
         patterns [:.git :.hg :.svn :Cargo.toml :packages.json]]
     {:event [:VimEnter]
      :option {:pattern ["*"] :callback #(rooter.to_root patterns)}})
   ;; trigger fcitx5
   (let [callback #(let [status (?. $1 :code)
                         stdout (?. $1 :stdout)
                         running? (not= :2 (stdout:gsub "%s+" ""))]
                     (if (not= 0 status)
                         (vim.notify "Fail to execute fcitx5")
                         (when running?
                           (vim.fn.system "fcitx5-remote -c")
                           (tset vim.g :Fcitx5Closed 1))))]
     {:event [:InsertLeave] :option {: callback}})
   {:event [:InsertEnter]
    :option {:callback #(when (= 1 (?. vim.g :Fcitx5Closed))
                          (vim.fn.system "fcitx5-remote -o")
                          (tset vim.g :Fcitx5Closed 0))}}
   ;; Auto close cursor line
   (let [augid (augroup :CursorLine {:clear true})
         setopt #(tset vim.wo :cursorline $1)]
     {:event [:WinEnter :VimEnter :InsertLeave]
      :option {:group augid :callback #(setopt true)}}
     {:event [:WinLeave :InsertEnter]
      :option {:group augid :callback #(setopt false)}})
   ;; end of array
   ;; fnlfmt: skip
   ])
