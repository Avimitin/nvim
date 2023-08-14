(let [gs (require :gitsigns)
      keymapper (require :core.keymapper)
      keymaps {:normal [["]g"
                         #(if vim.wo.diff "]g"
                              (vim.schedule #(gs.next_hunk)) :<nop>)
                         {:expr true :desc "Go to next hunk"}]
                        ["[g"
                         #(if vim.wo.diff "[g"
                              (vim.schedule #(gs.prev_hunk)) :<nop>)
                         {:expr true :desc "Go to previous hunk"}]
                        [:<leader>gs
                         (keymapper.to_cmd "Gitsigns stage_hunk")
                         {:desc "Stage git hunk"}]
                        [:<leader>gr
                         (keymapper.to_cmd "Gitsigns reset_hunk")
                         {:desc "Reset git hunk"}]
                        [:<leader>gS
                         gs.stage_buffer
                         {:desc "Stage whole buffer"}]
                        [:<leader>gu
                         gs.undo_stage_hunk
                         {:desc "Undo staged git hunk"}]
                        [:<leader>gR
                         gs.reset_buffer
                         {:desc "Reset the whole buffer"}]
                        [:<leader>gp gs.preview_hunk {:desc "Preview changes"}]
                        [:<leader>gB
                         #(gs.blame_line {:full true})
                         {:desc "Open git blame preview"}]
                        [:<leader>gb
                         gs.toggle_current_line_blame
                         {:desc "Toggle blame virtual text"}]
                        [:<leader>gd gs.diffthis {:desc "Open diff"}]
                        [:<leader>gt
                         gs.toggle_deleted
                         {:desc "Toggle deleted line"}]]}
      on_attach #(keymapper.setup_bufmap $1 keymaps)
      options {:current_line_blame false
               :signs {:add {:linehl :GitSignsAddLn
                             :text "▎"
                             :numhl :GitSignsAddNr
                             :hl :GitSignsAdd}
                       :change {:linehl :GitSignsChangeLn
                                :text "▎"
                                :numhl :GitSignsChangeNr
                                :hl :GitSignsChange}
                       :topdelete {:linehl :GitSignsDeleteLn
                                   :text "‾"
                                   :numhl :GitSignsDeleteNr
                                   :hl :GitSignsDelete}
                       :changedelete {:linehl :GitSignsChangeLn
                                      :text "▎"
                                      :numhl :GitSignsChangeNr
                                      :hl :GitSignsDelete}
                       :delete {:linehl :GitSignsDeleteLn
                                :text "_"
                                :numhl :GitSignsDeleteNr
                                :hl :GitSignsDelete}}
               :sign_priority 6
               : on_attach
               :linehl false
               :diff_opts {:internal true}
               :word_diff false
               :update_debounce 100
               :watch_gitdir {:interval 1000 :follow_files true}
               :numhl true
               :current_line_blame_opts {:delay 1000
                                         :virt_text true
                                         :virt_text_pos :eol}}]
  (gs.setup options))
