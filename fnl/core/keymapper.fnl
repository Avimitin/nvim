(lambda extend_options [orig_opt]
  (let [default {:noremap true :silent true :desc :Undocumented}]
    (vim.tbl_deep_extend :force default (or orig_opt {}))))

(lambda map [mode keyseq]
  (each [_ keyspec (ipairs keyseq)]
    (case keyspec
      [lhs rhs opt] (vim.keymap.set mode lhs rhs (extend_options opt)))))

(lambda setup_keymap [keymap_set]
  (map :n keymap_set.normal)
  (map :x keymap_set.visual)
  (map :i keymap_set.insert)
  (map :t keymap_set.terminal)
  true)

(lambda batch_set_buf_id [id keymap_set]
  "insert {buffer = id} into all keymap_set"
  (collect [mode opt (pairs keymap_set)]
    (do
      (tset opt :buffer id)
      (values mode opt))))

(lambda bufmap [id keymap_set]
  (let [_keymap_set (batch_set_buf_id id keymap_set)]
    (setup_keymap _keymap_set)))

(lambda to_cmd [literal]
  (.. :<CMD> literal :<CR>))

{: setup_keymap :setup_bufmap bufmap : to_cmd}
