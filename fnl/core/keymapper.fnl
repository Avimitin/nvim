(lambda extend_options [orig_opt]
  (let [default {:noremap true :silent true :desc :Undocumented}]
    (vim.tbl_deep_extend :force default (or orig_opt {}))))

(lambda map [mode keyset]
  (vim.keymap.set mode (unpack keyset.key) (extend_options keyset.opt)))

(lambda setup_keymap [keymap_set]
  (map :n keymap_set.normal)
  (map :x keymap_set.visual)
  (map :i keymap_set.insert)
  (map :t keymap_set.terminal)
  true)

(lambda bufmap [id keymap_set]
  (let [_keymap_set (collect [mode opt (pairs keymap_set)]
                      (do
                        (tset opt :buffer id)
                        (values mode opt)))]))

(lambda to_cmd [literal]
  (.. :<CMD> literal :<CR>))

{: setup_keymap}
