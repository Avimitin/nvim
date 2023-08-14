(lambda set_vim_opts [opts]
  (each [k v (pairs opts)]
    (tset vim.opt k v))
  nil)

(lambda disable_builtin [spec]
  (each [_ plug (ipairs spec.plugins)]
    (tset vim.g (.. :loaded_ plug) 1))
  (each [_ prov (ipairs spec.providers)]
    (tset vim.g (.. :loaded_ prov :_provider) 1)))

(lambda setup []
  ;; setup key mapping
  (let [keymapper (require :core.keymapper)]
    (keymapper.setup_keymap (require :keymap)))
  ;; setup vim option & disable built-in
  (let [vimopt (require :opt)]
    (set_vim_opts vimopt.vimopt)
    (disable_builtin vimopt.disable_builtins))
  ;; setup vim auto commands
  (let [au vim.api.nvim_create_autocmd]
    (each [_ spec (ipairs (require :autocmd))]
      au
      (?. spec :event)
      (?. spec :option)))
  nil)

{: setup}
