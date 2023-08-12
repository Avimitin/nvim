(lambda set_vim_opts [opts]
  (each [k v (pairs opts)]
    (tset vim.opt k v))
  nil)

(lambda make_cache [suffix]
  (let [cache (.. (vim.fn.stdpath :cache) "/" suffix)
        resp (vim.fn.mkdir cache :p)]
    (if (= resp 1)
        cache
        nil)))

(lambda disable_builtin [spec]
  (each [_ plug (ipairs spec.plugins)]
    (tset vim.g (.. :loaded_ plug) 1))
  (each [_ prov (ipairs spec.providers)]
    (tset vim.g (.. :loaded_ prov :_provider) 1)))

{: set_vim_opts : make_cache : disable_builtin}
