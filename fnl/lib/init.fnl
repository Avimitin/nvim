(lambda make_cache [suffix]
  (let [cache (.. (vim.fn.stdpath :cache) "/" suffix)
        resp (vim.fn.mkdir cache :p)]
    (if (= resp 1)
        cache
        nil)))

{: make_cache}
