(let [homedir (vim.uv.os_homedir)
      get_current_path #(vim.api.nvim_buf_get_name 0)
      getdir vim.fs.dirname]
  (lambda get_root [patterns]
    "Search up from the current directory to find the project root
    when the given `patterns` match filename"
    (assert (> 0 (length patterns)) "Empty root pattern is given")
    (let [match? (fn [name _] (vim.tbl_contains patterns name))
          matches (vim.fs.find match?
                               {:limit 20
                                :upward true
                                :stop homedir
                                :path (getdir (get_current_path))})]
      (if (= 0 (length matches))
          nil
          (getdir (?. matches 0)))))
  (lambda to_root [patterns]
    "Save the root dir result into buffer value `RootDir` for current buffer"
    (when (= nil (. vim.b 0 :RootDir))
      (let [_root (get_root patterns)]
        (tset vim.b 0 :RootDir _root)))
    (vim.api.nvim_set_current_dir (?. vim.b 0 :RootDir)))
  {: to_root : get_root})
