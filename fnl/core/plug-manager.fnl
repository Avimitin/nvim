(lambda ensure_manager []
  (let [plug_install_path (.. (vim.fn.stdpath :data) :/lazy/lazy.nvim)]
    (when (not (vim.uv.fs_stat plug_install_path))
      (vim.print "Installing package manager")
      (let [(ok err) (pcall vim.system
                            [:git
                             :clone
                             "--filter=blob:none"
                             "https://github.com/folke/lazy.nvim.git"
                             :--branch=stable
                             plug_install_path])]
        (assert (not= nil ok) (.. "fail to install package manager: " err))))
    (: vim.opt.rtp :prepend plug_install_path)))

(lambda load_plugins [plugins]
  (let [lazy (require :lazy)
        canonicalize #(each [_ spec (ipairs $1)]
                        (let [url (?. spec :repo)] (tset spec 1 url))
                        (tset spec :repo nil)
                        nil)]
    (canonicalize plugins)
    (lazy.setup plugins {:install {:colorscheme [:kanagawa]}})))

{: ensure_manager : load_plugins}
