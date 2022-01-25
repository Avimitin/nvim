# vim-fugitive

vim-fugitive is a handy git tools. If you are a emacs user you may familiar with it.
It is like the magit in emacs.

See details in: <https://github.com/tpope/vim-fugitive>

## Commands

Always remember the key `g?`, it will provide cheatsheet.

* `:G/:Git`: Call the status page

![image](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/fugitive.png)

Press `s` to stage file, press `+` to open diff, press `u` to unstage file,
press `x` to discard changes, press `cc` to commit, press `ca` to amend changes.

* `:Git commit/rebase/push`: Same as the command line, but integrate into neovim
* `:Git blame`: view blame line by line

![image](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/neovim-fugitive.png)
