# Dependency

This neovim configuration require you have the below dependencies installed on your machine.

- [`neovim`](https://github.com/neovim/neovim/wiki/Installing-Neovim#install-from-package) v0.9.2 (The current stable version)
- curl, git
- [`ripgrep`](https://github.com/BurntSushi/ripgrep#installation)
- Any of the patched [nerd font](https://www.nerdfonts.com/font-downloads)

# Installation

It is recommended to use [`git`](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
to download and keep this configuration up to date.

```bash
git clone --depth=1 https://github.com/Avimitin/nvim ~/.config/nvim
```

# Download Plugin

All the plugin will be sync when open up neovim at the first time without any of your efforts.
If anything goes wrong, try to use `:message`, `:Lazy log` to figure out what is going wrong.
If you met any error after plugin installed, you can open a issue to report bug.

# Health check

After plugin downloaded, you can run command `:checkhealth` to check if the configuration
is properly serving the editor.
