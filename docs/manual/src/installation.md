# Dependency

This neovim configuration require you have the below dependencies installed on your machine.

- [`neovim`](https://github.com/neovim/neovim/wiki/Installing-Neovim#install-from-package) v0.8+
- [`ripgrep`](https://github.com/BurntSushi/ripgrep#installation)
- Any of the patched [nerd font](https://www.nerdfonts.com/font-downloads)

# Installation

It is recommended to use [`git`](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
to download and keep this configuration up to date.

```bash
git clone --depth=1 https://github.com/Avimitin/nvim ~/.config/nvim
```

## Manage **YOUR** neovim configuration

You can press the fork button on GitHub page and maintain your configuration.
If you want to keep your changes and keep track of mine modification, you can
maintain a upstream branch.

```bash
# rename the original master branch to upstream
git branch -m master upstream
# create a new master (or main) branch
git switch -c master
```

# Download Plugin

All the plugin will be sync when you first use this configuration.
So you don't need to do anything but just open the neovim editor by command `nvim`,
then it will setup itself automatically.

```bash
nvim
```

If you met any error since downloading plugin, you can open a issue to report bug.

# Health check

After plugin downloaded, you can run command `:checkhealth` to check if the configuration
is properly serving the editor.
