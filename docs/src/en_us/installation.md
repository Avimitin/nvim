# Prerequisites

* **Neovim** (MUST)

This configuration is compatible with neovim version v0.7.0-v0.8.0 only.

You can follow
[neovim installation](https://github.com/neovim/neovim/wiki/Installing-Neovim)
for the installation guidance.

## Install stable version in Arch Linux

If you are Arch Linux user, and you found that there are some bug in the latest version
, you can download PKGBUILD file with your aur helper, and then add a prepare script to
build stable version:

```bash
# Assuming that you are using paru
paru -G neovim-git
```

Then edit the PKGBUILD file and this lines before the `build()` function:

```diff
+prepare() {
+  cd "${pkgname}"
+  git checkout stable
+}
+
```

Finally, run the `makepkg -si` to install the neovim.

* **Nerdfont** (MUST)

Most of my setting are based on nerd font. It’s highly recommended to
install [nerdfont](https://www.nerdfonts.com/font-downloads) for
impressive icon support.

* **ripgrep** (MUST)

The telescope, nvim-cmp, and anyjump plugin are configured to use ripgrep
as the search program. It is an faster grep RIIR alternative.

See its [readme](https://github.com/BurntSushi/ripgrep) for more.

* *Surf* (OPTIONAL)

I am using [Surf](https://surf.suckless.org/) as my markdown preview
browser. Firefox is too heavy for the preview job. If you have interest
on it, please follow the instruction from the official pages. If not,
you can easily modify the settings:

```bash
sed -i 's/surf/firefox/g' lua/plugins/config/markdown_preview_cfg.lua
```

> * How to build surf
>
> ```bash
> # Arch linux contains most of the library
> # Other distro need to check out documents yourself
> git clone https://git.suckless.org/surf
> cd surf
> sudo make clean install
> ```

# foreword

I recommend you to fork my project.
The reason I build this configuration is that I don't want to use the same neovim
as the community.
I don't know if you agree with it or not, but I indeed expect that everyone can build
their neovim.
And I know that it is hard to start from empty.
So you can use my configuration as a base to build yours.

# Installation

```bash
# NO WINDOWS SUPPORT NOW
git clone https://github.com/YOUR_USER_NAME/nvim ~/.config/nvim
```

Open your neovim by command `nvim` and wait for all plugins installed.
The plugins will be installed automatically. Please quit and reopen the
neovim to load all the plugins.

If the neovim don’t install plugins automatically, use the command
`:PackerSync` to install those plugins manually. And please open a issue
to notify me about the error.

**NOTE:** Markdown preview plugin is installed in another thread, please
wait for it until it response message of installation success.
Otherwise, you will find that you can’t activate it.

# Clean up

You need to clean the below directory to do a fresh install.

```bash
# plugins directory
rm -rf ~/.local/share/nvim

# neovim cache file
rm -r ~/.cache/nvim

# neovim plugins load sequence
rm -r ~/.config/nvim/plugin
```

# Check health

Open your neovim and input following command to check if the dependence
is all installed or not.

```text
:checkhealth
```

# Docker

Just wanna have a try but do not want to mess up your local environment?
I have docker script for you!

```bash
docker run -w /root -it --rm alpine:edge sh -uelic '
      apk add git neovim ripgrep alpine-sdk --update
      git clone 'https://github.com/Avimitin/nvim' ~/.config/nvim
      nvim -c "autocmd User PackerComplete quitall"
      nvim /root/.config/nvim/README.md
  '
```
