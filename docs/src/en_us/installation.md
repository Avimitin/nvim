<!-- vim-markdown-toc GFM -->

* [Prerequisites](#prerequisites)
  * [Install stable version in Arch Linux](#install-stable-version-in-arch-linux)
* [Fork before pull](#fork-before-pull)
* [Installation](#installation)
* [How to get update](#how-to-get-update)
* [Clean up](#clean-up)
* [Check health](#check-health)
* [Docker](#docker)

<!-- vim-markdown-toc -->

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

> * How to build surf
>
> ```bash
> # Arch linux contains most of the library
> # Other distro need to check out documents yourself
> git clone https://git.suckless.org/surf
> cd surf
> sudo make clean install
> ```

# Fork before pull

I recommend you use my configuration as a base and build your
configuration.

This neovim config is biased and was not created for generic use.
The reason I build this configuration is that I don't want to use the same neovim
as the community. And it is really tired to always make compatibility for an
editor configuration. It will gradually expand the size of the configuration,
amount of the bug, and uninteresting of the maintenance.

And I know that someone like me want to build their own configurationt too,
but it is hard to start from empty.
So you can use my configuration as a base to build yours.


# Installation

```bash
# NO WINDOWS SUPPORT NOW
git clone https://github.com/YOUR_USER_NAME/nvim ~/.config/nvim

# Enable configuration for the configuration
mv ~/.config/nvim/lua/custom.example.lua ~/.config/nvim/lua/custom.lua
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

# How to get update

You can rename the default branch to `upstream` and switch to a new `master` branch:

```bash
# rename master to upstream. Not necessary to be upstream, you can pick whatever you like as branch name.
git branch -m master upstream

# create new branch with name "master". Not necessary to be master too.
git branch master
```

Working with two different branches, you can always pull or pick new bugfix or feature
from my configuration without messing up your configuration.

I will always write changes into CHANGELOG and release a new version after changes are made.
Please read the changelog each time you pull new changes. And if you are not satisfied
with the changes, you can `git checkout` the old version. You are also welcome to open an issue
to discuss with me. This config is considered as **MY** personal configuration,
and I can't guarantee I will stabilize it as the community do.

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
