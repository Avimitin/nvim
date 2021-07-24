# My Neovim Configuration 

![User Interface Preview](./image/screenshot.png)

## Intro

**[English](./README.md) (You are here)**
|
[中文](docs/README_CN.md)

## Credit

This project is originally inspired by
[theniceboy/nvim](https://github.com/theniceboy/nvim).

And lua code is inspired by
[siduck76/NvChad](https://github.com/siduck76/NvChad).

Take a look at their contribution, which is really fantastic.

## Features

- Really fast (loading in `65ms` with command `nvim --startuptime`) by packer
- LSP support by nvim-lspconfig
- Completion like VSCode by nvim-compe
- Tree file manager by nvim-tree
- Symbols by SymbolsOutline
- Buffer line manager by bufferline.nvim
- Fuzzy file finder by telescope
- Floating terminal by FTerm
- Git message by gitsign
- Quick jump by hop

## Installation

To keep your customization, fork the configuration and clone
your repository URL instead of mine. Everyone should build their
own neovim configuration in my opinion.

```bash
# linux or mac user
git clone https://github.com/Avimitin/nvim ~/.config/nvim
```

## Dependency

### neovim

> `IMPORTANT NOTES`: I'am using the pure Lua configuration so you 
> must use the latest built of the neovim.
>
> you will need at least `NVIM v0.5.0-dev+1411-gb28d458f8`.

- Linux

```bash
# if you are an Ubuntu user
# removed out of dated version
sudo apt remove neovim
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt install neovim

# OR if you are a Arch user
yay -S neovim-git
```

### Install nerdfont

It's highly recommended to install [nerdfont](https://www.nerdfonts.com/font-downloads) 
for impressive icon support.

## Check health

Open your neovim and input following command to check if the dependence is all installed or not.

```vim
:checkhealth
```

## Sync plugins

I'am using [packer](https://github.com/wbthomason/packer.nvim) to manage
plugins, which I'am not so familiar with. Although I've write script
to download and load plugin at the first boot, there is still some plugins
not working at the first boot. So you need to quit the neovim and reopen it.
But that's not a big issue as it only happen once.

## Keymaps

See [keymaps.md](./docs/keymaps.md)

## License

MIT License
