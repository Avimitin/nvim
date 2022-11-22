<!-- vim-markdown-toc GFM -->

  * [Plugin List](#plugin-list)
* [How to interact with projects files](#how-to-interact-with-projects-files)
  * [#1 Nvim-tree plugin](#1-nvim-tree-plugin)
    * [Keymap](#keymap)
  * [#2 Telescope Fuzzy Find](#2-telescope-fuzzy-find)
    * [Keymap](#keymap-1)
  * [#3 nvim-cmp completion](#3-nvim-cmp-completion)
* [How to use multi-cursor](#how-to-use-multi-cursor)
  * [Select multiple lines in same column.](#select-multiple-lines-in-same-column)
  * [Select same word.](#select-same-word)
  * [Use regex to select](#use-regex-to-select)
  * [Add visual select region to multiple cursor](#add-visual-select-region-to-multiple-cursor)
  * [How to unselect a matches](#how-to-unselect-a-matches)
* [nvim-bufferline.lua](#nvim-bufferlinelua)
  * [Configuration file](#configuration-file)
  * [keymap](#keymap-2)
* [sort.nvim](#sortnvim)
  * [Usage](#usage)
* [splitjoin](#splitjoin)
  * [Usage](#usage-1)
* [vim-surround](#vim-surround)
  * [Keymap](#keymap-3)
  * [Addtional](#addtional)
* [telescope.nvim](#telescopenvim)
  * [Key mappings](#key-mappings)
* [wildfire](#wildfire)
  * [Usage](#usage-2)
  * [Configuration](#configuration)
* [Create new split window](#create-new-split-window)
* [Navigate between window](#navigate-between-window)
* [Move window](#move-window)
* [Auto resize window](#auto-resize-window)

<!-- vim-markdown-toc -->

## Plugin List

* [folke/which-key.nvim](https://github.com/folke/which-key.nvim)
  > Display cheatsheets for keys
* [simnalamburt/vim-mundo](https://github.com/simnalamburt/vim-mundo)
  > Show undo history, use command `:Mundo` to toggle it
* [sindrets/winshift.nvim](https://github.com/sindrets/winshift.nvim)
  > Toggle window switch mode. Use command `:WinShift` to toggle.
* [kyazdani42/nvim-web-devicons](https://github.com/kyazdani42/nvim-web-devicons)
  > Nerd font icons library
* [glepnir/galaxyline.nvim](https://github.com/glepnir/galaxyline.nvim)
  > Status line library
* [akinsho/nvim-bufferline.lua](https://github.com/akinsho/nvim-bufferline.lua)
  > Show buffer as tab at the top, use `<TAB>` to forward, use `<Shift-TAB>` to backward.
* [kyazdani42/nvim-tree.lua](https://github.com/kyazdani42/nvim-tree.lua)
  > Tree-like file manager, use `;t` to toggle.
* [akinsho/toggleterm.nvim](https://github.com/akinsho/toggleterm.nvim)
  > Enhanced neovim terminal, use `_` to open it. Use number +`_` to toggle different terminal.
  > For example, press `2_` to open the second terminal.
* [uga-rosa/ccc.nvim](https://github.com/uga-rosa/ccc.nvim)
  > Color picker and highlighter, useful for frontend developer
* [mg979/vim-visual-multi](https://github.com/mg979/vim-visual-multi)
  > Multiple cursor.
* [glepnir/dashboard-nvim](https://github.com/glepnir/dashboard-nvim)
  > Add a start page when user open editor only.
* [notjedi/nvim-rooter.lua](https://github.com/notjedi/nvim-rooter.lua)
  > Find the root directory of the current file and change dir into it.
  > It use `.git`, `packages.json`, `Cargo.toml` as root dir pattern.
* [nvim-telescope/telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
  > Fuzzy search text and file. Use `;f` to search file, use `;s` to search text.
* [gcmt/wildfire.vim](https://github.com/gcmt/wildfire.vim)
  > Quick select text object by `<Enter>` key.
* [tpope/vim-surround](https://github.com/tpope/vim-surround)
  > Quickly add character surround the selected text, see its readme for usage.
* [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align)
  > Plugin to align text, use `<LEADER>e` to toggle it. See its readme for usage.
* [ggandor/lightspeed.nvim](https://github.com/ggandor/lightspeed.nvim)
  > Quickly hop inside buffer. Use `s/S`, `f/F`, `t/T` to toggle it.
* [andymass/vim-matchup](https://github.com/andymass/vim-matchup)
  > Hop between pair, use `,` to jump.
* [windwp/nvim-autopairs](https://github.com/windwp/nvim-autopairs)
  > Automatically add pairs of your brackets.
* [AndrewRadev/splitjoin.vim](https://github.com/AndrewRadev/splitjoin.vim)
  > Enhance the split line functionality, use `gS` to split single line to multiple line base on syntax.
* [lukas-reineke/indent-blankline.nvim](https://github.com/lukas-reineke/indent-blankline.nvim)
  > Add indent guide line
* [sQVe/sort.nvim](https://github.com/sQVe/sort.nvim)
  > Sort the text based on pattern. Use command `:Sort` to enable it
* [karb94/neoscroll.nvim](https://github.com/karb94/neoscroll.nvim)
  > Add smoothy animation for srolling. `<Ctrl-j>`, `Ctrl-k` `Ctrl-u` `Ctrl-d` `Ctrl-f` `Ctrl-b`
* [windwp/nvim-spectre](https://github.com/windwp/nvim-spectre)
  > Search and replace text. Use `SpectreOpen` to toggle it
* [beauwilliams/focus.nvim](https://github.com/beauwilliams/focus.nvim)
  > Automatically enlarge window when focus
* [rcarriga/nvim-notify](https://github.com/rcarriga/nvim-notify)
  > Add beautiful UI and animation for notification
* [tpope/vim-repeat](https://github.com/tpope/vim-repeat)
  > Use `.` to repeat your last operation
* [monaqa/dial.nvim](https://github.com/monaqa/dial.nvim)
  > Use `-` and `=` to decrease/increase date/number/boolean
* [petertriho/nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar)
  > Add scrollbar for your buffer
* [kevinhwang91/nvim-hlslens](https://github.com/kevinhwang91/nvim-hlslens)
  > Enhance the search experience
* [tpope/vim-sleuth](https://github.com/tpope/vim-sleuth)
  > Automatically setup indent, space/tab base on current project.
* [lewis6991/impatient.nvim](https://github.com/lewis6991/impatient.nvim)
  > Buffer everything

# How to interact with projects files

There are multiple plugin that can help you find and edit a file.

## #1 Nvim-tree plugin

Nvim-tree.lua is a plugin that show files in tree view.
The configuration is located in `lua/plugins/modules/enhance/config/nvim-tree.lua`

![image](https://raw.githubusercontent.com/kyazdani42/nvim-tree.lua/master/.github/screenshot.png)

### Keymap

* Use <kbd>;t</kbd> to toggle the tree view.
* Press <kbd>R</kbd> to refresh the file tree.
* Press `?` inside the nvim-tree to get list of keymap.

## #2 Telescope Fuzzy Find

Telescope support fuzzy find file like what fzf did.

![fuzzy find](https://github.com/Avimitin/nvim/raw/master/docs/images/telescope-find-file.png)

### Keymap

* Press <kbd>;f</kbd> to open fuzzy find file.

## #3 nvim-cmp completion

nvim-cmp can fuzzy complete file path too! Press `:` into command line, then input
file path to trigger completion.

# How to use multi-cursor

I don't enable all the functionality in vim-visual-multi plugins.
Features that I kept are all inspired by
[helix-editor](https://helix-editor.com/) and [Kakoune](https://kakoune.org/):
multiple select and manipulation by words and regex.

## Select multiple lines in same column.

Use <kbd>Ctrl up/down</kbd> to create multiple cursor vertically.

## Select same word.

Use <kbd>\ n</kbd> to select word under cursor, then press <kbd>n</kbd> to select more.

You can also use <kbd>uA</kbd> to select all the same words at once.

## Use regex to select

Use <kbd>ux</kbd> to enable regex search. Input the regex match and press enter to confirm.
Then use <kbd>n</kbd> to select forward, use <kbd>N</kbd> to select backward.

The key <kbd>ux</kbd> is also usable in visual mode. You can use <kbd>V</kbd> to select a region
and then press <kbd>ux</kbd> to regex select region. Just like the Kakoune editor style.

## Add visual select region to multiple cursor

You can use `v` to enter visual select, then press <kbd>ua</kbd> to add the visual select
region.

## How to unselect a matches

You can use <kbd>]</kbd> to select forward, and <kbd>[</kbd> to select backward. Then use
<kbd>Q</kbd> to unselect the matches under cursor.

<sub>More documents: <a href="https://github.com/mg979/vim-visual-multi/wiki/Mappings">vim-visual-multi wiki</a></sub>

# nvim-bufferline.lua

nvim-bufferline.lua is a buffer manager. It looks like tab page, but it can
manage more than tab page.

![image](https://user-images.githubusercontent.com/22454918/111993085-1d299700-8b0e-11eb-96eb-c1c289e36b08.png)

## Configuration file

lua/plugins/modules/enhance/config/init.lua

## keymap

* Use <kbd>Tab</kbd> to go to next page.
* Use <kbd>Shift Tab</kbd> to go to previous page.
* Use <kbd>Ctrl-c</kbd> to open picker mode and close the buffer.
* Use <kbd>;p</kbd> to pick buffer.

![image](https://user-images.githubusercontent.com/22454918/111993296-5bbf5180-8b0e-11eb-9ad9-fcf9619436fd.gif)

# sort.nvim

This plugin gives you ability to easily sort the item.

## Usage

Use command `:Sort` in visual or normal mode.

<video src="https://user-images.githubusercontent.com/2284724/145567686-3b52978c-58fe-4f32-ad27-c2b1060870ba.mp4" controls>
</video>

# splitjoin

![image](https://camo.githubusercontent.com/842b1cd04688d9623d341c1bdbbaa87e1a3dd32cca97eb48a64af6b3be107ccb/687474703a2f2f692e616e6472657772616465762e636f6d2f32666363396630313338313665633734346335346535373437366166616333322e676966)

## Usage

Press `gS` to split multiple line.
Press `gJ` to join multiple line.

# vim-surround

Vim-surround can help you manage the surrounding text easily.

## Keymap

| keymap | functionality                    |
|--------|----------------------------------|
| cs     | Change surround text             |
| ds     | Delete surround text             |
| gs     | Add surround text in visual mode |
| ys     | add surround text in normal mode |

## Addtional

The vim-surround use `S` in visual mode by default, but it is conflict
with the lightspeed.nvim plugin. So it is now set to `gs` now.


# telescope.nvim

Telescope is a powerful fuzzy search and pick plugin.
It works like `fzf`, but more extensible.

## Key mappings

* <kbd>;f</kbd> to open fuzzy file finder

![findfile](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/telescope-find-file.png)

* <kbd>;s</kbd> to open grep/symbol search finder

![Search](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/telescope-live-grep.png)

![symbols](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/telescope-symbols.png)

* Use command `:Telescope` to see other useful builtin finder

# wildfire

The wildfire plugin can help you select text object on fly.

## Usage

Press <kbd>Enter</kbd> in
`""`, `()`, `{}`, `[]`, `''`, <code>``</code>, `<tag></tag>`.
And this plugin will automatically select text inside this surrounding signs.

## Configuration

The configuration is defined in lua/plugins/modules/enhance/config/init.lua by `pre()` function.
You can modify the `vim.g.wildfire_objects` variable.

You can add the bracket you want inside it.
For example, assuming that you want to select text inside the `$$` on fly,
you can append `"i$"` into the list.

# Create new split window

In nvim tree, you can press <kbd>Ctrl v</kbd> to create a vertical split window.
And press <kbd>Ctrl h</kbd> to create horizon split window.

This keymap is as same as the telescope.

# Navigate between window

You can use `;` + `h/j/k/l` to move arround the windows.

# Move window

Some time you may want to move the left panel to right panel or else.
There is a plugin [winshift.nvim](https://github.com/sindrets/winshift.nvim)
can help you do this thing.

![winshift demo](https://user-images.githubusercontent.com/2786478/133154376-539474eb-73c9-4cd7-af8c-a6abb037c061.gif)

Press command `:WinShift` to trigger windows move mode, then just use `hjkl` to
move the window.

# Auto resize window

I've add a [focus.nvim](https://github.com/beauwilliams/focus.nvim) plugin to help
auto resizing the windows.

![focus.nvim demo](https://camo.githubusercontent.com/ae3ba19ad8ab00219e8a7dae22d4529ffbf9dc6f8d2d2047e4012f814ebdf855/68747470733a2f2f692e6962622e636f2f3074734b7777342f666f6375736f702e676966)

The auto resize mode will be trigger when you open any new windows.
You don't need to do anything.

This is powered by vim `WinEnter` command. This event is happened when you entering
***another*** window, which means that it will not be trigger when you first open the
neovim.

This plugin provides `:FocusSplitNicely` command to help you create tiled windows layout.
You should input this command twice to get the below layout.

```text
+----------------+------------+
|                |    S1      |
|                |            |
|                +------------+
|                |            |
|   MAIN PANE    |    S2      |
|                |            |
|                |            |
|                |            |
+----------------+------------+
```
