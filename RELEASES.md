# Changelog

All notable changes to this project will be documented in this file

## Unreleased

- Remove vim-matchup

## [cv2023.02.21]

### Changed

- Plugins setup/config/key mappings are merged together
- `require("editor").setup({})` is replace with `.neovim.lua` file injection
- Add more configurable option in `core/custom.lua`, please read [document](./lua/core/README.md)
- Replace packer.nvim with lazy.nvim

## [cv2023.01.21]

### Added

- New command alias `Sed` to trigger the `Spectre` plugin
- New mapping `Shift-Space` to `Space` in terminal mode
- `i>` text object for `wildfire`
- Re-enter nvim-tree window for mapping `<leader>t` when nvim-tree is opened
- New scroll key mapping
- New plugin `flit.nvim` to enhance key `f/t`

### Fixed

- Fix line-count doesn't applied to `gg` issue
- Update lspsage settings to latest version

### Removed

- `friendly-snippets` in commit [`37e6eaecea53692012661d7a88028596d953da5d`]

cv2023.01.03
============

BREAKING:
---------
* plugin: replace plugin lightspeed.nvim with leap.nvim [deprecated]
* plugin: remove nvim-rooter [vendored]
* plugin: remove FixCursorHold [merged]
* plugin: move `setup` function into `after/plugin` directories

FIXED:
------
* highlight: fix nvim-cmp icon highlight
* config: fix the darker background field never apply issue
* multicursor: fix loading mechanism
* match-up: fix key mapping
* null-ls: fix injected sources occupying key mapping issue
* gitsigns: fix key mapping required confirmation issue
* rust: fix format-on-save messing up file issue
* lspsaga: remove deprecated command
* option: increase timeoutlen for better triggering multiple key sequence
* treesitter: fix `@parameter` occupying `paragraph` text object issue
* treesitter: fix condition text object toggle key
* rust: disable `locationLinks` option to fix inlay hint

ADDED:
------
* auto commands
  - add auto command to always display status line
  - add auto command to automatically exit when nvim-tree is last buffer
  - add auto command to load nvim-tree for directories argument
* git
  - toggle deleted line when enter git mode
  - add new plugin `flog.vim` to view git log
* lspconfig: add key mapping to toggle outline

cv2022.12.12
============

REMOVED:
--------
* key mappings: replace which-keys.nvim with hydra.nvim
* docs: remove mdbook

FIX:
----
* null-ls: fix null-ls custom settings never load issue
* filetype: fix `.zsh` can't be recognized issue
* statusline: fix statusline not shown on first enter issue
* statusline: fix statusline readability on vertical split issue
* lspsaga: update deprecated commands
* completion: fix always pre-select behavior
* lspconfig: fix eslint and tsserver conflict

CHANGED:
--------
* [BREAK] Configuration
  - plugins: move `plugins/<name>/repos.lua` to `overlays/<name>.lua`
  - config: move `plugins/<name>/config.lua` to `overlays/rc/<plug>.lua`
  - custom: move `lua/custom.lua` to `init.lua`
* UI
  - telescope: make the search background darker
  - statusline: set command line height to 0
* Coding
  - rust: enable format on save
* Completion
  - nvim-cmp: use `<ctrl-c>` to abort completion.
  - option: move `vim.opt.completopt` into cmp rc file
* Key mappings
  - lspconfig: replace `g-` prefix key to `<leader>a`
  - git: replace `gi-` prefix key to `<leader>g`
  - multi-cursor: replace `\n` with `un`

ADDED:
------
* keys: add `<space>c` to open action menu for `Cargo.toml` and `packages.json`
* keys: add `<Ctrl-w>` to open windows operation menu
* keys: add `<leader>r` to open Rust action in Rust buffer
* plugin: add hlargs plugin to highlight function argument
* null-ls: add eslint diagnostic into sources
* config: add per-project neovim configuration
* ci: add Mac OS
* keys: add smooth scroll for `J` and `K`


cv2022.11.21
============

ADDED:
------
* scrollbar: add lsp signature in scrollbar
* scrollbar: add git status in scrollbar
* plugin: add TODO comment highlighter
* plugin: add same token highlighter
* clipboard: yank text to system clipboard only when user clicked `y`
* keyboard: add `;x` to perform `:wq`

CHANGED:
--------
* Statusline: update shortline style
* options: disable useless provider(python, ruby, perl) and plugins
* options: remove nvui settings
* plugins: optimize dashboard and gitsigns load time
* scrollbar: add lsp signature in scrollbar
* crates: fix nvim-cmp not loading issue
* docs: split docuement into each sub directory
* gitcommit: fix the buggy diff split panel

cv2022.11.04
============

ADDED:
------
* Completion
  - Add signature completion to indicate the parameters
  - Add command line lsp symbol search completion
* Enhancement
  - Add hlslens to indicate the search context
  - Add scrollbar to indicate the LSP status
* Document
  - Add README.md for each sub-directory

CHANGED:
--------
* option: set lazyredraw to true, to have responsive pop up menu
* terminal: replace normal mode keymap to `<A-;>`
* terminal: add ability to spawn multiple terminal
* git: use same icon for git add/change
* notify: truncate message when the string length is larger than 72


cv2022.10.16
============

FEATURES:
---------
* Replace color preview plugin to ccc.nvim
* Add plugin to dim the unused variable

CHANGES:
--------
* Hide the telescope search bar border
* Redesign the status line
* Close lazyredraw option


cv2022.10.06
============

FIX:
----
* Fix the dashboard configuration and enable it by automatically
* Fix lspconfig and treesitter don't loaded for jsx/tsx filetype issue
* Fix conflict key mapping

CHANGES:
--------
* Use tsserver for js/jsx/ts/tsx by default, and inject eslint into lspconfig
* Enable treesitter indent functionality
* Enable treesitter fold functionality
* Inject prettier as default formatter into lspconfig


cv2022.09.19
============

CHANGES:
--------
* UI
  - Use lower contrast yellow as warning background
  - Reset nvim-tree to left
  - Move icons at the front of the completion menu
* Update lspsaga configuration

cv2022.09.11
============

BREAKING CHANGES:
-----------------
* Plugin neoclip.lua was removed. (Commit 4c5d8d5)
* Plugin vim-go was removed. (Commit 327e413)

FEATURES:
---------
* Add key `-/=` to increase/decrease number/date/version...etc

CHANGES:
--------
* Clean up and update the main UI
  - Status line background now turn into dark black.
  - Remove all the icon in signcolumn. Now signcolumn will display git status only.
  - Code action icon is now moved to virtual text, just like VSCode.
  - LSP Error/Hint/Warning/Info will now display as background color.
  - LSP Error/Hint/Warning/Info icon is now moved into diagnostic panel.
  - Always enable filename in winbar to stablize the window.
  - Use darker background for kanagawa theme and github_dark theme.
  - Use filename for shortline.
  - Remove source from completion menu.
  - Add completion kind into completion menu.
* Remove buffer sources from completion menu
* Move Neovim Lua settings into custom.lua file.
* Split auto commands into different module.

FIX:
----
* Fix git blame key mapping.
* Fix incorrect condition detect for fcitx5 auto commands.
* Add the missing `github_dark_default` theme.

cv2022.08.21
============

BREAKING CHANGES:
-----------------
* As this configuration will be changed constantly, and there will be
a great deals of breaking changes from plugins, so from now on the configuration
will use calendar versioning.
* rest.nvim is removed in commit 3577818.
* vim-eunch is removed in commit a30e46e.

FIX:
----
* fix nvim-tree.lua ignore file regex match

FEATURES:
---------
* Add command `SudoW` to help writing content when we forget open neovim as root.
* Map `<backspace>` to delete without copying.
* Enable VSCode like winbar when using neovim 0.8.0+

CHANGES:
--------
* Update Markdown highlight
* (Internal) use new positive approach to expand the custom.lua configuration
* Change the status line FileFormat component to FileEncode component.
* Enlarge the sign column.


v7.3.1 (Aug 10 2022)
====================

FIX:
----
* Fix treesitter not recognize `typescriptreact` filetype issue

FEATURE:
--------
* Add plugin to help auto close and rename HTML tag


v7.3.0 (Aug 10 2022)
====================

FIX:
----
* Use same doc scrolling key mapping for lspsaga and nvim-cmp

CHANGES:
--------
* Neovim will now default using system clipboard as main register
* Exclude the 'd' and 'x' key mapping for using clipboard
* Update rust-tools configuration

BREAKING CHANGES:
-----------------
* Remove lsp installer (Explained in commit 564b780)
* Remove vfiler.nvim and using nvim-tree as file manager (Press `<SPACE>t`)


v7.2.2 (Aug 8 2022)
===================

FIX:
----
- fix incorrect condition when reading browser option

v7.2.1 (Aug 8 2022)
===================

CHANGES:
--------
- Remove git track of custom.lua file
- Add custom.example.lua file
- Add configuration rename step in install script
- Group auto commands option together


v7.2.0 (Jul 26 2022)
====================

BREAKING CHANGES
----------------
- License is now change to Apache 2.0
- Agit.vim is removed because its functionality is redundant with vim-fugitive
- lazygit.nvim is removed because it is buggy
- Cargo clippy is now the default linter for rust project.

FEATURES:
---------
- Add lsp-line.nvim
- Add auto command to diff staged file when git commit is opened
- Add vim-repeat to produce repeat motion


v7.1.0 (Jul 19 2022)
========================

BREAKING CHANGE:
----------------
* remove built-in vale support. Commit [11733f3](https://github.com/Avimitin/nvim/commit/11733f33a24bb440fde890b5f01c1b3fce7f18b7) explain the reason.

FEATURES:
---------
* Stylua will be the first choice to format the Lua code.
* The benchmark finally become better (~28ms)
  - Use libuv to do async file IO (-10ms)
  - Lazy-load the stausline component (-10ms)

CHANGES:
--------
* nvim-notify will load after UI rendered
* multi-cursor can be load with key `u` too
* which-key will only be load for limited-keys
* lspsaga: update finder icons


v7.0.0-rc5 (Jul 16 2022)
========================

BREAKING CHANGES:
-----------------
* remove vimrc cuz we are neovim config
* remove vim-markdown cuz it is laggy

FEATURES:
---------
* rewrite vim-markdown edit file feature in Lua
  - Use `ge` to open file under cursor in markdown
* rewrite vim-lastplace in Lua
  - Auto resume cursor to last edit line
* Layer style language specification in custom.lua

CHANGES:
--------
* lspsaga: remove out-dated configuration
* trouble: map key `gq` to trouble.nvim

DOCS:
-----
* add js document
* update rust document
* simplify the lspconfig guide (Finally QWQ)
* add more multi-cursor usage
* update install steps to avoid read pipe to shell
* docs: add warning about the rust lsp
* docs: update document for custom.lua file

FIX:
----
* fix nvim-rooter fail to load at bootstrap step issue


v7.0.0-rc4 (Jul 15 2022)
========================
This release candidate finish all the v7.0.0 design and fixes most of the
current known bugs. Next release might be the stable release.

CHANGES:
--------
* plugins
  - nvim-cmp: enable vsnip for markdown filetype
  - flog: replace flog with agit.vim
  - rust: rewrite the `.rust-analyzer.json` file detection with async lib
  - rooter: replace the vim-rooter with nvim-rooter for its public API
  - toggleterm: fix auto insert when re-enter terminal
  - null-ls: fix lspconfig key mappings import path

* keymaps
  - map `,` to `%` for pair jumping

* autocmd:
  - remove TermEnter event
  - rewrite fcitx5 auto toggle with async lib

DOCS:
-----
* Add multiple cursor usage guidance
* Add file management usage guidance
* Fill the necessary comment for code


v7.0.0-rc3 (Jul 13 2022)
========================

CHANGES:
--------
* plugins: redesign the plugin structure
    - Remove module directory and expose module to upper level
    - Move colorscheme plugin and colorscheme settings to single folder
    - Move all plugins repository into single `repo.lua` file.
    - Move lua cache plugin into plugins init script
* key mappings: `<alt>+n/p` is now removed. Use `<Tab>` and `<Shift-Tab>` instead.

FIX:
----
* filetype: filetype.lua is enabled by default in nvim-0.8.0, assign those variable
only when neovim version is lower than 0.8.0.
* lspsaga: lspsaga is now using table instead of boolean for winbar.


v7.0.0-rc2 (Jul 09 2022)
========================

New Features:
-------------
key mappings: add `<Alt>+left/right` as a fallback buffer switch key mappings.

Fix:
----
rust/lspconfig: fix lspconfig import path

Changes:
--------
galaxyline: load galaxyline after VimEnter event
filetype: use builtin filetype.nvim and remove filetype.nvim plugin
lspconfig: lspconfig buffer key mapping definition is now moved to coding/keymap.lua file.
development: abandon the commit prefix convention


v7.0.0-rc1 (Jul 5 2022)
====================
v7.0.0-rc1 introduced the new plugin structure design. In the new design,
plugins definition, key mappings, configuration are split by their functionality
and placed inside the `lua/plugins/modules` directory.

BREAKING CHANGES!:
------------------
  * plugins: redesign plugin structure (See commit [`26f365d2`](https://github.com/Avimitin/nvim/commit/26f365d2))
  * multi-cursor: simplify the multi cursor keymap (See commit [`834cc902`](https://github.com/Avimitin/nvim/commit/834cc902))
  * keymap: map bufferline switch to `<M-{n,p}>` (See commit [`b730e06c`](https://github.com/Avimitin/nvim/commit/b730e06c))
  * core: rename core module to editor (See commit [`c6b5860b`](https://github.com/Avimitin/nvim/commit/c6b5860b))
  * which-key: move which-key into enhance module (See commit [`5ad09ad2`](https://github.com/Avimitin/nvim/commit/5ad09ad2))
  * keymap: move editor keymap into core (See commit [`1b2c24e7`](https://github.com/Avimitin/nvim/commit/1b2c24e7))
  * init: move library logic into core (See commit [`dc07b2da`](https://github.com/Avimitin/nvim/commit/dc07b2da))
  * plugins: move libs into plugins/modules/lib (See commit [`12ce72db`](https://github.com/Avimitin/nvim/commit/12ce72db))
  * mapping: split keymap to modules (See commit [`fb417070`](https://github.com/Avimitin/nvim/commit/fb417070))
  * mapping: integrate mappings.utils with core.utils (See commit [`df41c21e`](https://github.com/Avimitin/nvim/commit/df41c21e))
  * plugins: move all the config under modules (See commit [`9a7241b8`](https://github.com/Avimitin/nvim/commit/9a7241b8))
  * packer: clean up code (See commit [`5797699c`](https://github.com/Avimitin/nvim/commit/5797699c))
  * commands: wrap api-create-command into utils.alias function (See commit [`521be3cd`](https://github.com/Avimitin/nvim/commit/521be3cd))
  * plugins: remove load.lua (See commit [`24f0c4c0`](https://github.com/Avimitin/nvim/commit/24f0c4c0))
  * cmp: enable dictionary source for text ft only (See commit [`90713b6e`](https://github.com/Avimitin/nvim/commit/90713b6e))

Fix:
----
  * nvim-tree: fix custom match (See commit [`9fa517c6`](https://github.com/Avimitin/nvim/commit/9fa517c6))
  * ts: fix treesitter filetype not found issue (See commit [`6246353e`](https://github.com/Avimitin/nvim/commit/6246353e))
  * packer: fix packer.nvim not load issue (See commit [`d7026cad`](https://github.com/Avimitin/nvim/commit/d7026cad))
  * init: fix plugins initialize function name (See commit [`9478d4d1`](https://github.com/Avimitin/nvim/commit/9478d4d1))
  * null-ls: fix wrong filetype keyword (See commit [`8e1edb93`](https://github.com/Avimitin/nvim/commit/8e1edb93))

Changes:
--------
  * options: decrease timeoutlen for responsive panel (See commit [`a743b220`](https://github.com/Avimitin/nvim/commit/a743b220))
  * keymap: move git related keymap to git module (See commit [`6f45d78a`](https://github.com/Avimitin/nvim/commit/6f45d78a))
  * commands: split commands into modules (See commit [`4d3cb014`](https://github.com/Avimitin/nvim/commit/4d3cb014))
  * packer: use event loop to detect packer file (See commit [`5275f32b`](https://github.com/Avimitin/nvim/commit/5275f32b))
  * packer: let load process panic on packadd fail (See commit [`701a03e1`](https://github.com/Avimitin/nvim/commit/701a03e1))

Others:
-------
  * docs: update documents (See commit [`c19faed8`](https://github.com/Avimitin/nvim/commit/c19faed8))
  * readme: add another installation way (See commit [`2d3ce315`](https://github.com/Avimitin/nvim/commit/2d3ce315))


v6.10.1 (Jul 4 2022)
====================

plugins:
-------
lspsaga: replace old lspsaga with glepnir original lspsaga
galaxyline: replace old galaxyline with glepnir original galaxyline


v6.10.0 (Jun 22 2022)
====================

BREAKING
--------
* nvim-cmp: simplify the completion menu style
* colors: remove nightfox

plugins:
--------
* packer: Compile on save is more robust now
* kanagawa: use lighter Pmenu highlight. Completion menu looks more distinguishable now.


v6.9.2 (Jun 22 2022)
====================

Plugins:
--------
* crates.nvim: fix completion menu source name
* vsnip: rewrite the Rust closure snippet

Key Mapping:
------------
* Add `<Ctrl-j>` and `<Ctrl-k>` for scrolling.

Options:
--------
* `conceallevel` is now set for `markdown` filetype only.
The double quote for `JSON` and `Dockerfile` will not be concealed now.


v6.9.1 (Jun 18 2022)
====================

Plugins:
--------

* Add null-ls.nvim and vale supports
* Add completion icon for crates.nvim
* Add trouble.nvim to show diagnostic
* Add shortline style for trouble.nvim
* Rewrite lspconfig and lsp-installer load sequences
* Remove redundant which-key descriptions

Options:
--------
* Add `rust` into lspconfig filetype list

v6.9.0 (Jun 13 2022)
===================

New Features:
-------------
  * plugin: add nvim-cmp support for crates.nvim (See commit [`e7551d20`](https://github.com/Avimitin/nvim/commit/e7551d20))
  * autocmd: add statusline auto resize command (See commit [`9782e99d`](https://github.com/Avimitin/nvim/commit/9782e99d))

Fix:
----
  * kanagawa: fix outdated options (See commit [`fe21cdf5`](https://github.com/Avimitin/nvim/commit/fe21cdf5))
  * nvim-tree: fix ignore key mapping (See commit [`5bfb530b`](https://github.com/Avimitin/nvim/commit/5bfb530b))
  * packer: fix packer UI freeze issues (See commit [`6162fdda`](https://github.com/Avimitin/nvim/commit/6162fdda))

Changes:
--------
  * nvim-tree: fix outdated options settings (See commit [`bd6ecea1`](https://github.com/Avimitin/nvim/commit/bd6ecea1))
  * neovide: add blur for neovide (See commit [`012205fa`](https://github.com/Avimitin/nvim/commit/012205fa))

Others:
-------
  * readme: update screenshot (See commit [`2219cbbf`](https://github.com/Avimitin/nvim/commit/2219cbbf))


v6.8.0 (May 26 2022)
===================

BREAKING CHANGES!:
------------------
  * plugin: remove any-jump.vim (See commit [`e2248977`](https://github.com/Avimitin/nvim/commit/e2248977))


v6.7.0 (May 26 2022)
===================

BREAKING CHANGES!:
------------------
  * lspsaga: remapping the `gd` key to `lsp_finder` action, mapping the `gp` key to preview definition (See commit [`f63140c9`](https://github.com/Avimitin/nvim/commit/f63140c9))
  * tag: use `vX.Y.Z[-rcN]` as git tag convention now


v6.6.2 (May 18 2022)
===================

New Features:
-------------
  * custom: add lspconfig filetype customization (See commit [`d547cada`](https://github.com/Avimitin/nvim/commit/d547cada))

Others:
-------
  * books: update lspconfig document (See commit [`c404dd92`](https://github.com/Avimitin/nvim/commit/c404dd92))
  * docs: add telescope document (See commit [`705a2ce1`](https://github.com/Avimitin/nvim/commit/705a2ce1))


v6.6.1 (May 18 2022)
===================

Changes:
--------
  * custom: move default settings to custom region (See commit [`31dcb0dd`](https://github.com/Avimitin/nvim/commit/31dcb0dd))


v6.6.0 (May 18 2022)
===================

BREAKING CHANGES!:
------------------
  * telescope: merge search keymaps (See commit [`998f1a58`](https://github.com/Avimitin/nvim/commit/998f1a58))

New Features:
-------------
  * telescope: add keymaps to trigger symbols search (See commit [`b9ff8685`](https://github.com/Avimitin/nvim/commit/b9ff8685))
  * treesitter: add more text objects (See commit [`8a70bc34`](https://github.com/Avimitin/nvim/commit/8a70bc34))
  * wildfire: append treesitter text objects into wildfire objects (See commit [`ef60df35`](https://github.com/Avimitin/nvim/commit/ef60df35))
  * vsnip: add closure snippet (See commit [`d614150d`](https://github.com/Avimitin/nvim/commit/d614150d))

Changes:
--------
  * lsp: rewrite the outdated lsp configuration (See commit [`58d04fd4`](https://github.com/Avimitin/nvim/commit/58d04fd4))
  * lightspeed: use more friendly icons for listchars (See commit [`5ae52cc2`](https://github.com/Avimitin/nvim/commit/5ae52cc2))

Others:
-------
  * readme: simplify the project description, add more pictures (See commit [`7885dcf8`](https://github.com/Avimitin/nvim/commit/7885dcf8))


v6.5.2 (May 14 2022)
===================

Fix:
----
  * mkd-preview: remove zero affect settings (See commit [`e16db969`](https://github.com/Avimitin/nvim/commit/e16db969))

Changes:
--------
  * lspsaga: replace code action icon (See commit [`8d89df1e`](https://github.com/Avimitin/nvim/commit/8d89df1e))
  * markdown: change preview plugin upstream (See commit [`95edb3ba`](https://github.com/Avimitin/nvim/commit/95edb3ba))


v6.5.1 (May 13 2022)
===================

New Features:
-------------
  * statusline: hide lsp server when empty (See commit [`483b4afe`](https://github.com/Avimitin/nvim/commit/483b4afe))

Changes:
--------
  * colors: modify the markdown heading highlight (See commit [`069cd32b`](https://github.com/Avimitin/nvim/commit/069cd32b))
  * rust: replace upstream (See commit [`0b120f3a`](https://github.com/Avimitin/nvim/commit/0b120f3a))


v6.5.0 (May 5 2022)
==================

BREAKING CHANGES!:
------------------
  * lspconfig: fix conflict key mapping (See commit [`c8106efe`](https://github.com/Avimitin/nvim/commit/c8106efe))
  * json: update json library (See commit [`4e64646b`](https://github.com/Avimitin/nvim/commit/4e64646b))

New Features:
-------------
  * whichkey: add lspconfig key description (See commit [`317d1c0f`](https://github.com/Avimitin/nvim/commit/317d1c0f))
  * whichkey: add more descriptions (See commit [`4fc5144d`](https://github.com/Avimitin/nvim/commit/4fc5144d))
  * which-key: add text object key descriptions (See commit [`de2234e5`](https://github.com/Avimitin/nvim/commit/de2234e5))
  * autocmd: add fcitx5 auto switcher (See commit [`6e0495d9`](https://github.com/Avimitin/nvim/commit/6e0495d9))
  * autocmd: hightlight the yanked text (See commit [`7e50204b`](https://github.com/Avimitin/nvim/commit/7e50204b))

Fix:
----
  * markdown: replace markdown preview upstream (See commit [`68833b9b`](https://github.com/Avimitin/nvim/commit/68833b9b))

Changes:
--------
  * which-key: remove useless blocklist (See commit [`48d728d4`](https://github.com/Avimitin/nvim/commit/48d728d4))
  * json: fix linting (See commit [`e7ed1389`](https://github.com/Avimitin/nvim/commit/e7ed1389))

Others:
-------
  * readme: add 0.8.0 alert (See commit [`02a1bbf4`](https://github.com/Avimitin/nvim/commit/02a1bbf4))
  * lspconfig: increase code readability (See commit [`02d14c9a`](https://github.com/Avimitin/nvim/commit/02d14c9a))
  * book: add insert mode keymapping document (See commit [`3c972328`](https://github.com/Avimitin/nvim/commit/3c972328))
  * book: add customize document (See commit [`8160db87`](https://github.com/Avimitin/nvim/commit/8160db87))


v6.4.1 (May 5 2022)
==================

Fix:
----
  * lsp: fix Lua language server settings (See commit [`59856ba3`](https://github.com/Avimitin/nvim/commit/59856ba3))

Changes:
--------
  * telescope: change find file theme to ivy (See commit [`69e09104`](https://github.com/Avimitin/nvim/commit/69e09104))


v6.4.0 (May 4 2022)
==================

BREAKING CHANGES!:
------------------
  * keys: fix conflict keymapping (See commit [`40d28348`](https://github.com/Avimitin/nvim/commit/40d28348))

New Features:
-------------
  * keys: add some motion key in insert mode (See commit [`d5e39e1d`](https://github.com/Avimitin/nvim/commit/d5e39e1d))

Changes:
--------
  * which-key: ignore some special key (See commit [`276b09db`](https://github.com/Avimitin/nvim/commit/276b09db))
  * lsp: use version compare to increase capability (See commit [`7e9ded3c`](https://github.com/Avimitin/nvim/commit/7e9ded3c))

Others:
-------
  * books: update key mapping document (See commit [`6f1b92f8`](https://github.com/Avimitin/nvim/commit/6f1b92f8))


v6.3.0 (May 4 2022)
==================

New Features:
-------------
  * plugin: add which-key to show keymappings (See commit [`b787d8e7`](https://github.com/Avimitin/nvim/commit/b787d8e7))

Changes:
--------
  * mapping: use dispatch to run git push (See commit [`3da9a0d1`](https://github.com/Avimitin/nvim/commit/3da9a0d1))

Others:
-------
  * readme: add which-key document (See commit [`98a68ad4`](https://github.com/Avimitin/nvim/commit/98a68ad4))


v6.2.2 (May 3 2022)
==================

Changes:
--------
  * vsnip: replace date provider with built-in strftime (See commit [`69d39368`](https://github.com/Avimitin/nvim/commit/69d39368))


v6.2.1 (May 3 2022)
==================

New Features:
-------------
  * plugin: add vim-mundo for undo history management (See commit [`e5a43baf`](https://github.com/Avimitin/nvim/commit/e5a43baf))

Others:
-------
  * book: replace <kbd> css with Apple magic keyboard style (See commit [`c3ace766`](https://github.com/Avimitin/nvim/commit/c3ace766))


v6.2.0 (May 2 2022)
====================================================================

Changes:
--------
  * rust: replace upstream (See commit [`e695763b`](https://github.com/Avimitin/nvim/commit/e695763b))


v6.1.0 (May 2 2022)
====================================================================

Changes:
--------
  * lsp: replace lspsaga upstream (See commit [`9b5e9f77`](https://github.com/Avimitin/nvim/commit/9b5e9f77))

Others:
-------
  * readme: add version reminder (See commit [`284775d7`](https://github.com/Avimitin/nvim/commit/284775d7))
  * readme: add API stablize hint (See commit [`1054482d`](https://github.com/Avimitin/nvim/commit/1054482d))


v6.0.0 (May 1 2022)
====================================================================

BREAKING CHANGES!:
------------------
  * keys: rewrite bufferline key mappings (See commit [`3b9918d9`](https://github.com/Avimitin/nvim/commit/3b9918d9))
  * plugin: remove repeat.vim (See commit [`ca761cfa`](https://github.com/Avimitin/nvim/commit/ca761cfa))
  * lspconfig: update deprecated options (See commit [`44e43da8`](https://github.com/Avimitin/nvim/commit/44e43da8))

Others:
-------
  * book: update nvim-cmp document (See commit [`611bd8f0`](https://github.com/Avimitin/nvim/commit/611bd8f0))
  * book: update bufferline document (See commit [`01456373`](https://github.com/Avimitin/nvim/commit/01456373))
  * book: merge and correct the lspconfig document (See commit [`260f2eae`](https://github.com/Avimitin/nvim/commit/260f2eae))
  * book: update capability hint (See commit [`de9525d4`](https://github.com/Avimitin/nvim/commit/de9525d4))
  * book: fix color section (See commit [`fa24c72a`](https://github.com/Avimitin/nvim/commit/fa24c72a))


v5.2.2 (Apr 30 2022)
====================================================================

New Features:
-------------
  * vsnip: add collase block snippet (See commit [`3d6b9556`](https://github.com/Avimitin/nvim/commit/3d6b9556))

Others:
-------
  * readme: add more descriptions (See commit [`ad791488`](https://github.com/Avimitin/nvim/commit/ad791488))


v5.2.1 (Apr 29 2022)
====================================================================

New Features:
-------------
  * rust: detect ".rust-analyzer.json" file per projects (See commit [`c246a6a8`](https://github.com/Avimitin/nvim/commit/c246a6a8))

Fix:
----
  * nvim-tree: update outdated options (See commit [`039f4f68`](https://github.com/Avimitin/nvim/commit/039f4f68))

Changes:
--------
  * rust: remove outdated configuration (See commit [`360e758d`](https://github.com/Avimitin/nvim/commit/360e758d))

Others:
-------
  * docs: fix colors custom theme (See commit [`2798cd97`](https://github.com/Avimitin/nvim/commit/2798cd97))
  * readme: add table mode demo (See commit [`661c57aa`](https://github.com/Avimitin/nvim/commit/661c57aa))


v5.2.0 (Apr 25 2022)
====================================================================
BREAKING CHANGES:
-----------------
  * keymaps: rewrite the telescope key mappings
  * vim-visual-multi: rewrite the key mappings

changes:
--------
  * commands: emebed flog commands into `Glog` and `GlogS`


v5.1.0 (Apr 25 2022)
====================================================================
New Features:
-------------
  * treesitter: add text object detection for function call

BREAKING CHANGES:
-----------------
  * treesitter: remove the ``move`` keymappings


v5.0.2 (Apr 25 2022)
====================================================================
Fix:
----
plugins:
  * nvim-tree: fix outdated options
  * nvim-cmp: fix keymaps


v5.0.1 (Apr 20 2022)
====================================================================
New Features:
-------------
  * rust: enable all features for analyzer (See commit [`54b564a6`](https://github.com/Avimitin/nvim/commit/54b564a6))

Others:
-------
  * docs: add configuration structure description (See commit [`cfe9e82b`](https://github.com/Avimitin/nvim/commit/cfe9e82b))
  * readme: add incompatible notice (See commit [`8db142cb`](https://github.com/Avimitin/nvim/commit/8db142cb))


v5.0.0 (Apr 20 2022)
====================================================================
BREAKING CHANGES!:
------------------
  * nvim: implement new neovim v0.7.0 Lua API (See commit [`0f24a99a`](https://github.com/Avimitin/nvim/commit/0f24a99a))
  * scripts: rename fixtures to script (See commit [`0320de72`](https://github.com/Avimitin/nvim/commit/0320de72))

New Features:
-------------
  * autocmd: use new api for autocommands (See commit [`d93b9455`](https://github.com/Avimitin/nvim/commit/d93b9455))
  * commands: use new api to define command (See commit [`c6aaca63`](https://github.com/Avimitin/nvim/commit/c6aaca63))
  * keys: add more description for keys (See commit [`8a637246`](https://github.com/Avimitin/nvim/commit/8a637246))
  * treesitter: enable more filetype (See commit [`103cf557`](https://github.com/Avimitin/nvim/commit/103cf557))
  * vsnip: create snippet for cargo.toml (See commit [`e9d7b42e`](https://github.com/Avimitin/nvim/commit/e9d7b42e))

Changes:
--------
  * scripts: grant execute permission for perl script (See commit [`90b61c50`](https://github.com/Avimitin/nvim/commit/90b61c50))
  * ci: update test script (See commit [`2eb73b0e`](https://github.com/Avimitin/nvim/commit/2eb73b0e))


v4.0.2 (Apr 15 2022)
====================================================================
Fix:
----
  * treesitter: enable fish (See commit [`17d17827`](https://github.com/Avimitin/nvim/commit/17d17827))


Others:
-------
  * changelog: release 4.0.1 (See commit [`35adeb1f`](https://github.com/Avimitin/nvim/commit/35adeb1f))
  * Fix linting error


v4.0.1 (Apr 13 2022)
====================================================================

New Features:
-------------
  * rust: add key mapping for rust (See commit [`a4b6f266`](https://github.com/Avimitin/nvim/commit/a4b6f266))
  * plugins: enable nix for treesitter (See commit [`8c4a7e47`](https://github.com/Avimitin/nvim/commit/8c4a7e47))
  * statusline: add colorscheme for dawnfox (See commit [`dffd2b36`](https://github.com/Avimitin/nvim/commit/dffd2b36))
  * galaxyline: adjust the github light theme (See commit [`231cdb79`](https://github.com/Avimitin/nvim/commit/231cdb79))
  * colorscheme: add github colorscheme (See commit [`7f773e14`](https://github.com/Avimitin/nvim/commit/7f773e14))
  * colorscheme: add color set nightfox (See commit [`ec1a81c4`](https://github.com/Avimitin/nvim/commit/ec1a81c4))
  * commands: add command for highlight multiple lines (See commit [`2b3686f6`](https://github.com/Avimitin/nvim/commit/2b3686f6))
  * mappings: add terminal keymapping (See commit [`4a7aacc5`](https://github.com/Avimitin/nvim/commit/4a7aacc5))
  * nvim-cmp: add load trigger for key ":" and "/" (See commit [`b5e56203`](https://github.com/Avimitin/nvim/commit/b5e56203))

Fix:
----
  * vimrc: fix invalid listchars (See commit [`605d02a8`](https://github.com/Avimitin/nvim/commit/605d02a8))
  * colorscheme: fix colorscheme not loading issue (See commit [`08f8f674`](https://github.com/Avimitin/nvim/commit/08f8f674))

Changes:
--------
  * colorscheme: move default value inside table (See commit [`6429755a`](https://github.com/Avimitin/nvim/commit/6429755a))
  * neovide: use monospace for flexible font choice (See commit [`5dce1f8d`](https://github.com/Avimitin/nvim/commit/5dce1f8d))
  * nvim-cmp: change the source name to Vim (See commit [`41d370f0`](https://github.com/Avimitin/nvim/commit/41d370f0))
  * treesitter: remove markdown regex (See commit [`f910c12b`](https://github.com/Avimitin/nvim/commit/f910c12b))
  * kanagawa: enable inactive window dim color (See commit [`e3a22e01`](https://github.com/Avimitin/nvim/commit/e3a22e01))

Others:
-------
  * books: add document for how to switch nightmode automatically (See commit [`47cf5f59`](https://github.com/Avimitin/nvim/commit/47cf5f59))
  * colorscheme: update colorscheme document (See commit [`b95d897f`](https://github.com/Avimitin/nvim/commit/b95d897f))
  * plugins: add component document (See commit [`fcefe899`](https://github.com/Avimitin/nvim/commit/fcefe899))


v4.0.0 (Mar 12 2022)
====================================================================
In this version, I rewrite the whole configuration structure.
This is how it looks like now::

    lua
    ├── core             ==> editor config
    │   ├── autocmd.lua  ==> autocommands
    │   ├── colors.lua   ==> colorscheme
    │   ├── commands.lua ==> user commands
    │   ├── options.lua  ==> editor settings
    │   └── utils.lua    ==> my helper function
    ├── mappings         ==> KEYMAPPINGS
    │   ├── init.lua     ==> editor keymappings
    │   ├── other.lua    ==> plugins keymappings
    │   └── utils.lua    ==> keymapping functions
    └── plugins
        ├── bufdel.lua   ==> buffer delete plugin
        ├── config       ==> plugins config
        │   └── ...
        ├── init.lua     ==> set up packer
        ├── load.lua     ==> load all plugins definition
        └── options.lua  ==> plugins settings (load before plugins)

Others:
-------

* The commit convention is now been updated. Please read [development specific]
for the commit meaning.

* nvim-lspconfig and treesitter is now only enabled for limited filetype.
Please update the filetype in `lua/plugins/options.lua`.

* I am now using [focus.nvim] to auto resize window. If you feel weird about the
resize behavior, feel free to turn this plugin off.

* All git related keymap are re-mapping with `gi` prefix. E.g `gic` for git commit.

* Everforest, gruvbox and ayu colorschemes are removed.

* Documents are now rewriting as tutorial. It is still work in progress.
My future goal is to finish all the tutorial about the workflow I use with neovim.

[development specific]: https://avimitin.github.io/nvim/en_us/development.html
[focus.nvim]: https://github.com/beauwilliams/focus.nvim

Breaking Changes:
-----------------
  * lspconfig: move lsp_attach to lspconfig (See commit [`bcc3ca70`](https://github.com/Avimitin/nvim/commit/bcc3ca70))
  * plugins: remove vim-lua-format (See commit [`f0ee2689`](https://github.com/Avimitin/nvim/commit/f0ee2689))
  * plugins: rename all plugin config suffix with cfg (See commit [`86706692`](https://github.com/Avimitin/nvim/commit/86706692))
  * format: replace luaformat with stylua (See commit [`51bb5119`](https://github.com/Avimitin/nvim/commit/51bb5119))
  * reft!/keymap: remove wildfire range keymap (See commit [`25b4e453`](https://github.com/Avimitin/nvim/commit/25b4e453))
  * reft!/plugin: remove orgmode (See commit [`fc94ef62`](https://github.com/Avimitin/nvim/commit/fc94ef62))
  * gitsigns: update git sign keymaps (See commit [`abf53d99`](https://github.com/Avimitin/nvim/commit/abf53d99))
  * commands: remove fugitive command alias (See commit [`f4a45ba4`](https://github.com/Avimitin/nvim/commit/f4a45ba4))
  * lspinstall: remove some ensure install server (See commit [`66ef74e4`](https://github.com/Avimitin/nvim/commit/66ef74e4))
  * colorscheme: remove colorschemes (See commit [`bf3f7a21`](https://github.com/Avimitin/nvim/commit/bf3f7a21))
  * plugins: move prehook settings to options.lua (See commit [`b91fec41`](https://github.com/Avimitin/nvim/commit/b91fec41))


New Features:
-------------
plugins:
  * plugins: add cmp-cmdline (See commit [`1cbe60ce`](https://github.com/Avimitin/nvim/commit/1cbe60ce))
  * nvim-cmp: add cmdline completion (See commit [`4d092550`](https://github.com/Avimitin/nvim/commit/4d092550))
  * markdown: enable latex math conceal (See commit [`7e2aee94`](https://github.com/Avimitin/nvim/commit/7e2aee94))
  * plugins: add winshift.nvim to handle window movement (See commit [`b019cc99`](https://github.com/Avimitin/nvim/commit/b019cc99))
  * plugins: extend text object by treesitter (See commit [`36526ed4`](https://github.com/Avimitin/nvim/commit/36526ed4))
  * plugins: add dressing.nvim to pretty the vim.ui.select panel (See commit [`a71358df`](https://github.com/Avimitin/nvim/commit/a71358df))
  * plugins: add focus.nvim for split window (See commit [`9a879728`](https://github.com/Avimitin/nvim/commit/9a879728))
  * nvim-cmp: add cmp source dictionary (See commit [`4113b285`](https://github.com/Avimitin/nvim/commit/4113b285))
  * plugins: use a global filetype settings for treesitter and lspconfig (See commit [`36e37358`](https://github.com/Avimitin/nvim/commit/36e37358))
  * rust: enable variable name in inlay hint (See commit [`2f3b25f7`](https://github.com/Avimitin/nvim/commit/2f3b25f7))
  * keymap: add `;d` for dispatch (See commit [`4619fe33`](https://github.com/Avimitin/nvim/commit/4619fe33))
  * plugin: add plugin to view git log (See commit [`921b5817`](https://github.com/Avimitin/nvim/commit/921b5817))
  * plugin: add plugin to show lsp loading status (See commit [`c4d2f8df`](https://github.com/Avimitin/nvim/commit/c4d2f8df))
  * plugin: add rest.nvim (See commit [`60163157`](https://github.com/Avimitin/nvim/commit/60163157))
  * vsnip: add fronmatter snippet (See commit [`e6b997c5`](https://github.com/Avimitin/nvim/commit/e6b997c5))
  * plugin: add nvim-notify (See commit [`3e7f46e9`](https://github.com/Avimitin/nvim/commit/3e7f46e9))
  * plugins: add bufdel.lua (See commit [`c23be93b`](https://github.com/Avimitin/nvim/commit/c23be93b))
  * plugins: add impatient.nvim to optimize the start up time (See commit [`4ea10f41`](https://github.com/Avimitin/nvim/commit/4ea10f41))
  * nvim-cmp: add load trigger for key ":" and "/" (See commit [`b5e56203`](https://github.com/Avimitin/nvim/commit/b5e56203))
  * nvim-cmp: use special format for cmdline cmp (See commit [`c542b111`](https://github.com/Avimitin/nvim/commit/c542b111))
  * plugins: add spetre to enhance search and replace (See commit [`852bebcc`](https://github.com/Avimitin/nvim/commit/852bebcc))
  * plugins: add neoscroll to scroll smoothly (See commit [`c2ae4bff`](https://github.com/Avimitin/nvim/commit/c2ae4bff))
  * treesitter: enable matchup in treesitter (See commit [`23a9eab4`](https://github.com/Avimitin/nvim/commit/23a9eab4))
  * plugins: use toggleterm instead of fterm (See commit [`c9c22f11`](https://github.com/Avimitin/nvim/commit/c9c22f11))

autocmd:
  * autocmd: add rnu and nu autocmd (See commit [`2e535362`](https://github.com/Avimitin/nvim/commit/2e535362))

FIX:
----
  * nvim-tree: fix auto resize when open file (See commit [`91bccbcc`](https://github.com/Avimitin/nvim/commit/91bccbcc))
  * lsp: use spacing option to fix the nerdfont align issue (See commit [`11c01a99`](https://github.com/Avimitin/nvim/commit/11c01a99))
  * options: set no conceal for normal mode (See commit [`a86c47a8`](https://github.com/Avimitin/nvim/commit/a86c47a8))
  * plugins: fix vim-rooter (See commit [`6291602f`](https://github.com/Avimitin/nvim/commit/6291602f))
  * plugins: set up autocmd before packer sync (See commit [`978dc724`](https://github.com/Avimitin/nvim/commit/978dc724))

Changes:
--------
  * kanagawa: use darker pmenu highlight (See commit [`3209e838`](https://github.com/Avimitin/nvim/commit/3209e838))
  * kanagawa: use default float highlight (See commit [`ff06f4fb`](https://github.com/Avimitin/nvim/commit/ff06f4fb))
  * kanagawa: enable inactive window dim color (See commit [`e3a22e01`](https://github.com/Avimitin/nvim/commit/e3a22e01))
  * colors: remove redundant highlight group (See commit [`60943299`](https://github.com/Avimitin/nvim/commit/60943299))
  * nvim-cmp: use local icon list for completion (See commit [`08941738`](https://github.com/Avimitin/nvim/commit/08941738))
  * nvim-cmp: increase the lspconfig priority (See commit [`69b358c0`](https://github.com/Avimitin/nvim/commit/69b358c0))


v4.0.0-rc5 (Mar 09 2022)
====================================================================

New Features:
-------------
  * plugins: add cmp-cmdline (See commit [`1cbe60ce`](https://github.com/Avimitin/nvim/commit/1cbe60ce))
  * nvim-cmp: add cmdline completion (See commit [`4d092550`](https://github.com/Avimitin/nvim/commit/4d092550))
  * markdown: enable latex math conceal (See commit [`7e2aee94`](https://github.com/Avimitin/nvim/commit/7e2aee94))

Fix:
----
  * plugins: fix vim-rooter (See commit [`6291602f`](https://github.com/Avimitin/nvim/commit/6291602f))
  * plugins: set up autocmd before packer sync (See commit [`978dc724`](https://github.com/Avimitin/nvim/commit/978dc724))

Changes:
--------
  * plugins: re-categorize treesitter plugin (See commit [`562dbe6d`](https://github.com/Avimitin/nvim/commit/562dbe6d))
  * plugins: move treesitter and lspconfig filetype setting to options.lua (See commit [`f1886fba`](https://github.com/Avimitin/nvim/commit/f1886fba))
  * plugins: remove prehook and posthook function (See commit [`9f7bcff4`](https://github.com/Avimitin/nvim/commit/9f7bcff4))
  * nvim-cmp: use local icon list for completion (See commit [`08941738`](https://github.com/Avimitin/nvim/commit/08941738))
  * nvim-cmp: remove orgmode completion (See commit [`3b325147`](https://github.com/Avimitin/nvim/commit/3b325147))
  * nvim-cmp: increase the lspconfig priority (See commit [`69b358c0`](https://github.com/Avimitin/nvim/commit/69b358c0))
  * treesitter: remove markdown support (See commit [`5ca5f4fb`](https://github.com/Avimitin/nvim/commit/5ca5f4fb))
  * book: update the commit convention (See commit [`0e71dbb3`](https://github.com/Avimitin/nvim/commit/0e71dbb3))

Others:
-------
  * benchmark: update benchmark (See commit [`8b023f93`](https://github.com/Avimitin/nvim/commit/8b023f93))
  * book: add lspconfig tutorial (See commit [`2074ccd2`](https://github.com/Avimitin/nvim/commit/2074ccd2))
  * book: add workflow about the windows (See commit [`b056c7b4`](https://github.com/Avimitin/nvim/commit/b056c7b4))
  * readme: fix the vimrc link (See commit [`751898d9`](https://github.com/Avimitin/nvim/commit/751898d9))
  * genlog: rewrite the log generate script for new commit convention (See commit [`b095dad9`](https://github.com/Avimitin/nvim/commit/b095dad9))


v4.0.0-rc4 (Mar 05 2022)
====================================================================

BREAKING CHANGES!:
------------------
  * lspinstall: remove some ensure install server (See commit [`66ef74e4`](https://github.com/Avimitin/nvim/commit/66ef74e4))
  * colorscheme: remove colorschemes (See commit [`bf3f7a21`](https://github.com/Avimitin/nvim/commit/bf3f7a21))
  * plugins: move prehook to options.lua (See commit [`b91fec41`](https://github.com/Avimitin/nvim/commit/b91fec41))

New Features:
-------------
  * vsnip: add tag snippet for markdown (See commit [`5e5dc265`](https://github.com/Avimitin/nvim/commit/5e5dc265))
  * plugins: use a global filetype settings for treesitter and lspconfig (See commit [`36e37358`](https://github.com/Avimitin/nvim/commit/36e37358))
  * nvim-cmp: add cmp source dictionary (See commit [`4113b285`](https://github.com/Avimitin/nvim/commit/4113b285))
  * plugins: add winshift.nvim to handle window movement (See commit [`b019cc99`](https://github.com/Avimitin/nvim/commit/b019cc99))
  * plugins: extend text object by treesitter (See commit [`36526ed4`](https://github.com/Avimitin/nvim/commit/36526ed4))
  * plugins: add dressing.nvim to pretty the vim.ui.select panel (See commit [`a71358df`](https://github.com/Avimitin/nvim/commit/a71358df))
  * plugins: add focus.nvim for split window (See commit [`9a879728`](https://github.com/Avimitin/nvim/commit/9a879728))
  * plugins: use defer to delay plugins module loading (See commit [`87ffff14`](https://github.com/Avimitin/nvim/commit/87ffff14))

Fix:
----
  * mappings: update outdated term keymap (See commit [`37b60a20`](https://github.com/Avimitin/nvim/commit/37b60a20))

Changes:
--------
  * autocmd: move TermOpen command to autocmd.lua file (See commit [`e80d81ef`](https://github.com/Avimitin/nvim/commit/e80d81ef))
  * treesitter: remove org file (See commit [`a323dba4`](https://github.com/Avimitin/nvim/commit/a323dba4))
  * mappings: add bang to the git push key (See commit [`5091ab3f`](https://github.com/Avimitin/nvim/commit/5091ab3f))
  * plugins: use module to lazy-load notify (See commit [`219df876`](https://github.com/Avimitin/nvim/commit/219df876))
  * options: set no conceal for normal mode (See commit [`a86c47a8`](https://github.com/Avimitin/nvim/commit/a86c47a8))
  * plugins: expose load cfg as a public function (See commit [`1931be24`](https://github.com/Avimitin/nvim/commit/1931be24))

Others:
-------
  * readme: fix vimrc download link (See commit [`868e665c`](https://github.com/Avimitin/nvim/commit/868e665c))
  * benchmark: update benchmark (See commit [`20ccc06f`](https://github.com/Avimitin/nvim/commit/20ccc06f))
  * book: remove colorscheme description (See commit [`1c99b7b1`](https://github.com/Avimitin/nvim/commit/1c99b7b1))
  * book: finish the git workflow pages (See commit [`bd72ab27`](https://github.com/Avimitin/nvim/commit/bd72ab27))


v4.0.0-rc3 (Mar 05 2022)
====================================================================

BREAKING CHANGES!
------------------
  * gitsigns: update git sign keymaps (See commit [`abf53d99`](https://github.com/Avimitin/nvim/commit/abf53d99))
  * commands: remove fugitive command alias (See commit [`f4a45ba4`](https://github.com/Avimitin/nvim/commit/f4a45ba4))

New Features:
-------------
  * plugins: add spetre to enhance search and replace (See commit [`852bebcc`](https://github.com/Avimitin/nvim/commit/852bebcc))
  * plugins: add neoscroll to scroll smoothly (See commit [`c2ae4bff`](https://github.com/Avimitin/nvim/commit/c2ae4bff))
  * keymap: add keymap for fugitive (See commit [`c1c9e66b`](https://github.com/Avimitin/nvim/commit/c1c9e66b))
  * everforest: enable virtual text highlight (See commit [`d506a1e6`](https://github.com/Avimitin/nvim/commit/d506a1e6))
  * treesitter: enable matchup in treesitter (See commit [`23a9eab4`](https://github.com/Avimitin/nvim/commit/23a9eab4))

Changes:
--------
  * plugins: replace terminal plugin (See commit [`c9c22f11`](https://github.com/Avimitin/nvim/commit/c9c22f11))

Others:
-------
  * book: add fugitive workflows (See commit [`8e66b190`](https://github.com/Avimitin/nvim/commit/8e66b190))
  * genlog: fix feature list reference (See commit [`2d1a0c69`](https://github.com/Avimitin/nvim/commit/2d1a0c69))


v4.0.0-rc2 (Mar 02 2022)
====================================================================

Fix:
----
  * nvim-tree: fix auto resize when open file (See commit [`91bccbcc`](https://github.com/Avimitin/nvim/commit/91bccbcc))
  * lsp: use spacing option to fix the nerdfont align issue (See commit [`11c01a99`](https://github.com/Avimitin/nvim/commit/11c01a99))
  * benchmark: fix file handler warning (See commit [`ea34ca7d`](https://github.com/Avimitin/nvim/commit/ea34ca7d))

Changes:
--------
  * plugins: update nvim-tree icon (See commit [`2c78214e`](https://github.com/Avimitin/nvim/commit/2c78214e))
  * plugins: rewrite neocilp load condition (See commit [`240b8e0e`](https://github.com/Avimitin/nvim/commit/240b8e0e))

Others:
-------
  * Use perl to generate changelog and run benchmark script
    (See fixtures/genlog.pl and benmark.pl)


v4.0.0-rc1 (Feb 26 2022)
====================================================================
In this version, I rewrite the whole configuration structure and simplify
some plugins.

Also I am now using [stylua] to format the Lua code>

Besides, I am using [impatient.nvim] to cache the neovim modules.
So remember to call the function `:LuaCacheClear` after you modify the neovim
configuration.

[stylua]: https://github.com/johnnymorganz/stylua
[impatient.nvim]: https://github.com/lewis6991/impatient.nvim


New Features:
-------------
plugins:
  * rust: enable variable name in inlay hint (See commit [`2f3b25f7`](https://github.com/Avimitin/nvim/commit/2f3b25f7))
  * keymap: add `;d` for dispatch (See commit [`4619fe33`](https://github.com/Avimitin/nvim/commit/4619fe33))
  * plugin: add plugin to view git log (See commit [`921b5817`](https://github.com/Avimitin/nvim/commit/921b5817))
  * plugin: add plugin to show lsp loading status (See commit [`c4d2f8df`](https://github.com/Avimitin/nvim/commit/c4d2f8df))
  * plugin: add rest.nvim (See commit [`60163157`](https://github.com/Avimitin/nvim/commit/60163157))
  * vsnip: add fronmatter snippet (See commit [`e6b997c5`](https://github.com/Avimitin/nvim/commit/e6b997c5))
  * plugin: add nvim-notify (See commit [`3e7f46e9`](https://github.com/Avimitin/nvim/commit/3e7f46e9))
  * plugins: add bufdel.lua (See commit [`c23be93b`](https://github.com/Avimitin/nvim/commit/c23be93b))
  * plugins: add impatient.nvim to optimize the start up time (See commit [`4ea10f41`](https://github.com/Avimitin/nvim/commit/4ea10f41))

autocmd:
  * autocmd: add rnu and nu autocmd (See commit [`2e535362`](https://github.com/Avimitin/nvim/commit/2e535362))

keymap:
  * keymap: separate the editor keymap and plugin keymap (See commit [`87709236`](https://github.com/Avimitin/nvim/commit/87709236))

Breaking Changes:
-----------------
  * plugins: rewrite the plugin file structure (See commit [`dfd2f18a`](https://github.com/Avimitin/nvim/commit/dfd2f18a))
  * lspconfig: move lsp_attach to lspconfig (See commit [`bcc3ca70`](https://github.com/Avimitin/nvim/commit/bcc3ca70))
  * commands: move utils.new_cmd to commands.alias (See commit [`9fe3ad46`](https://github.com/Avimitin/nvim/commit/9fe3ad46))
  * plugins: remove vim-lua-format (See commit [`f0ee2689`](https://github.com/Avimitin/nvim/commit/f0ee2689))
  * plugins: remove setup function for flog (See commit [`df90cdc2`](https://github.com/Avimitin/nvim/commit/df90cdc2))
  * plugins: rename all plugin config suffix with cfg (See commit [`86706692`](https://github.com/Avimitin/nvim/commit/86706692))
  * plugins: move config dir into plugins dir (See commit [`bb16ba1a`](https://github.com/Avimitin/nvim/commit/bb16ba1a))
  * format: replace luaformat with stylua (See commit [`51bb5119`](https://github.com/Avimitin/nvim/commit/51bb5119))
  * nvim: move utils.map to mappings.utils.map (See commit [`49de74ee`](https://github.com/Avimitin/nvim/commit/49de74ee))
  * nvim: move neovim settings to core directory (See commit [`6137b27d`](https://github.com/Avimitin/nvim/commit/6137b27d))
  * reft!/keymap: remove wildfire range keymap (See commit [`25b4e453`](https://github.com/Avimitin/nvim/commit/25b4e453))
  * reft!/plugin: remove orgmode (See commit [`fc94ef62`](https://github.com/Avimitin/nvim/commit/fc94ef62))


3.17.0 (Feb 07 2022)
====================================================================

BREAKING CHANGES:
-----------------
keymaps:
  * bufferline: rewrite bufferline keymap (See commit [`c5be031e`](https://github.com/Avimitin/nvim/commit/c5be031e))
  * fugitive: rewrite the fugitive keymap (See commit [`e9c44d93`](https://github.com/Avimitin/nvim/commit/e9c44d93))
  * keymap: rewrite the telescope keymap (See commit [`8cd716a5`](https://github.com/Avimitin/nvim/commit/8cd716a5))

plugins:
  * plugin: remove neogen (See commit [`96694e6b`](https://github.com/Avimitin/nvim/commit/96694e6b))
  * plugin: replace Hexokinase with colorizer (See commit [`455ab684`](https://github.com/Avimitin/nvim/commit/455ab684))

changes:
--------
* plugin: add new plugin tpope/vim-sleuth (See commit [`697cc6ab`](https://github.com/Avimitin/nvim/commit/697cc6ab))
* plugin: update the autocmd filename (See commit [`44da667f`](https://github.com/Avimitin/nvim/commit/44da667f))
* plugin: add diffview.nvim (See commit [`c49688f3`](https://github.com/Avimitin/nvim/commit/c49688f3))
* plugin: add vim-repeat (See commit [`8f618162`](https://github.com/Avimitin/nvim/commit/8f618162))
* dispatch: add key trigger (See commit [`e4c10789`](https://github.com/Avimitin/nvim/commit/e4c10789))


3.16.0 (Jan 26 2022)
====================================================================

BREAKING CHANGES:
-----------------
keymaps:
  * remap the Sayonara command (See commit [`a7282480`](https://github.com/Avimitin/nvim/commit/a7282480))
  * integrate the `;` key (See commit [`27fa7637`](https://github.com/Avimitin/nvim/commit/27fa7637))
  * rewrite the window navigate keys (See commit [`460a5137`](https://github.com/Avimitin/nvim/commit/460a5137))
  * fix the multi cursor keymap conflict (See commit [`5c81c484`](https://github.com/Avimitin/nvim/commit/5c81c484))
  * rewrite the nvim tree keymap (See commit [`47d4432d`](https://github.com/Avimitin/nvim/commit/47d4432d))

plugins:
  * markdown: update the vim-markdown config (See commit [`3e57ba13`](https://github.com/Avimitin/nvim/commit/3e57ba13))


3.15.0 (Jan 24 2022)
====================================================================

This version rewrite the plugins structure. Now all plugins are classified and put into the
lua/partial directory.

BREAKING CHANGES:
-----------------
* nvim-cmp source rg had been removed due to the performance problem (See commit [`3dd333d6`](https://github.com/Avimitin/nvim/commit/3dd333d6))
* vim-commentary is replaced by Comment.nvim due to absence of maintenance (See commit [`6af6af2b`](https://github.com/Avimitin/nvim/commit/6af6af2b))
* vim-startuptime and focus.nvim is removed as they are rarely used (See commit [`ead4715c`](https://github.com/Avimitin/nvim/commit/ead4715c))

Changes:
--------
* plugins: separate plugins to the lua/partial directory. (See commit b87f2efa to ead4715c)
* vim-surround: rewrite the conditions of the plugin call (See commit [`3fe64080`](https://github.com/Avimitin/nvim/commit/3fe64080))
* orgmode: replace the default file path (See commit [`cda57c67`](https://github.com/Avimitin/nvim/commit/cda57c67))
* keymap: add keymap for wildfire quick select functionality(See commit [`87a72276`](https://github.com/Avimitin/nvim/commit/87a72276))
* feat/vsnip: add snippet for orgmode (See commit [`d86a30c6`](https://github.com/Avimitin/nvim/commit/d86a30c6))
* reft/keymap: remove the vfiler keymap (See commit [`577a520f`](https://github.com/Avimitin/nvim/commit/577a520f))


-----------------------------------------------------------------------------------------


3.14.7 (Jan 21 2022)
==================================================================

The installation guide is now moved into the book:
https://avimitin.github.io/nvim

Changes:
--------
* Snippet:
  - add snippet "<kbd>" (See commit [`0743464d`](https://github.com/Avimitin/nvim/commit/0743464d))
  - add custom markdown snippets (See commit [`b32aa8c4`](https://github.com/Avimitin/nvim/commit/b32aa8c4))
* Plugins:
  - feat/plugin: add new plugin nvim-orgmode (See commit [`6e47dcc3`](https://github.com/Avimitin/nvim/commit/6e47dcc3))
  - treesitter: disable treesitter lazy-loading (See commit [`47cb9ca5`](https://github.com/Avimitin/nvim/commit/47cb9ca5))
  - lazygit: update the lazygit to upstream version (See commit [`abcefe9f`](https://github.com/Avimitin/nvim/commit/abcefe9f))
  - wildfire: add new text object for inline code (See commit [`14374bbc`](https://github.com/Avimitin/nvim/commit/14374bbc))
* Options:
  - options: rewrite the tab listchars (See commit [`ed506d03`](https://github.com/Avimitin/nvim/commit/ed506d03))


-----------------------------------------------------------------------------------------


3.14.6
======

Changes:
--------
* commands: add commands for crates.nvim (See commit [`253794e7`](https://github.com/Avimitin/nvim/commit/253794e7))
* plugins: add new plugin "crates.nvim" (See commit [`b8c04f4e`](https://github.com/Avimitin/nvim/commit/b8c04f4e))


-----------------------------------------------------------------------------------------


3.14.5
======

Changes:
--------

* colors: add colorscheme gruvbox (See commit [`0ad3b12c`](https://github.com/Avimitin/nvim/commit/0ad3b12c))
* colors: add new colorscheme "everforest" (See commit [`9c6def1a`](https://github.com/Avimitin/nvim/commit/9c6def1a))
* colors: add lazyload for all colors (See commit [`52fc2681`](https://github.com/Avimitin/nvim/commit/52fc2681))
* plugins: add lazyload for lightspeed and matchup (See commit [`99fb3261`](https://github.com/Avimitin/nvim/commit/99fb3261))


-----------------------------------------------------------------------------------------


3.14.4
======
I am working on the new document: https://avimitin.github.io/nvim.
The basic keymap section is finished now.

Breaking Chnages:
----------------
* keymap: add ctrl as prefix key when resize (See commit [`b545fafe`](https://github.com/Avimitin/nvim/commit/b545fafe))
* keymap: use arrow key to navigate between windows (See commit [`70576e9d`](https://github.com/Avimitin/nvim/commit/70576e9d))
* keymap: delete the duplicate new tab keymap, only `<C-t> n` is kept (See commit [`6369ca63`](https://github.com/Avimitin/nvim/commit/6369ca63))
* keymap: set `gf` to buffer format, set `gq` to set_loclist, set `gt` to...
  ...type_definition (See commit 346bfe58 and 3111d496)

Features:
---------
* plugin: add plugin vim-startuptime, you can see the startuptime graph by...
  ...command `:StartupTime` (See commit [`7018a3e3`](https://github.com/Avimitin/nvim/commit/7018a3e3))
* command: add `Cmt` to make a commit, add `Psh` to push changes. (See commit [`8b9d959a`](https://github.com/Avimitin/nvim/commit/8b9d959a))
* plugin: add new plugin FixCursorHold.nvim to fix the laggy event (See commit [`0164a37d`](https://github.com/Avimitin/nvim/commit/0164a37d))


-----------------------------------------------------------------------------------------


3.14.0
======
Some plugin had been replaced in this release. But their functionality is still
keep the same. Like the command :SudaWrite is just replaced by :SudoWrite.

Breaking Changes
----------------
* plugin: replace neoterm with dispather
* plugin: replace suda.vim with vim-eunuch
* plugin: remove rust-lang/rust.vim plugin

Features
--------
* plugin: add neogen to help generating annotation
* option: add rust fmt option
* plugin: add splitjoin.vim

Changes
--------
* lspsaga: move lspsaga settings under lua/config
* lspsaga: replace icon to nerd font icon
* lsp: rewrite the lspconfig code
* keymap: fix lsp keymap conflict
* plugin: add more filetype for treesitter
* option: increase nvui font size
* option: set fallbackfont for nvui
* option: enable settings only when neovide open


v3.13.0
======


Breaking Changes
----------------
* keymap: remove keymap for go plugin


Features
--------
* lsp: integrate lspsaga into lspconfig
* plugin: add nerd font icon for vfiler.vim


Fix
---
* keymap: fix <C-P> cursor place


Changes
--------
* color: set type style to bold
* plugin: disable treesitter in neorg
* colors: set normal float background to blue


v3.12.0
==========

Breaking Changes
----------------
* plugin: remove plugin neorg

Changes
--------
* plugin: set bufferline separator to 'slant'
* color: set `TODO` bg to samuraiRed


v3.11.2
==========

Changes
--------
* colorscheme: update markdown hightlight group


v3.11.1
==============

Features
--------
* plugin: add plugin vim-matchup


v3.11.0
==========

Breaking Changes
----------------
* plugin: delete fterm lazygit setting (See commit [`5f2cd7ae`](https://github.com/Avimitin/nvim/commit/5f2cd7ae))


Features
--------
* plugin: add new plugin lazygit.nvim (See commit [`7c0c2dfe`](https://github.com/Avimitin/nvim/commit/7c0c2dfe))
* nvim-cmp: add square bracket in menu (See commit [`563bf2fb`](https://github.com/Avimitin/nvim/commit/563bf2fb))
* options: set fileencoding to utf-8 forcely


Changes
--------
* options: set tab char as "->" (See commit [`c2994e5a`](https://github.com/Avimitin/nvim/commit/c2994e5a))


v3.10.0
=============

Features
========
* plugin: add new plugin neorg (See commit [`c5fcd4b`](https://github.com/Avimitin/nvim/commit/c5fcd4b))

Fix
===
* lsp: replace show line diagnostic api (See commit [`d4d290b`](https://github.com/Avimitin/nvim/commit/d4d290b))
* plugin: fix nvim-cmp hook (See commit [`fd44872`](https://github.com/Avimitin/nvim/commit/fd44872))
* plugin: fix luaformat load sequence (See commit [`f2780c1`](https://github.com/Avimitin/nvim/commit/f2780c1))

Chore
=====
* colors: use kanagawa as default theme (See commit [`f2c2890`](https://github.com/Avimitin/nvim/commit/f2c2890))

v3.9.11
=============

This update contains commit convention update.
Please read: https://commit-convention.sh1mar.in/ for details.

Breaking Changes
================
* plugin: replace file picker plugin (4662acb)
* keymap: update lazygit keymap (2b99966)
* plugin: remove neogit (1232b1d)
* plugins: remove famiu/nvim-reload (12c3db5)
* escape: replace `jj` with `<A-;>` (d7bdd1c)

Features
========
* statusline: add short line icon for fern and neoterm (See commit 0e7def8 and ab2160a)
* plugins: add plugin vim-table-mode (See commit [`faebcd9`](https://github.com/Avimitin/nvim/commit/faebcd9))
* command: add new command `Glog` (See commit [`e6e0c4c`](https://github.com/Avimitin/nvim/commit/e6e0c4c))

Rewrite
=======
* fterm: update fterm and lazygit colorscheme (See commit [`fcc2c30`](https://github.com/Avimitin/nvim/commit/fcc2c30))
* keymap: update ~neogit~ and fugitive keymap (See commit [`2debc8d`](https://github.com/Avimitin/nvim/commit/2debc8d))
* plugin: update vim-lua-format load condition (See commit [`89674a4`](https://github.com/Avimitin/nvim/commit/89674a4))

<!-- vim: set filetype=markdown: -->
