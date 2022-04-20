# Introduction

Here is a detail document. It is still work in progress.
And PR is welcomed.

中文版的更新可能有延迟，请以英文版为主。

# Structure

The configuration structure:

```text
.
├── lua
│  ├── core            ==> neovim settings
│  │  ├── autocmd.lua  ==> auto commands definition
│  │  ├── colors.lua   ==> colorscheme settings
│  │  ├── commands.lua ==> user commands
│  │  ├── options.lua  ==> neovim options
│  │  └── utils.lua    ==> other utility script, not important
│  ├── mappings        ==> neovim key mappings
│  │  ├── init.lua     ==> neovim builtin mappings
│  │  ├── other.lua    ==> plugin mappings
│  │  └── utils.lua    ==> other utility script, not important
│  └── plugins         ==> plugins definition
│     ├── config       ==> configuration for single plugin
│     ├── bufdel.lua   ==> buffer delete script
│     ├── init.lua     ==> plugin manager settings
│     ├── load.lua     ==> plugins definition
│     └── options.lua  ==> options for plugins
├── vsnip              ==> snippets
│  ├── markdown.json
│  ├── org.json
│  ├── rust.json
│  └── toml.json
├── ginit.vim          ==> gui settings for nvim-qt
├── init.lua           ==> main entry
└── README.md

```
