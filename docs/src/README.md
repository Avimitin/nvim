# Introduction

Here is a detail document. It is still work in progress.
And PR is welcomed.

中文版的更新可能有延迟，请以英文版为主。

# Structure

The configuration structure:

```text
lua/
├── custom.lua
├── editor
│   ├── autocmd.lua
│   ├── colors.lua
│   ├── init.lua
│   ├── keymap.lua
│   ├── options.lua
│   └── utils.lua
└── plugins
    ├── init.lua
    └── modules
        ├── coding
        │   ├── commands.lua
        │   ├── config
        │   │   ├── init.lua
        │   │   └── ...
        │   └── init.lua
        ├── completion
        │   ├── config.lua
        │   └── init.lua
        ├── enhance
        │   ├── commands.lua
        │   ├── config/
        │   │   ├── autopairs.lua
        │   │   └── ...
        │   ├── init.lua
        │   └── keymap.lua
        ├── git
        │   ├── commands.lua
        │   ├── config.lua
        │   ├── init.lua
        │   └── keymap.lua
        ├── libs
        │   ├── bufdel.lua
        │   └── json.lua
        └── markdown
            ├── config.lua
            └── init.lua
```

Neovim configuration are place in `lua/editor` folders.
Plugins definition and configuration are separated by their funcitonality and were placed
inside the plugins directory.

Each modules contains a `init.lua` file to tell neovim what plugin should be download.
And there may be a `commands.lua` file or `keymaps.lua` file to define user command and
define user keys mappings.
There are also a `config.lua` file or `config/` directory to configure the plugins.
