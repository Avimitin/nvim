# rust

The plugin rust-tools.nvim has already set up LSP, format, and debug utilities.

See https://github.com/simrat39/rust-tools.nvim/

## Inlay hint

The rust-tools.nvim setting is located in lua/config/rust.lua.
And inlay hint is set up automatically after you open rust file.

But it will not prompt up by default due to some unknown bug.
It will only show up after you save the buffer.
So you need to manually run command `:w` when you first open the rust code.

## Code action

You can use the keymap <kbd>LEADER ra</kbd> or keymap `gx` set by LSP to view and select
the action for Rust code.

## Code format

You can use `gf` to run the LSP built in format API. It will called the rustfmt
program.

## Debug

You need to install lldb-vscode. Then run `:RustDebuggables`, it will open the
debug panel automatically.
