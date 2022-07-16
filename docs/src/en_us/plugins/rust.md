# rust

The plugin rust-tools.nvim has already set up LSP, format, and debug utilities.

See <https://github.com/simrat39/rust-tools.nvim/> for what it can do.

> This plugin will setup lspconfig itself, please don't write lspconfig manually.

## Inlay hint

The rust-tools.nvim setting is located in lua/plugins/coding/config/rust-tools.lua.
And inlay hint is set up automatically after you open rust file.

But it will not prompt up by default due to some unknown bug.
It will only show up after you save the buffer.
So you need to manually run command `:w` when you first open the rust code.

## Code action

You can use the keymap <kbd>LEADER ra</kbd> or keymap `gx` set by LSP to view and select
the action for Rust code. Also you can press <kbd>LEADER rr</kbd> to run test or function.

## Code format

You can use `gf` to run the LSP built in format API. It will called the rustfmt
program.

## Debug

You need to install `lldb-vscode`. Then run `:RustDebuggables`, it will open the
debug panel automatically.

## Rust Analyzer Settings

You might want to make some specific rust-analyzer settings for your project.
You can create a file with name `.rust-analyzer.json` in the same directory
with the `Cargo.toml` file. Then put all the configuration you want into it.

Configuration reference: <https://rust-analyzer.github.io/manual.html#configuration>

For example, if you want to enable all feature when editing the code:

```bash
# cd to the root directory of your project
echo '{
  "cargo": {
    "allFeatures": true
  }
}' > .rust-analyzer.json
```

## Gallery

Please read [demo](https://github.com/simrat39/rust-tools.nvim/#demos).
