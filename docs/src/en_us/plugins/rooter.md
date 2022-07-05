# rooter

Rooter will find the some root pattern and cd into it.
The root pattern is like `.git`, `go.mod`, `Cargo.toml`...etc.

## Usage

Rooter will run once when you open the editor at the first time.
You can also trigger it by command `:Rooter`.

## Configuration

The rooter config is set in the lua/plugins/modules/enhance/config/init.lua file.

```lua
-- set this to 0, the rooter will automatically cd into root dir
vim.g.rooter_manual_only = 1

-- If there is no root pattern found, cd to the current file's location
vim.g.rooter_change_directory_for_non_project_files = 'current'

-- Some root pattern definition
vim.g.rooter_patterns = {
  '.git',
  'Cargo.toml'
}
```

**NOTES**: I set the manual_only to 1 due to some submodule conflict.
The rooter will cd into the parent `.git` directory when I am commiting inside
some submodule project. That's fucked. So I close the auto change directory feature.
