# How to setup vale

The vale command-line tool provides code-like linting on writing article.
The configuration integrate it into NeoVIM using null-ls.nvim.

## Usage

1. Install vale binary

For Arch Linux, you can install vale by Arch Linux User Repository helper.

```bash
paru/yay -S yale
```

OR you can install binary here: <https://github.com/errata-ai/vale/releases/tag/v2.18.0>

2. Configure vale

Create a file in home directory and named it `.vale.ini`, then open the configuration generator
to setup vale: <https://vale.sh/generator/>.

After creating your configuration, copy it to the `.vale.ini` file and run command `vale sync` to
download resources.

3. Enable it in NeoVIM

Update the `g.enable_vale` value in `lua/plugins/options.lua` file.

OR

You can create a file called `custom.lua` in `lua/` directory, and enable vale by the below form:

```lua
-- lua/custom.lua
local M = {
  enable_vale = true,
}

return M
```

You can read [customization](../customize.md) for more about the `custom.lua` file.
