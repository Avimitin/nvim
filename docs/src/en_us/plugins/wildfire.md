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
