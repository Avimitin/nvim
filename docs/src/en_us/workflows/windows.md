<!-- vim: set tw=80 fo+=t -->
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
