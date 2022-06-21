# Keymap

## Leader key

leader key is set to <kbd>Space</kbd>.

## Navigation

```text
Basic movement

  ^
  k
<h l>
  j
```

| Keymap                      | Function                               |
|-----------------------------|----------------------------------------|
| <kbd>H</kbd>                | Go to the first character of the line  |
| <kbd>L</kbd>                | Go to the last character of the line   |
| <kbd>J</kbd>                | Go down 5 lines                        |
| <kbd>K</kbd>                | Go up 5 lines                          |
| <kbd>W</kbd>                | Go 5 words forward                     |
| <kbd>B</kbd>                | Go 5 words backward                    |

## Cursor move in insert mode

| Keymap            | Function                             |
|-------------------|--------------------------------------|
| <kbd>Ctrl h</kbd> | Act like <kbd>Home</kbd>             |
| <kbd>Ctrl e</kbd> | Act like <kbd>End</kbd>              |
| <kbd>Ctrl f</kbd> | Act like <kbd>w</kbd> in normal mode |
| <kbd>Ctrl b</kbd> | Act like <kbd>b</kbd> in normal mode |

## Windows Navigation

| Keymap        | Function               |
|---------------|------------------------|
| <kbd>;k</kbd> | Go to the window above |
| <kbd>;j</kbd> | Go to the window below |
| <kbd>;h</kbd> | Go to the right window |
| <kbd>;l</kbd> | Go to the left window  |

## Windows resize

| Keymap                      | Function                               |
|-----------------------------|----------------------------------------|
| <kbd>Ctrl Shift Up</kbd>    | Resize the top border of window        |
| <kbd>Ctrl Shift Down</kbd>  | Resize the bottom border of the window |
| <kbd>Ctrl Shift Left</kbd>  | Resize the left border of the window   |
| <kbd>Ctrl Shift Right</kbd> | Resize the right border of the window  |

## Search

| Keymap         | Function                    |
|----------------|-----------------------------|
| <kbd>N</kbd>   | Go to the before matches    |
| <kbd>n</kbd>   | Go to the following matches |
| <kbd>ESC</kbd> | Close the search highlight  |

## Tabpages

| Keymap              | Function               |
|---------------------|------------------------|
| <kbd>Ctrl-t h</kbd> | Go to the previous tab |
| <kbd>Ctrl-t l</kbd> | Go to the next tab     |
| <kbd>Ctrl-t n</kbd> | Create new tab         |

## Copy and Paste

| Keymap            | Function                 | notes                     |
|-------------------|--------------------------|---------------------------|
| <kbd>y</kbd>      | Copy to register         |                           |
| <kbd>p</kbd>      | Paste from the register  |                           |
| <kbd>Ctrl-p</kbd> | Paste from the clipboard | In normal and insert mode |
| <kbd>Ctrl-y</kbd> | Copy to clipboard        | Only in visual mode       |

## Text move

| Keymap         | Function                   | notes                          |
| -------------- | -------------------------- | ------------------------------ |
| <kbd><</kbd>  | Reduce one indent level    | In normal and selection mode   |
| <kbd>></kbd>  | Increse one indent level   | In normal and selection mode   |

## Save and Quit

| Keymap           | Function                                                 | Notes               |
|------------------|----------------------------------------------------------|---------------------|
| <kbd>; w</kbd>   | Save                                                     |                     |
| <kbd>; q</kbd>   | Quit buffer (Auto quit nvim when last buffer is deleted) |                     |
| <kbd>: q</kbd>   | Quit neovim, window, tabs                                |                     |
| <kbd>Alt ;</kbd> | Leave the insert mode (Same as ESC)                      | Only in insert mode |

## Scrolling

| Keymap            | Function                                   |
|-------------------|--------------------------------------------|
| <kbd>Ctrl j</kbd> | Scroll down                                |
| <kbd>Ctrl k</kbd> | Scroll up                                  |
| <kbd>Ctrl f</kbd> | Like "PgDown" key, scroll down half a page |
| <kbd>Ctrl b</kbd> | Like "PgUp" key, scroll up half a page     |
| <kbd>Ctrl y</kbd> | Scroll up, but cursor will not move        |
| <kbd>Ctrl e</kbd> | Scroll down, but curson will not move        |

## Others

| Keymap            | Function        |
|-------------------|-----------------|
| <kbd>Ctrl-z</kbd> | Reverse changes |
