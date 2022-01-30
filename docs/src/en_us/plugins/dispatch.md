# Dispatch

The dispatch plugin can help you run build job in a separate window.
If you are using tmux, it will create the window in tmux.

If the build job contains any error, it will prompt up a quick fix list.

![image](https://raw.githubusercontent.com/Avimitin/nvim/3.16.0/docs/images/dispatch.png)

## Usage

You can use command:

```text
:Dispatch <Your Commands>
```

or keymap <kbd>\`</kbd> <kbd>space</kbd> to trigger it.

## keymaps

| keymaps     | function             |
|-------------|----------------------|
| `m<CR>`     | `:Make<CR>`          |
| `m<Space>`  | `:Make<Space>`       |
| m!          | :Make!               |
| m?          | Show 'makeprg'       |
| \``<CR>`    | :Dispatch`<CR>`      |
| \``<Space>` | :Dispatch`<Space>`   |
| \`!         | :Dispatch!           |
| \`?         | :FocusDispatch`<CR>` |
| '`<CR>`     | :Start`<CR>`         |
| '`<Space>`  | :Start`<Space>`      |
| '!          | :Start!              |
| '?          | show b:start         |
| g'`<CR>`    | :Spawn`<CR>`         |
| g'`<Space>` | :Spawn`<Space>`      |
| g'!         | :Spawn!              |
| g'?         | Show 'shell'         |

## More

See `:h dispatch`
