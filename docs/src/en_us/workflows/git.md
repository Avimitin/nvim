# How to interact with git

There are some plugins that helps you interact with git.

- vim-fugitive

This is a wrapper plugin for git.
You can use all the git CLI command in vim like it is in the terminal.
For example, to run command `git rebase -i HEAD~2`,
you can use `:Git rebase -i HEAD~2`.

See the full list of command in its readme file:
<https://github.com/tpope/vim-fugitive>.

- gitsigns.nvim

This plugin can add sign column on the left to highlight the changes.
And it gives functionality to do some git work like stage hunks, reset changes,
preview change, git blame...etc.

## Workflow 1: The git status panel

The fugitive plugin provides a command `:G` to open a magit like panel.

![fugitive-status-img](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/fugitive.png)

You can stage file with key `s`, commit file with key `cc`, view inline
diff with key `=`, jump between file with key `)(`. There are also rebase
key mappings, amend mappings...etc.

And the `:G` command is now mapped with key `;g`.

You can see full list of the keys definition with key `g?`.

## Workflow 2: Save by key mappings

I map the key `gic` to run command `:Git commit -sS`, and key `giP` to
open the command line with `:Git! push ` input already.[^1]

This built a new workflow: 

1. You can stage current hunk[^2] with key `gis`.
2. You can view the diff page by keymap `gid`.
3. Then just run `gic` to open commit panel.
4. Finally click `giP` and input the remote and branch to push.

[^1]: The bang sign `!` here means run the `git push` command in the
background.

[^2]: The "hunk" word means a range of the changes. It is highlighted by
the icon on the left.

## Other available key mappings

| key mappings   | functionality            |
|----------------|--------------------------|
| <kbd>gij</kbd> | Jump to next hunk        |
| <kbd>gik</kbd> | Jump to prev hunk        |
| <kbd>gis</kbd> | Stage current hunk       |
| <kbd>giS</kbd> | Stage current buffer     |
| <kbd>gir</kbd> | Reset current hunk       |
| <kbd>giR</kbd> | Reset the buffer         |
| <kbd>giu</kbd> | Undo stage               |
| <kbd>gip</kbd> | Preview changes          |
| <kbd>gib</kbd> | Toggle inline blame      |
| <kbd>giB</kbd> | Toggle full page blame   |
| <kbd>gid</kbd> | Open diff pages          |
| <kbd>giD</kbd> | Open diff with ~ as base |
| <kbd>gih</kbd> | Toggle deleted line      |
| <kbd>gic</kbd> | Open commit panel        |
| <kbd>giP</kbd> | Git push                 |
| <kbd>;g</kbd>  | Open git status          |
| <kbd>;l</kbd>  | Open lazygit             |
