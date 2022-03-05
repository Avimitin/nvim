# How to interact with git

There are four plugin that helps you interact with git.

## #1 vim-fugitive

This is a wrapper plugin for git.
You can use all the git CLI command in vim like it is in the terminal.
For example, to run command `git rebase -i HEAD~2`,
you can use `:Git rebase -i HEAD~2`.

See the full list of command in its readme file:
<https://github.com/tpope/vim-fugitive>.

### Workflow 1: The git status panel

The fugitive plugin provides a command `:G` to open a magit like panel.

![fugitive-status-img](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/fugitive.png)

You can stage file with key `s`, commit file with key `cc`, view inline
diff with key `=`, jump between file with key `)(`. There are also rebase
key mappings, amend mappings...etc.
You can see full list of the keys definition with key `g?`.

### Workflow 2: The key mappings

I map the key `gic` to run command `:Git commit -sS`, and key `giP` to
open the command line with `:Git! push ` input already.[^1]

This built a new workflow: 

1. You can stage file with key `gis` which is provided by plugin *gitsigns.nvim*.
I will describe this plugin later.
2. Then just run `gic` to open commit panel.
3. Finally click `giP` and input the remote and branch to push.

[^1]: The bang sign `!` here means run the `git push` command in the
background.
