local ok, tree_c = pcall(require, "nvim-tree.config")
if not ok then
	print(tree_c)
	return
end

local tree_cb = tree_c.nvim_tree_callback

vim.g.nvim_tree_side = "left"
vim.g.nvim_tree_width = 25
vim.g.nvim_tree_ignore = {".git", "node_modules", ".cache"}
vim.g.nvim_tree_gitignore = 0
vim.g.nvim_tree_auto_ignore_ft = {"startify","dashboard"} -- don't open tree on specific fiypes.
vim.g.nvim_tree_auto_open = 0
vim.g.nvim_tree_auto_close = 1 -- closes tree when it's the last window
vim.g.nvim_tree_quit_on_open = 0 -- closes tree when file's opened
vim.g.nvim_tree_follow = 1
vim.g.nvim_tree_indent_markers = 0
vim.g.nvim_tree_hide_dotfiles = 1
vim.g.nvim_tree_git_hl = 1
vim.g.nvim_tree_highlight_opened_files = 1
vim.g.nvim_tree_root_folder_modifier = ":t"
vim.g.nvim_tree_tab_open = 1
vim.g.nvim_tree_allow_resize = 1
vim.g.nvim_tree_add_trailing = 0 -- append a trailing slash to folder names
vim.g.nvim_tree_disable_netrw = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.g.nvim_tree_update_cwd = 1

vim.g.nvim_tree_show_icons = {
    git = 1,
    folders = 1,
    files = 1
    -- folder_arrows= 1
}
vim.g.nvim_tree_icons = {
    default = "",
    symlink = "",
    git = {
        unstaged = "",
        staged = "",
        unmerged = "",
        renamed = "凜",
        untracked = "",
        deleted = "",
        ignored = ""
    },
    folder = {
        -- arrow_open = "",
        -- arrow_closed = "",
        default = "",
        open = "",
        empty = "", -- 
        empty_open = "",
        symlink = "",
        symlink_open = ""
    }
}

vim.g.nvim_tree_bindings = {
    {key = {"<CR>", "o", "<2-LeftMouse>"}, cb = tree_cb("edit")},
    {key = {"<2-RightMouse>", "<C-}>"}, cb = tree_cb("cd")},
    {key = "<C-v>", cb = tree_cb("vsplit")},
    {key = "<C-x>", cb = tree_cb("split")},
    {key = "<C-t>", cb = tree_cb("tabnew")},
    {key = "<", cb = tree_cb("prev_sibling")},
    {key = ">", cb = tree_cb("next_sibling")},
    {key = "P", cb = tree_cb("parent_node")},
    {key = "<BS>", cb = tree_cb("close_node")},
    {key = "<S-CR>", cb = tree_cb("close_node")},
    {key = "<Tab>", cb = tree_cb("preview")},
    {key = "K", cb = tree_cb("first_sibling")},
    {key = "J", cb = tree_cb("last_sibling")},
    {key = "I", cb = tree_cb("toggle_ignored")},
    {key = "H", cb = tree_cb("toggle_dotfiles")},
    {key = "R", cb = tree_cb("refresh")},
    {key = "a", cb = tree_cb("create")},
    {key = "d", cb = tree_cb("remove")},
    {key = "r", cb = tree_cb("rename")},
    {key = "<C->", cb = tree_cb("full_rename")},
    {key = "x", cb = tree_cb("cut")},
    {key = "c", cb = tree_cb("copy")},
    {key = "p", cb = tree_cb("paste")},
    {key = "y", cb = tree_cb("copy_name")},
    {key = "Y", cb = tree_cb("copy_path")},
    {key = "gy", cb = tree_cb("copy_absolute_path")},
    {key = "[c", cb = tree_cb("prev_git_item")},
    {key = "}c", cb = tree_cb("next_git_item")},
    {key = "-", cb = tree_cb("dir_up")},
    {key = "q", cb = tree_cb("close")},
    {key = "?", cb = tree_cb("toggle_help")}
}

-- toggle nvim tree
vim.api.nvim_set_keymap("n", "tt", ":NvimTreeRefresh<CR>:NvimTreeToggle<CR>", {noremap=true, silent=true})
vim.cmd[[au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif]]
