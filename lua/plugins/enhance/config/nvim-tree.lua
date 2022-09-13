return function()
  local ok, tree_c = pcall(require, "nvim-tree.config")
  if not ok then
    vim.notify(tree_c, vim.log.levels.ERROR)
    return
  end

  -- following options are the default
  require("nvim-tree").setup({
    disable_netrw = true,
    hijack_netrw = true,
    open_on_setup = false,
    ignore_ft_on_setup = { "startify", "dashboard" },
    open_on_tab = false,
    hijack_cursor = true,
    update_cwd = true,
    hijack_directories = { enable = true, auto_open = true },
    diagnostics = {
      enable = false,
      icons = { hint = "", info = "", warning = "", error = "" },
    },
    update_focused_file = { enable = false, update_cwd = false, ignore_list = {} },
    system_open = { cmd = nil, args = {} },
    git = {
      ignore = false,
    },
    filters = {
      dotfiles = true,
      custom = {
        "^\\.git$",
        "node_modules",
        "^\\.cache$",
      },
    },
    actions = {
      change_dir = {
        enable = true,
        global = false,
      },
      open_file = {
        quit_on_open = false,
        resize_window = false,
        window_picker = {
          enable = true,
          chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
          exclude = {
            filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
            buftype = { "nofile", "terminal", "help" },
          },
        },
      },
    },
    renderer = {
      add_trailing = false,
      highlight_git = true,
      highlight_opened_files = "bold",
      root_folder_modifier = ":t",
      indent_markers = {
        enable = false,
        icons = {
          corner = "└ ",
          edge = "│ ",
          none = "  ",
        },
      },
      icons = {
        webdev_colors = true,
        show = {
          git = true,
          folder = true,
          file = true,
          folder_arrow = false,
        },
        glyphs = {
          default = "",
          symlink = "",
          git = {
            unstaged = "",
            staged = "",
            unmerged = "",
            renamed = "凜",
            untracked = "",
            deleted = "",
            ignored = "",
          },
          folder = {
            -- arrow_open = "",
            -- arrow_closed = "",
            default = "",
            open = "ﱮ",
            empty = "", -- 
            empty_open = "",
            symlink = "",
            symlink_open = "",
          },
        },
      },
    },
    view = {
      width = 25,
      -- height = 30,
      side = "left",
      mappings = {
        custom_only = false,
        list = {
          { key = "?", action = "toggle_help" },
        },
      },
    },
  })
end
