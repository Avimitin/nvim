return {
  change_rnu = {
    enable = true,
  },
  insert_mode_in_term = {
    enable = true,
  },
  smart_yank = {
    enable = true,
  },
  cd_project_root = {
    enable = true,
    root_patterns = {
      "Cargo.toml",
      "go.mod",
      "package.json",
    },
  },
  fcitx5_toggle = {
    enable = false,
  },
  lastline = {
    enable = true,
    ignore_buffer_type = { "gitcommit", "gitrebase", "svn", "hgcommit", "Dashboard" },
    ignore_file_type = { "quickfix", "nofile", "help" },
  },
  cursor_line = {
    enable = true,
  },
}
