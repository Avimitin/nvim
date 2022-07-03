local saga = require("lspsaga")

-- use custom config
saga.init_lsp_saga({
  -- "single" | "double" | "rounded" | "bold" | "plus"
  border_style = "single",
  -- when cursor in saga window you config these to move
  move_in_saga = { prev = "k", next = "j" },
  diagnostic_header = { " ", " ", " ", " " },
  -- show diagnostic source
  show_diagnostic_source = true,
  -- add bracket or something with diagnostic source, just have 2 elements
  diagnostic_source_bracket = {},
  -- use emoji lightbulb in default
  code_action_icon = " ",
  -- if true can press number to execute the codeaction in codeaction window
  code_action_num_shortcut = true,
  -- same as nvim-lightbulb but async
  code_action_lightbulb = {
    enable = true,
    sign = true,
    sign_priority = 40,
    virtual_text = false,
  },
  -- separator in finder
  finder_separator = " ﰲ ",
  -- preview lines of lsp_finder and definition preview
  max_preview_lines = 10,
  finder_action_keys = {
    open = "<CR>",
    vsplit = "s",
    split = "i",
    tabe = "t",
    quit = "q",
    scroll_down = "<C-f>",
    scroll_up = "<C-b>", -- quit can be a table
  },
  code_action_keys = {
    quit = "q",
    exec = "<CR>",
  },
  rename_action_quit = "<C-c>",
  definition_preview_icon = "  ",
  -- show symbols in winbar must nightly
  symbol_in_winbar = false,
  winbar_separator = ">",
  winbar_show_file = true,
})
