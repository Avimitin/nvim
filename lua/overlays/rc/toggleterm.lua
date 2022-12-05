require("toggleterm").setup({
  -- size can be a number or function which is passed the current terminal
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  hide_numbers = true,
  open_mapping = "_",
  insert_mappings = false,
  start_in_insert = true,
  terminal_mappings = false,
  direction = "horizontal", -- 'window' | 'float' | 'vertical' ,
  close_on_exit = true, -- close the terminal window when the process exits
  shade_terminals = false,
  shell = vim.o.shell, -- change the default shell
  float_opts = {
    border = "single",
    winblend = 3,
    highlights = {
      border = "Normal",
      background = "Normal",
    },
  },
  winbar = {
    enabled = true,
    name_formatter = function(term)
      local _, _, name, id = term.name:find([[#(%w+)#(%d)]])
      return string.format("%s (%s)", name, id)
    end,
  },
})
