require("scrollbar").setup({
  marks = {
    Error = { text = { "" } },
    Warn = { text = { "" } },
    Hint = { text = { "" } },
    Info = { text = { "" } },
    GitAdd = { text = "▕" },
    GitChange = { text = "▕" },
  },
  excluded_buftypes = {
    "terminal",
  },
  excluded_filetypes = {
    "prompt",
    "TelescopePrompt",
    "noice",
    "Git",
  },
  handlers = {
    cursor = false,
  },
})
