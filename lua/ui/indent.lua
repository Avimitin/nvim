local cache = {}
local hooks = require("ibl.hooks")
hooks.register(hooks.type.ACTIVE, function(bufnr)
  if cache[bufnr] ~= nil then
    return cache[bufnr]
  end
  local test = vim.api.nvim_buf_line_count(bufnr) < 5000
  cache[bufnr] = test
  return test
end)

require("ibl").setup({
  indent = {
    char = "▏",
  },
  scope = {
    show_start = false,
    show_end = false,
  },
  exclude = {
    filetypes = {
      "help",
      "terminal",
      "dashboard",
      "packer",
      "lspinfo",
      "TelescopePrompt",
      "TelescopeResults",
      "startify",
      "dashboard",
      "dotooagenda",
      "log",
      "fugitive",
      "gitcommit",
      "packer",
      "vimwiki",
      "markdown",
      "txt",
      "vista",
      "help",
      "todoist",
      "NvimTree",
      "peekaboo",
      "git",
      "TelescopePrompt",
      "undotree",
      "flutterToolsOutline",
      "lsp-installer",
      "hydra_hint",
      "noice",
      "",
    },
  },
})
