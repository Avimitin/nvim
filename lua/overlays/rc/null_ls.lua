local attachment = require("plugins.coding.keymap")
local builtins = require("null-ls").builtins

local sources = {}

local map = {
  ["stylua"] = {
    builtins.formatting.stylua,
  },
  ["eslint"] = {
    builtins.code_actions.eslint,
    builtins.diagnostics.eslint,
  },
  ["prettier"] = {
    builtins.formatting.prettier,
  },
}

for _, want in ipairs(vim.g.nvcfg.null_ls_sources) do
  sources:insert(map[want])
end

require("null-ls").setup({
  sources = sources,
  on_attach = attachment.lsp_keymap,
})
