return {
  setup = function()
    require("lsp").start(vim.cfg.javascript.server, {})

    local builtins = require("null-ls").builtins
    local sources = {}
    if vim.cfg.javascript.eslint then
      table.insert(sources, builtins.code_actions.eslint)
      table.insert(sources, builtins.diagnostics.eslint)
    end

    if vim.cfg.javascript.prettier then
      table.insert(sources, builtins.formatting.eslint)
    end

    require("null-ls").setup({
      sources = sources,
    })
  end,
}
