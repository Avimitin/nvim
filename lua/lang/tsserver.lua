return {
  setup = function(opts)
    require("lang").run_lsp("tsserver", opts.config or {})

    local builtins = require("null-ls").builtins
    local sources = {}
    if opts.eslint then
      table.insert(sources, builtins.code_actions.eslint)
      table.insert(sources, builtins.diagnostics.eslint)
    end

    if opts.prettier then
      table.insert(sources, builtins.formatting.eslint)
    end

    require("null-ls").setup({
      sources = sources,
    })
  end,
}
