---@type vim.lsp.Config
return {
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell" },
  root_markers = {
    "hie.yaml",
    "stack.yaml",
    "cabal.project",
    ".git",
  },
  settings = {
    haskell = {
      formattingProvider = "ormolu",
      cabalFormattingProvider = "cabal-fmt",
    },
  },
}
