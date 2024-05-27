if require("libs.cache")["haskell"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "hls", {
  settings = {
    haskell = {
      plugin = {
        rename = {
          config = {
            crossModule = true,
          },
        },
      },
    },
  },
})
