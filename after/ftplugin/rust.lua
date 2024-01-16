if require("libs.cache")["rust_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()

-- setup lsp key mappings
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function()
    local clients = vim.lsp.get_clients({ bufnr = bufnr, name = "rust-analyzer" })
    if clients and clients[1] then
      require("lang.on_attach").setup_all(clients[1], bufnr)
    end
  end,
})
