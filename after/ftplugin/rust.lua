if require("libs.cache")["rust_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()

-- setup lsp key mappings
require("lang.keymaps")(nil, bufnr)
require("lang.icons").setup()

-- create auto command to format on save
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  buffer = bufnr,
  desc = "Format Rust code on save",
  callback = function()
    vim.lsp.buf.format({ async = true })
  end,
})
