if require("libs.cache")["rust_lsp"] then
  return
end

local find_config = function()
  local root_dir = require("libs.find_root").find_root({
    patterns = { ".rust-analyzer.json", "Cargo.toml", ".git" },
  })
  local cfg_file = vim.fs.joinpath(root_dir, ".rust-analyzer.json")

  -- prompt when try to read the file, to avoid security issue
  local file = vim.secure.read(cfg_file)
  if not file then
    return nil
  end

  local rust_settings = { ["rust-analyzer"] = vim.json.decode(file) }
  return rust_settings
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "rust_analyzer", {
  settings = find_config(),
})
