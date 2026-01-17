local disable = function(_, buf)
  return vim.api.nvim_buf_line_count(buf) >= 5000
end

-- Enable native highlighting and indentation
vim.api.nvim_create_autocmd("FileType", {
  callback = function(args)
    if disable(nil, args.buf) then
      return
    end

    -- Enable highlighting
    local ok = pcall(vim.treesitter.start, args.buf)

    -- Enable indentation if treesitter is active
    if ok then
      vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})

-- Setup autotag
require("nvim-ts-autotag").setup()

-- Note: nvim-treesitter-textobjects is loaded and provides queries.
-- Keymaps were empty in original config, so none are configured here.
-- Incremental selection module is removed in nvim-treesitter main.
