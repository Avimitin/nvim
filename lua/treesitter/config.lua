local disable = function(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return true
  end
  return vim.api.nvim_buf_line_count(buf) >= 5000
end

vim.api.nvim_create_autocmd("FileType", {
  callback = function(args)
    if disable(args.buf) then
      return
    end

    vim.schedule(function()
      -- Double-check buffer validity in case it closed quickly
      if not vim.api.nvim_buf_is_valid(args.buf) then
        return
      end

      local status, _ = pcall(vim.treesitter.start, args.buf)

      if status then
        -- Use 'vim.bo' for buffer-local options
        vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

        -- Use 'vim.wo' for window-local options.
        -- vim.wo[0] refers to the current window.
        vim.wo[0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
        vim.wo[0].foldmethod = "expr"
      end
    end)
  end,
})

-- Setup autotag
require("nvim-ts-autotag").setup()
