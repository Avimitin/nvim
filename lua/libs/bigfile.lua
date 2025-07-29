-- Source: https://github.com/folke/snacks.nvim
-- License: Apache License 2.0
-- w/ modification to integrate in this dotfile

---@class snacks.bigfile
local M = {}

---@class snacks.bigfile.Config
local defaults = {
  notify = true, -- show notification when big file detected
  size = 1.5 * 1024 * 1024, -- 1.5MB
  -- Enable or disable features when big file detected
  ---@param ctx {buf: number, ft:string}
  setup = function(ctx)
    vim.g.loaded_matchparen = false
    vim.wo.foldmethod = "manual"
    vim.wo.statuscolumn = ""
    vim.wo.conceallevel = 0
    vim.b.minianimate_disable = true
    vim.schedule(function()
      vim.bo[ctx.buf].syntax = ctx.ft
    end)
  end,
}

function M.setup(user)
  local opts = user or defaults

  vim.filetype.add({
    pattern = {
      [".*"] = {
        function(path, buf)
          return vim.bo[buf]
              and vim.bo[buf].filetype ~= "bigfile"
              and path
              and vim.fn.getfsize(path) > opts.size
              and "bigfile"
            or nil
        end,
      },
    },
  })

  vim.api.nvim_create_autocmd({ "FileType" }, {
    group = vim.api.nvim_create_augroup("snacks_bigfile", { clear = true }),
    pattern = "bigfile",
    callback = function(ev)
      if opts.notify then
        local path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(ev.buf), ":p:~:.")
        vim.notify(
          ("Big file detected `%s`. Some Neovim features have been **disabled**."):format(path),
          vim.log.levels.WARN
        )
      end
      vim.api.nvim_buf_call(ev.buf, function()
        opts.setup({
          buf = ev.buf,
          ft = vim.filetype.match({ buf = ev.buf }) or "",
        })
      end)
    end,
  })
end

return M
