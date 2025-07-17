require("lang.plugins")

local M = {}

function M.setup_lsp()
  vim.lsp.enable({
    "ccls",
    "hls",
    "lua_ls",
    "nil_ls",
    "pyright",
    "rust_analyzer",
    "tinymist",
  })

  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      require("lang.on_attach").run(client, args.buf)
    end,
  })

  vim.lsp.set_log_level(vim.log.levels.OFF)
end

return M
