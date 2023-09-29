local scala_config = require("metals").bare_config()
scala_config.settings = {
  showImplicitArguments = true,
  excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
}

local exepath = vim.fn.exepath("metals")
if not exepath or exepath == "" then
  return
end
scala_config.settings.metalsBinaryPath = exepath
scala_config.capabilities = require("lang.config").capabilities
scala_config.on_attach = function(client, bufnr)
  -- require("metals").setup_dap()
  require("lang.keymaps")(client, bufnr)
  require("lang.icons").setup()
end

require("metals").initialize_or_attach(scala_config)

-- I don't know why scala treesitter highlight won't update in time, so here is a trivial fix to trigger highlight render each time editing is done.
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = { "*.scala", "*.sc" },
  callback = function(ev)
    vim.treesitter.start(ev.buf, "scala")
  end,
})
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = { "*.scala", "*.sc" },
  callback = function(ev)
    vim.treesitter.start(ev.buf, "scala")
  end,
})
