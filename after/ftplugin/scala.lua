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
