if require("libs.cache")["scala"] then
  return
end

local scala_config = require("metals").bare_config()
scala_config.settings = {
  showImplicitArguments = true,
  excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
}

local metals_binary = vim.fn.exepath("metals")
if not metals_binary or metals_binary == "" then
  vim.notify("metals not found", vim.log.levels.ERROR)
  return
end
scala_config.settings.metalsBinaryPath = metals_binary

local mill_exe = vim.fn.exepath("mill")
if not mill_exe or mill_exe == "" then
  vim.notify("mill not found", vim.log.levels.ERROR)
  return
end

scala_config.settings.millScript = mill_exe
scala_config.capabilities = require("lang.config").capabilities
scala_config.on_attach = function(client, bufnr)
  -- require("metals").setup_dap()
  require("lang.keymaps")(client, bufnr)
  require("lang.icons").setup()
end

require("metals").initialize_or_attach(scala_config)
