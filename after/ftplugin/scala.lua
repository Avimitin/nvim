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
  vim.notify("metals not found, stop configuring LSP.")
  return
end
scala_config.settings.metalsBinaryPath = metals_binary

local mill_exe = vim.fn.exepath("mill")
if not mill_exe or mill_exe == "" then
  vim.notify("mill not found, stop configuring metals.")
  return
end

scala_config.settings.millScript = mill_exe
scala_config.capabilities = require("cmp_nvim_lsp").default_capabilities()
scala_config.on_attach = require("lang.on_attach").setup_all
require("metals").initialize_or_attach(scala_config)
