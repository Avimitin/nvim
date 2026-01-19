local M = {}

function M.setup()
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
    vim.notify("metals not found, stop configuring LSP.", vim.log.levels.WARN)
    return
  end
  scala_config.settings.metalsBinaryPath = metals_binary

  local mill_exe = vim.fn.exepath("mill")
  if not mill_exe or mill_exe == "" then
    vim.notify("mill not found, stop configuring metals.", vim.log.levels.WARN)
    return
  end

  scala_config.init_options.statusBarProvider = "off"
  scala_config.root_patterns = {
    "build.mill",
    "build.sbt",
    "build.sc",
    "build.gradle",
    "pom.xml",
    ".git",
  }
  scala_config.find_root_dir_max_project_nesting = 0
  scala_config.settings.millScript = mill_exe
  scala_config.capabilities = require("cmp_nvim_lsp").default_capabilities()
  scala_config.on_attach = function(client, bufnr)
    require("lang.on_attach").run(client, bufnr)

    -- Disable metals document hightlight, it is really ugly
    client.server_capabilities.semanticTokensProvider = nil
  end
  require("metals").initialize_or_attach(scala_config)

  -- nvim-metals will send metals/didFocus protocol to all the buffer, which will cause other LSP to exits.
  -- Here I recreate the nvim-metasls-forcus auto commands to force it run on *.scala/*.sc only.
  -- Using LspAttach here to delay the erase execution, because the nvim-metals plugin define the
  -- auto commands after buffer attached.
  vim.api.nvim_create_autocmd("LspAttach", {
    pattern = { "*.scala", "*.sc" },
    callback = function()
      vim.schedule(function()
        vim.api.nvim_clear_autocmds({ group = "nvim-metals-focus" })
        vim.api.nvim_create_autocmd("BufEnter", {
          pattern = { "*.scala", "*.sc" },
          callback = function()
            require("metals").did_focus()
          end,
          group = vim.api.nvim_create_augroup("nvim-metals-focus", { clear = true }),
        })
      end)
    end,
  })
end

return M
