local Export = {}

Export.settings = {
  deno = {
    enable = true,
    suggest = {
      imports = {
        hosts = {
          ["https://deno.land"] = true,
          ["https://crux.land"] = true,
        },
      },
    },
  },
}

Export.handlers = {
  ["deno/registryState"] = function(_, result, ctx)
    local client = vim.lsp.get_client_by_id(ctx.client_id)
    local config = (client.config.settings.deno.suggest or {}).imports or {}
    local hosts = config.hosts or {}
    hosts[result.origin] = true

    local new_config = {
      deno = {
        suggest = {
          imports = {
            hosts = hosts,
          },
        },
      },
    }
    client.config.settings = vim.tbl_deep_extend("force", client.config.settings, new_config)
    client.notify("workspace/didChangeConfiguration", {
      settings = new_config,
    })
  end,
}

return Export
