local extra_config = {
  settings = {
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
  },
  handlers = {
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
  },
}

local Export = {}

function Export.setup()
  -- fix highlight on codefences
  vim.g.markdown_fenced_languages = {
    "ts=typescript",
  }

  local apply_keymaps = require("lsp.keymaps")

  local on_attach = function(client, bufnr)
    apply_keymaps(client, bufnr)

    ---@return table|nil
    local function get_deno_client()
      local clients = vim.lsp.get_active_clients({
        bufnr = bufnr,
        name = "denols",
      })

      local _, deno = next(clients)
      if deno == nil then
        vim.notify("No Deno client found")
        return nil
      end

      return deno
    end

    local cmd = function(...)
      vim.api.nvim_buf_create_user_command(bufnr, ...)
    end

    cmd("DenoReloadImports", function()
      local deno = get_deno_client()
      if deno == nil then
        return
      end

      deno.request("deno/reloadImportRegistries", nil, function()
        vim.notify("Registries reloaded")
      end)
    end, {
      desc = "Reload cached response from import registries",
    })

    cmd("DenoCache", function()
      local deno = get_deno_client()
      if deno == nil then
        return
      end

      local api = vim.api
      local current_window = api.nvim_get_current_win()
      local current_buffer = api.nvim_win_get_buf(current_window)

      deno.request(
        "deno/cache",
        { referrer = { uri = vim.lsp.util.make_text_document_params(current_buffer) }, uris = {} },
        function()
          vim.notify("Dependencies cached")
        end
      )

      vim.notify("Cache request sent, downloading dependencies...")
    end, {
      desc = "Cache uncached module",
    })
  end

  extra_config.on_attach = on_attach

  require("lsp").start("denols", extra_config)
end

return Export
