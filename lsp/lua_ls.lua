return {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = {
    ".luarc.json",
    ".luarc.jsonc",
    ".luacheckrc",
    ".stylua.toml",
    "stylua.toml",
    "selene.toml",
    "selene.yml",
    ".git",
  },
  on_init = function(client)
    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if not ((vim.fn.getcwd()):find("nvim") and (vim.uv.fs_stat(path .. "/ftdetect"))) then
        return
      end
    end

    client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
      diagnostics = {
        enable = true,
        globals = { "vim" },
      },
      runtime = {
        version = "LuaJIT",
        path = {
          "lua/?.lua",
          "lua/?/init.lua",
        },
      },
      -- Make the server aware of Neovim runtime files
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
          vim.api.nvim_get_runtime_file("", true),
        },
      },
    })
  end,
  settings = {
    Lua = {},
  },
}
