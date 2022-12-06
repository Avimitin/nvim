-- The entry point of the whole configuration
---@param opts CoreCfg
local function setup(opts)
  require("editor.cfg_processor")(opts)

  -- load configuration from lua/editor/<module>
  for _, module_name in ipairs({
    "options",
    "auto_commands",
    "keymap",
  }) do
    local ok, err = pcall(require, "editor." .. module_name)
    if not ok then
      local msg =
        string.format("Internal ERROR: fail to load editor module %s : %s", module_name, err)
      vim.notify(msg, vim.log.levels.ERROR)
    end
  end

  -- since we have packer compiled, we don't need to load this immediately
  vim.schedule(function()
    require("overlays")
  end)
end

return {
  setup = setup,
}
