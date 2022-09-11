local autocmd_cfg = require("editor").config.autocmd_enable

local cwd = "editor.auto_commands."
local function load(module)
  require(cwd .. module)
end

require("editor.auto_commands.default")

if autocmd_cfg.lastline then
  load("lastline")
end

if autocmd_cfg.fcitx5 then
  load("fcitx")
end
