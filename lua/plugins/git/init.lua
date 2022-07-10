-- initialize commands
require("plugins.git.commands")

-- initialize key mappings
require("plugins.git.keymap")

local repos = require("plugins.git.repos")
require("plugins").register(repos)

require("plugins.git.config").pre()
