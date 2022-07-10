-- initialize commands
require("plugins.git.commands")

-- initialize key mappings
require("plugins.git.keymap")

-- initialize git configuration
require("plugins.git.config").pre()

-- register plugin repos
local repos = require("plugins.git.repos")
require("plugins").register(repos)
