require("plugins.enhance.keymap")

require("plugins.enhance.config").pre()

local repo = require("plugins.enhance.repos")
require("plugins").register(repo)

-- setup commands
require("plugins.enhance.commands")
