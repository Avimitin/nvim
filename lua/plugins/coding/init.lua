local repos = require("plugins.coding.repos")
require("plugins.coding.config").pre()
require("plugins").register(repos)

-- setup commands
require("plugins.coding.commands")
