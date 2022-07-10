-- configuration before repos
require("plugins.coding.config").pre()

-- register all the plugins
local repos = require("plugins.coding.repos")
require("plugins").register(repos)

-- setup commands
require("plugins.coding.commands")
