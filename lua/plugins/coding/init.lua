-- configuration before repos
require("plugins.coding.config").pre()

-- register all the plugins
local repos = require("plugins.coding.repos")
local repos_diy = require("plugins.coding.repos_diy")
require("plugins").register(repos)
require("plugins").register(repos_diy)

-- setup commands
require("plugins.coding.commands")
