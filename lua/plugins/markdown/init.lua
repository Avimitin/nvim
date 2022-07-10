require("plugins.markdown.config").pre()

local repo = require("plugins.markdown.repos")
require("plugins").register(repo)
