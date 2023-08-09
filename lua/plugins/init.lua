return vim
  .iter({
    "completion",
    "git",
    "lang",
    "note",
    "tools",
    "ui",
  })
  :map(function(cat)
    return require("plugins." .. cat)
  end)
  :totable()
