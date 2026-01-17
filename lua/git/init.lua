local pack = require("pack").register

pack("lewis6991/gitsigns.nvim", {
  rev = "42d6aed4e94e0f0bbced16bbdcc42f57673bd75e",
  sha256 = "sha256-L89x9n2OKCyUuWaNXPkuNGBEU9EBX+9zRlzS1Kfw428=",
  config = function()
    require("git.gitsigns")
  end,
})
