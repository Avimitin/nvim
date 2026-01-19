local register = require("pack").register

register("hrsh7th/nvim-cmp", {
  rev = "85bbfad83f804f11688d1ab9486b459e699292d6",
  sha256 = "sha256-gwuiUgz3UEFpaKs79BSWS4qkwOi+XMHIDFdYRatWt0g=",
  config = function()
    require("completion.rc")
  end,
})

register("hrsh7th/cmp-nvim-lsp", {
  rev = "cbc7b02bb99fae35cb42f514762b89b5126651ef",
  sha256 = "sha256-CYZdfAsJYQyW413fRvNbsS5uayuc6fKDvDLZ2Y7j3ZQ=",
})

register("hrsh7th/cmp-nvim-lsp-signature-help", {
  rev = "fd3e882e56956675c620898bf1ffcf4fcbe7ec84",
  sha256 = "sha256-gIlax5z+J7ZbygiUMvnKvz0m2bmck/W1swIYq6WDAGo=",
})

register("hrsh7th/cmp-path", {
  rev = "c642487086dbd9a93160e1679a1327be111cbc25",
  sha256 = "sha256-e4Rd2y1Wekp7aobpTGaUeoSBnlfIASDaBR8js5dh2Vw=",
})

register("hrsh7th/cmp-cmdline", {
  rev = "d126061b624e0af6c3a556428712dd4d4194ec6d",
  sha256 = "sha256-w1HwR13rXRyLgWabHwJYABXO++gPEOMr46poUzOChzg=",
})
