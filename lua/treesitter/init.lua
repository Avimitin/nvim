local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  rev = "d19def46c112c26c17adeef88dd1253cc6d623a1",
  sha256 = "sha256-u2f1nhred9AK5s0DoF6x1cg/TxvR5rc5/G00fG1s+b4=",
  branch = "main",
  config = function()
    vim.schedule(function()
      require("treesitter.config")
    end)
  end,
})

register(
  "nvim-treesitter/nvim-treesitter-textobjects",
  {
    rev = "baa6b4ec28c8be5e4a96f9b1b6ae9db85ec422f8",
    sha256 = "sha256-kbnZaUtIKXoBj4tFiQWSpGmI6Doke7PjoetqIhsI8lc=",
    branch = "main",
  }
)
register(
  "windwp/nvim-ts-autotag",
  {
    rev = "c4ca798ab95b316a768d51eaaaee48f64a4a46bc",
    sha256 = "sha256-nT2W5gKFEfzP7MztLjm7yqwam3ADk0svcMdLg2nmI/4=",
  }
)
