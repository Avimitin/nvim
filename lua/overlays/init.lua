local function load_plugins()
  vim.cmd("packadd packer.nvim")
  local packer = require("packer")

  packer.init({
    display = {
      open_fn = function()
        return require("packer.util").float({
          border = "single",
        })
      end,
    },
    git = {
      clone_timeout = 60,
    },
    auto_clean = false,
    compile_on_sync = true,
    profile = {
      enable = false,
    },
    max_jobs = 50,
  })

  packer.set_handler("rc", function(_, plugin, rc_val)
    plugin.config = string.format([[require("overlays.rc.%s")]], rc_val)
  end)

  local all_repos = {}

  local overlays = {
    "coding",
    "markdown",
    "theme",
    "enhance",
    "completion",
  }

  for _, ova in ipairs(overlays) do
    local repos = require("overlays." .. ova)
    vim.list_extend(all_repos, repos)
  end

  packer.startup(function(use)
    -- Packer can manage itself
    use({
      "wbthomason/packer.nvim",
      event = "VimEnter",
    })

    for _, repo in ipairs(all_repos) do
      use(repo)
    end
  end)
end

-- TODO: bootstrap
load_plugins()
