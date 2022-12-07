local function load_plugins()
  vim.cmd("packadd packer.nvim")
  local packer = require("packer")

  -- Yet another disgustin hack for packer
  -- Issue: https://github.com/wbthomason/packer.nvim/issues/751
  local get_max_jobs = function()
    if #vim.api.nvim_list_uis() == 0 then
      return nil
    else
      return 30
    end
  end

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
    max_jobs = get_max_jobs(),
  })

  packer.set_handler("rc", function(_, plugin, rc_val)
    local setup = require("overlays.rc._setups")
    if not type(rc_val) == "string" then
      return
    end
    local stat =
      vim.loop.fs_stat(string.format("%s/lua/overlays/rc/%s.lua", vim.fn.stdpath("config"), rc_val))
    if stat then
      plugin.config = string.format([[require("overlays.rc.%s")]], rc_val)
    end
    -- explicit return nil to avoid unexpected type
    plugin.setup = setup[rc_val] or nil
  end)

  local all_repos = {}

  local overlays = {
    "coding",
    "markdown",
    "theme",
    "enhance",
    "completion",
    "git",
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

-- install_packer will use git to install packer to the install_path
local function install_packer(path)
  vim.notify("Installing packer to " .. path)
  vim.fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    path,
  })
end

local function bootstrap(path)
  install_packer(path)
  load_plugins()
  -- notify user to quit neovim when bootstrap process done
  vim.cmd("au User PackerComplete echom 'Plugins installed, please restart neovim'")
  require("packer").sync()
end

local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

-- Async packer loader
vim.loop.fs_stat(
  install_path,
  vim.schedule_wrap(function(err, _)
    if err ~= nil then
      if err:find("no such file") then
        bootstrap(install_path)
      else
        vim.notify(("Fail to find packer: %s"):format(err), vim.log.levels.ERROR)
      end

      return
    end

    load_plugins()
  end)
)
