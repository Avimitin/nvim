local utils = require("core.utils")
local uv = vim.loop

local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

-- has_packer return the packer install status
local function has_packer()
  return uv.fs_stat(install_path) ~= nil
end

-- install_packer will use git to install packer to the install_path
local function install_packer()
  -- detecting plugin manager
  local fn = vim.fn

  utils.infoL("Installing packer to " .. install_path)
  fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end

-- add_packer will add packer into the optional plugin directory
local function add_packer()
  vim.cmd("packadd packer.nvim")
end

-- init_packer will setup the packer style
local function init_packer()
  require("packer").init({
    display = {
      open_fn = function()
        return require("packer.util").float({
          border = "single",
        })
      end,
    },
    git = {
      clone_timeout = 60, -- Timeout, in seconds, for git clones
    },
    auto_clean = true,
    compile_on_sync = true,
    profile = {
      enable = true,
    },
    max_jobs = 50,
  })
end

-- setup_plugins will get list of plugins definition and use them
local function setup_plugins()
  require("packer").startup(function(use)
    -- Packer can manage itself
    use({
      "wbthomason/packer.nvim",
      event = "VimEnter",
    })

    for _, plugin in ipairs(require("plugins.load")) do
      use(plugin)
    end
  end)
end

-- ======================================================
-- public functions
-- ======================================================
local plugins = {
  repos = {}
}

-- load will try to detect the packer installation status.
-- It will automatically install packer to the install_path.
-- Then it will called the setup script to setup all the plugins.
plugins.init = function()
  if not has_packer() then
    install_packer()
    add_packer()
    setup_plugins()
    vim.cmd(
      "au User PackerComplete echom 'Plugins are installed successfully, please use :qa to restart the neovim'"
    )
    require("packer").sync()
    return
  end

  add_packer()
  init_packer()
  setup_plugins()
end

plugins.load = function()
  local awake = function(mod)
    local prefix = "plugins.modules."
    require(prefix..mod)
  end

  local modules = {
    "coding",
    "completion",
    "markdown",
    "enhance",
    "git"
  }

  for _, mod in ipairs(modules) do
    awake(mod)
  end

  require("packer").startup(function(use)
    -- Packer can manage itself
    use({
      "wbthomason/packer.nvim",
      event = "VimEnter",
    })

    for _, repo in ipairs(plugins.repos) do
      use(repo)
    end
  end)
end

plugins.register = function(plug)
  vim.list_extend(plugins.repos, plug)
end

return plugins
-- vim: foldmethod=marker
