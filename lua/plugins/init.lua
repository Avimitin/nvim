local utils = require("editor.utils")
local uv = vim.loop

local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

local plugins = {
  repos = {},
}

-- load plugins from modules
plugins.load = function()
  local modules = {
    "coding",
    "completion",
    "markdown",
    "enhance",
    "git",
    "colorscheme",
  }

  for _, mod in ipairs(modules) do
    require("plugins." .. mod)
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

-- has_packer return the packer install status
local function has_packer()
  return uv.fs_stat(install_path) ~= nil
end

-- install_packer will use git to install packer to the install_path
local function install_packer()
  utils.infoL("Installing packer to " .. install_path)
  vim.fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end

-- init_packer will setup the packer style
local function init_packer()
  vim.cmd("packadd packer.nvim")

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

local function bootstrap()
  install_packer()
  vim.cmd("packadd packer.nvim")
  plugins.load()
  -- notify user to quit neovim when bootstrap process done
  vim.cmd(
    "au User PackerComplete echom 'Plugins are installed successfully, please use :qa to exit and restart the neovim'"
  )
  require("packer").sync()
end

-- init plugins
plugins.init = function()
  if not has_packer() then
    bootstrap()
    return
  end

  require("impatient")

  init_packer()
  plugins.load()
end

-- register plugins
plugins.register = function(plug)
  vim.list_extend(plugins.repos, plug)
end

local au = vim.api.nvim_create_autocmd
-- auto compile when editing the load.lua file
au({ "BufWritePost" }, {
  pattern = { "config.lua", "init.lua" },
  callback = function()
    local full_path = vim.fn.expand("%:p")
    -- if the load.lua file is not inside our configuration directory
    -- abort the operation
    if not full_path:match(vim.fn.stdpath("config")) then
      return
    end

    local current_file = vim.fn.expand("%:t")

    if current_file == "init.lua" then
      local parent_dir = vim.fn.expand("%:p:h:t")
      if not (parent_dir == "config") then
        return
      end
    end

    vim.cmd("source " .. full_path)

    require("packer").clean()
    require("packer").compile()
    vim.notify("Packer compile done!", "info", { title = "packer" })
  end,
})

return plugins
-- vim: foldmethod=marker
