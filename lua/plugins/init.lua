local utils = require("utils")
local M = {}

local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

-- has_packer return the packer install status
local function has_packer()
  return vim.fn.empty(vim.fn.glob(install_path)) == 0
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
  local packer_call, error_msg = pcall(vim.cmd, [[packadd packer.nvim]])
  if not packer_call then
    utils.errorL(error_msg, "load plugin")
    return
  end

  vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])
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
    }
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

-- prehook setup global variables before plugins loaded
local function prehook()
  if not vim.fn.has("nvim-0.6") then
    -- for filetype.nvim
    -- If using a Neovim version earlier than 0.6.0
    vim.g.did_load_filetypes = 1
  end

  -- for vsnip
  vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")

  -- for wildfire
  vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }

  -- for vim-markdown
  vim.g.vim_markdown_conceal_code_blocks = 0
  vim.g.vim_markdown_strikethrough = 1
end

-- posthook run script after plugins loaded
local function posthook()
  -- Run rooter when it is the first time enter the neovim
  vim.cmd([[autocmd VimEnter * Rooter]])
end

-- load will try to detect the packer installation status.
-- It will automatically install packer to the install_path.
-- Then it will called the setup script to setup all the plugins.
M.load = function()
  if not has_packer() then
    install_packer()
    add_packer()
    setup_plugins()
    require("packer").sync()
    vim.cmd(
      "au User PackerComplete echom 'Plugins are installed successfully, please use :qa to restart the neovim'"
    )
    return
  end

  add_packer()
  init_packer()
  prehook()
  setup_plugins()
  posthook()
end

return M
-- vim: foldmethod=marker
