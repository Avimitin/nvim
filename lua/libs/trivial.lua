local M = {}

function M.disable_builtin_plugins(plugins)
  for _, plug in ipairs(plugins) do
    local var = "loaded_" .. plug
    vim.g[var] = 1
  end
end

function M.disable_builtin_providers(providers)
  for _, prov in ipairs(providers) do
    local var = "loaded_" .. prov .. "_provider"
    vim.g[var] = 0
  end
end

function M.set_vim_opt(options)
  for k, v in pairs(options) do
    vim.opt[k] = v
  end
end

function M.make_cache(dir)
  local cache = vim.fn.stdpath("cache") .. "/" .. dir
  local resp = vim.fn.mkdir(cache, "p")
  if resp == 1 then
    return cache
  end

  return nil
end

function M.load_plugins(specs)
  local logger = require("libs.logger")

  local lazy_nvim_install_path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.uv.fs_stat(lazy_nvim_install_path) then
    logger.info("Package manager not found, installing...")
    local success, maybe_error = pcall(vim.fn.system, {
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazy_nvim_install_path,
    })

    if not success then
      logger.error("ERROR: Fail to install plugin manager lazy.nvim", maybe_error)
      logger.info("You can cleanup the " .. lazy_nvim_install_path .. " and start again")
      return
    end
  end

  vim.opt.rtp:prepend(lazy_nvim_install_path)

  require("lazy").setup(specs, {
    install = {
      colorscheme = { "kanagawa" },
    },
  })
end

return M
