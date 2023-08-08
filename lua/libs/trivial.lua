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

return M
