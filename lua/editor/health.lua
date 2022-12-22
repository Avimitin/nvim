local M = {}
local vh = vim.health
local fn = vim.fn

local function run(cmd)
  local handle = io.popen(cmd)
  if handle == nil then
    print("Something unexpected happen during ripgrep version check")
    return
  end
  local stdout = handle:read("*a")
  handle:close()
  return stdout
end

local function dependencies_check()
  vh.report_start("Dependencies health check")

  if fn.has("nvim-0.8") == 0 then
    vh.report_error("This configuration require at least neovim 0.8 to run")
  else
    vh.report_ok("Running neovim 0.8+")
  end

  if fn.executable("rg") == 0 then
    vh.report_error("ripgrep executable `rg` not found, required for telescope.nvim")
  else
    local stdout = run("rg -V")
    local version = vim.split(stdout, "\n")[1]
    vh.report_ok(version .. " founded")
  end

  if fn.executable("git") == 0 then
    vh.report_warning("`git` executable not found, required for gitsigns.nvim/fugitive")
  else
    vh.report_ok("`git` executable founded")
  end

  if vim.g.nvcfg.autocmds.fcitx5 then
    if fn.executable("fcitx5-remote") == 0 then
      vh.report_error("Auto command fcitx5 enabled but `fcitx5-remote` executable not found")
    else
      vh.report_ok("`fcitx5` executable founded")
    end
  end

  if fn.executable(vim.g.nvcfg.markdown.previewer) == 0 then
    vh.report_error(vim.g.nvcfg.markdown.previewer .. " executable not found")
  else
    vh.report_ok(vim.g.nvcfg.markdown.previewer .. " executable founded")
  end
end

local function null_ls_check()
  vh.report_start("Checking null-ls sources")
  local ok = true
  for _, source in ipairs(vim.g.nvcfg.null_ls_sources) do
    if fn.executable(source) == 0 then
      vh.report_error(source .. " is enable but executable not found")
      ok = false
    end
  end

  if ok then
    vh.report_ok("All the sources are ready")
  end
end

M.check = function()
  dependencies_check()
  null_ls_check()
end

return M
