local ok, custom = pcall(require, "custom")
if not ok then
  return
end

local settings = {
  color_scheme = "deus",
  background = "dark",
}

print("done")

if custom ~= nil then
  if custom.background ~= nil then
    settings.background = custom.background
  end
  if custom.color_scheme ~= nil then
    settings.color_scheme = custom.color_scheme
  end
end

-- theme
vim.opt.termguicolors=true
vim.opt.background=settings.background

local _, error = pcall(vim.cmd, "colorscheme "..settings.color_scheme)
print(error)
