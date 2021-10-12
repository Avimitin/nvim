local ok, custom = pcall(require, "custom")

local settings = {
  color_scheme = "deus",
  background = "dark",
}

if ok and custom ~= nil then
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

local loaded = pcall(vim.cmd, "colorscheme "..settings.color_scheme)
if not loaded then
  if settings.color_scheme == nil then
    print("Unknow empty colorscheme")
  end

  print("failed to load colorscheme: "..settings.color_scheme)
end
