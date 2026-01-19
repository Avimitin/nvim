--
-- vim option configuration
--

vim.g.mapleader = ";"

local options = {
  completeopt = { "menuone", "noselect", "menu" },
  -- Enables 24-bit RGB color in the TUI
  termguicolors = true,
  encoding = "utf-8",
  -- When file encoding forcely set to UTF-8, some file with non-Unicode
  -- encoding will lose information during the encoding conversion.
  -- If you have problem with this encoding, set value to empty string "".
  fileencoding = "utf-8",
  -- enable number
  number = true,
  -- enable relative number
  rnu = true,
  -- enable highlightfor current line
  cursorline = true,
  -- TAB SETTING
  -- Use 2 spaces forcely. But don't worry, vim-sleuth will handle the indent
  -- gracefully. See <https://github.com/tpope/vim-sleuth> for more.
  --
  -- Use the appropriate number of spaces to insert a <Tab>.
  expandtab = true,
  -- Number of spaces that a <Tab> in the file counts for.
  tabstop = 4,
  shiftwidth = 2,
  softtabstop = 2,
  -- A List is an ordered sequence of items.
  list = true,
  listchars = { tab = " ", trail = "·" },
  fillchars = {
    fold = " ",
    foldopen = "",
    foldclose = "",
    foldsep = " ",
    diff = "╱",
    eob = " ",
  },
  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 5,
  -- Time in milliseconds to wait for a mapped sequence to complete.
  timeoutlen = 500,
  -- Time in milliseconds to wait for a key code sequence to complete
  ttimeoutlen = 50,
  -- use timeout for showing which-keys
  timeout = true,
  -- remember where to recover cursor
  viewoptions = { "cursor", "folds", "slash", "unix" },
  -- lines longer than the width of the window will wrap and displaying continues
  -- on the next line.
  wrap = true,
  -- set text width to zero to use the wrap functionality
  tw = 0,
  cindent = true,
  -- set windows split at bottom-right by default
  splitright = true,
  splitbelow = true,
  -- don't show the "--VISUAL--" "--INSERT--" text
  showmode = false,
  -- show chars, selected block in visual mode
  showcmd = false,
  -- auto completion on command
  wildmenu = true,
  -- ignore case when searching and only on searching
  ignorecase = true,
  smartcase = true,
  inccommand = "split",
  ttyfast = true,
  visualbell = true,
  -- affect swap file write and cursorhold. Set it to 5secs to reduce performance impact
  updatetime = 5000,
  virtualedit = "block",
  -- enable all the mouse functionality
  mouse = "a",
  -- use indent as the fold method
  foldcolumn = "1",
  foldmethod = "indent",
  foldlevel = 99,
  foldenable = true,
  formatoptions = "qj",
  hidden = true,
  -- options to truncate hit-enter prompts causesd by file message
  -- 1. "a": Use all abbreviations, such as truncate "Modified" to "[+]"
  -- 2. "T": Truncate file message in the middle if it is too long
  -- 3. "W": Do not show "written" or "[w]" when writing a file
  -- 4. "F": Do not show file info when editing a file
  shortmess = "aTWF",
  -- let status bar (at bottom) be global status bar, don't change with winbar
  laststatus = 3,
  grepprg = function()
    if vim.fn.executable("rg") == 1 then
      return "rg --vimgrep --no-heading --smart-case"
    else
      return "grep -nIR $* /dev/null"
    end
  end,
  grepformat = function()
    if vim.fn.executable("rg") then
      return "%f:%l:%c:%m,%f:%l:%m"
    else
      return nil
    end
  end,
}

local function ensure_cache(suffix)
  local dir = vim.fn.stdpath("cache") .. "/" .. suffix
  -- TODO: Use vim.loop.fs_stat
  local resp = vim.fn.mkdir(dir, "p")
  if resp == 1 then
    return dir
  end

  return nil
end

local backup_dir = ensure_cache("backup")
if backup_dir then
  options.backupdir = backup_dir
  options.directory = backup_dir
end

local undo_dir = ensure_cache("undo")
local has_persist = vim.fn.has("persistent_undo")
if has_persist == 1 then
  options.undofile = true
  options.undodir = undo_dir
end

for k, v in pairs(options) do
  if type(v) == "function" then
    local a = v()
    if a ~= nil then
      vim.opt[k] = a
    end
  else
    vim.opt[k] = v
  end
end
-- END of vim options configuration

--
-- built-in configuration
--

-- built-in plugins that really useless
for _, plugin in ipairs({
  "gzip",
  "zip",
  "zipPlugin",
  "tar",
  "tarPlugin",
  "getscript",
  "getscriptPlugin",
  "vimball",
  "vimballPlugin",
  "2html_plugin",
  "matchit",
  "matchparen",
  "logiPat",
  "rust_vim",
  "rust_vim_plugin_cargo",
  "rrhelper",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
}) do
  local var = "loaded_" .. plugin
  vim.g[var] = 1
end

-- built-in neovim RPC provider that I never used
for _, provider in ipairs({
  "perl",
  "node",
  "ruby",
  "python",
  "python3",
}) do
  local var = "loaded_" .. provider .. "_provider"
  vim.g[var] = 0
end
-- END of built-in configuration

-- If anything went wrong, rm -r ~/.cache/nvim/luac
vim.loader.enable()

--
-- Filetype configuration
--
vim.filetype.add({
  extension = {
    asl = "asl",
    mill = "scala",
    mlir = "mlir",
    sail = "sail",
    sc = "scala",
    td = "tablegen",
    ll = "llvm",
  },
  filename = {
    [".makepkg.conf"] = "bash",
  },
  pattern = {
    [".*%.cpp%.inc"] = "cpp",
    [".*%.h%.inc"] = "cpp",
    [".*/lit.*cfg"] = "python",
    ["lit.*cfg"] = "python",
  },
})
