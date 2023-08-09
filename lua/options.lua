local libs = require("libs")

local backupdir = libs.make_cache("backup")
local undodir = libs.make_cache("undo")
return {
  vim_options = {
    backupdir = backupdir,
    directory = backupdir,
    undofile = true,
    undodir = undodir,
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
    -- disable relative line number(Use `[]` to trigger leap.nvim)
    rnu = false,
    -- TAB SETTING
    -- Use 2 spaces forcely. But don't worry, vim-sleuth will handle the indent
    -- gracefully. See <https://github.com/tpope/vim-sleuth> for more.
    --
    -- Use the appropriate number of spaces to insert a <Tab>.
    expandtab = true,
    -- Number of spaces that a <Tab> in the file counts for.
    tabstop = 2,
    shiftwidth = 2,
    softtabstop = 2,
    -- Copy indent from current line when starting a new line
    autoindent = true,
    -- A List is an ordered sequence of items.
    list = true,
    listchars = { tab = "> ", trail = "·" },
    -- Minimal number of screen lines to keep above and below the cursor.
    scrolloff = 5,
    -- 200 is more appropriate for which-keys. You can quickly input keys without prompting up
    -- the which-keys panel, or wait 200ms if you forget keymappings.
    timeoutlen = 800,
    -- Time in milliseconds to wait for a key code sequence to complete
    ttimeoutlen = 200,
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
    showcmd = true,
    -- auto completion on command
    wildmenu = true,
    -- ignore case when searching and only on searching
    ignorecase = true,
    smartcase = true,
    inccommand = "split",
    ttyfast = true,
    visualbell = true,
    updatetime = 100,
    virtualedit = "block",
    -- always draw signcolumn, with 1 fixed space to show 2 icon at the same time
    signcolumn = "yes:1",
    -- enable all the mouse functionality
    mouse = "a",
    -- use indent as the fold method
    foldmethod = "indent",
    foldlevel = 99,
    foldenable = true,
    formatoptions = "qj",
    hidden = true,
    -- command line line-height property, set it to 0 to gain
    -- more compat UI
    ch = 0,
    -- options to truncate hit-enter prompts causesd by file message
    -- 1. "a": Use all abbreviations, such as truncate "Modified" to "[+]"
    -- 2. "T": Truncate file message in the middle if it is too long
    -- 3. "W": Do not show "written" or "[w]" when writing a file
    -- 4. "F": Do not show file info when editing a file
    shortmess = "aTWF",
  },
  disabled = {
    builtin_plugins = {
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
    },
    builtin_providers = {
      "perl",
      "node",
      "ruby",
      "python",
      "python3",
    },
  },
}
