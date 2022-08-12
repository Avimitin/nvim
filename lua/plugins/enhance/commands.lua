local alias = require("editor.utils").alias

--
-- neoclip
--
alias("ClipView", [[Telescope neoclip]])
alias("ClipRec", function()
  require("neoclip").start()
  require("telescope").load_extension("neoclip")
end)

--
-- Stylua
--
alias("LuaFormat", [[Dispatch! stylua %]])

--
-- nvim-spectre
--
alias("SpectreOpen", function()
  require("spectre").open()
end)

--
-- BufferLine
--
alias("BufCL", [[BufferLineCloseLeft]])
alias("BufCR", [[BufferLineCloseRight]])

--
-- My Custom function
--

-- Highlight current line
alias("HiCurLine", [[call matchadd('HighlightLineMatches', '\%'.line('.').'l')]])

-- Close all highlight
alias("HiCurLineOff", [[call clearmatches()]])

-- Invoke sudo and tee to write content when you forget opening the neovim as root
--
-- You will need to configure the ssh-askpass executable in /etc/sudo.conf
--
--   * KDE use ksshaskpass
--   * Gnome use seahorse
--   * Other user can search corresponding program by keyword "ssh-askpass"
--
alias("SudoW", [[execute 'w !sudo tee % > /dev/null' <bar> edit!]])
