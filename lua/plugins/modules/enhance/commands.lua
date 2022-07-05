local alias = require("core.utils").alias

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
