return function(_, bufnr)
  local bnmap = function(mappings)
    require("libs.keymap").buf_map(bufnr, "n", mappings)
  end

  local lspsaga = function(action)
    return require("libs.keymap").wrap_cmd("Lspsaga " .. action)
  end

  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  bnmap({
    {
      "gf",
      function()
        vim.lsp.buf.format({ async = true })
      end,
      desc = "Format code",
    },

    { "gd", lspsaga("lsp_finder"), desc = "Find symbol" },
    { "gp", lspsaga("peek_definition"), desc = "Peek definition" },
    { "gh", lspsaga("hover_doc"), desc = "Open document" },
    { "gr", lspsaga("rename"), desc = "Rename symbol" },
    { "ga", lspsaga("code_action"), desc = "Open code action" },
    { "go", lspsaga("show_line_diagnostics"), desc = "Show diagnostics" },
    { "gO", lspsaga("outline"), desc = "Open code outline" },
    { "gT", lspsaga("peek_type_definition"), desc = "Peek type definition" },
    { "[d", lspsaga("diagnostic_jump_prev"), desc = "Jump to previous error" },
    { "]d", lspsaga("diagnostic_jump_next"), desc = "Jump to next error" },
    { "gt", "<CMD>TroubleToggle<CR>", desc = "Open quickfix" },
    {
      "gl",
      function()
        require("lsp_lines").toggle()
      end,
      desc = "Show diagnostic inline",
    },
  })
end
