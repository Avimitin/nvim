local lsp_keymap = function(client, bufnr)
  local Hydra = require("hydra")
  local cmd = require("hydra.keymap-util").cmd
  local hint = [[
                                Code Actions

      _a_: ﯧ Code Action      |  _f_:  Run format         | _o_:  Show diagnostic

  _d_:  Search Symbol         | _r_:  Rename Symbol       | _h_:  Open Document
  _t_:  Workspace Diagnostics | _p_:  Preview Definition  | _O_:  Open outline Window   
  _D_:  Goto declaration      | _M_:  Goto implementation | _T_:  Goto Type Define

  _q_: Quit
]]
  local opts = function(desc)
    return { exit = true, nowait = true, desc = desc }
  end
  Hydra({
    name = "LSP Utilities",
    hint = hint,
    config = {
      buffer = bufnr,
      color = "red",
      invoke_on_body = true,
      hint = {
        border = "rounded",
        position = "top",
      },
      on_enter = function()
        vim.g.diagnostic_virtual_text_config = vim.diagnostic.config().virtual_text
        vim.diagnostic.config({ virtual_text = false })
        require("lsp_lines").toggle()
      end,
      on_exit = function()
        require("lsp_lines").toggle()
        vim.diagnostic.config({ virtual_text = vim.g.diagnostic_virtual_text_config })
      end,
    },
    mode = { "n", "x" },
    body = "<leader>a",
    heads = {
      { "d", cmd("Lspsaga lsp_finder"), opts("Symbol Finder") },
      { "p", cmd("Lspsaga peek_definition"), opts("Preview definition") },
      { "h", cmd("Lspsaga hover_doc"), opts("Open document") },
      { "t", cmd("TroubleToggle"), opts("Show diagnostic") },
      { "r", cmd("Lspsaga rename"), opts("Rename") },
      { "a", cmd("Lspsaga code_action"), opts("Open Action") },
      { "o", cmd("Lspsaga show_line_diagnostics"), opts("Show diagnostic in current line") },
      { "O", cmd("SymbolsOutline"), opts("Show outline panel") },
      {
        "D",
        function()
          if client.server_capabilities.declarationProvider then
            vim.lsp.buf.declaration()
          else
            vim.notify(("%s doesn't support jump to declaration"):format(client.name))
          end
        end,
        opts("Go to declaration"),
      },
      {
        "M",
        function()
          if client.server_capabilities.implementationProvider then
            vim.lsp.buf.implementation()
          else
            vim.notify(("%s doesn't support jump to implementation"):format(client.name))
          end
        end,
        opts("Go to implementation"),
      },
      {
        "T",
        function()
          if client.server_capabilities.typeDefinitionProvider then
            vim.lsp.buf.type_definition()
          else
            vim.notify(("%s doesn't support jump to type"):format(client.name))
          end
        end,
        opts("Go to type definition"),
      },
      {
        "f",
        function()
          vim.lsp.buf.format({ async = true })
        end,
        opts("Run code formatter"),
      },
      { "q", nil, { exit = true, nowait = true, desc = "quit" } },
    },
  })

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  vim.api.nvim_buf_set_keymap(bufnr, "x", "gf", "", {
    silent = true,
    noremap = true,
    callback = function()
      vim.lsp.buf.range_formatting()
    end,
  })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[[", "<CMD>Lspsaga diagnostic_jump_prev<CR>", {
    silent = true,
    noremap = true,
  })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]]", "<CMD>Lspsaga diagnostic_jump_next<CR>", {
    silent = true,
    noremap = true,
  })
end

return {
  lsp_keymap = lsp_keymap,
}
