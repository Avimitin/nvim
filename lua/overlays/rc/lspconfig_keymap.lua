local lsp_keymap = function(client, bufnr)
  local Hydra = require("hydra")
  local cmd = require("hydra.keymap-util").cmd
  local hint = [[
                                Code Actions

  _a_: ﯧ Code Action      |  _f_:  Run format         | _l_:  Toggle Line diagnostic

              _j_:  Goto next error | _k_:  Goto previous error

  _d_:  Search Symbol    | _r_:  Rename Symbol       | _h_:  Open Document
  _s_:  Signature Help   | _t_:  Show Diagnostic     | _p_:  Preview Definition
  _D_:  Goto declaration | _M_:  Goto implementation | _T_:  Goto Type Define

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
      color = "pink",
      invoke_on_body = true,
      hint = {
        border = "rounded",
        position = "bottom",
      },
    },
    mode = { "n", "x" },
    body = "<leader>a",
    heads = {
      { "d", cmd("Lspsaga lsp_finder"), opts("Symbol Finder") },
      { "p", cmd("Lspsaga peek_definition"), opts("Preview definition") },
      { "h", cmd("Lspsaga hover_doc"), opts("Open document") },
      { "s", cmd("Lspsaga signature_help"), opts("Open signature help") },
      { "t", cmd("TroubleToggle"), opts("Show diagnostic") },
      { "r", cmd("Lspsaga rename"), opts("Rename") },
      { "a", cmd("Lspsaga code_action"), opts("Open Action") },
      { "j", cmd("Lspsaga diagnostic_jump_next"), { desc = "Jump to next error" } },
      { "k", cmd("Lspsaga diagnostic_jump_prev"), { desc = "Jump to previous error" } },
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
      {
        "l",
        function()
          local current = vim.diagnostic.config().virtual_text
          if current ~= false then
            vim.g.diagnostic_virtual_text_config = current
            vim.diagnostic.config({ virtual_text = false })
          else
            vim.diagnostic.config({ virtual_text = vim.g.diagnostic_virtual_text_config })
          end
          require("lsp_lines").toggle()
        end,
        opts("Run code formatter"),
      },
      { "q", nil, { exit = true, nowait = true, desc = "quit" } },
    },
  })

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  vim.api.nvim_buf_set_keymap(bufnr, "x", "gf", "", {
    silent = true,
    noremap = true,
    callback = function()
      vim.lsp.buf.range_formatting()
    end,
  })
end

return {
  lsp_keymap = lsp_keymap,
}
