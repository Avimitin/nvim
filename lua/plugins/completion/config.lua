local M = {}

M.pre = function()
  vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")
end

M.nvim_cmp_config = function()
  local cmp = require("cmp")

  local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0
      and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
  end

  local feedkey = function(key, mode)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
  end

  local kind_icons = {
    Text = "",
    Method = "",
    Function = "",
    Constructor = "",
    Field = "",
    Variable = "",
    Class = "ﴯ",
    Interface = "",
    Module = "",
    Property = "ﰠ",
    Unit = "",
    Value = "",
    Enum = "",
    Keyword = "",
    Snippet = "",
    Color = "",
    File = "",
    Reference = "",
    Folder = "",
    EnumMember = "",
    Constant = "",
    Struct = "",
    Event = "",
    Operator = "",
    TypeParameter = "",
  }

  if cmp == nil or cmp.setup == nil then
    require("editor.utils").infoL("fail to load nvim-cmp", "nvim-cmp")
    return
  end

  cmp.setup({
    snippet = {
      expand = function(args)
        -- For `vsnip` user.
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },
    window = {
      documentation = cmp.config.window.bordered(),
    },
    formatting = {
      fields = { "kind", "abbr", "menu" },
      format = function(entry, item)
        -- return special icon for cmdline completion
        if entry.source.name == "cmdline" then
          item.kind = ""
          item.menu = "Vim"
          return item
        end
        item.menu = item.kind
        item.kind = kind_icons[item.kind]
        return item
      end,
    },
    mapping = {
      ["<C-u>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
        "i",
        "c",
      }),
      ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(4), {
        "i",
        "c",
      }),
      ["<C-e>"] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ["<CR>"] = cmp.mapping.confirm({
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      }),
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif vim.fn["vsnip#available"]() == 1 then
          feedkey("<Plug>(vsnip-expand-or-jump)", "")
        elseif has_words_before() then
          cmp.complete()
        else
          fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
        end
      end, { "i", "s" }),

      ["<S-Tab>"] = cmp.mapping(function()
        if cmp.visible() then
          cmp.select_prev_item()
        elseif vim.fn["vsnip#jumpable"](-1) == 1 then
          feedkey("<Plug>(vsnip-jump-prev)", "")
        end
      end, { "i", "s" }),
    },
    sources = {
      { name = "nvim_lsp", priority = 99 },
      { name = "vsnip" },
      { name = "path" },
    },
    experimental = {
      ghost_text = true,
    },
  })

  cmp.setup.filetype({ "markdown", "asciidoc", "text", "gitcommit" }, {
    sources = {
      { name = "dictionary", keyword_length = 2 },
      { name = "path" },
      { name = "vsnip" },
    },
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline("/", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      {
        name = "buffer",
      },
    },
  })

  cmp.setup.cmdline(":", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })
end

M.cmp_dictionary_config = function()
  require("cmp_dictionary").setup({
    dic = {
      ["*"] = "/usr/share/dict/en.dic",
    },
    first_case_insensitive = true,
    document = true,
  })
  require("cmp_dictionary").update()
end

return M
