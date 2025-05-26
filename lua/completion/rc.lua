local cmp = require("cmp")

local keymap = {
  scroll_up = "<C-u>",
  scroll_down = "<C-d>",
  abort = "<C-c>",
  confirm = "<CR>",
  select_next = "<Tab>",
  select_prev = "<S-Tab>",
}

local icons = {
  Text = "",
  Method = "󰆧",
  Function = "󰊕",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
  Vim = "",
}

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0
    and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local feedkey = function(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

cmp.setup({
  view = {
    entries = { name = "custom", selection_order = "near_cursor" },
  },
  snippet = {
    expand = function(args)
      -- For `vsnip` user.
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  window = {
    documentation = {
      winhighlight = "Normal:CmpDocumentation,FloatBorder:CmpDocumentation,Search:None",
      side_padding = 1,
    },
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, item)
      -- return special icon for cmdline completion
      if entry.source.name == "cmdline" then
        item.kind = icons["Vim"]
        item.menu = "Vim"
        return item
      elseif entry.source.name == "nvim_lsp_signature_help" then
        item.kind = "Property"
      end
      item.menu = item.kind
      item.kind = icons[item.kind]
      return item
    end,
  },
  mapping = {
    [keymap.scroll_up] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
      "i",
      "c",
    }),
    [keymap.scroll_down] = cmp.mapping(cmp.mapping.scroll_docs(4), {
      "i",
      "c",
    }),
    [keymap.abort] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    [keymap.confirm] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    }),
    [keymap.select_next] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif vim.fn["vsnip#available"]() == 1 then
        feedkey("<Plug>(vsnip-expand-or-jump)", "")
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { "i", "s" }),
    [keymap.select_prev] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      elseif vim.fn["vsnip#jumpable"](-1) == 1 then
        feedkey("<Plug>(vsnip-jump-prev)", "")
      end
    end, { "i", "s" }),
  },
  sources = {
    { name = "nvim_lsp", priority = 99 },
    { name = "nvim_lsp_signature_help" },
    { name = "vsnip" },
    { name = "path" },
  },
  experimental = {
    ghost_text = true,
  },
  preselect = cmp.PreselectMode.None,
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline("/", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "nvim_lsp_document_symbol" },
  }, {
    { name = "buffer" },
  }),
})

cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})
