local cmp = require("cmp")
local override = vim.cfg.completion
local kind_icons = vim.cfg.icons

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0
    and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local feedkey = function(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

cmp.setup({
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
        item.kind = kind_icons["Vim"]
        item.menu = "Vim"
        return item
      elseif entry.source.name == "nvim_lsp_signature_help" then
        item.kind = "Property"
      end
      item.menu = item.kind
      item.kind = kind_icons[item.kind]
      return item
    end,
  },
  mapping = {
    [override.keymap.scroll_up] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
      "i",
      "c",
    }),
    [override.keymap.scroll_down] = cmp.mapping(cmp.mapping.scroll_docs(4), {
      "i",
      "c",
    }),
    [override.keymap.abort] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    [override.keymap.confirm] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    }),
    [override.keymap.select_next] = cmp.mapping(function(fallback)
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
    [override.keymap.select_prev] = cmp.mapping(function()
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
  sorting = {
    comparators = {
      -- Return true: entry2 -> entry1
      -- Return false: entry1 -> entry2
      -- Return nil: entry1 <-> entry2
      function(entry1, entry2)
        local lsp_types = require("cmp.types").lsp

        if entry1.source.name ~= "nvim_lsp" then
          if entry2.source.name == "nvim_lsp" then
            return false
          else
            return nil
          end
        end

        local priority = {
          Field = 11,
          Property = 11,
          Constant = 10,
          Enum = 10,
          EnumMember = 10,
          Event = 10,
          Function = 10,
          Method = 10,
          Operator = 10,
          Reference = 10,
          Struct = 10,
          Variable = 9,
          File = 8,
          Folder = 8,
          Class = 5,
          Color = 5,
          Module = 5,
          Keyword = 2,
          Constructor = 1,
          Interface = 1,
          Snippet = 0,
          Text = 1,
          TypeParameter = 1,
          Unit = 1,
          Value = 1,
        }

        local kind1 = lsp_types.CompletionItemKind[entry1:get_kind()]
        local kind2 = lsp_types.CompletionItemKind[entry2:get_kind()]

        local priority1 = priority[kind1] or 0
        local priority2 = priority[kind2] or 0

        if priority1 == priority2 then
          return nil
        end

        return priority2 < priority1
      end,
    },
  },
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
