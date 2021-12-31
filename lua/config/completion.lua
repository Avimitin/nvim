local cmp = require 'cmp'

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and
             vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col)
                 :match("%s") == nil
end

local feedkey = function(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true),
                        mode, true)
end

cmp.setup({
  snippet = {
    expand = function(args)
      -- For `vsnip` user.
      vim.fn["vsnip#anonymous"](args.body)
    end
  },
  formatting = {
    format = require("lspkind").cmp_format({
      with_text = true,
      menu = ({
        buffer = "﬘ Buf",
        nvim_lsp = " LSP",
        luasnip = " LSnip",
        snippet = " VSnip",
        nvim_lua = " NvimLua",
        latex_symbols = "[Latex]",
        rg = "גּ RG",
        neorg = "[ ORG-MODE]",
      })
    })
  },
  mapping = {
    ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), {
      'i', 'c'
    }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), {
      'i', 'c'
    }),
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close()
    }),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    },
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
    end, {"i", "s"}),

    ["<S-Tab>"] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      elseif vim.fn["vsnip#jumpable"](-1) == 1 then
        feedkey("<Plug>(vsnip-jump-prev)", "")
      end
    end, {"i", "s"})
  },
  sources = {
    {name = 'nvim_lsp'},
    {name = 'vsnip'},
    {name = 'buffer'},
    {name = 'path'},
    {name = 'rg'},
    {name = 'neorg'}
  },
  experimental = {
    ghost_text = true
  }
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  sources = {
    {
      name = 'buffer'
    }
  }
})

local function setup_neorg_cmp()
    -- Get the current Neorg state
  local neorg = require('neorg')

  --- Loads the Neorg completion module
  local function load_completion()
      neorg.modules.load_module("core.norg.completion", nil, {
          engine = "nvim-cmp" -- Choose your completion engine here
      })
  end

  -- If Neorg is loaded already then don't hesitate and load the completion
  if neorg.is_loaded() then
      load_completion()
  else -- Otherwise wait until Neorg gets started and load the completion module then
      neorg.callbacks.on_event("core.started", load_completion)
  end
end

-- Register setup function here
local function cmp_setup_hook()
  local notify_list = { setup_neorg_cmp }
  for _, regis in ipairs(notify_list) do
    regis()
  end
end

cmp_setup_hook()
