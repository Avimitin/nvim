require('packer').init {
    display = {
        open_fn = function()
            return require("packer.util").float {border = "single"}
        end
    },
    git = {
        clone_timeout = 60 -- Timeout, in seconds, for git clones
    }
}

local map = require('utils').map

return require('packer').startup(function(use)
    -- Packer can manage itself
    use {"wbthomason/packer.nvim", event = "VimEnter"}

    -- ======================= EDITOR SETUP ==============================
    use {'kyazdani42/nvim-web-devicons', event = "BufRead"}

    use {
        'Avimitin/galaxyline.nvim',
        branch = 'main',
        after = "nvim-web-devicons",
        config = function() require("config.statusline") end
    }

    use {
        'kyazdani42/nvim-tree.lua',
        config = function() require("config.nvimtree") end,
        cmd = {"NvimTreeRefresh", "NvimTreeToggle"}
    }

    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function() require("config.indent") end,
        event = 'BufRead'
    }

    -- nvim-bufferline: better buffer line--
    use {
        'akinsho/nvim-bufferline.lua',
        config = function() require("config.bufferline_config") end,
        event = "BufRead"
    }

    -- neovim color theme
    use {
        'Avimitin/neovim-deus',
        'Shatur/neovim-ayu',
        'rebelot/kanagawa.nvim',
    }

    -- ==================== CODING ==================================

    use {'rafamadriz/friendly-snippets', event = "InsertEnter"}

    -- ======= Completion =========
    use {
        'hrsh7th/nvim-cmp',
        after = "friendly-snippets",
        config = function() require("config.completion") end,
        requires = {'onsails/lspkind-nvim'}
    }

    use {'hrsh7th/cmp-path', after = {'nvim-cmp'}}

    use {'hrsh7th/cmp-nvim-lsp', after = {'nvim-cmp'}}

    use {'hrsh7th/cmp-buffer', after = {'nvim-cmp'}}

    use {'hrsh7th/cmp-vsnip', after = {'nvim-cmp'}}

    use {'hrsh7th/vim-vsnip', after = {'nvim-cmp'}}

    use {"lukas-reineke/cmp-rg", after = {'nvim-cmp'}}

    use {
        'windwp/nvim-autopairs',
        config = function() require('config.autopairs') end,
        after = 'nvim-cmp'
    }

    -- ========= LSP ============
    use {
        'williamboman/nvim-lsp-installer',
        ft = {
            "bash", "sh", "c", "cpp", "lua", "go", "html",
            "toml", "json", "python", "javascript"
        },
        config = function() require("lspconfig") end,
    }

    use {
        'neovim/nvim-lspconfig',
        config = function() require("config.lsp") end,
        module = "lspconfig",
    }

    -- RUST
    use {
        'simrat39/rust-tools.nvim',
        ft = "rust",
        config = function() require("config.rust") end
    }

    use {
      'mfussenegger/nvim-dap',
      module = "dap",
      config = function()
        require("config.dap_config")
      end
    }

    use {
      'rcarriga/nvim-dap-ui',
      module = "dapui",
    }

    use {
        'rust-lang/rust.vim',
        after = "rust-tools.nvim",
        setup = function()
            vim.g.rust_clip_command = 'xclip -selection clipboard'
        end
    }

    use {
        'simrat39/symbols-outline.nvim',
        config = function() require("config.symbols") end,
        cmd = "SymbolsOutline"
    }

    -- treesitter: support more colorful highlighting
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        ft = {"cpp", "toml", "rust", "go", "json", "lua", "fish", "c", "javascript"},
        config = function() require('config.treesitter') end
    }

    -- git information
    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        event = "BufRead",
        config = function() require("config.gitsign") end
    }

    -- Golang
    use {
        'fatih/vim-go',
        config = function() require("config.vim-go") end,
        ft = {"go"}
    }

    -- CPP
    use {'rhysd/vim-clang-format', ft = {'cpp', 'c', 'h', 'hpp'}}

    -- =================== Utilities =========================

    -- markdown toc
    use {'mzlogin/vim-markdown-toc', cmd = {'GenTocGFM'}}

    -- markdown preview
    use {
        'iamcco/markdown-preview.nvim',
        run = function() vim.fn['mkdp#util#install']() end,
        config = function() require("config.mkdp") end,
        ft = {"markdown"}
    }

    use {
        'plasticboy/vim-markdown',
        ft = {"markdown"}
    }

    -- open a big terminal
    use {
        'numtostr/FTerm.nvim',
        config = function()
            vim.cmd [[hi LazygitBackground guibg=#1a1b26]]
            vim.cmd [[hi FTermBackground   guibg=#1a1b26]]
            require("config.fterm")
            require('utils').new_cmd("LazygitToggle",
                                     "lua require('config.lazygit').toggle()",
                                     true)
        end,
        cmd = {'FTermToggle', 'LazygitToggle'}
    }

    use {'kassio/neoterm', cmd = {'T', "Tkill", "Tclose", "Tmap"}}

    -- show color at words
    use {
        'RRethy/vim-hexokinase',
        run = 'make',
        cmd = "HexokinaseToggle",
        setup = function()
            vim.g.Hexokinase_highlighters = {'backgroundfull'}
            vim.g.Hexokinase_optInPatterns = {
                'full_hex', 'rgb', 'rgba', 'hsl', 'hsla'
            }
        end
    }

    -- vim-commentary: for quickly commenting--
    use {'tpope/vim-commentary', event = "BufRead"}

    -- mulit cursor
    use {'mg979/vim-visual-multi', event = "InsertEnter", branch = 'master'}

    -- open file when forget sudo
    use {'lambdalisue/suda.vim', cmd = {'SudaWrite', 'SudaRead'}}

    use {"andrejlevkovitch/vim-lua-format", cmd = {'LuaFormat'}}

    use {
        'glepnir/dashboard-nvim',
        cmd = {"Dashboard"},
        config = function() require("config.dashboard") end
    }

    use {
        'airblade/vim-rooter',
        cmd = "Rooter",
        setup = function()
            vim.g.rooter_manual_only = 1
            vim.g.rooter_change_directory_for_non_project_files = 'current'
            vim.g.rooter_patterns = {'.git', 'Cargo.toml'}
        end
    }

    use {
        'pechorin/any-jump.vim',
        setup = function()
            vim.g.any_jump_window_width_ratio = 0.8
            vim.g.any_jump_window_height_ratio = 0.9
            vim.g.any_jump_disable_default_keybindings = 1
        end,
        cmd = {'AnyJump', 'AnyJumpBack'}
    }

    -- telescope: extensible fuzzy file finder--
    use {
        'nvim-telescope/telescope.nvim',
        requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
        config = function() require("config.telescope_config") end,
        module = 'telescope'
    }

    use {
        "AckslD/nvim-neoclip.lua",
        cmd = {'ClipRec', 'ClipView'},
        config = function()
            require('neoclip').setup()
            require('telescope').load_extension('neoclip')
        end
    }

    -- Select text object
    use {'gcmt/wildfire.vim', event = "BufRead"}

    -- surrounding select text with given text
    use {"tpope/vim-surround", after = "wildfire.vim"}

    -- align
    use {'junegunn/vim-easy-align', cmd = 'EasyAlign'}

    -- speed up neovim!
    use {
        'nathom/filetype.nvim',
        config = function()
          require("filetype").setup({
            -- overrides the filetype or function for filetype
            -- See https://github.com/nathom/filetype.nvim#customization
            overrides = {}
          })
        end
    }

    use {
        'beauwilliams/focus.nvim',
        cmd = {"FocusSplitNicely", "FocusSplitCycle"},
        module = "focus",
        config = function()
            require("focus").setup({hybridnumber = true, bufnew = true})
            map("n", "<C-W>h", ":FocusSplitLeft<CR>")
            map("n", "<C-W>l", ":FocusSplitRight<CR>")
            map("n", "<C-W>j", ":FocusSplitDown<CR>")
            map("n", "<C-W>k", ":FocusSplitUp<CR>")
            map("n", "<C-W>s", ":FocusSplitNicely<CR>")
        end
    }

    use {
        'tpope/vim-fugitive',
        cmd = {'G', 'Git', 'Ggrep', 'Gdiffsplit', 'GBrowse'}
    }

    -- Dependency: tmux, nnn
    -- This is heavily based on my configured nnn
    use {
        "luukvbaal/nnn.nvim",
        config = function()
            require("nnn").setup({
                picker = {
                    cmd = [[NNN_PLUG="p:preview-tui" ICONLOOKUP=1 tmux new-session nnn -a -Pp]],
                    style = {border = "shadow"},
                    session = "shared"
                }
            })
        end,
        cmd = {'NnnPicker', 'NnnExplorer'}
    }

    use {
      "ggandor/lightspeed.nvim"
    }

    use {
      'mhinz/vim-sayonara',
      setup = function()
        vim.g.sayonara_confirm_quit = 1
      end,
      cmd = 'Sayonara'
    }
end)
