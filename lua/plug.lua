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

return require('packer').startup(function(use)
    -- Packer can manage itself
    use {"wbthomason/packer.nvim", event = "VimEnter"}

    -- ======================= EDITOR SETUP ==============================
    use {'kyazdani42/nvim-web-devicons', event = "BufRead"}

    use {
        'glepnir/galaxyline.nvim',
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
        after = "packer.nvim",
        config = function() require("colors") end
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

    use {
        'windwp/nvim-autopairs',
        config = function() require('config.autopairs') end,
        after = 'nvim-cmp'
    }

    -- ========= LSP ============
    use {
        'williamboman/nvim-lsp-installer',
        ft = {
            "bash", "sh", "rust", "c", "cpp", "lua", "markdown", "go", "html",
            "toml", "json", "python"
        }
    }

    use {
        'neovim/nvim-lspconfig',
        config = function() require("config.lsp") end,
        after = "nvim-lsp-installer"
    }

    use {
        "ray-x/lsp_signature.nvim",
        config = function() require("config.lsp-signature") end,
        after = "nvim-lspconfig"
    }

    -- RUST
    use {
        'simrat39/rust-tools.nvim',
        after = "nvim-lspconfig",
        config = function() require("config.rust") end
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
        event = 'BufRead',
        config = function() require('config.treesitter') end
    }

    -- git information
    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        event = "BufRead",
        config = function() require("config.gitsign") end
    }

    use {
        'TimUntersberger/neogit',
        requires = {'nvim-lua/plenary.nvim', 'sindrets/diffview.nvim'},
        config = function()
            require('neogit').setup {
                integrations = {diffview = true},
                -- Change the default way of opening neogit
                kind = "split_above",
                -- customize displayed signs
                signs = {
                    -- { CLOSED, OPENED }
                    section = {"", ""},
                    item = {"", ""},
                    hunk = {"", ""}
                }
            }
        end,
        cmd = "Neogit"
    }

    -- Golang
    use {
        'fatih/vim-go',
        config = function() require("config.vim-go") end,
        ft = {"go"}
    }

    -- CPP
    use {'rhysd/vim-clang-format', ft = {'cpp', 'c', 'h', 'hpp'}}

    use {
        'sbdchd/neoformat',
        cmd = "Neoformat",
        setup = function()
            vim.g.neoformat_cpp_clangformat = {
                exe = 'clang-format',
                args = {'-style=file'}
            }
            vim.g.neoformat_enabled_cpp = {'clangformat'}
            vim.g.neoformat_enabled_c = {'clangformat'}
        end
    }

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

    -- easy motion
    use {
        'phaazon/hop.nvim',
        config = function()
            require'hop'.setup {keys = 'etovxqpdygfblzhckisuran'}
        end,
        cmd = {'HopChar2', 'HopLine', 'HopPattern'}
    }

    -- open a big terminal
    use {
        'numtostr/FTerm.nvim',
        config = function()
            require("config.fterm")
            require("config.lazygit")
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

    -- TrueZen.nvim: zen mode in neovim--
    use {
        'Pocco81/TrueZen.nvim',
        cmd = {'TZAtaraxis', 'TZFocus', "TZMinimalist"}
    }

    -- vim-commentary: for quickly commenting--
    use {'tpope/vim-commentary', event = "BufRead"}

    -- mulit cursor
    use {'mg979/vim-visual-multi', event = "InsertEnter", branch = 'master'}

    -- open file when forget sudo
    use {'lambdalisue/suda.vim', cmd = {'SudaWrite', 'SudaRead'}}

    use {
        'famiu/nvim-reload',
        cmd = {"Reload", "Restart"},
        requires = "nvim-lua/plenary.nvim"
    }

    use {
        'andweeb/presence.nvim',
        event = "VimEnter",
        -- I don't use discord recently, and this plugin delay my neovim.
        -- Set disable = false to enable this plugin.
        disable = true,
        config = function()
            require("presence"):setup({
                neovim_image_text = "HELP!",
                editing_text = "STUCK IN THE %s FILE",
                workspace_text = "HELP! HOW TO QUIT VIM!"
            })
        end
    }

    use {"andrejlevkovitch/vim-lua-format", ft = {"lua"}}

    use {
        'glepnir/dashboard-nvim',
        cmd = {"Dashboard"},
        config = function() require("config.dashboard") end
    }

    -- highlight all the word below the cursor
    use {'RRethy/vim-illuminate', event = "BufRead"}

    -- file navigation
    use {
        'mcchrish/nnn.vim',
        config = function()
            require("nnn").setup({
                command = "nnn -e -d -D",
                set_default_mappings = 0,
                action = {
                    ["<c-t>"] = "tab split",
                    ["<c-s>"] = "split",
                    ["<c-v>"] = "vsplit",
                    ["<c-o>"] = "copy_to_clipboard"
                },
                layout = {
                    window = {width = 0.7, height = 0.8, highlight = 'Debug'}
                }
            })
        end,
        cmd = "NnnPicker"
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
        end,
        cmd = {'AnyJump', 'AnyJumpBack'}
    }

    -- telescope: extensible fuzzy file finder--
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-media-files.nvim'
        },
        config = function() require("config.telescope_config") end,
        cmd = {"Telescope"}
    }

    use {
        "AckslD/nvim-neoclip.lua",
        after='telescope.nvim',
        config = function()
          require('neoclip').setup()
          require('telescope').load_extension('neoclip')
        end
    }

    -- Select text object
    use {'gcmt/wildfire.vim', event = "VimEnter"}

    -- surrounding select text with given text
    use {"tpope/vim-surround", after = "wildfire.vim"}

    -- align
    use {'junegunn/vim-easy-align', cmd = 'EasyAlign'}

    -- Vim alternative
    use {
        'chentau/marks.nvim',
        config = function()
            require'marks'.setup {
                default_mappings = true,
                cyclic = true,
                force_write_shada = false,
                refresh_interval = 250,
                sign_priority = {
                    lower = 10,
                    upper = 15,
                    builtin = 8,
                    bookmark = 20
                },
                mappings = {}
            }
        end
    }

    -- speed up neovim!
    use {
        'nathom/filetype.nvim'
        -- If using a Neovim version earlier than 0.6.0
        -- setup = function()
        --   vim.g.did_load_filetypes = 1
        -- end
    }
end)
