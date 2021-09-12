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

    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function() require("plugins.indent") end,
        event = 'BufEnter'
    }

    -- telescope: extensible fuzzy file finder--
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-media-files.nvim'
        },
        config = function() require("plugins.telescope") end,
        event = "BufEnter"
    }

    -- nvim-bufferline: better buffer line--
    use {
        'akinsho/nvim-bufferline.lua',
        config = function() require("plugins.bufferline") end
    }

    -- nvim-compe: code completion--
    use {
        'hrsh7th/nvim-compe',
        config = function() require("plugins.compe") end,
        event = 'InsertEnter',
        requires = {
            {
                "L3MON4D3/LuaSnip",
                config = function() require("plugins.luasnip") end
            }, {"rafamadriz/friendly-snippets", after = 'LuaSnip'}
        }
    }

    use {'kabouzeid/nvim-lspinstall', event = "BufRead"}

    -- nvim-lspconfig: built-in lsp--
    use {
        'neovim/nvim-lspconfig',
        config = function() require("plugins.lsp") end,
        after = "nvim-lspinstall"
    }

    use {
        after = "nvim-lspconfig",
        "ray-x/lsp_signature.nvim",
        config = function() require("plugins.lsp-signature") end
    }

    -- nvim-tree.lua--
    use {
        'kyazdani42/nvim-tree.lua',
        config = function() require("plugins.nvimtree") end,
        cmd = {"NvimTreeRefresh", "NvimTreeToggle"},
        requires = 'kyazdani42/nvim-web-devicons'
    }

    -- TrueZen.nvim: zen mode in neovim--
    use {
        'Pocco81/TrueZen.nvim',
        cmd = {'TZAtaraxis', 'TZFocus', "TZMinimalist"}
    }

    -- vim-commentary: for quickly commenting--
    use {'tpope/vim-commentary', keys = {{'n', 'gcc'}, {'v', 'gc'}}}

    -- markdown preview
    use {
        'iamcco/markdown-preview.nvim',
        run = function() vim.fn['mkdp#util#install']() end,
        config = function() require("plugins.mkdp") end,
        ft = {"markdown"}
    }

    -- mulit cursor
    use {'mg979/vim-visual-multi', event = "BufRead", branch = 'master'}

    -- open file when forget sudo
    use {'lambdalisue/suda.vim', cmd = {'SudaWrite', 'SudaRead'}}

    -- treesitter: support more colorful highlighting
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        event = 'VimEnter',
        config = function() require('plugins.treesitter') end
    }

    -- neovim color theme
    use {
        'Avimitin/neovim-deus',
        config = function() vim.cmd([[colorscheme deus]]) end
    }

    -- use 'morhetz/gruvbox'

    use {
        'glepnir/galaxyline.nvim',
        branch = 'main',
        requires = {{'kyazdani42/nvim-web-devicons'}},
        config = function() require("plugins.galaxyline") end
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
        event = "VimEnter",
        config = function()
            vim.g.rooter_patterns = {'.git', 'Cargo.toml'}
        end
    }

    use {
        'pechorin/any-jump.vim',
        config = function()
            vim.g.any_jump_window_width_ratio = 0.8
            vim.g.any_jump_window_height_ratio = 0.9
        end,
        cmd = {'AnyJump', 'AnyJumpBack'}
    }

    -- git information
    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        event = "BufRead",
        config = function() require("plugins.gitsign") end
    }

    -- Golang support
    use {
        'fatih/vim-go',
        ft = {'go'},
        config = function() require("plugins.vim-go") end
    }

    -- Select text object
    use {'gcmt/wildfire.vim', event = "BufRead"}

    -- surrounding select text with given text
    use {"tpope/vim-surround", event = "BufRead"}

    -- align
    use {'junegunn/vim-easy-align', cmd = 'EasyAlign'}

    -- find and replace
    use {'brooth/far.vim', cmd = {'Farr', "Farf"}}

    -- markdown toc
    use {'mzlogin/vim-markdown-toc', cmd = {'GenTocGFM'}}

    -- clang-format
    use {'rhysd/vim-clang-format', ft = {'cpp', 'c', 'h', 'hpp'}}

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
            require("plugins.fterm")
            require("plugins.lazygit")
        end
    }

    use {
        'windwp/nvim-autopairs',
        config = function() require('plugins/autopairs') end,
        after = 'nvim-compe'
    }

    use {
        'sbdchd/neoformat',
        cmd = "Neoformat",
        config = function()
            vim.g.neoformat_cpp_clangformat = {
                exe = 'clang-format',
                args = {'-style=file'}
            }
            vim.g.neoformat_enabled_cpp = {'clangformat'}
            vim.g.neoformat_enabled_c = {'clangformat'}
        end
    }

    -- show color at words
    use {
        'RRethy/vim-hexokinase',
        run = 'make hexokinase',
        cmd = {"HexokinaseToggle"},
        config = function()
            vim.g.Hexokinase_highlighters = {'backgroundfull'}
            vim.g.Hexokinase_optInPatterns = {
                'full_hex', 'rgb', 'rgba', 'hsl', 'hsla'
            }
        end
    }

    use {
        'simrat39/symbols-outline.nvim',
        config = function() require("plugins.symbols") end,
        cmd = "SymbolsOutline"
    }

    use {
        'simrat39/rust-tools.nvim',
        ft = {"rust"},
        requires = {{"nvim-lspconfig"}, {"telescope.nvim"}},
        config = function()
            require("plugins.rust")
        end
    }

    use {
        'rust-lang/rust.vim',
        ft = {"rust"},
        config = function()
            vim.g.rust_clip_command = 'xclip -selection clipboard'
        end
    }

    use {
        'glepnir/dashboard-nvim',
        cmd = {"Dashboard"},
        config = function() require("plugins.dashboard") end
    }

    use {
        'andweeb/presence.nvim',
        event = "VimEnter",
        config = function()
            require("presence"):setup({
                neovim_image_text = "HELP!",
                editing_text = "STUCK IN THE %s FILE",
                workspace_text = "HELP! HOW TO QUIT VIM!"
            })
        end
    }

    use {"andrejlevkovitch/vim-lua-format", ft = {"lua"}}
end)
