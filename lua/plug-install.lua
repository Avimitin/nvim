local installed, _ = pcall(require, 'packer')
local firsttime = false
if not installed then
	local install_path = vim.fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
	print("Installing packer to "..install_path)
	vim.fn.delete(install_path, "rf")

	vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
	firsttime=true

	installed, error = pcall(require, 'packer')
	if not installed then
		print(error)
		return
	end
	vim.cmd [[packadd packer.nvim]]
end

vim.cmd([[autocmd BufWritePost plug-install.lua source <afile> | PackerCompile]])

require('packer').init{
	display = {
		open_fn = function()
			return require("packer.util").float {border = "single"}
		end
	},
	git = {
		clone_timeout = 60 -- Timeout, in seconds, for git clones
	}
}

require('packer').startup(function(use)
	  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

	use {
		'lukas-reineke/indent-blankline.nvim',
		config=function() require("plugins.indent") end,
	}

  --telescope: extensible fuzzy file finder--
	use {
		'nvim-telescope/telescope.nvim',
		requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
		config=function() require("plugins.telescope") end,
	}

  --nvim-bufferline: better buffer line--
  use {
		'akinsho/nvim-bufferline.lua',
		config=function() require("plugins.bufferline") end,
	}

  --nvim-lspinstall: lsp manager--
  use 'kabouzeid/nvim-lspinstall'

  --nvim-compe: code completion--
	use {
		'hrsh7th/nvim-compe',
		config=function() require("plugins.compe") end,
	}

	use {
		"L3MON4D3/LuaSnip",
		config=function()
			require("plugins.luasnip")
		end
	}

	use {
		"rafamadriz/friendly-snippets",
		after='LuaSnip',
	}

  --nvim-lspconfig: built-in lsp--
  use {
		'neovim/nvim-lspconfig',
		config=function() require("plugins.lsp") end,
	}

  --nvim-tree.lua--
  use {
		'kyazdani42/nvim-tree.lua',
		config=function()
			require("plugins.nvimtree")
		end,
		requires='kyazdani42/nvim-web-devicons',
	}

  --TrueZen.nvim: zen mode in neovim--
  use 'Pocco81/TrueZen.nvim'

  --vim-commentary: for quickly commenting--
  use 'tpope/vim-commentary'

  --barbar.nvim: bufferline bar--
  use 'kyazdani42/nvim-web-devicons'

  --fancy start page--
  use 'mhinz/vim-startify'

  --markdown preview
	use {
		'iamcco/markdown-preview.nvim',
		run = function() vim.fn['mkdp#util#install']() end,
		cmd = 'MarkdownPreview',
		ft={'md'},
	}

  --mulit cursor
	use {
		'mg979/vim-visual-multi',
		branch = 'master',
	}

  --open file when forget sudo
  use 'lambdalisue/suda.vim'

  --treesitter: support more colorful highlighting
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
		config = function() require('plugins.treesitter') end,
	}

  --neovim color theme
  use {
	  'Avimitin/neovim-deus',
	  config=function()
		  vim.cmd([[colorscheme deus]])
	  end,
  }

  use 'morhetz/gruvbox'

	use {
		'glepnir/galaxyline.nvim',
		branch='main',
		requires={
			{'kyazdani42/nvim-web-devicons'}
		},
		config=function() require("plugins.galaxyline") end
	}

  --highlight all the word below the cursor
  use 'RRethy/vim-illuminate'

  --file navigation
  use {
		'mcchrish/nnn.vim',
		config=function() require("plugins.nnn") end,
	}

  use 'airblade/vim-rooter'

  use {
		'pechorin/any-jump.vim',
		config=function()
			require("plugins.anyjump")
		end,
	}

	--git information
	use {
		'lewis6991/gitsigns.nvim',
		requires = {
			'nvim-lua/plenary.nvim'
		},
		config=function() require("plugins.gitsign") end,
	}

  --Golang support
	use {
		'fatih/vim-go',
		ft={'go'},
		config=function() require("plugins.vim-go") end,
	}

  --Select text object
  use 'gcmt/wildfire.vim'

  --surrounding select text with given text
  use 'tpope/vim-surround'

  --align
  use {
		'junegunn/vim-easy-align',
		config=function ()
			vim.api.nvim_set_keymap("v", "<leader>e", ":EasyAlign<CR>", {noremap=true, silent=true})
		end
	}

  --find and replace
  use 'brooth/far.vim'

  --markdown toc
  use {
		'mzlogin/vim-markdown-toc',
		ft={'md'},
	}

  --clang-format
  use {
		'rhysd/vim-clang-format',
		ft={'cpp', 'c', 'h', 'hpp'},
	}

  --easy motion
	use {
		'phaazon/hop.nvim',
		config=function()
			require'hop'.setup {
				keys = 'etovxqpdygfblzhckisuran'
			}
			local options={noremap=true, silent=true}
			vim.api.nvim_set_keymap('n', 'u'        , ':HopChar2<CR>', options)
			vim.api.nvim_set_keymap('n', '<Leader>j', ':HopLine<CR>', options)
			vim.api.nvim_set_keymap('n', '<Leader>k', ':HopLine<CR>', options)
		end,
	}

  -- open a big terminal
  use {
		'numtostr/FTerm.nvim',
		config=function()
			require("plugins.fterm")
			require("plugins.lazygit")
		end,
	}

	use {
		'windwp/nvim-autopairs',
		config=function() require('nvim-autopairs').setup() end,
	}

	use {
		'sbdchd/neoformat'
	}

	-- show color at words
	use {
		'RRethy/vim-hexokinase',
		run='make hexokinase',
		config=function()
			vim.g.Hexokinase_highlighters = {'backgroundfull'}
			vim.g.Hexokinase_optInPatterns = {'full_hex','rgb','rgba','hsl','hsla'}
		end
	}

	use {
		'simrat39/symbols-outline.nvim',
		config=function()
			require("plugins.symbols")
		end
	}

	use {
		'simrat39/rust-tools.nvim',
		ft={'rust','rs'},
		config=function()
			require("plugins.rust")
		end,
	}
end)

if firsttime then vim.cmd[[PackerSync]] end
