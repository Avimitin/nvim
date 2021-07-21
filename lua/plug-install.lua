local install_path = "~/.local/share/nvim/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
	vim.api.nvim_command([[
		function PackerSetup()
			PackerCompile
			PackerInstall
		endfunction
		autocmd VimEnter * call PackerSetup()
	]])
end

vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])

require('packer').startup(function(use)
	use 'lukas-reineke/indent-blankline.nvim'

  --telescope: extensible fuzzy file finder--
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'

  --nvim-bufferline: better buffer line--
  use 'akinsho/nvim-bufferline.lua'

  --nvim-lspinstall: lsp manager--
  use 'kabouzeid/nvim-lspinstall'

  --nvim-compe: code completion--
  use 'hrsh7th/nvim-compe'

  --nvim-lspconfig: built-in lsp--
  use 'neovim/nvim-lspconfig'

  --nvim-tree.lua--
  use 'kyazdani42/nvim-tree.lua'

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
		cmd = 'MarkdownPreviewk'
	}

  --mulit cursor
	use {
		'mg979/vim-visual-multi',
		branch = 'master',
	}

  --open file when forget sudo
  use 'lambdalisue/suda.vim'

  --treesitter: support more colorful highlighting
	use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  --neovim color theme
  use 'Avimitin/neovim-deus'
  use 'morhetz/gruvbox'

  --status bar
	use {
		'Avimitin/nerd-galaxyline',
		requires = {
			'glepnir/galaxyline.nvim',
			branch='main',
			requires = {'kyazdani42/nvim-web-devicons'}
		}
	}

  --highlight all the word below the cursor
  use 'RRethy/vim-illuminate'

  --file navigation
  use 'mcchrish/nnn.vim'
  use 'airblade/vim-rooter'
  use 'pechorin/any-jump.vim'

  --list function/module/struct tag
  use 'liuchengxu/vista.vim'

	--git information
	use {
		'lewis6991/gitsigns.nvim',
		requires = {
			'nvim-lua/plenary.nvim'
		}
	}

  --Golang support
	use {
		'fatih/vim-go',
	}

  --Select text object
  use 'gcmt/wildfire.vim'

  --surrounding select text with given text
  use 'tpope/vim-surround'

  --amazing icon
  use 'ryanoasis/vim-devicons'

  --align
  use 'junegunn/vim-easy-align'

  --find and replace
  use 'brooth/far.vim'

  --markdown toc
  use 'mzlogin/vim-markdown-toc'

  --clang-format
  use 'rhysd/vim-clang-format'

  --rust
  use 'rust-lang/rust.vim'

  --easy motion
	use {
		'phaazon/hop.nvim',
	}

  -- open a big terminal
  use 'numtostr/FTerm.nvim'

	use {
		'windwp/nvim-autopairs',
	}

	use {
		'sbdchd/neoformat'
	}
end)
