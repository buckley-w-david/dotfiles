-- Inspiration: https://github.com/wbthomason/dotfiles/blob/linux/neovim/.config/nvim/lua/plugins.lua
-- TODO: Clean this up a little, I have some new plugins in here from the above config, they should either
-- be investigated or removed.
-- TODO: Look into config for more plugins, now that we can separate it into config modules we can be liberal with configuration
return require('packer').startup(function(use)
  -- Packer
  use 'wbthomason/packer.nvim'

  use 'lewis6991/impatient.nvim'

  -- Async building & commands
  use { 'tpope/vim-dispatch', cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } }

  -- Marks
  -- use { 'kshenoy/vim-signature', config = [[require('config.signature')]], disable = true }

  use { 'tversteeg/registers.nvim', keys = { { 'n', '"' }, { 'i', '<c-r>' } } }

  -- Movement
  use { 'justinmk/vim-sneak' }

  -- Search
  use {
    {
      'nvim-telescope/telescope.nvim',
      requires = {
        'nvim-lua/popup.nvim',
        'nvim-lua/plenary.nvim',
        'telescope-frecency.nvim',
        'telescope-fzf-native.nvim',
      },
      wants = {
        'popup.nvim',
        'plenary.nvim',
        'telescope-frecency.nvim',
        'telescope-fzf-native.nvim',
      },
      setup = [[require('config.telescope_setup')]],
      config = [[require('config.telescope')]],
      cmd = 'Telescope',
      module = 'telescope',
    },
    {
      "nvim-telescope/telescope-frecency.nvim",
      config = function()
        require"telescope".load_extension("frecency")
      end,
      requires = {"tami5/sqlite.lua"}
    },
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      run = 'make',
    },
  }

  -- Git
  use {
    { 'tpope/vim-fugitive' },
    {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
      -- config = [[require('config.gitsigns')]],
    },
  }

  -- Pretty symbols
  use 'kyazdani42/nvim-web-devicons'

  -- Completion and linting
  use {
    'onsails/lspkind-nvim',
    { 'neovim/nvim-lspconfig', config = [[require('config.nvim_lsp')]] },
    { 'kosayoda/nvim-lightbulb', config = [[require('config.lightbulb')]] },
  }

  -- Highlights
  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    run = ':TSUpdate',
    config = [[require('config.treesitter')]],
  }

  -- Completion
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'L3MON4D3/LuaSnip',
      { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' },
      'hrsh7th/cmp-nvim-lsp',
      { 'hrsh7th/cmp-path', after = 'nvim-cmp' },
      { 'saadparwaiz1/cmp_luasnip', after = 'nvim-cmp' },
    },
    config = [[require('config.cmp')]],
    event = 'InsertEnter *',
  }

  use({ "jose-elias-alvarez/null-ls.nvim",
    requires = {"nvim-lua/plenary.nvim", "neovim/nvim-lspconfig"}
    })

  -- Status Bar
  use 'vim-airline/vim-airline'

  -- comment out stuff (gcc, gc)
  use 'tpope/vim-commentary'

  -- Git dif (+/-) in gutter
  use 'airblade/vim-gitgutter'

  -- Commit viewer
  -- :GV, :GV!, :GV?
  use 'junegunn/gv.vim'

  -- See git diff in commit window as another pane
  use 'rhysd/committia.vim'

  -- Expand visual selection regions with +, shrink with _
  use 'terryma/vim-expand-region'

  -- Nicer split/join lines with gJ/gS
  use 'AndrewRadev/splitjoin.vim'

  -- Opperate on surrounded text
  use 'tpope/vim-surround'

  -- Repeat stuff (including plugin stuff)
  use 'tpope/vim-repeat'

  -- Add syntax sugar to run shell commands
  -- :Delete, :Move, :Rename, etc
  -- Basically make it so I need to go into the directory tree less
  use 'tpope/vim-eunuch'

  -- Sensible defaults
  use 'tpope/vim-sensible'

  -- Allow operating from the cursor to the beginning or end of text objects
  use 'tommcdo/vim-ninja-feet'
end)
