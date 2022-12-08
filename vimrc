"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General - Note this is actually an nvim config, I'm a bad person for calling
" it .vimrc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO: Should I remove the general config here that is handled by vim-sensible
" TODO: I should probably go though this config in detail and make some
" attempt at better organization. Maybe split into multiple files?

" Use Vim settings, rather then Vi settings.
set nocompatible

" set character encoding to utf-8
scriptencoding utfs8
set encoding=utf-8

" Map our leader key to comma
let mapleader = ","
noremap \ ,

" Reload files if changed outside vim
set autoread

set expandtab          " tabs to spaces
set tabstop=4          " visual spaces per tabs
set shiftwidth=4       " indentation is also 4 spaces

set ruler               " turn on ruler
set number              " add line numbers
set cursorline          " highlight currrent line
filetype indent on      " load filetype-specific indent files
filetype plugin on      " load filetype-specific plugin files
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]

" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" next/previous in arglist with wrapping
nnoremap <silent> <leader>s :exe ( argidx() == argc() - 1 ? 'first' : 'next') <cr>
nnoremap <silent> <leader>a :exe ( argidx() == 0 ? 'last' : 'previous') <cr>

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Makes splits happen in the direction I expect
set splitbelow
set splitright

syntax enable           " enable syntax processing
syntax on           " enable syntax processing

" move vertically by visual line
nnoremap j gj
nnoremap k gk

" highlight last inserted text
nnoremap gV `[v`]

" Moving Lines - From VimTricks 2020-10-08
nnoremap <c-j> :m .+1<CR>==
nnoremap <c-k> :m .-2<CR>==
inoremap <c-j> <Esc>:m .+1<CR>==gi
inoremap <c-k> <Esc>:m .-2<CR>==gi
vnoremap <c-j> :m '>+1<CR>gv=gv
vnoremap <c-k> :m '<-2<CR>gv=gv

" Paste on new line
nnoremap ,p :pu<CR>

set showcmd             " show the command
set hidden " Hide a buffer when abandoning it instead of unloading it - This allows you to navigate away without having to save

" Before writing the buffer in .py files
" remove trailing whitespace
autocmd BufWritePre *.py :%s/\s\+$//e

command W w
command Q q
command WQ wq
command Wq wq

command GST terminal ++close gst % -

color slate-with-skyblue-comments

set exrc

""""
" Functions
"""""

""""""""""""""""""""""""
" Plugin Configuration "
""""""""""""""""""""""""

call plug#begin()

" Status Bar
Plug 'vim-airline/vim-airline'

" comment out stuff (gcc, gc)
Plug 'tpope/vim-commentary'

" :Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'

" Git dif (+/-) in gutter
Plug 'airblade/vim-gitgutter'

" Fuzzy finding
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'kyazdani42/nvim-web-devicons'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'nvim-treesitter/playground'


" Commit viewer
" :GV, :GV!, :GV?
Plug 'junegunn/gv.vim'

" Git stuff
Plug 'tpope/vim-fugitive'

" See git diff in commit window as another pane
Plug 'rhysd/committia.vim'

" Expand visual selection regions with +, shrink with _
Plug 'terryma/vim-expand-region'

" Create your own text objects
Plug 'kana/vim-textobj-user'
  Plug 'nelstrom/vim-textobj-rubyblock' " Text object for ruby blocks (r object)
  Plug 'michaeljsmith/vim-indent-object' " Indent based text object (i object)

" Nicer split/join lines with gJ/gS
Plug 'AndrewRadev/splitjoin.vim'

" Opperate on surrounded text
Plug 'tpope/vim-surround'

" Repeat stuff (including plugin stuff)
Plug 'tpope/vim-repeat'

" Dispatch ... for async running of stuff
" TODO actually get comfortable with using this
Plug 'tpope/vim-dispatch'

" Add syntax sugar to run shell commands
" :Delete, :Move, :Rename, etc
" Basically make it so I need to go into the directory tree less
Plug 'tpope/vim-eunuch'

" Sensible defaults
Plug 'tpope/vim-sensible'

" Allow operating from the cursor to the beginning or end of text objects
Plug 'tommcdo/vim-ninja-feet'

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp' " Autocompletion plugin
Plug 'hrsh7th/cmp-nvim-lsp' " LSP source for nvim-cmp
Plug 'kosayoda/nvim-lightbulb' " Show where code actions can happen

call plug#end()

lua << EOF

local map = vim.api.nvim_set_keymap
local normal_opts = { noremap=true, silent=true }

--
--        nvim-treesitter
--

require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  textobjects = {
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim 
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}

--
--        nvim-lsp           
--
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua require(\'telescope.builtin\').lsp_definitions()<cr>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua require(\'telescope.builtin\').lsp_implementations()<cr>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>ca', '<cmd>lua require(\'telescope.builtin\').lsp_code_actions()<cr>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua require(\'telescope.builtin\').lsp_references()<cr>', opts)
  buf_set_keymap('n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

end

-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { 'pyright', 'rust_analyzer' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    }
  }
end


-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
  },
}


-- require'lspconfig'.sorbet.setup{}
--
--       Telescope        
--
local telescope = require 'telescope'
telescope.setup({
  defaults = {
    mappings = {
      i = {
        ["<C-h>"] = "which_key"
      }
    }
  }
})
telescope.load_extension('fzf')

map('n', 'ff', "<cmd>lua require('telescope.builtin').find_files()<cr>", normal_opts)
map('n', 'fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>", normal_opts)
map('n', 'fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", normal_opts)
map('n', 'fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", normal_opts)
map('n', 'fs', "<cmd>lua require('telescope.builtin').git_status()<cr>", normal_opts)

EOF

" nnoremap <silent> <C-p> :FZF<CR>
" let g:fzf_action = {
" \   'ctrl-t': 'tab split',
" \   'ctrl-x': 'split',
" \   'ctrl-v': 'vsplit',
" \   'ctrl-a': 'argedit',
" \}


"""""""""""""""""""""""""""
"         End             "
"""""""""""""""""""""""""""
set secure
