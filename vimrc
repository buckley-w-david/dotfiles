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

" Tired of your wrong shit
" noremap <Left> :echoe "Use h"<CR>
" nnoremap <Right> :echoe "Use l"<CR>
" nnoremap <Up> :echoe "Use k"<CR>
" nnoremap <Down> :echoe "Use j"<CR>

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

" fzf - Fuzzy finding
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Commit viewer
" :GV, :GV!, :GV?
Plug 'junegunn/gv.vim'

" See git diff in commit window as another pane
Plug 'rhysd/committia.vim'

" Expand visual selection regions with +, shrink with _
Plug 'terryma/vim-expand-region'

" Adds a new object based on indentation
Plug 'michaeljsmith/vim-indent-object'

" Create your own text objects
Plug 'kana/vim-textobj-user'
  Plug 'nelstrom/vim-textobj-rubyblock'

Plug 'michaeljsmith/vim-indent-object'

" Autocompletion
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Show git diff in the side gutter (+/-)
Plug 'airblade/vim-gitgutter'

"Split and join constructs using gJ and gS
Plug 'AndrewRadev/splitjoin.vim'

" Opperate on surrounded text
Plug 'tpope/vim-surround'

" Allow plugin stuff to repeat
Plug 'tpope/vim-repeat'

" Add syntax sugar to run shell commands
" :Delete, :Move, :Rename, etc
" Basically make it so I need to go into the directory tree less
Plug 'tpope/vim-eunuch'

" Asynchronous Lint Engine
" Plug 'dense-analysis/ale'

" Sensible defaults
Plug 'tpope/vim-sensible'

" Dispatch ... for async running of stuff
" TODO actually get comfortable with using this
Plug 'tpope/vim-dispatch'

" Allow operating from the cursor to the beginning or end of text objects
Plug 'tommcdo/vim-ninja-feet'

Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

call plug#end()

"""""""""""""""""""""""""""
"  vim-lsp/autocomplete   "
"""""""""""""""""""""""""""
imap <c-space> <Plug>(asyncomplete_force_refresh)

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'allowlist': ['python'],
        \ })
endif

if executable('rust-analyzer')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rust-analyzer',
        \ 'cmd': {server_info->['rust-analyzer']},
        \ 'allowlist': ['rust'],
        \ })
endif

if executable('clangd')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd']},
        \ 'allowlist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })
endif

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> <F12> <plug>(lsp-definition)
    nmap <buffer> <leader><F12> <plug>(lsp-references)
    nmap <buffer> <leader>rn <plug>(lsp-rename)
    nmap <buffer> ga <plug>(lsp-code-action)
    nmap <buffer> K <plug>(lsp-hover)

    nmap <buffer> gs <plug>(lsp-document-symbol-search)
    nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)
    inoremap <buffer> <expr><c-f> lsp#scroll(+4)
    inoremap <buffer> <expr><c-d> lsp#scroll(-4)

    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
    
    " refer to doc to add more commands
endfunction

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

"""""""""""""""""""""""""""
"          FZF            "
"""""""""""""""""""""""""""
nnoremap <silent> <C-p> :FZF<CR>

"""""""""""""""""""""""""""
"         End             "
"""""""""""""""""""""""""""
set secure
