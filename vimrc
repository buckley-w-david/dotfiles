""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use Vim settings, rather then Vi settings.
set nocompatible

" set character encoding to utf-8
scriptencoding utfs8
set encoding=utf-8

" Map our leader key to comma
:let mapleader = ","

" Tired of your wrong shit
" noremap <Left> :echoe "Use h"<CR>
" nnoremap <Right> :echoe "Use l"<CR>
" nnoremap <Up> :echoe "Use k"<CR>
" nnoremap <Down> :echoe "Use j"<CR>

" Reload files if changed outside vim
set autoread

:set expandtab          " tabs to spaces
:set tabstop=4          " visual spaces per tabs
:set shiftwidth=4       " indentation is also 4 spaces

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

" Before writing the buffer in .py files
" remove trailing whitespace
autocmd BufWritePre *.py :%s/\s\+$//e

command W w
command Q q
command WQ wq
command Wq wq

command GST terminal ++close gst % -

color slate-with-skyblue-comments

""""""""""""""""""""""""
" Plugin Configuration "
""""""""""""""""""""""""
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Status Bar
Plugin 'vim-airline/vim-airline'
Plugin 'editorconfig/editorconfig'
" comment out stuff (gcc, gc)
Plugin 'tpope/vim-commentary'
" :Git
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
" Adds a new object based on indentation
Plugin 'michaeljsmith/vim-indent-object'
" Create your own text objects
" Plugin 'kana/vim-textobj-user'
Plugin 'tpope/vim-surround'
" Allow plugin stuff to repeat
Plugin 'tpope/vim-repeat'
" Align text (Needed for vim-markdown)
Plugin 'godlygeek/tabular'
" Syntax highlighting, matching rules and mappings for Markdown
Plugin 'plasticboy/vim-markdown'
" Distinct highlighting of keywords vs values, JSON-specific (non-JS)
" warnings, quote concealing.
Plugin 'elzr/vim-json'
" Asynchronous Lint Engine
" Plugin 'w0rp/ale'
" Plugin 'elixir-editors/vim-elixir'
Plugin 'chrisbra/csv.vim'
" Plugin 'plytophogy/vim-virtualenv'
" Plugin 'mg979/vim-visual-multi'
" Sensible defaults - I'm a noob
Plugin 'tpope/vim-sensible'
" Dispatch ... for async running of stuff
Plugin 'tpope/vim-dispatch'
" Plugin 'ambv/black'
Plugin 'fatih/vim-go'
" Plugin 'prettier/vim-prettier'
" Plugin 'vim-scripts/st.vim'
" Plugin 'francoiscabrol/ranger.vim'
" Plugin 'xuhdev/vim-latex-live-preview'
" Plugin 'dart-lang/dart-vim-plugin'
" Plugin 'thosakwe/vim-flutter'
Plugin 'neovimhaskell/haskell-vim.git'
call vundle#end()

" Ale
" nmap <silent> <C-k> <Plug>(ale_previous_wrap)
" nmap <silent> <C-j> <Plug>(ale_next_wrap)

" vim-markdown
let g:vim_markdown_folding_disabled = 1

" editorconfig + vim-fugitive
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" vim-prettier
" let g:prettier#config#single_quote = 'true'
" let g:prettier#config#semi = 'false'

" Flutter
" call FlutterMenu()

" haskell-vim
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
