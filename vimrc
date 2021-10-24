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
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

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
Plug 'dense-analysis/ale'

" Sensible defaults
Plug 'tpope/vim-sensible'

" Dispatch ... for async running of stuff
" TODO actually get comfortable with using this
Plug 'tpope/vim-dispatch'

" Allow operating from the cursor to the beginning or end of text objects
Plug 'tommcdo/vim-ninja-feet'

call plug#end()

"""""""""""""""""""""""""""
"       deoplete          "
"""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1
" Use ALE an the completion sources for all code.
call deoplete#custom#option('sources', {
\ '_': ['ale'],
\})

"""""""""""""""""""""""""""
"          ALE            "
"""""""""""""""""""""""""""

nmap <silent> <A-k> <Plug>(ale_previous_wrap)
nmap <silent> <A-j> <Plug>(ale_next_wrap)

" Detect whether the file is inside a poetry, and set the executable to `poetry` if true.
let g:ale_python_auto_poetry = 1

" Fixers
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'ruby': ['rubocop'],
\   'python': ['black'],
\   'rust': ['rustfmt'],
\}
let g:ale_fix_on_save = 1

" Linters
" let g:ale_linters = {
" \   'rust': ['analyzer'],
" \}

" Autocomplete
" set omnifunc=ale#completion#OmniFunc
let g:ale_completion_autoimport = 1

" GOTO
nnoremap <F12> :ALEGoToDefinition<CR>
nnoremap <leader><F12> :ALEFindReferences<CR>

" Hovering
" g:ale_hover_to_preview = 1

" Refactoring
" :ALERename, ALECodeAction

"""""""""""""""""""""""""""
"          FZF            "
"""""""""""""""""""""""""""
nnoremap <silent> <C-p> :FZF<CR>

"""""""""""""""""""""""""""
"         End             "
"""""""""""""""""""""""""""
set secure
