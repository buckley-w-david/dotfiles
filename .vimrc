""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use Vim settings, rather then Vi settings.
set nocompatible

" set character encoding to utf-8
scriptencoding utfs8
set encoding=utf-8

" Tired of your wrong shit
noremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" Reload files if changed outside vim
set autoread

:set expandtab          " tabs to spaces
:set tabstop=4          " visual spaces per tabs
:set shiftwidth=4       " indentation is also 4 spaces

set ruler               " turn on ruler
set number              " add line numbers
set cursorline          " highlight currrent line
filetype indent on      " load filetype-specific indent files
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]

" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>


" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Highlight characters in a line past column 120
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%121v.\+/

syntax enable           " enable syntax processing

" move vertically by visual line
nnoremap j gj
nnoremap k gk

" highlight last inserted text
nnoremap gV `[v`]

let mapleader=","       " leader is comma
set showcmd             " show the command

autocmd BufWritePre *.py :%s/\s\+$//e
