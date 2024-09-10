-- Attempt at pure lua neovim config
--
local g = vim.g
local cmd = vim.cmd
local o, wo, bo = vim.o, vim.wo, vim.bo
local utils = require 'config.utils'
local opt = utils.opt
local autocmd = utils.autocmd
local map = utils.map

-- Leader/local leader
g.mapleader = [[,]]

local buffer = { o, bo }
local window = { o, wo }
opt('encoding', 'utf-8')
opt('autoread', true)
opt('expandtab', true)          -- tabs to spaces
opt('tabstop', 4)          -- visual spaces per tabs
opt('shiftwidth', 4)       -- indentation is also 4 spaces
opt('ruler', true)               -- turn on ruler
opt('number', true)              -- add line numbers
opt('cursorline', true)          -- highlight currrent line
opt('wildmenu', true)            -- visual autocomplete for command menu
opt('lazyredraw', true)          -- redraw only when we need to.
opt('showmatch', true)           -- highlight matching [{()}]
opt('hlsearch', true)            -- Highlight search results
opt('incsearch', true)      -- Makes search act like search in modern browsers
-- Makes splits happen in the direction I expect
opt('splitbelow', true)
opt('splitright', true)
opt('showcmd', true)             -- show the command
opt('hidden', true)              -- Hide a buffer when abandoning it instead of unloading it - This allows you to navigate away without having to save

-- Commands
cmd [[command! PackerInstall packadd packer.nvim | lua require('plugins').install()]]
cmd [[command! PackerUpdate packadd packer.nvim | lua require('plugins').update()]]
cmd [[command! PackerSync packadd packer.nvim | lua require('plugins').sync()]]
cmd [[command! PackerClean packadd packer.nvim | lua require('plugins').clean()]]
cmd [[command! PackerCompile packadd packer.nvim | lua require('plugins').compile()]]

-- Keybindings
local silent = { silent = true }
local nore = { noremap=true }

-- turn off search highlight
map('n', '<leader><space>', '<cmd>nohlsearch<CR>')

-- move vertically by visual line
map('n', 'j', 'gj', nore)
map('n', 'k', 'gk', nore)

-- highlight last inserted text
map('n', 'gV', '`[v`]', nore)

-- Moving Lines - From VimTricks 2020-10-08
map('n', '<c-j>', '<cmd>m .+1<CR>==', nore)
map('n', '<c-k>', '<cmd>m .-2<CR>==', nore)
map('i', '<c-j>', '<Esc><cmd>m .+1<CR>==gi', nore)
map('i', '<c-k>', '<Esc><cmd>m .-2<CR>==gi', nore)
map('v', '<c-j>', '<cmd>m \'>+1<CR>gv=gv', nore)
map('v', '<c-k>', '<cmd>m \'<-2<CR>gv=gv', nore)

-- Paste on new line
map('n', '<leader>p', '<cmd>pu<CR>', nore)

cmd [[color slate-with-skyblue-comments]]

-- syntax enable           " enable syntax processing
-- syntax on           " enable syntax processing

-- TODO: Lazy loading?
require('plugins')
