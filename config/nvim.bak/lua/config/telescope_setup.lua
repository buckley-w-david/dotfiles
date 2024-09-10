local map = require('config.utils').map

-- local silent = { silent = true }
-- Navigate buffers and repos
-- map('n', '<c-a>', [[<cmd>Telescope buffers show_all_buffers=true theme=get_dropdown<cr>]], silent)
-- map('n', '<c-e>', [[<cmd>Telescope frecency theme=get_dropdown<cr>]], silent)
-- map('n', '<c-s>', [[<cmd>Telescope git_files theme=get_dropdown<cr>]], silent)
-- map('n', '<c-d>', [[<cmd>Telescope find_files theme=get_dropdown<cr>]], silent)
-- map('n', '<c-g>', [[<cmd>Telescope live_grep theme=get_dropdown<cr>]], silent)


local opts = { noremap=true, silent=true }


map('n', 'ff', "<cmd>lua require('telescope.builtin').find_files()<cr>", opts)
map('n', 'fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>", opts)
map('n', 'fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)
map('n', 'fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", opts)
map('n', 'fs', "<cmd>lua require('telescope.builtin').git_status()<cr>", opts)


map('n', 'gd', '<cmd>lua require(\'telescope.builtin\').lsp_definitions()<cr>', opts)
map('n', 'gi', '<cmd>lua require(\'telescope.builtin\').lsp_implementations()<cr>', opts)
map('n', '<leader>ca', '<cmd>lua require(\'telescope.builtin\').lsp_code_actions()<cr>', opts)
map('n', 'gr', '<cmd>lua require(\'telescope.builtin\').lsp_references()<cr>', opts)
