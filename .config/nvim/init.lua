local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')
    Plug 'junegunn/fzf.vim'
    Plug('nvim-treesitter/nvim-treesitter', { ["do"] = ':TSUpdate' })
    Plug 'mbbill/undotree'
    Plug 'tpope/vim-obsession'
vim.call('plug#end')

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.updatetime = 50
vim.opt.colorcolumn = "80"

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.clipboard=unnamedplus

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.g.auto_save = 1 -- enable AutoSave on Vim startup
vim.g.auto_save_events = { "FocusLost", "TabLeave", "WinLeave", "BufLeave" }

-- Keymaps
vim.g.mapleader = " "
vim.keymap.set("v", "J", ":m '>+0<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z")

vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
