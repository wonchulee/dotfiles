-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

-- kj to escape insert mode (from .vimrc)
vim.keymap.set("i", "kj", "<Esc>`^", { desc = "Escape insert mode" })
vim.keymap.set("i", "lkj", "<Esc>`^:w<CR>", { desc = "Escape and save" })
