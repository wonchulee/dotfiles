-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

vim.g.snacks_animate = false

local opt = vim.opt

opt.tabstop = 2
opt.shiftwidth = 2
opt.shiftround = true
opt.expandtab = true

opt.wrap = false
opt.textwidth = 0
vim.opt.formatoptions:remove({ "t", "c" })
opt.colorcolumn = ""

opt.updatetime = 100

opt.list = true
opt.listchars = { tab = "!·", trail = "·" }
