-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

local opt = vim.opt

opt.tabstop = 2
opt.shiftwidth = 2
opt.shiftround = true
opt.expandtab = true

opt.textwidth = 82
opt.colorcolumn = "+1"

opt.updatetime = 100

opt.list = true
opt.listchars = { tab = "!·", trail = "·" }
