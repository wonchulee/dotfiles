#!/bin/sh

ln -s `pwd`/.vimrc $HOME/.vimrc
ln -s `pwd`/.tmux.conf $HOME/.tmux.conf
ln -s `pwd`/.config/fish/config.fish $HOME/.config/fish/config.fish
ln -s `pwd`/.doom.d $HOME/.doom.d

mkdir -p ~/.local/share/applications
ln -s `pwd`/.local/share/applications/alacritty.desktop ~/.local/share/applications/alacritty.desktop

# vim plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# tmux
## tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# alacritty config
mkdir -p ~/.config/alacritty
ln -s `pwd`/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# environment variable for applications
# mkdir -p ~/.config/environment.d/
# ln -s `pwd`/.config/environment.d/envvars.conf ~/.config/environment.d/envvars.conf

# install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# install rust pacakges
cargo install exa ripgrep

# install starship rs
curl -fsSL https://starship.rs/install.sh | bash

# git
git config --global core.editor vim

# fish shell
brew install fish
echo `which fish` | sudo tee -a /etc/shells

# install doom emacs
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
brew install shellcheck shfmt markdown jq aspell

# install golang
brew install go

# REPL!
go install github.com/x-motemen/gore/cmd/gore@latest

# Autocompletion
go install github.com/stamblerre/gocode@latest

# Documentation
go install golang.org/x/tools/cmd/godoc@latest

# Add/Removed Necessary Imports
go install golang.org/x/tools/cmd/goimports@latest

# Type-Safe Renaming of Go identifiers
go install golang.org/x/tools/cmd/gorename@latest

# Asks questions about your Gocode
go install golang.org/x/tools/cmd/guru@latest

# Generate tests based off of the func you're on
go install github.com/cweill/gotests/gotests@latest

# Add `json` or `bson` to structs easily
go install github.com/fatih/gomodifytags@latest

# language server
go install golang.org/x/tools/gopls@latest
