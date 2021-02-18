#!/bin/sh

ln -s `pwd`/.vimrc $HOME/.vimrc
ln -s `pwd`/.tmux.conf.local $HOME/.tmux.conf.local
ln -s `pwd`/.config/fish/config.fish $HOME/.config/fish/config.fish
ls -s `pwd`/.doom.d $HOME/.doom.d

mkdir -p ~/.local/share/applications
ln -s `pwd`/.local/share/applications/alacritty.desktop ~/.local/share/applications/alacritty.desktop

# vim plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

## coc web
vim -c 'CocInstall -sync coc-tsserver coc-json coc-html coc-css|q'

# tmux
## tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# alacritty config
mkdir -p ~/.config/alacritty
ln -s `pwd`/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# nord gnome terminal theme
git clone https://github.com/arcticicestudio/nord-gnome-terminal.git ~/
~/nord-gnome-terminal/src/nord.sh

# environment variable for applications
# mkdir -p ~/.config/environment.d/
# ln -s `pwd`/.config/environment.d/envvars.conf ~/.config/environment.d/envvars.conf

# install packages
sudo dnf groupinstall "Development Tools" "Development Libraries"
sudo dnf install cmake fontconfig-devel nodejs fish emacs

# install gstreamer packages
sudo dnf install gstreamer1-devel gstreamer1-plugins-base-tools gstreamer1-plugins-base-devel gstreamer1-plugins-good gstreamer1-plugins-good-extras gstreamer1-plugins-ugly  gstreamer1-plugins-bad-free gstreamer1-plugins-bad-free-devel gstreamer1-plugins-bad-free-extras

# install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# install rust pacakges
cargo install exa ripgrep aracritty

# install starship rs
curl -fsSL https://starship.rs/install.sh | bash

# git
git config --global core.editor vim

# install doom emacs
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
