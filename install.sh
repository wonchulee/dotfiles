#!/bin/sh

ln -s `pwd`/.vimrc $HOME/.vimrc
ln -s `pwd`/.tmux.conf.local $HOME/.tmux.conf.local
ln -s `pwd`/.telnetrc $HOME/.telnetrc
ln -s `pwd`/direnv/direnvrc $HOME/.direnvrc

# zsh
if [ `which zsh` -ne 0 ]
then
  sudo dnf install zsh
  chsh -s /usr/bin/zsh
fi

# ohmyzsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
rm $HOME/.zshrc && ln -s `pwd`/.zshrc $HOME/.zshrc

# tmux
## tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# vim
## coc settings
ln -s `pwd`/coc/coc-settings.json $HOME/.vim/coc-settings.json
## coc web
vim -c 'CocInstall -sync coc-tsserver coc-json coc-html coc-css|q'
## coc rust
vim -c 'CocInstall -sync coc-rls |q'

# nord gnome terminal theme
git clone https://github.com/arcticicestudio/nord-gnome-terminal.git ~/
~/nord-gnome-terminal/src/nord.sh

mkdir -p ~/.config/alacritty
ln -s `pwd`/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# environment variable for applications
mkdir -p ~/.config/environment.d/
ln -s `pwd`/.config/environment.d/envvars.conf ~/.config/environment.d/envvars.conf
