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

#tmux
# TODO:

# vim
vim -c 'CocInstall -sync coc-tsserver coc-json coc-html coc-css|q'
