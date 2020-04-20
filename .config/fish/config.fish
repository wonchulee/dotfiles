alias vi='vimx'
alias vim='vimx'

set -gx NPM_PACKAGES_BIN $HOME/.npm-packages/bin
set -gx PATH $HOME/.local/bin $NPM_PACKAGES_BIN $PATH

set -gx EDITOR vim
