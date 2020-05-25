alias vi='vimx'
alias vim='vimx'

alias cat='bat'
alias grep='rg'

set -gx NPM_PACKAGES_BIN $HOME/.npm-packages/bin
set -gx PATH $HOME/.local/bin $NPM_PACKAGES_BIN $PATH
set -gx MANPATH $MANPATH $NPM_PACKAGES/share/man

set -gx LD_LIBRARY_PATH ~/.local/lib64 ~/.local/lib $LD_LIBRARY_PATH
set -gx PKG_CONFIG_PATH ~/.local/lib64/pkgconfig ~/.local/lib/pkgconfig $PKG_CONFIG_PATH

set -gx EDITOR vim

set -gx GTK_IM_MODULE ibus
