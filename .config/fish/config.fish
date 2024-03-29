alias cat='bat'
alias grep='rg'
alias ls='exa'

alias ec='emacsclient -c -nw'

set -gx NPM_PACKAGES_BIN $HOME/.npm-packages/bin
set -gx PATH $HOME/.local/bin $NPM_PACKAGES_BIN $PATH ~/.emacs.d/bin $HOME/.cargo/bin
set -gx MANPATH $MANPATH $NPM_PACKAGES/share/man

set -gx LD_LIBRARY_PATH ~/.local/lib64 ~/.local/lib $LD_LIBRARY_PATH
set -gx PKG_CONFIG_PATH ~/.local/lib64/pkgconfig ~/.local/lib/pkgconfig $PKG_CONFIG_PATH

set -gx EDITOR vim

set -gx GTK_IM_MODULE ibus

starship init fish | source
