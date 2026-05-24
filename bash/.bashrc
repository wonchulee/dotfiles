# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export PATH="$PATH:/opt/nvim-linux-x86_64/bin"
export EDITOR=nvim

# alias
alias g='grep -IErn'
alias vim='nvim'
alias vi='nvim'
