#!/usr/bin/env bash
# setup_common.sh — shared setup: symlinks, tools, git config

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# -- symlinks --------------------------------------------------------------
setup_symlinks() {
  log_info "=== Creating symlinks ==="

  ensure_symlink "$DOTFILES_DIR/vim/.vimrc"              "$HOME/.vimrc"
  ensure_symlink "$DOTFILES_DIR/tmux/.tmux.conf"         "$HOME/.tmux.conf"
  ensure_symlink "$DOTFILES_DIR/fish/config.fish"        "$HOME/.config/fish/config.fish"
  ensure_symlink "$DOTFILES_DIR/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"
  ensure_symlink "$DOTFILES_DIR/bash/.bashrc"            "$HOME/.bashrc"
}

# -- tools -----------------------------------------------------------------
setup_tools() {
  log_info "=== Installing tools ==="

  # vim-plug
  if [ ! -f "$HOME/.vim/autoload/plug.vim" ]; then
    log_info "Installing vim-plug..."
    curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  else
    log_info "vim-plug already installed"
  fi

  # tpm (tmux plugin manager)
  if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    log_info "Installing tpm..."
    git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
  else
    log_info "tpm already installed"
  fi

  # rustup
  if ! has_command rustup; then
    log_info "Installing rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  else
    log_info "rustup already installed"
  fi

  # starship
  if ! has_command starship; then
    log_info "Installing starship..."
    curl -fsSL https://starship.rs/install.sh | sh -s -- -y
  else
    log_info "starship already installed"
  fi

  # git config
  log_info "Setting git config..."
  git config --global core.editor vim
}
