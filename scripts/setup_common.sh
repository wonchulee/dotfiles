#!/usr/bin/env bash
# setup_common.sh — shared setup: symlinks, tools, git config

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# -- symlinks --------------------------------------------------------------
setup_symlinks() {
  log_info "=== Creating symlinks ==="

  ensure_symlink "$DOTFILES_DIR/tmux/.tmux.conf"         "$HOME/.tmux.conf"
  ensure_symlink "$DOTFILES_DIR/fish/config.fish"        "$HOME/.config/fish/config.fish"
  ensure_symlink "$DOTFILES_DIR/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"
  ensure_symlink "$DOTFILES_DIR/bash/.bashrc"            "$HOME/.bashrc"
  ensure_symlink "$DOTFILES_DIR/nvim"                    "$HOME/.config/nvim"
}

# -- tools -----------------------------------------------------------------
setup_tools() {
  log_info "=== Installing tools ==="

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

  # Claude Code
  install_claude_code
  setup_claude_plugins

  # git config
  log_info "Setting git config..."
  git config --global core.editor nvim
}
