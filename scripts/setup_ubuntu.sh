#!/usr/bin/env bash
# setup_ubuntu.sh — Ubuntu-specific setup

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# -- packages --------------------------------------------------------------
setup_packages() {
  log_info "=== Installing Ubuntu packages ==="

  sudo apt-get update

  sudo apt-get install -y \
    aspell \
    build-essential \
    ca-certificates \
    cmake \
    curl \
    fish \
    git \
    jq \
    libfontconfig1-dev \
    neovim \
    pkg-config \
    shellcheck \
    shfmt \
    tmux \
    unzip

  # alacritty build dependencies
  sudo apt-get install -y \
    libfreetype6-dev \
    libxcb-xfixes0-dev \
    libxkbcommon-dev \
    python3

  install_node_with_volta
  install_cargo_packages
  install_go_packages
}

# -- Ubuntu-only symlinks --------------------------------------------------
setup_symlinks_ubuntu() {
  log_info "=== Creating Ubuntu-specific symlinks ==="

  ensure_symlink "$DOTFILES_DIR/linux/applications/alacritty.desktop" \
    "$HOME/.local/share/applications/alacritty.desktop"
}
