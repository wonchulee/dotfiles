#!/usr/bin/env bash
# setup_fedora.sh — Fedora-specific setup

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# -- packages --------------------------------------------------------------
setup_packages() {
  log_info "=== Installing Fedora packages ==="

  sudo dnf groupinstall -y "Development Tools" "Development Libraries"
  sudo dnf install -y cmake fontconfig-devel nodejs fish

  # gstreamer
  sudo dnf install -y \
    gstreamer1-devel \
    gstreamer1-plugins-base-tools \
    gstreamer1-plugins-base-devel \
    gstreamer1-plugins-good \
    gstreamer1-plugins-good-extras \
    gstreamer1-plugins-ugly \
    gstreamer1-plugins-bad-free \
    gstreamer1-plugins-bad-free-devel \
    gstreamer1-plugins-bad-free-extras

  # cargo packages
  if has_command cargo; then
    local cargo_pkgs=(eza ripgrep alacritty)
    for pkg in "${cargo_pkgs[@]}"; do
      if has_command "$pkg" || cargo install --list | grep -q "^${pkg} "; then
        log_info "cargo: $pkg already installed"
      else
        log_info "cargo: installing $pkg..."
        cargo install "$pkg"
      fi
    done
  fi
}

# -- Fedora-only symlinks --------------------------------------------------
setup_symlinks_fedora() {
  log_info "=== Creating Fedora-specific symlinks ==="

  ensure_symlink "$DOTFILES_DIR/linux/applications/alacritty.desktop" \
    "$HOME/.local/share/applications/alacritty.desktop"
}
