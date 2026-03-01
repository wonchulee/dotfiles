#!/usr/bin/env bash
# setup_macos.sh — macOS-specific setup

set -euo pipefail

# -- packages --------------------------------------------------------------
setup_packages() {
  log_info "=== Installing macOS packages ==="

  if ! has_command brew; then
    log_info "Homebrew not found. Installing..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi

  local packages=(fish shellcheck shfmt jq aspell go bat eza ripgrep neovim fd lazygit)
  for pkg in "${packages[@]}"; do
    if brew list "$pkg" &>/dev/null; then
      log_info "brew: $pkg already installed"
    else
      log_info "brew: installing $pkg..."
      brew install "$pkg"
    fi
  done

  # casks (fonts)
  local casks=(font-blex-mono-nerd-font)
  for cask in "${casks[@]}"; do
    if brew list --cask "$cask" &>/dev/null; then
      log_info "brew cask: $cask already installed"
    else
      log_info "brew cask: installing $cask..."
      brew install --cask "$cask"
    fi
  done

  # register fish as login shell
  local fish_path
  fish_path="$(which fish)"
  if ! grep -qF "$fish_path" /etc/shells; then
    log_info "Registering fish in /etc/shells..."
    echo "$fish_path" | sudo tee -a /etc/shells
  else
    log_info "fish already in /etc/shells"
  fi
}

# shellcheck source=scripts/setup_claude.sh
source "$DOTFILES_DIR/scripts/setup_claude.sh"
