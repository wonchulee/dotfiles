#!/usr/bin/env bash
# setup_common.sh — shared setup: symlinks, tools, git config

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# shellcheck source=scripts/setup_claude.sh
source "$DOTFILES_DIR/scripts/setup_claude.sh"

# -- symlinks --------------------------------------------------------------
setup_symlinks() {
  log_info "=== Creating symlinks ==="

  ensure_symlink "$DOTFILES_DIR/tmux/.tmux.conf"         "$HOME/.tmux.conf"
  ensure_symlink "$DOTFILES_DIR/fish/config.fish"        "$HOME/.config/fish/config.fish"
  ensure_symlink "$DOTFILES_DIR/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"
  ensure_symlink "$DOTFILES_DIR/bash/.bashrc"            "$HOME/.bashrc"
  ensure_symlink "$DOTFILES_DIR/nvim"                    "$HOME/.config/nvim"
  ensure_symlink "$DOTFILES_DIR/cargo/config.toml"       "$HOME/.cargo/config.toml"
}

# -- language/package-manager helpers --------------------------------------
load_cargo_env() {
  if [ -f "$HOME/.cargo/env" ]; then
    # shellcheck source=/dev/null
    source "$HOME/.cargo/env"
  fi
}

load_volta_env() {
  export VOLTA_HOME="$HOME/.volta"
  export PATH="$VOLTA_HOME/bin:$PATH"
}

load_go_env() {
  export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"
}

ensure_go() {
  load_go_env

  if has_command go; then
    log_info "go already installed: $(go version)"
    return
  fi

  local os arch version archive url tmpdir
  os="$(uname -s | tr '[:upper:]' '[:lower:]')"
  case "$(uname -m)" in
    x86_64|amd64) arch="amd64" ;;
    aarch64|arm64) arch="arm64" ;;
    *)
      log_error "Unsupported Go architecture: $(uname -m)"
      return 1
      ;;
  esac

  version="$(curl -fsSL 'https://go.dev/VERSION?m=text' | head -n 1)"
  if [ -z "$version" ]; then
    log_error "Failed to resolve latest Go version"
    return 1
  fi

  archive="${version}.${os}-${arch}.tar.gz"
  url="https://go.dev/dl/${archive}"
  tmpdir="$(mktemp -d)"

  log_info "Installing Go ${version}..."
  curl -fsSL "$url" -o "$tmpdir/$archive"
  rm -rf "$HOME/.local/go"
  mkdir -p "$HOME/.local"
  tar -C "$HOME/.local" -xzf "$tmpdir/$archive"
  rm -rf "$tmpdir"

  load_go_env
}

ensure_volta() {
  load_volta_env

  if ! has_command volta; then
    log_info "Installing volta..."
    curl -fsSL https://get.volta.sh | bash
    load_volta_env
  else
    log_info "volta already installed"
  fi
}

install_node_with_volta() {
  ensure_volta

  if ! has_command volta; then
    log_error "volta is not available after installation"
    return 1
  fi

  if volta list node 2>/dev/null | grep -q '^default node'; then
    log_info "volta: node already installed"
  else
    log_info "volta: installing node..."
    volta install node
  fi
}

ensure_rustup() {
  if ! has_command rustup; then
    log_info "Installing rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  else
    log_info "rustup already installed"
  fi

  load_cargo_env
}

ensure_cargo_binstall() {
  ensure_rustup

  if ! has_command cargo; then
    log_error "cargo is not available after rustup installation"
    return 1
  fi

  if has_command cargo-binstall || cargo binstall --version >/dev/null 2>&1; then
    log_info "cargo-binstall already installed"
    return
  fi

  log_info "Installing cargo-binstall..."
  curl -fsSL \
    https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh \
    | bash
  load_cargo_env

  if ! has_command cargo-binstall && ! cargo binstall --version >/dev/null 2>&1; then
    log_error "cargo-binstall is not available after installation"
    return 1
  fi
}

install_cargo_packages() {
  ensure_cargo_binstall

  # Format: cargo-package:command-to-check
  local cargo_pkgs=(
    eza:eza
    ripgrep:rg
    alacritty:alacritty
    sccache:sccache
    bat:bat
    fd-find:fd
  )

  local spec pkg cmd
  for spec in "${cargo_pkgs[@]}"; do
    pkg="${spec%%:*}"
    cmd="${spec##*:}"

    if has_command "$cmd"; then
      log_info "cargo: $pkg already installed"
    else
      log_info "cargo: installing $pkg binary..."
      cargo binstall -y "$pkg"
    fi
  done
}

install_go_packages() {
  ensure_go

  if ! has_command go; then
    log_error "go is not available after installation"
    return 1
  fi

  # Format: command:go-install-target
  local go_pkgs=(
    lazygit:github.com/jesseduffield/lazygit@latest
  )

  local spec cmd target
  for spec in "${go_pkgs[@]}"; do
    cmd="${spec%%:*}"
    target="${spec#*:}"

    if has_command "$cmd"; then
      log_info "go: $cmd already installed"
    else
      log_info "go: installing $cmd..."
      go install "$target"
    fi
  done
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

  install_node_with_volta
  install_cargo_packages
  install_go_packages

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
