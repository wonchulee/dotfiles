#!/usr/bin/env bash
# install.sh — dotfiles entry point
# Usage: ./install.sh [--all|--symlinks|--packages|--tools]

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=scripts/utils.sh
source "$DOTFILES_DIR/scripts/utils.sh"
# shellcheck source=scripts/setup_common.sh
source "$DOTFILES_DIR/scripts/setup_common.sh"

OS="$(detect_os)"
log_info "Detected OS: $OS"

case "$OS" in
  macos)
    # shellcheck source=scripts/setup_macos.sh
    source "$DOTFILES_DIR/scripts/setup_macos.sh"
    ;;
  fedora)
    # shellcheck source=scripts/setup_fedora.sh
    source "$DOTFILES_DIR/scripts/setup_fedora.sh"
    ;;
  *)
    log_error "Unsupported OS: $OS"
    exit 1
    ;;
esac

MODE="${1:---all}"

case "$MODE" in
  --symlinks)
    setup_symlinks
    [ "$OS" = "fedora" ] && setup_symlinks_fedora
    ;;
  --packages)
    setup_packages
    ;;
  --tools)
    setup_tools
    ;;
  --all)
    setup_symlinks
    [ "$OS" = "fedora" ] && setup_symlinks_fedora
    setup_packages
    setup_tools
    ;;
  *)
    log_error "Unknown mode: $MODE"
    echo "Usage: $0 [--all|--symlinks|--packages|--tools]"
    exit 1
    ;;
esac

log_info "Done!"
