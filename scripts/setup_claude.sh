#!/usr/bin/env bash
# setup_claude.sh — Claude Code installation and configuration

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CLAUDE_PLUGINS=(
  rust-analyzer-lsp
  pyright-lsp
  ralph-loop
  frontend-design
  playwright
  commit-commands
)

# -- install Claude Code ---------------------------------------------------
install_claude_code() {
  if has_command claude; then
    log_info "Claude Code already installed: $(claude --version)"
    return
  fi

  log_info "Installing Claude Code..."
  curl -fsSL https://claude.ai/install.sh | sh
}

# -- install plugins -------------------------------------------------------
setup_claude_plugins() {
  if ! has_command claude; then
    log_warn "Claude Code not found, skipping plugin installation"
    return
  fi

  log_info "=== Installing Claude Code plugins ==="

  for plugin in "${CLAUDE_PLUGINS[@]}"; do
    log_info "Installing plugin: $plugin"
    claude plugin add "$plugin" 2>/dev/null || log_warn "Failed to install plugin: $plugin (may need auth)"
  done
}
