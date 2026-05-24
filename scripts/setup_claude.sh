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

# -- plugin guidance ------------------------------------------------------
print_claude_plugins_hint() {
  log_info "Claude Code plugins are not installed by default."
  log_info "To install recommended plugins, run: ./install.sh --claude-plugins"
}

# -- install plugins -------------------------------------------------------
setup_claude_plugins() {
  if ! has_command claude; then
    log_warn "Claude Code not found, skipping plugin installation"
    return
  fi

  log_info "=== Installing Claude Code plugins ==="

  local installed
  installed=$(claude plugin list 2>/dev/null || true)

  for plugin in "${CLAUDE_PLUGINS[@]}"; do
    if echo "$installed" | grep -q "$plugin"; then
      log_info "Plugin already installed: $plugin"
    else
      log_info "Installing plugin: $plugin"
      claude plugin add "$plugin" 2>/dev/null || log_warn "Failed to install plugin: $plugin (may need auth)"
    fi
  done
}
