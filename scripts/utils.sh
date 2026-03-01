#!/usr/bin/env bash
# utils.sh — shared helper functions

set -euo pipefail

# -- colors ----------------------------------------------------------------
RED='\033[0;31m'
YELLOW='\033[0;33m'
GREEN='\033[0;32m'
NC='\033[0m'

log_info()  { printf "${GREEN}[INFO]${NC}  %s\n" "$*"; }
log_warn()  { printf "${YELLOW}[WARN]${NC}  %s\n" "$*"; }
log_error() { printf "${RED}[ERROR]${NC} %s\n" "$*" >&2; }

# -- OS detection ----------------------------------------------------------
detect_os() {
  case "$(uname -s)" in
    Darwin) echo "macos" ;;
    Linux)
      if [ -f /etc/fedora-release ]; then
        echo "fedora"
      else
        echo "linux"
      fi
      ;;
    *) echo "unknown" ;;
  esac
}

# -- command check ---------------------------------------------------------
has_command() {
  command -v "$1" >/dev/null 2>&1
}

# -- idempotent symlink ----------------------------------------------------
# ensure_symlink <source> <target>
# - skip if already correct
# - backup existing file as .bak
# - create parent directories as needed
ensure_symlink() {
  local src="$1"
  local target="$2"

  # create parent directory
  mkdir -p "$(dirname "$target")"

  # already correct
  if [ -L "$target" ] && [ "$(readlink "$target")" = "$src" ]; then
    log_info "symlink OK: $target -> $src"
    return
  fi

  # backup existing file/symlink
  if [ -e "$target" ] || [ -L "$target" ]; then
    log_warn "backing up $target -> ${target}.bak"
    mv "$target" "${target}.bak"
  fi

  ln -s "$src" "$target"
  log_info "symlink created: $target -> $src"
}
