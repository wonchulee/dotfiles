#!/usr/bin/env bash
# setup_pi.sh — pi-coding-agent 로컬 패치 재적용
#
# 무엇을 하나
# ------------
# pi-tui 의 terminal-image.js 는 tmux 안에서 OSC 8 하이퍼링크를 무조건 비활성화
# 한다 (`hyperlinks: false`). 사용자가 tmux 에 `allow-passthrough on` 과
# `terminal-features *:hyperlinks` 를 이미 설정한 환경에서는 이 가드가 과도하므로,
# `PI_FORCE_HYPERLINKS=1` 환경변수로 오버라이드할 수 있도록 패치한다.
#
# pi / volta / pi-tui 가 재설치되면 dist 파일이 덮어쓰여 패치가 사라진다.
# 그때 이 스크립트를 다시 실행하면 idempotent 하게 재적용한다.
#
# 사용
# -----
#   bash ~/me/dotfiles/scripts/setup_pi.sh
#
# 추가 옵션:
#   --check   현재 패치 상태만 확인 (변경 X)
#   --revert  패치를 원복 (pi 원본 동작으로)
#
# 의존
# -----
# - PI_FORCE_HYPERLINKS=1 환경변수 — fish/zsh/bash rc 에서 설정되어 있어야 효과 발생
#   (이 스크립트는 dist 파일만 패치한다)
# - ~/.tmux.conf 의 `allow-passthrough on`, `terminal-features *:hyperlinks`

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./utils.sh
. "$SCRIPT_DIR/utils.sh"

# -- locate pi-tui terminal-image.js ---------------------------------------
# pi-tui 는 보통 pi-coding-agent 내부 node_modules 에 들어 있다.
# volta / npm / yarn / pnpm 어디든 글로벌로 깔린 경로를 후보로 모은다.
locate_pi_tui() {
  local candidates=()

  # 1) volta global
  if [ -d "$HOME/.volta/tools/image/packages" ]; then
    while IFS= read -r f; do
      candidates+=("$f")
    done < <(find "$HOME/.volta/tools/image/packages" \
              -path "*@earendil-works/pi-tui/dist/terminal-image.js" \
              2>/dev/null)
  fi

  # 2) npm global prefix
  if has_command npm; then
    local npm_prefix
    npm_prefix="$(npm config get prefix 2>/dev/null || true)"
    if [ -n "$npm_prefix" ] && [ -d "$npm_prefix" ]; then
      while IFS= read -r f; do
        candidates+=("$f")
      done < <(find "$npm_prefix" \
                -path "*@earendil-works/pi-tui/dist/terminal-image.js" \
                2>/dev/null)
    fi
  fi

  # 3) pnpm / bun store (옵션)
  for d in "$HOME/.local/share/pnpm" "$HOME/.bun/install/global"; do
    [ -d "$d" ] || continue
    while IFS= read -r f; do
      candidates+=("$f")
    done < <(find "$d" \
              -path "*@earendil-works/pi-tui/dist/terminal-image.js" \
              2>/dev/null)
  done

  # 중복 제거
  printf '%s\n' "${candidates[@]}" | awk 'NF' | sort -u
}

# -- patch 본체 ------------------------------------------------------------
# 원본:
#   const inTmuxOrScreen = !!process.env.TMUX || term.startsWith("tmux") || term.startsWith("screen");
#   if (inTmuxOrScreen) {
#       const trueColor = colorTerm === "truecolor" || colorTerm === "24bit";
#       return { images: null, trueColor, hyperlinks: false };
#   }
#
# 패치 후:
#   ...
#   const forceHyperlinks = process.env.PI_FORCE_HYPERLINKS === "1";
#   if (inTmuxOrScreen && !forceHyperlinks) {
#       ...
#   }

PATCH_MARKER='process.env.PI_FORCE_HYPERLINKS'

is_patched() {
  local file="$1"
  grep -q "$PATCH_MARKER" "$file"
}

apply_patch() {
  local file="$1"

  if is_patched "$file"; then
    log_info "already patched: $file"
    return 0
  fi

  # 원본 패턴이 존재하는지 확인
  if ! grep -q 'const inTmuxOrScreen = !!process.env.TMUX' "$file"; then
    log_error "expected pattern not found in $file"
    log_error "pi-tui 코드 구조가 바뀐 것 같습니다. setup_pi.sh 를 업데이트해야 합니다."
    return 1
  fi

  # 백업 (한 번만)
  if [ ! -f "${file}.orig" ]; then
    cp -p "$file" "${file}.orig"
    log_info "backup created: ${file}.orig"
  fi

  # 인플레이스 치환 (BSD/GNU sed 양쪽 호환 위해 perl 사용)
  perl -i -0pe '
    s{
      (\s*)const\ inTmuxOrScreen\ =\ !!process\.env\.TMUX\ \|\|\ term\.startsWith\("tmux"\)\ \|\|\ term\.startsWith\("screen"\);\n
      \s*if\ \(inTmuxOrScreen\)\ \{\n
    }{
$1// [LOCAL PATCH by setup_pi.sh] PI_FORCE_HYPERLINKS=1 로 tmux 내 OSC 8 활성화 가능
$1const inTmuxOrScreen = !!process.env.TMUX || term.startsWith("tmux") || term.startsWith("screen");
$1const forceHyperlinks = process.env.PI_FORCE_HYPERLINKS === "1";
$1if (inTmuxOrScreen && !forceHyperlinks) {
}gx;
  ' "$file"

  if is_patched "$file"; then
    log_info "patched: $file"
    return 0
  else
    log_error "patch failed: $file"
    return 1
  fi
}

revert_patch() {
  local file="$1"

  if [ -f "${file}.orig" ]; then
    cp -p "${file}.orig" "$file"
    log_info "reverted from backup: $file"
  elif is_patched "$file"; then
    log_warn "no .orig backup. doing inline revert: $file"
    # 1) LOCAL PATCH 코멘트 블록 제거 (한 줄 또는 여러 줄 모두 대응)
    # 2) forceHyperlinks 선언 제거
    # 3) 조건문에서 && !forceHyperlinks 제거
    perl -i -0pe '
      s{^[ \t]*//\ \[LOCAL\ PATCH[^\n]*\n(?:[ \t]*//[^\n]*\n)*}{}gm;
      s{[ \t]*const\ forceHyperlinks\ =\ process\.env\.PI_FORCE_HYPERLINKS\ ===\ "1";\n}{}g;
      s{if\ \(inTmuxOrScreen\ &&\ !forceHyperlinks\)}{if (inTmuxOrScreen)}g;
    ' "$file"
    log_info "inline-reverted: $file"
  else
    log_info "not patched, skip: $file"
  fi
}

# -- shell rc 확인 (정보 출력만) ------------------------------------------
check_env_var() {
  local found=0
  for f in \
      "$HOME/me/dotfiles/fish/config.fish" \
      "$HOME/.config/fish/config.fish" \
      "$HOME/.zshrc" \
      "$HOME/.zshenv" \
      "$HOME/.bashrc" \
      "$HOME/.bash_profile" \
      "$HOME/.profile"; do
    [ -f "$f" ] || continue
    if grep -q "PI_FORCE_HYPERLINKS" "$f"; then
      log_info "PI_FORCE_HYPERLINKS 선언 발견: $f"
      found=1
    fi
  done
  if [ "$found" -eq 0 ]; then
    log_warn "PI_FORCE_HYPERLINKS 를 어떤 shell rc 에서도 못 찾았습니다."
    log_warn "다음 줄을 셸 rc 에 추가하세요:"
    log_warn "  fish: set -gx PI_FORCE_HYPERLINKS 1"
    log_warn "  zsh/bash: export PI_FORCE_HYPERLINKS=1"
  fi
}

check_tmux_conf() {
  local conf="$HOME/.tmux.conf"
  [ -f "$conf" ] || return 0
  local missing=()
  grep -q "allow-passthrough on" "$conf" || missing+=("allow-passthrough on")
  grep -q "hyperlinks" "$conf"          || missing+=("terminal-features ',*:hyperlinks'")
  if [ ${#missing[@]} -gt 0 ]; then
    log_warn "~/.tmux.conf 에 다음 설정이 누락되어 있습니다:"
    for m in "${missing[@]}"; do
      log_warn "  set -g $m"
    done
  else
    log_info "tmux config OK (allow-passthrough + hyperlinks)"
  fi
}

# -- 메인 -----------------------------------------------------------------
main() {
  local mode="apply"
  case "${1:-}" in
    --check)  mode="check"  ;;
    --revert) mode="revert" ;;
    "")       mode="apply"  ;;
    *) log_error "unknown option: $1"; exit 2 ;;
  esac

  log_info "locating pi-tui terminal-image.js ..."
  local files
  files="$(locate_pi_tui)"

  if [ -z "$files" ]; then
    log_error "pi-tui 의 terminal-image.js 를 찾지 못했습니다."
    log_error "pi 가 설치되어 있나요? (which pi / npm ls -g 확인)"
    exit 1
  fi

  echo "$files" | while IFS= read -r f; do
    [ -z "$f" ] && continue
    case "$mode" in
      check)
        if is_patched "$f"; then
          log_info "[patched] $f"
        else
          log_warn "[not patched] $f"
        fi
        ;;
      apply)
        apply_patch "$f" || exit 1
        ;;
      revert)
        revert_patch "$f"
        ;;
    esac
  done

  echo
  log_info "환경 점검:"
  check_env_var
  check_tmux_conf

  if [ "$mode" = "apply" ]; then
    echo
    log_info "완료. 변경 사항을 보려면 pi 를 재시작하세요 (현재 세션은 캐시된 capability 사용)."
  fi
}

main "$@"
