# scripts

dotfiles 설치 / 재설정 헬퍼 모음.

| 스크립트 | 설명 |
|---|---|
| `setup_macos.sh` / `setup_fedora.sh` | OS별 패키지 설치 진입점 |
| `setup_common.sh` | OS 공통 설치 |
| `setup_claude.sh` | Claude Code 설치 + 플러그인 |
| `setup_pi.sh` | pi-coding-agent 로컬 패치 (tmux 안에서 OSC 8 클릭 가능 링크 활성화) |
| `utils.sh` | 공용 로깅·심볼릭 링크 헬퍼 |

## setup_pi.sh

### 무엇을 하나

pi-tui (`@earendil-works/pi-tui`) 의 `terminal-image.js` 는 tmux 환경에서 OSC 8
하이퍼링크를 무조건 비활성화합니다. 그 결과 pi 가 출력하는 마크다운 링크
(`[#239](URL)`) 가 ⌘+클릭으로 열리지 않습니다.

이 스크립트는 해당 가드를 `PI_FORCE_HYPERLINKS=1` 환경변수로 오버라이드할 수
있도록 패치합니다. 동시에 다음 두 가지 사전 조건을 점검합니다:

1. `~/.tmux.conf` 에 `allow-passthrough on` + `terminal-features ',*:hyperlinks'`
2. 셸 rc 에 `PI_FORCE_HYPERLINKS=1` 선언

### 사용

```bash
# 패치 적용 (idempotent)
bash ~/me/dotfiles/scripts/setup_pi.sh

# 상태만 확인
bash ~/me/dotfiles/scripts/setup_pi.sh --check

# 원복 (백업 .orig 가 있으면 우선 사용)
bash ~/me/dotfiles/scripts/setup_pi.sh --revert
```

### 언제 다시 실행하나

- **pi / volta 재설치 후** — `dist/terminal-image.js` 가 덮어쓰여 패치가 사라짐
- **pi 버전 업그레이드 후** — 같은 이유. 단, upstream 에서 코드 구조가 바뀌면
  스크립트가 "expected pattern not found" 로 실패하므로, 그때는 스크립트의
  `apply_patch` 안 `perl -i -0pe ...` 부분과 매처를 새 코드에 맞춰 갱신해야 함

### 검증

```bash
# 1) 셸 환경변수 확인
echo $PI_FORCE_HYPERLINKS   # → 1

# 2) tmux 옵션 확인
tmux show-options -g allow-passthrough         # → allow-passthrough on
tmux show-options -g terminal-features | grep hyper  # → *:hyperlinks 포함

# 3) pi 재시작 후 마크다운 링크를 출력시켜 ⌘+클릭 동작 확인
```

### 영향 범위

이 스크립트가 건드리는 파일:

- `~/.volta/tools/image/packages/.../pi-tui/dist/terminal-image.js`
  - (npm/pnpm/bun 글로벌에도 pi-tui 가 깔려 있으면 동일 파일 함께 패치)
  - 최초 패치 시 같은 디렉토리에 `terminal-image.js.orig` 백업 생성

`~/.tmux.conf` 및 셸 rc 파일은 직접 수정하지 않고 누락만 안내합니다. 의도적으로
범위를 좁혀 둔 부분이라, 누락 경고가 뜨면 수동으로 추가해야 합니다.

### 참고: 관련 dotfiles 변경

| 위치 | 추가된 내용 |
|---|---|
| `tmux/.tmux.conf` | `set -g allow-passthrough on`, `set -as terminal-features ',*:hyperlinks'` |
| `fish/config.fish` | `set -gx PI_FORCE_HYPERLINKS 1` |
