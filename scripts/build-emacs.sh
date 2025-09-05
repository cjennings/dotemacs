#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>
# Build & install Emacs from source
# safe, user-local, fast, versioned.

set -Eeuo pipefail
IFS=$'\n\t'

# ---------- Config (override via env) ----------
SRC_DIR="${SRC_DIR:-$HOME/code/emacs-src}"
EMACS_REPO="${EMACS_REPO:-https://git.savannah.gnu.org/git/emacs.git}"
CHECKOUT_REF="${CHECKOUT_REF:-emacs-30.2}"
PREFIX_BASE="${PREFIX_BASE:-$HOME/.local/src/emacs}"
PREFIX="${PREFIX:-$PREFIX_BASE/${CHECKOUT_REF}}"
LOG_DIR="${LOG_DIR:-$HOME/.cache/emacs-build-logs}"
ENABLE_NATIVE="${ENABLE_NATIVE:-1}"
WITH_PGTK="${WITH_PGTK:-auto}"
JOBS="${JOBS:-auto}"
EXTRA_CONFIG="${EXTRA_CONFIG:-}"

# ---------- Preflight ----------
umask 022
mkdir -p "$LOG_DIR" "$PREFIX_BASE" "$HOME/.local/bin"
: "${LOGFILE:=$LOG_DIR/emacs-build-$(date +%Y%m%d-%H%M%S)-${CHECKOUT_REF}.log}"

say() { printf '>>> %s\n' "$*" | tee -a "$LOGFILE" ; }
run() { say "+ $*"; eval "$@" >>"$LOGFILE" 2>&1; }
on_err(){ ec=$?; echo "ERROR [$ec] - see $LOGFILE" >&2; tail -n 80 "$LOGFILE" >&2 || true; exit "$ec"; }
trap on_err ERR

# ---------- Clone/update ----------
if [[ -d "$SRC_DIR/.git" ]]; then
  run "git -C '$SRC_DIR' fetch --tags --prune"
else
  mkdir -p "$(dirname "$SRC_DIR")"
  run "git clone --origin origin '$EMACS_REPO' '$SRC_DIR'"
fi

run "git -C '$SRC_DIR' reset --hard HEAD"
run "git -C '$SRC_DIR' clean -fdx"
run "git -C '$SRC_DIR' checkout -f '$CHECKOUT_REF'"
run "git -C '$SRC_DIR' submodule update --init --recursive || true"

# ---------- Autogen ----------
if [[ -x "$SRC_DIR/autogen.sh" ]]; then
  run "cd '$SRC_DIR' && ./autogen.sh"
fi

# ---------- Configure flags (ARRAY!) ----------
conf_flags=(
  "--prefix=${PREFIX}"
  "--with-json"
  "--with-modules"
)

# Wayland/X choice
if [[ "$WITH_PGTK" == "auto" ]]; then
  if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then WITH_PGTK="yes"; else WITH_PGTK="no"; fi
fi
if [[ "$WITH_PGTK" == "yes" ]]; then
  conf_flags+=("--with-pgtk")
else
  conf_flags+=("--with-x-toolkit=lucid")
fi

# Native-compilation
if [[ "$ENABLE_NATIVE" == "1" ]]; then
  conf_flags+=("--with-native-compilation=yes")
else
  conf_flags+=("--with-native-compilation=no")
fi

# Useful extras (disable later via EXTRA_CONFIG if missing deps)
conf_flags+=(
  "--with-cairo"
  "--with-harfbuzz"
  "--with-tree-sitter"
  "--with-imagemagick"
  "--with-mailutils"
)

# Optional extra flags from env
if [[ -n "$EXTRA_CONFIG" ]]; then
  # shellcheck disable=SC2206
  conf_flags+=($EXTRA_CONFIG)
fi

# ---------- Build & install ----------
mkdir -p "$PREFIX"

# Temporarily change IFS to space for configure argument expansion
old_ifs="$IFS"
IFS=' '
run "cd '$SRC_DIR' && ./configure ${conf_flags[*]}"
IFS="$old_ifs"

if [[ "$JOBS" == "auto" ]]; then
  if command -v nproc >/dev/null 2>&1; then JOBS=$(nproc); else JOBS=4; fi
fi
run "cd '$SRC_DIR' && make -j$JOBS"

# Build documentation (info files)
say "...building info files"
(
  cd "$SRC_DIR"
  run "make info"
)

run "cd '$SRC_DIR' && make install"
run "cd '$SRC_DIR' && make install-info"

# ---------- Symlinks ----------
run "ln -sfn '$PREFIX' '$PREFIX_BASE/emacs-current'"
tmpdir="$(mktemp -d)"
for exe in "$PREFIX/bin/emacs" "$PREFIX/bin/emacsclient"; do
  [[ -x "$exe" ]] || continue
  ln -s "$exe" "$tmpdir/$(basename "$exe")"
done
mv -f "$tmpdir"/* "$HOME/.local/bin/" || { echo "Failed to install shims to ~/.local/bin" >&2; exit 72; }
rmdir "$tmpdir"

command -v emacs >/dev/null && emacs --version | head -n1 || true
command -v emacsclient >/dev/null && emacsclient --version | head -n1 || true
echo "Done. See $LOGFILE"
