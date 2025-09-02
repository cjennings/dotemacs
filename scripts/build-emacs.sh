#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>
# Build & install Emacs from source
# safe, versioned, user-local, blunt, fast, reversible.

set -euo pipefail

### --------- CONFIG (edit these) ---------
# Where the git checkout lives:
SRC_DIR="${SRC_DIR:-$HOME/code/emacs-src}"

# Which repo/branch/tag to build:
EMACS_REPO="${EMACS_REPO:-https://git.savannah.gnu.org/git/emacs.git}"
CHECKOUT_REF="${CHECKOUT_REF:-emacs-29.2}"   # e.g. "emacs-30.1", "master", "emacs-29"

# Install prefix (keeps versions side-by-side):
PREFIX_BASE="${PREFIX_BASE:-$HOME/.local/opt}"
PREFIX="${PREFIX:-$PREFIX_BASE/${CHECKOUT_REF}}"

# Optional knobs:
ENABLE_NATIVE="${ENABLE_NATIVE:-0}"      # 1 to enable native-comp if toolchain supports it
ENABLE_PGTK="${ENABLE_PGTK:-0}"          # 1 to use pgtk (Wayland-only build)
ENABLE_XWIDGETS="${ENABLE_XWIDGETS:-0}"  # 1 to enable xwidgets (known flaky on some 29/30 tags)
MAKE_JOBS="${MAKE_JOBS:-$(command -v nproc >/dev/null 2>&1 && nproc || sysctl -n hw.ncpu || echo 4)}"

# CFLAGS (tune lightly; keep reproducible):
CFLAGS_OVERRIDES="${CFLAGS_OVERRIDES:--O2 -march=native}"

LOG_DIR="${LOG_DIR:-$HOME/.cache/build-logs}"
mkdir -p "$LOG_DIR"
LOGFILE="$LOG_DIR/emacs-build-$(date +%Y%m%d-%H%M%S)-${CHECKOUT_REF}.log"
### ---------------------------------------

say() { printf '%s\n' "$*" | tee -a "$LOGFILE" ; }
run() { say "+ $*"; eval "$@" >>"$LOGFILE" 2>&1; }

trap 'echo "ERROR: see $LOGFILE" >&2' ERR

say ">>> Building Emacs"
say "SRC_DIR=$SRC_DIR"
say "CHECKOUT_REF=$CHECKOUT_REF"
say "PREFIX=$PREFIX"
say "LOGFILE=$LOGFILE"

# Fetch or update source
if [[ -d "$SRC_DIR/.git" ]]; then
  say "...updating existing checkout"
  (cd "$SRC_DIR" && run "git fetch --tags --prune" && run "git status --porcelain")
else
  say "...cloning source"
  mkdir -p "$(dirname "$SRC_DIR")"
  run "git clone --depth=1 --branch ${CHECKOUT_REF} ${EMACS_REPO} ${SRC_DIR}" || {
    # If ref isn’t a branch, do full clone then checkout tag
    run "git clone ${EMACS_REPO} ${SRC_DIR}"
  }
fi

# Checkout desired ref/tag
(
  cd "$SRC_DIR"
  run "git fetch --tags --force"
  run "git checkout ${CHECKOUT_REF}"
)

# Autogen/bootstrap (needed for git checkouts)
if [[ -x "$SRC_DIR/autogen.sh" ]]; then
  say "...running autogen.sh"
  (cd "$SRC_DIR" && run "./autogen.sh")
fi

# Configure flags
conf_flags=(
  "--prefix=${PREFIX}"
  "--with-json"
  "--with-modules"
  "--with-cairo"
  "--with-harfbuzz"
  "--with-native-compilation=$( [[ $ENABLE_NATIVE -eq 1 ]] && echo yes || echo no )"
  "--with-mailutils"
  "--with-imagemagick"
)

# pgtk vs X
if [[ $ENABLE_PGTK -eq 1 ]]; then
  conf_flags+=("--with-pgtk" "--without-x")
else
  conf_flags+=("--with-x" "--with-x-toolkit=gtk3")
fi

# xwidgets if requested
if [[ $ENABLE_XWIDGETS -eq 1 ]]; then
  conf_flags+=("--with-xwidgets")
fi

# Prefer system tree-sitter if present
if pkg-config --exists tree-sitter 2>/dev/null; then
  conf_flags+=("--with-tree-sitter")
fi

say "...configure flags:"
printf '   %s\n' "${conf_flags[@]}" | tee -a "$LOGFILE"

# Configure
mkdir -p "$PREFIX"  # ensure we can write there
(
  cd "$SRC_DIR"
  run "env CFLAGS='${CFLAGS_OVERRIDES}' ./configure ${conf_flags[*]}"
)

# Build (and bootstrap if needed)
say "...compiling (jobs=$MAKE_JOBS)"
(
  cd "$SRC_DIR"
  # master sometimes needs bootstrap after significant changes; harmless otherwise:
  run "make -j${MAKE_JOBS}"
)

# Install to user-local prefix
say "...installing to ${PREFIX}"
(
  cd "$SRC_DIR"
  run "make install"
)

# ymlink all installed executables (emacs, emacsclient, etags, etc.)
mkdir -p "$HOME/.local/bin"

# Atomic swap: build symlinks in a temp dir, then move them into place.
tmpdir="$(mktemp -d)"
for exe in "$PREFIX/bin/"*; do
  [ -x "$exe" ] || continue
  name="$(basename "$exe")"
  ln -s "$exe" "$tmpdir/$name"
done

# Move into ~/.local/bin (overwriting existing symlinks/files with same names)
for link in "$tmpdir"/*; do
  name="$(basename "$link")"
  mv -f "$link" "$HOME/.local/bin/$name"
done
rmdir "$tmpdir"

# Optional: quick sanity print
command -v emacs >/dev/null && emacs --version | head -n1 || true
command -v emacsclient >/dev/null && emacsclient --version | head -n1 || true

