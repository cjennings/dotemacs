#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>
# Build & install Emacs from source
# safe, user-local, fast, versioned.

set -Eeuo pipefail
IFS=$'\n\t'

#  ------------------------ Config (overwrite via ENV) -----------------------

SRC_DIR="${SRC_DIR:-$HOME/.local/src/emacs}"
EMACS_REPO="${EMACS_REPO:-https://git.savannah.gnu.org/git/emacs.git}"
CHECKOUT_REF="${CHECKOUT_REF:-emacs-30.2}"
PREFIX_BASE="${PREFIX_BASE:-$HOME/.local/src/emacs}"

LOG_DIR="${LOG_DIR:-$HOME/}"
WITH_PGTK="${WITH_PGTK:-0}"
JOBS="${JOBS:-auto}"
EXTRA_CONFIG="${EXTRA_CONFIG:-}"

#  --------------------------------- Preflight ---------------------------------

umask 022
mkdir -p "$LOG_DIR" "$PREFIX_BASE" "$HOME/.local/bin"
: "${LOGFILE:=$LOG_DIR/emacs-build-$(date +%Y%m%d-%H%M%S)-${CHECKOUT_REF}.log}"

say() { printf '>>> %s\n' "$*" | tee -a "$LOGFILE" ; }
run() {
	say "+ $*"
	# shellcheck disable=SC2294
	# eval is necessary here because run() is called with shell command strings
	if ! eval "$@" >>"$LOGFILE" 2>&1; then
		echo "ERROR: Command failed: $*" >&2
		echo "Last 20 lines of output:" >&2
		tail -n 20 "$LOGFILE" >&2
		return 1
	fi
}

on_err(){
	ec=$?
	echo "ERROR [$ec] - Full log at: $LOGFILE" >&2
	echo "Last 100 lines of log:" >&2
	tail -n 100 "$LOGFILE" >&2 || true
	exit "$ec"
}

trap on_err ERR

#  -------------------------- Arch Linux Dependencies --------------------------

if [[ -r /etc/os-release ]]; then
  . /etc/os-release
fi

if [[ "${ID:-}" == "arch" || "${ID_LIKE:-}" == *arch* ]]; then
  say "Arch Linux detected; checking required build/runtime packages"

  # Base packages needed for the requested configure flags
  # Always include libgccjit since we'll be building the regular version with native-comp
  pkgs=(jansson tree-sitter imagemagick mailutils harfbuzz cairo gnutls libxml2 texinfo gtk3 libgccjit)

  # List packages that are not installed
  missing="$(pacman -T "${pkgs[@]}" || true)"

  if [[ -z "$missing" ]]; then
	say "All required packages are already installed."
  else
	say "Missing packages: $missing"
	if command -v sudo >/dev/null 2>&1 && sudo -n true 2>/dev/null; then
	  run "sudo -n pacman -Sy --needed --noconfirm $missing"
	else
	  say "sudo (passwordless) not available; please install missing packages manually:"
	  echo "  sudo pacman -Sy --needed $missing"
	  exit 70
	fi
  fi
else
  say "Non-Arch system detected; skipping Arch-specific dependency check."
fi

#  ----------------------------- Clone And Update ----------------------------

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

#  ---------------------------------- Autogen ----------------------------------

if [[ -x "$SRC_DIR/autogen.sh" ]]; then
  run "cd '$SRC_DIR' && ./autogen.sh"
fi

#  ----------------------------- Build Function ------------------------------

build_emacs() {
  local build_type="$1"
  local PREFIX CFLAGS ENABLE_CHECKING ENABLE_NATIVE

  say "=========================================="
  say "Building ${build_type} version"
  say "=========================================="

  # Set build-specific variables
  if [[ "$build_type" == "debug" ]]; then
    PREFIX="$PREFIX_BASE/${CHECKOUT_REF}-debug"
    CFLAGS="-O0 -g"
    ENABLE_CHECKING="yes,glyphs"
    ENABLE_NATIVE="0"  # Disable native comp for debug
  else
    PREFIX="$PREFIX_BASE/${CHECKOUT_REF}"
    CFLAGS="-O2 -g"
    ENABLE_CHECKING="no"
    ENABLE_NATIVE="1"
  fi

#  ------------------------------ Configure Flags ------------------------------

  local conf_flags=(
    "--prefix=${PREFIX}"
    "--with-json"
    "--with-modules"
  )

  # Add debug-specific flags
  if [[ "$build_type" == "debug" ]]; then
    say "Building DEBUG version with symbols and checking"
    conf_flags+=(
      "--enable-checking=${ENABLE_CHECKING}"
      "--enable-check-lisp-object-type"
      "CFLAGS=${CFLAGS}"
    )
  fi

  # Wayland/X choice
  if [[ "$WITH_PGTK" == "auto" ]]; then
    if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then WITH_PGTK="yes"; else WITH_PGTK="no"; fi
  fi
  if [[ "$WITH_PGTK" == "yes" ]]; then
    conf_flags+=("--with-pgtk")
  else
    conf_flags+=("--with-x-toolkit=gtk3")
  fi

  # Native-compilation
  if [[ "$ENABLE_NATIVE" == "1" ]]; then
    conf_flags+=("--with-native-compilation=yes")
  else
    conf_flags+=("--with-native-compilation=no")
  fi

  # Useful extras
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
    # Word splitting is intentional here to allow multiple flags in EXTRA_CONFIG
    conf_flags+=($EXTRA_CONFIG)
  fi

#  ----------------------------- Build And Install -----------------------------

  mkdir -p "$PREFIX"

  # Temporarily change IFS to space for configure argument expansion
  local old_ifs="$IFS"
  IFS=' '
  cd "$SRC_DIR"
  say "Running configure with flags: ${conf_flags[*]}"
  if ! "./configure" "${conf_flags[@]}" >>"$LOGFILE" 2>&1; then
    echo "ERROR: Configure failed" >&2
    echo "Last 50 lines of output:" >&2
    tail -n 50 "$LOGFILE" >&2
    exit 1
  fi
  IFS="$old_ifs"

  local num_jobs="$JOBS"
  if [[ "$num_jobs" == "auto" ]]; then
    if command -v nproc >/dev/null 2>&1; then num_jobs=$(nproc); else num_jobs=4; fi
  fi
  run "cd '$SRC_DIR' && make -j$num_jobs"

  # Build documentation (info files)
  say "...building info files"
  (
    cd "$SRC_DIR"
    run "make info"
  )

  run "cd '$SRC_DIR' && make install"
  run "cd '$SRC_DIR' && make install-info"

#  --------------------------------- Symlinks --------------------------------

  if [[ "$build_type" == "debug" ]]; then
    run "ln -sfn '$PREFIX' '$PREFIX_BASE/emacs-debug'"
    # Create debug-specific symlinks
    for exe in emacs emacsclient; do
      target="$PREFIX/bin/$exe"
      link="$HOME/.local/bin/${exe}-debug"
      if [[ -x "$target" ]]; then
        run "rm -f '$link'"
        run "ln -s '$target' '$link'"
      fi
    done
    say "Debug build available as 'emacs-debug' and 'emacsclient-debug'"
  else
    run "ln -sfn '$PREFIX' '$PREFIX_BASE/emacs-current'"
    for exe in emacs emacsclient; do
      target="$PREFIX/bin/$exe"
      link="$HOME/.local/bin/$exe"
      if [[ -x "$target" ]]; then
        run "rm -f '$link'"
        run "ln -s '$target' '$link'"
      fi
    done
  fi
}  # End of build_emacs function

#  ----------------------------- Build Both Versions -----------------------------

say "Building both regular and debug versions of Emacs..."

# Build regular version first
build_emacs "regular"

# Build debug version second
build_emacs "debug"

#  ---------------------------- Post-Build Validation ----------------------------

say "Running post-build validation checks..."

# Check that both binaries exist and are executable
if [[ ! -x "$HOME/.local/bin/emacs" ]]; then
  echo "ERROR: Regular emacs binary not found or not executable" >&2
  exit 1
fi
say "✓ Regular emacs binary exists and is executable"

if [[ ! -x "$HOME/.local/bin/emacs-debug" ]]; then
  echo "ERROR: Debug emacs binary not found or not executable" >&2
  exit 1
fi
say "✓ Debug emacs binary exists and is executable"

# Check that the binaries are actually different
if cmp -s "$HOME/.local/bin/emacs" "$HOME/.local/bin/emacs-debug"; then
  echo "ERROR: Regular and debug builds are identical!" >&2
  exit 1
fi
say "✓ Regular and debug builds are different"

# Check that debug build has debug symbols
if ! readelf -S "$HOME/.local/bin/emacs-debug" 2>/dev/null | grep -q "\.debug_"; then
  echo "WARNING: Debug build may not have debug symbols" >&2
else
  say "✓ Debug build contains debug symbols"
fi

# Smoke test both versions
if ! "$HOME/.local/bin/emacs" --batch --eval '(message "Regular: %s" emacs-version)' 2>&1 | grep -q "Regular:"; then
  echo "ERROR: Regular emacs failed smoke test" >&2
  exit 1
fi
say "✓ Regular emacs smoke test passed"

if ! "$HOME/.local/bin/emacs-debug" --batch --eval '(message "Debug: %s" emacs-version)' 2>&1 | grep -q "Debug:"; then
  echo "ERROR: Debug emacs failed smoke test" >&2
  exit 1
fi
say "✓ Debug emacs smoke test passed"

say "All validation checks passed!"

#  ---------------------------------- Wrap Up ----------------------------------

say "=========================================="
say "Both builds completed successfully!"
say "=========================================="

# Show version info for both builds
command -v emacs >/dev/null && emacs --version | head -n1 || true
command -v emacs-debug >/dev/null && emacs-debug --version | head -n1 || true

#  ----------------------------- Show Build Features ----------------------------

show_build_features() {
  local emacs_bin="$1"
  local build_name="$2"

  say "Launching $build_name to display version and build features..."
  "$emacs_bin" -Q --batch --eval '
(progn
  (princ (format "Emacs version: %s\n" emacs-version))
  (princ (format "Build configuration:\n"))
  (princ (format "  Native compilation: %s\n"
				 (if (and (fboundp (quote native-comp-available-p))
						  (native-comp-available-p))
					 "yes" "no")))
  (princ (format "  PGTK: %s\n" (if (featurep (quote pgtk)) "yes" "no")))
  (princ (format "  Tree-sitter: %s\n"
				 (if (fboundp (quote treesit-available-p)) "yes" "no")))
  (princ (format "  JSON: %s\n" (if (fboundp (quote json-parse-string)) "yes" "no")))
  (princ (format "  ImageMagick: %s\n" (if (image-type-available-p (quote imagemagick)) "yes" "no")))
  (princ (format "  Cairo: %s\n" (if (featurep (quote cairo)) "yes" "no")))
  (princ (format "\nFull system configuration:\n%s\n" system-configuration))
  (princ (format "\nConfigured features:\n%s\n" system-configuration-features)))
' 2>&1 | tee -a "$LOGFILE"
}

show_build_features "$HOME/.local/bin/emacs" "Regular build"
echo ""
show_build_features "$HOME/.local/bin/emacs-debug" "Debug build"

echo "Done. See $LOGFILE"
