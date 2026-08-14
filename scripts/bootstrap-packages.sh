#!/usr/bin/env bash
#
# Install every package this config asks for, headlessly, before first launch.
#
# On a fresh machine init.el pulls ~190 packages over the network one at a
# time.  A single dead download used to abort startup outright;
# modules/package-resilience.el now records the failure and lets init finish,
# and this script is what turns that into a completed install: load init.el in
# batch, retry whatever is still missing, and repeat while progress is being
# made.  Doing it here rather than in a GUI session means a fresh install never
# meets the debugger.
#
# Usage:  scripts/bootstrap-packages.sh
# Exit:   0 when every package is installed, non-zero otherwise.
#
# Environment:
#   EMACS              emacs binary to use          (default: emacs)
#   BOOTSTRAP_PASSES   maximum passes over the set  (default: 4)
#   BOOTSTRAP_TIMEOUT  seconds allowed per pass     (default: 1800)
#
# Note: a pass loads the whole config in batch, so every :config block runs
# headlessly.  stdin is closed and each pass is bounded by a timeout so a
# prompt or a hung network fetch fails the pass instead of stalling forever.

set -uo pipefail

emacs_bin="${EMACS:-emacs}"
max_passes="${BOOTSTRAP_PASSES:-4}"
pass_timeout="${BOOTSTRAP_TIMEOUT:-1800}"

emacs_dir="${BOOTSTRAP_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
log_dir="$(mktemp -d -t emacs-bootstrap-XXXXXX)"

cleanup() { rm -rf "$log_dir"; }
trap cleanup EXIT

# --batch implies -q, which skips early-init.el.  That file is where the package
# archives, use-package-always-ensure, and package-resilience all live, so a
# pass that loaded only init.el would install almost nothing and would not even
# have cj/package-bootstrap-batch defined.  Load both, in the order a real
# startup does.  user-emacs-directory is set first so the pass bootstraps the
# checkout this script lives in rather than whatever $HOME/.emacs.d happens to
# be.
load_form="$(cat <<EOF
(progn
  (setq load-prefer-newer t)
  (setq user-emacs-directory "${emacs_dir}/")
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  (cj/package-bootstrap-batch))
EOF
)"

echo "bootstrap: installing packages for $emacs_dir"
echo "bootstrap: up to $max_passes passes, ${pass_timeout}s each"

# use-package calls its ensure function at macro-expansion time when a file is
# being byte-compiled, and emits no runtime call at all.  So a pass that loads
# .elc files installs nothing and would still exit 0 -- a false pass, the same
# shape as every other gate in this repo that was green because it never ran.
# Refuse rather than warn: there is no use for a bootstrap that cannot install,
# and a warning above a success line is read as a success.  A genuinely fresh
# machine has no .elc and never sees this.
# Every directory the config puts on its load-path, not just modules/, since a
# use-package form anywhere in them would be consumed the same way.
if compgen -G "$emacs_dir/modules/*.elc" >/dev/null 2>&1 \
       || compgen -G "$emacs_dir/custom/*.elc" >/dev/null 2>&1 \
       || compgen -G "$emacs_dir/assets/*.elc" >/dev/null 2>&1 \
       || compgen -G "$emacs_dir/*.elc" >/dev/null 2>&1; then
    echo "bootstrap: REFUSING - byte-compiled modules are present." >&2
    echo "bootstrap: use-package consumes :ensure at compile time, so a pass over" >&2
    echo "bootstrap: .elc files installs nothing and would report success anyway." >&2
    echo "bootstrap: run 'make clean-compiled' first, then bootstrap." >&2
    exit 2
fi

pass=1
passes_run=0
status=1
while [ "$pass" -le "$max_passes" ]; do
    log="$log_dir/pass-$pass.log"
    echo "bootstrap: pass $pass of $max_passes ..."

    timeout "$pass_timeout" "$emacs_bin" --batch \
            --eval "$load_form" </dev/null >"$log" 2>&1
    status=$?
    passes_run=$((passes_run + 1))

    case "$status" in
        0)
            echo "bootstrap: every package is installed (pass $pass)"
            break
            ;;
        1)
            # Exit 1 is only meaningful when the pass actually said what is
            # missing.  Anything else exiting 1 is a different failure, and
            # retrying it four times then blaming packages would be a lie.
            if grep -E '^package-bootstrap: [0-9]+ missing:' "$log"; then
                : # another pass can clear them; installing one unblocks others
            else
                echo "bootstrap: pass $pass exited 1 without reporting missing packages" >&2
                tail -30 "$log" >&2
                break
            fi
            ;;
        124)
            echo "bootstrap: pass $pass hit the ${pass_timeout}s timeout" >&2
            tail -20 "$log" >&2
            ;;
        *)
            # init itself failed for some reason other than a missing package.
            echo "bootstrap: pass $pass failed to load init (exit $status)" >&2
            tail -30 "$log" >&2
            break
            ;;
    esac

    pass=$((pass + 1))
done

if [ "$status" -ne 0 ]; then
    echo "bootstrap: FAILED after $passes_run pass(es)" >&2
    # No pass ran at all when the ceiling is zero, and the glob would then match
    # nothing and print a tail error over the real message.
    if [ "$passes_run" -gt 0 ]; then
        echo "bootstrap: tail of the last pass follows" >&2
        tail -30 "$log" >&2
    fi
    exit "$status"
fi

exit 0
