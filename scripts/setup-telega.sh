#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>
#
# Prepares everything telega.el needs on a fresh clone.
#
# Sequence:
# - Verifies docker is installed and the daemon is responsive.
# - Verifies the user can talk to docker without sudo (group membership).
# - Pulls the telega-server image if a public one is configured (env var
#   `TELEGA_DOCKER_IMAGE'); otherwise prints the in-Emacs build command
#   (`M-x telega-server-build') for the user to run once.
# - Installs the `telega' Emacs package via package.el if it isn't
#   already in package-user-dir.  modules/telega-config.el uses
#   `:ensure nil' (a stale MELPA index can 404 and take startup down
#   if auto-install runs in init), so the install has to happen
#   explicitly somewhere.  This is that somewhere.
#
# Does NOT handle Telegram account auth -- phone number + verification
# code is interactive and runs inside `M-x telega' on first launch.
#
# Sourceable: only `main' runs when the script is executed directly, so
# the helpers can be tested with bats.

set -euo pipefail

# Public image override.  When set, the script pulls this image directly
# instead of falling back to the in-Emacs build flow.  Default empty so
# the script doesn't assume an image name that may or may not exist
# upstream.
: "${TELEGA_DOCKER_IMAGE:=}"

ensure_docker_installed() {
    if ! command -v docker >/dev/null 2>&1; then
        echo "  ✗ docker not found on PATH"
        echo "    install docker (https://docs.docker.com/get-docker/) and rerun"
        return 1
    fi
    echo "  ✓ docker present: $(command -v docker)"
}

ensure_docker_running() {
    if ! docker info >/dev/null 2>&1; then
        echo "  ✗ cannot talk to the docker daemon"
        echo "    start the daemon (e.g. 'sudo systemctl start docker') or"
        echo "    add your user to the 'docker' group, then re-login and rerun"
        return 1
    fi
    echo "  ✓ docker daemon reachable"
}

pull_or_announce_image() {
    if [[ -z "$TELEGA_DOCKER_IMAGE" ]]; then
        cat <<EOF
  → no public image configured (set TELEGA_DOCKER_IMAGE to override)
    build the telega-server image once from inside Emacs:
      M-x telega-server-build
    telega.el handles the docker build under the hood when
    \`telega-use-docker' is t (set in modules/telega-config.el).
EOF
        return 0
    fi
    echo "  → pulling $TELEGA_DOCKER_IMAGE..."
    if docker pull "$TELEGA_DOCKER_IMAGE"; then
        echo "  ✓ pulled $TELEGA_DOCKER_IMAGE"
    else
        echo "  ✗ pull failed for $TELEGA_DOCKER_IMAGE"
        return 1
    fi
}

ensure_telega_package() {
    if ! command -v emacs >/dev/null 2>&1; then
        echo "  ✗ emacs not on PATH; install Emacs first"
        return 1
    fi
    # Probe quietly whether telega is already in package-user-dir.
    if emacs --batch --eval "(progn
        (require 'package)
        (package-initialize)
        (kill-emacs (if (package-installed-p 'telega) 0 1)))" \
        >/dev/null 2>&1; then
        echo "  ✓ telega Emacs package already installed"
        return 0
    fi
    echo "  → installing telega via package.el..."
    if emacs --batch --eval "(progn
        (require 'package)
        (package-initialize)
        (unless (assoc \"melpa\" package-archives)
          (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t))
        (package-refresh-contents)
        (package-install 'telega))" >/dev/null 2>&1; then
        echo "  ✓ telega installed"
    else
        echo "  ✗ telega install failed"
        echo "    try interactively:"
        echo "      M-x package-refresh-contents"
        echo "      M-x package-install RET telega"
        return 1
    fi
}

main() {
    echo "→ checking docker..."
    ensure_docker_installed
    ensure_docker_running
    echo "→ preparing telega-server image..."
    pull_or_announce_image
    echo "→ checking telega Emacs package..."
    ensure_telega_package
    echo "✅ Telega setup complete."
    echo "   First launch: M-x telega -- enter phone + verification code interactively."
}

# Run main only when the script is executed directly.  Sourcing this file
# (for example from a bats test) just defines the helpers above.
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
