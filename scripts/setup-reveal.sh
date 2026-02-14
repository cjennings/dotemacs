#!/usr/bin/env bash
# Setup reveal.js for org-reveal presentations (offline, pinned version)
# Usage: setup-reveal.sh [--yes]   # --yes for non-interactive mode

set -euo pipefail

REVEAL_VERSION="5.1.0"
REVEAL_DIR="$HOME/.emacs.d/reveal.js"

# Non-interactive mode
ASSUME_YES=false
if [[ "${1:-}" == "--yes" ]] || [[ "${1:-}" == "-y" ]]; then
  ASSUME_YES=true
fi

echo "=== reveal.js Setup for org-reveal ==="
echo

# Check if correct version already installed
if [[ -d "$REVEAL_DIR" ]]; then
  if [[ -f "$REVEAL_DIR/dist/reveal.js" ]]; then
    INSTALLED_VERSION=$(cd "$REVEAL_DIR" && git describe --tags 2>/dev/null || echo "unknown")
    if [[ "$INSTALLED_VERSION" == "$REVEAL_VERSION" ]]; then
      echo "✓ reveal.js $REVEAL_VERSION already installed at $REVEAL_DIR"
      exit 0
    else
      echo "Found reveal.js $INSTALLED_VERSION, need $REVEAL_VERSION"
      if [[ "$ASSUME_YES" == false ]]; then
        read -p "Replace with correct version? [Y/n] " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Nn]$ ]]; then
          echo "Aborted."
          exit 1
        fi
      fi
      echo "Removing old version..."
      rm -rf "$REVEAL_DIR"
    fi
  else
    echo "Found incomplete reveal.js directory, removing..."
    rm -rf "$REVEAL_DIR"
  fi
fi

# Clone reveal.js at pinned version (shallow clone for speed)
echo "Step 1/2: Cloning reveal.js $REVEAL_VERSION..."
git clone --depth 1 --branch "$REVEAL_VERSION" \
  https://github.com/hakimel/reveal.js.git "$REVEAL_DIR"
echo "✓ Cloned reveal.js $REVEAL_VERSION"

# Verify installation
echo
echo "Step 2/2: Verifying installation..."
if [[ -f "$REVEAL_DIR/dist/reveal.js" ]]; then
  echo "✓ reveal.js $REVEAL_VERSION installed at $REVEAL_DIR"
  echo
  echo "=== Setup Complete! ==="
  echo
  echo "Usage in Emacs:"
  echo "  C-; p n  Create new presentation"
  echo "  C-; p e  Export to HTML and open"
  echo "  C-; p p  Start live preview"
else
  echo "✗ Installation failed - dist/reveal.js not found"
  echo
  echo "Troubleshooting:"
  echo "1. Check git access to github.com"
  echo "2. Verify disk space at $REVEAL_DIR"
  echo "3. Try manual clone: git clone https://github.com/hakimel/reveal.js.git $REVEAL_DIR"
  exit 1
fi
