#!/usr/bin/env bash
# Uninstall OpenAI Whisper

set -euo pipefail

echo "=== Whisper Uninstallation ==="
echo

REMOVED=false

# Check if installed via AUR
if command -v yay &> /dev/null; then
  if yay -Qi python-openai-whisper &> /dev/null 2>&1; then
    echo "Detected AUR installation (python-openai-whisper)"
    read -p "Remove via yay? [Y/n] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
      yay -R python-openai-whisper
      echo "✓ Removed via AUR"
      REMOVED=true
    fi
  fi
fi

# Check if installed via pip
if pip list 2>/dev/null | grep -q openai-whisper; then
  echo "Detected pip installation (openai-whisper)"
  read -p "Remove via pip? [Y/n] " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
    pip uninstall -y openai-whisper
    echo "✓ Removed via pip"
    REMOVED=true
  fi
fi

if [[ "$REMOVED" == false ]]; then
  echo "No whisper installation found (checked AUR and pip)"
fi

# Ask about ffmpeg
echo
read -p "Remove ffmpeg? (may be used by other apps) [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
  sudo pacman -R ffmpeg
  echo "✓ Removed ffmpeg"
fi

# Ask about model cache
CACHE_DIR="$HOME/.cache/whisper"
if [[ -d "$CACHE_DIR" ]]; then
  echo
  echo "Whisper models are cached in: $CACHE_DIR"
  du -sh "$CACHE_DIR" 2>/dev/null || echo "Size: unknown"
  read -p "Delete cached models? [y/N] " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$CACHE_DIR"
    echo "✓ Deleted model cache"
  fi
fi

echo
echo "=== Uninstallation Complete ==="
