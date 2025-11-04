#!/usr/bin/env bash
# Install OpenAI Whisper for transcription on Arch Linux
# Usage: install-whisper.sh [--yes]   # --yes for non-interactive mode

set -euo pipefail

# Non-interactive mode
ASSUME_YES=false
if [[ "${1:-}" == "--yes" ]] || [[ "${1:-}" == "-y" ]]; then
  ASSUME_YES=true
fi

echo "=== Whisper Installation for Arch Linux ==="
echo

# Check if running on Arch
if [[ ! -f /etc/arch-release ]]; then
  echo "Warning: This script is designed for Arch Linux"
  if [[ "$ASSUME_YES" == false ]]; then
    read -p "Continue anyway? [y/N] " -n 1 -r
    echo
    [[ ! $REPLY =~ ^[Yy]$ ]] && exit 1
  else
    echo "Continuing anyway (--yes mode)"
  fi
fi

# 1. Install system dependencies
echo "Step 1/3: Installing system dependencies (ffmpeg)..."
if ! command -v ffmpeg &> /dev/null; then
  sudo pacman -S --needed ffmpeg
  echo "✓ ffmpeg installed"
else
  echo "✓ ffmpeg already installed"
fi

# 2. Check for AUR package first (optional but cleaner)
echo
echo "Step 2/3: Checking for AUR package..."
AUR_INSTALLED=false

if command -v yay &> /dev/null; then
  echo "Found yay. Checking AUR for python-openai-whisper..."
  if yay -Ss python-openai-whisper | grep -q 'python-openai-whisper'; then
    INSTALL_AUR=false
    if [[ "$ASSUME_YES" == true ]]; then
      echo "Installing from AUR (--yes mode)"
      INSTALL_AUR=true
    else
      read -p "Install from AUR via yay? [Y/n] " -n 1 -r
      echo
      if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        INSTALL_AUR=true
      fi
    fi

    if [[ "$INSTALL_AUR" == true ]]; then
      yay -S --needed --noconfirm python-openai-whisper
      echo "✓ Installed from AUR"
      AUR_INSTALLED=true
    fi
  else
    echo "Package python-openai-whisper not found in AUR"
  fi
else
  echo "yay not found. Skipping AUR installation."
  echo "(Install yay if you prefer AUR packages)"
fi

# 3. Install via pip if not from AUR
if [[ "$AUR_INSTALLED" == false ]]; then
  echo
  echo "Step 3/3: Installing openai-whisper via pip..."
  pip install --user -U openai-whisper
  echo "✓ openai-whisper installed via pip"
  echo
  echo "Note: Ensure ~/.local/bin is in your PATH"
  echo "Add to ~/.bashrc or ~/.zshrc: export PATH=\"\$HOME/.local/bin:\$PATH\""
fi

# Verify installation
echo
echo "=== Verifying Installation ==="
if command -v whisper &> /dev/null; then
  echo "✓ whisper command found at: $(which whisper)"
  whisper --help | head -n 3
  echo
  echo "=== Installation Complete! ==="
  echo
  echo "Models available: tiny, base, small, medium, large"
  echo "Recommended: small (good balance of speed/accuracy)"
  echo "Model will download automatically on first use."
  echo
  echo "Test with: whisper your-audio.m4a --model small --language en"
else
  echo "✗ Installation failed - whisper command not found"
  echo
  echo "Troubleshooting:"
  echo "1. Ensure ~/.local/bin is in your PATH"
  echo "2. Run: source ~/.bashrc (or ~/.zshrc)"
  echo "3. Try: python -m whisper --help"
  exit 1
fi
