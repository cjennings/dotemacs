#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>

# Typically run on a fresh installation on a new machine.
# - Decrypts mail passwords from encrypted .gpg files to ~/.config/
# - Validates all email components of my Emacs email setup are in place
# - Validates local email directories exist; creates them if they don't exist
# - Performs initial email sync to local directories
# - Performs initial email indexing for both of my email accounts

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_DIR="$(dirname "$SCRIPT_DIR")"
ENCRYPTED_PASSWORDS_DIR="$EMACS_DIR/assets/mail-passwords"
PASSWORD_DEST_DIR="$HOME/.config"

MBSYNC="$(command -v mbsync || true)"
MU="$(command -v mu || true)"
MU4EDIR="/usr/share/emacs/site-lisp/mu4e"
MSMTP="$(command -v msmtp || true)"

MBSYNCRC="$HOME/.mbsyncrc"
MSMTPRC="$HOME/.msmtprc"
MAILROOT="$HOME/.mail"
GMAILDIR="$MAILROOT/gmail"
CMAILDIR="$MAILROOT/cmail"

# Decrypt Mail Passwords
# Loop through all .gpg files in assets/mail-passwords/
# Skip if destination already exists, decrypt if missing
echo "→ checking mail passwords..."
if [[ -d "$ENCRYPTED_PASSWORDS_DIR" ]]; then
    for gpg_file in "$ENCRYPTED_PASSWORDS_DIR"/*.gpg; do
        [[ -f "$gpg_file" ]] || continue  # Skip if no .gpg files

        filename=$(basename "$gpg_file")
        dest_file="$PASSWORD_DEST_DIR/${filename%.gpg}"  # Strip .gpg extension

        if [[ -f "$dest_file" ]]; then
            echo "  ✓ $dest_file already exists, skipping"
        else
            echo "  → decrypting $filename..."
            if gpg -q -d "$gpg_file" > "$dest_file" 2>/dev/null; then
                chmod 600 "$dest_file"
                echo "  ✓ created $dest_file"
            else
                echo "  ✗ failed to decrypt $filename"
                rm -f "$dest_file"  # Clean up partial file
                exit 1
            fi
        fi
    done
else
    echo "  ⚠ encrypted passwords directory not found: $ENCRYPTED_PASSWORDS_DIR"
fi

# Check All Prerequisites
[[ -x "$MBSYNC"   ]] || { echo "ERROR: mbsync not found. Install 'isync'."; exit 1; }
[[ -x "$MU"       ]] || { echo "ERROR: mu not found. Install 'mu'."; exit 1; }
[[ -d "$MU4EDIR"  ]] || { echo "ERROR: mu4e elisp not found at $MU4EDIR. Install 'mu'."; exit 1; }
[[ -f "$MBSYNCRC" ]] || { echo "ERROR: '~/.mbsyncrc' missing."; exit 1; }
[[ -x "$MSMTP"    ]] || { echo "ERROR: msmtp not found. Install 'msmtp'."; exit 1; }
[[ -f "$MSMTPRC"  ]] || { echo "ERROR: '~/.msmtprc' missing."; exit 1; }

# Ensure Mail Dirs Exist
mkdir -p "$GMAILDIR" "$CMAILDIR"

# Initial Sync
echo "→ syncing all mail with mbsync ..."
"$MBSYNC" -aV

# Init MU and Index Email
echo "→ initializing mu ..."
"$MU" init --maildir="$MAILROOT" \
		  --my-address="craigmartinjennings@gmail.com" \
		  --my-address="c@cjennings.net"

echo "→ indexing mail ..."
"$MU" index

echo "✅ Mail setup complete."
