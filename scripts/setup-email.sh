#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>

# Typically run on a fresh installation on a new machine.
# - Installs or decrypts mail password files into ~/.config/
# - Validates all email components of my Emacs email setup are in place
# - Validates local email directories exist; creates them if they don't exist
# - Performs initial email sync to local directories
# - Performs initial email indexing for all email accounts

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
DMAILDIR="$MAILROOT/dmail"

install_encrypted_password() {
    local filename="$1"
    local source_file="$ENCRYPTED_PASSWORDS_DIR/$filename"
    local dest_file="$PASSWORD_DEST_DIR/$filename"

    if [[ -f "$dest_file" ]]; then
        echo "  ✓ $dest_file already exists, skipping"
        return
    fi

    if [[ ! -f "$source_file" ]]; then
        echo "  ✗ missing $dest_file and $source_file"
        exit 1
    fi

    echo "  → installing $filename..."
    cp "$source_file" "$dest_file"
    chmod 600 "$dest_file"
    echo "  ✓ created $dest_file"
}

decrypt_password() {
    local encrypted_filename="$1"
    local dest_filename="$2"
    local source_file="$ENCRYPTED_PASSWORDS_DIR/$encrypted_filename"
    local dest_file="$PASSWORD_DEST_DIR/$dest_filename"

    if [[ -f "$dest_file" ]]; then
        echo "  ✓ $dest_file already exists, skipping"
        return
    fi

    if [[ ! -f "$source_file" ]]; then
        echo "  ✗ missing $dest_file and $source_file"
        exit 1
    fi

    echo "  → decrypting $encrypted_filename..."
    if gpg -q -d "$source_file" > "$dest_file" 2>/dev/null; then
        chmod 600 "$dest_file"
        echo "  ✓ created $dest_file"
    else
        echo "  ✗ failed to decrypt $encrypted_filename"
        rm -f "$dest_file"
        exit 1
    fi
}

# Decrypt Mail Passwords
# Skip if destination already exists, install or decrypt if missing.
echo "→ checking mail passwords..."
if [[ ! -d "$ENCRYPTED_PASSWORDS_DIR" ]]; then
    echo "  ✗ encrypted passwords directory not found: $ENCRYPTED_PASSWORDS_DIR"
    exit 1
fi
mkdir -p "$PASSWORD_DEST_DIR"
install_encrypted_password ".gmailpass.gpg"
decrypt_password ".cmailpass.gpg" ".cmailpass"
install_encrypted_password ".dmailpass.gpg"

# Check All Prerequisites
[[ -x "$MBSYNC"   ]] || { echo "ERROR: mbsync not found. Install 'isync'."; exit 1; }
[[ -x "$MU"       ]] || { echo "ERROR: mu not found. Install 'mu'."; exit 1; }
[[ -d "$MU4EDIR"  ]] || { echo "ERROR: mu4e elisp not found at $MU4EDIR. Install 'mu'."; exit 1; }
[[ -f "$MBSYNCRC" ]] || { echo "ERROR: '~/.mbsyncrc' missing."; exit 1; }
[[ -x "$MSMTP"    ]] || { echo "ERROR: msmtp not found. Install 'msmtp'."; exit 1; }
[[ -f "$MSMTPRC"  ]] || { echo "ERROR: '~/.msmtprc' missing."; exit 1; }

# Ensure Mail Dirs Exist
mkdir -p "$GMAILDIR" "$CMAILDIR" "$DMAILDIR"

# Initial Sync
echo "→ syncing all mail with mbsync ..."
"$MBSYNC" -aV

# Init MU and Index Email
echo "→ initializing mu ..."
"$MU" init --maildir="$MAILROOT" \
		  --my-address="craigmartinjennings@gmail.com" \
		  --my-address="c@cjennings.net" \
		  --my-address="craig.jennings@deepsat.com"

echo "→ indexing mail ..."
"$MU" index

echo "✅ Mail setup complete."
