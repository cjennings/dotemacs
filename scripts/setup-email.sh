#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>

# Typically run on a fresh installation on a new machine.
# - Validates all email components of my Emacs email setup are in place
# - Validates local email directories exist; creates them if they don't exist
# - Performs initial email sync to local directories
# - Performs initial email indexing for both of my email accounts

set -euo pipefail

MBSYNC="$(command -v mbsync || true)"
MU="$(command -v mu || true)"
MU4EDIR="/usr/share/emacs/site-lisp/mu4e"
MSMTP="$(command -v msmtp || true)"

MBSYNCRC="$HOME/.mbsyncrc"
MSMTPRC="$HOME/.msmtprc"
MAILROOT="$HOME/.mail"
GMAILDIR="$MAILROOT/gmail"
CMAILDIR="$MAILROOT/cmail"

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
