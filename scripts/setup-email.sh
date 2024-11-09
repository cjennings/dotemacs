#!/usr/bin/env bash
#  ------------------------------ Notes ------------------------------
# Craig Jennings <c@cjennings.net>
#
# this mu4e mail setup script is particular to my own setup
# ===== this will not work for you without adjustments =====

MBSYNC=/usr/bin/mbsync
MBSYNCRC="$HOME/.mbsyncrc"
MU4E_DIR=/usr/share/emacs/site-lisp/mu4e/
MSMTP=/usr/bin/msmtp
MSMTPRC="$HOME/.msmtprc"
MU=/usr/bin/mu
GMAIL="$HOME/.mail/gmail"
CMAIL="$HOME/.mail/cmail"

#  ----------------------- Preliminary Checks ----------------------
# is mbsync installed?
if ! [ -f $MBSYNC ]; then echo "mbsync not installed at $MBSYNC. Install package 'isync' to continue"; exit 1; fi
if ! [ -f $MU ]; then echo "mu not installed at $MU. Install package 'mu' to continue"; exit 1; fi
if ! [ -d $MU4E ]; then echo "mu4e elisp files not at $MU4E_DIR. Did you install the 'mu' package?"; exit 1; fi

# does .mbsyncrc exist?
if ! [ -f $MBSYNCRC ]; then echo "necessary file .mbsyncrc not at $MBSYNCRC"; exit 1; fi

# is msmtp installed?
if ! [ -f $MSMTP ]; then echo "msmtp not installed at $MSMTP. Install package 'msmtp and msmtp-mta' to continue"; exit 1; fi

# does .msmtprc exist
if ! [ -f $MSMPTRC ]; then echo "necessary file .msmtprc not at $MBSYNCRC"; exit 1; fi

# if mail directories don't exist, create them
if ! [ -f $GMAIL ]; then echo "creating gmail directory" && mkdir -p $GMAIL; fi
if ! [ -f $CMAIL ]; then echo "creating cmail directory" && mkdir -p $CMAIL; fi

#  -------------------------- Initial Sync -------------------------

# sync
echo "syncing email... Note: You will be asked for your password"
$MBSYNC -aV

# init
echo "running mu init..."
$MU init --maildir="$HOME/.mail" --my-address=craigmartinjennings@gmail.com --my-address=c@cjennings.net

# index
echo "running mu index..."
$MU index

# report completion
echo "" && echo "Mu4e mail setup script completed." && echo "" && echo ""
