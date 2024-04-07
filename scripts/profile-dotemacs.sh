#!/usr/bin/env bash

# profile-dotemacs.sh
# Craig Jennings <c@cjennings.net>
# a convenience script to load an emacs-lisp file which will
# startup emacs (with or without an early-init) and provide
# benchmark information on the Emacs config.

EMACS_HOME="$HOME/.emacs.d/"
EARLY_INIT_FILE="$EMACS_HOME/early-init.el"
PROFILE_FILE="$EMACS_HOME/custom/profile-dotemacs.el"

if [ -f "$EARLY_INIT_FILE" ]
then
    emacs -Q --load $PROFILE_FILE --eval "(progn (load-file \"~/.emacs.d/early-init.el\") (profile-dotemacs))"
else
    echo "No early init found. Proceeding to benchmark init.el."
    emacs -Q --load $PROFILE_FILE --eval "(profile-dotemacs)"
fi

