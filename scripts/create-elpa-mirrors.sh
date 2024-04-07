#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>

# Clones the Elpa mirrors repository to a local directory

ELPA_MIRRORS_REPO="https://github.com/d12frosted/elpa-mirror.git"
ELPA_MIRRORS_DIR=".elpa-mirrors"


# Identify EMACS_CONFIG location
if [ $# -eq 1 ] && [ -d "$1" ]; then
    EMACS_CONFIG="$1"
elif [ -d "$HOME/.emacs.d" ]; then
    EMACS_CONFIG="$HOME/.emacs.d"
elif [ -d "$HOME/.config/emacs" ]; then
    EMACS_CONFIG="$HOME/.config/emacs"
else
    echo
    "Unable to locate Emacs configuration directory. Please check that your Emacs
    configuration is in ~/.emacs.d/ or ~/.config/emacs/. Alternatively, you can
    specify a different directory by passing the directory path as an argument."
    exit 1
fi

# Check if git is installed
if ! command -v git &> /dev/null; then
    echo "git was not found. Please install git first."
    exit 1
fi

# Check if the .elpa-mirrors already exists
if [ -d "$EMACS_CONFIG/$ELPA_MIRRORS_DIR" ]; then
    echo "The directory $EMACS_CONFIG/$ELPA_MIRRORS_DIR already exists. Please remove or rename this directory and run the script again."
    exit 1
fi

# Confirm directory selection
read -p "The following directory has been selected for cloning ELPA mirror repo: $EMACS_CONFIG. Continue? [y/N] " REPLY
if ! [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Operation cancelled by user."
    exit 0
fi

# Proceed with cloning
git clone --depth 1 "$ELPA_MIRRORS_REPO" "$EMACS_CONFIG/$ELPA_MIRRORS_DIR"

# Display completion notification with location of mirrors.
printf "\n\nCompleted. Elpa mirrors cloned to %s\n\n" "$EMACS_CONFIG/$ELPA_MIRRORS_DIR"
