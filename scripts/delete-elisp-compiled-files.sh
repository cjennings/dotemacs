#!/bin/bash

location=$HOME/.emacs.d/

echo "Deleting emacs lisp compiled files (.eln and .elc) from $location..."
find $location -type f \( -name "*.eln" -o -name "*.elc" \) -exec rm -f {} +
echo "Done."
