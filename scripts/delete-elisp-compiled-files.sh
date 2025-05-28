#!/bin/bash

location=$HOME/.emacs.d/

echo ""; echo "You are about to delete emacs lisp compiled files (.eln and .elc) recursively from $location";

# Show the files it will delete
echo "The following files will be deleted:"
find $location -type f \( -name "*.eln" -o -name "*.elc" \) -print


echo ""; echo ""
read -p "Are you sure you want to continue? (y/n) " -n 1 -r
echo    # move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo "Deleting files..."
    find $location -type f \( -name "*.eln" -o -name "*.elc" \) -exec rm -f {} +
    echo "Files deleted."
else
    echo "Operation cancelled."
fi

echo ""; echo ""
