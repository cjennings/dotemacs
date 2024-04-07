#!/bin/sh
# script for  Emacs config testing
# - clears out all but necessary init/config files
# - removes native ad bytecode files.
rm -rf ~/.emacs.d/.cache/
rm -rf ~/.emacs.d/auto-save-list/
rm -rf ~/.emacs.d/backups/
rm -rf ~/.emacs.d/crossword/
rm -rf ~/.emacs.d/dirvish/
rm -rf ~/.emacs.d/eln-cache/
rm -rf ~/.emacs.d/elpa/
rm -rf ~/.emacs.d/emojis/
rm -rf ~/.emacs.d/erc/
rm -rf ~/.emacs.d/eshell/
rm -rf ~/.emacs.d/localrepo/
rm -rf ~/.emacs.d/nov-places
rm -rf ~/.emacs.d/nov-places/
rm -rf ~/.emacs.d/quelpa/
rm -rf ~/.emacs.d/transient/
rm -rf ~/.emacs.d/tree-sitter/
rm -rf ~/.emacs.d/url/
rm ~/.emacs.d/.lsp-session*
rm ~/.emacs.d/.org-id-locations
rm ~/.emacs.d/.pdf-view-restore
rm ~/.emacs.d/org-roam.db
rm ~/.emacs.d/projectile-bookmarks.eld
rm ~/.emacs.d/projects
rm ~/.emacs.d/recentf
rm ~/.emacs.d/tramp-connection-history
rm ~/sync/org/emacs-theme.persist
find ~/.emacs.d -name "*.eln" -type f -delete
find ~/.emacs.d -name "*.elc" -type f -delete
