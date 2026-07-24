;;; help-utils.el --- Help Integrations and Searches -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/D.
;; Load shape: eager.
;; Eager reason: documentation-search commands; eager only by init order, a
;;   deferral candidate (autoload commands) for Phase 4.
;; Top-level side effects: one global key, package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; This module provides various utilities for accessing documentation and help resources
;; directly within Emacs.
;; DevDocs for programming documentation
;; TLDR command line cheat sheets
;; Wikipedia pages through the `wiki-summary` package.
;; ArchWiki pages are also browsable via EWW.
;;
;;; Keybindings:
;; - C-h D s: Search DevDocs for documentation.
;; - C-h D b: Peruse DevDocs browsing through documentation.
;; - C-h D l: Lookup specific documentation in DevDocs.
;; - C-h D i: Install documentation for a chosen library in DevDocs.
;; - C-h D d: Delete documentation from DevDocs.
;; - C-h D u: Update all installed DevDocs documentation.
;; - C-h T: Access TLDR (cheat sheets for command-line tools).
;; - C-h W: Summarize wiki pages with the wiki-summary package.
;; - C-h A: Search and browse local Arch Wiki topics in EWW.
;;
;;; Code:

;; Lazily-loaded functions referenced below.
(declare-function devdocs-go-back "devdocs")
(declare-function devdocs-go-forward "devdocs")

;; ---------------------------------- Devdocs ----------------------------------

(use-package devdocs
  :commands (devdocs-search devdocs-peruse devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :init
  (keymap-global-set "C-h D s" #'devdocs-search)
  (keymap-global-set "C-h D b" #'devdocs-peruse)
  (keymap-global-set "C-h D l" #'devdocs-lookup)
  (keymap-global-set "C-h D i" #'devdocs-install)
  (keymap-global-set "C-h D d" #'devdocs-delete)
  (keymap-global-set "C-h D u" #'devdocs-update-all)
  :config
  (define-key devdocs-mode-map "b" #'devdocs-go-back)
  (define-key devdocs-mode-map "f" #'devdocs-go-forward))

;; ------------------------------------ TLDR -----------------------------------

(use-package tldr
  :bind ("C-h T" . tldr))

;; -------------------------------- Wiki Summary -------------------------------

(use-package wiki-summary
  :bind ("C-h W" . wiki-summary))

;; --------------------------- Browse Local Arch Wiki --------------------------
;; on Arch: yay (or whatever your AUR package manager is) -S  arch-wiki-docs
;; browse the arch wiki topics offline

(defvar cj/arch-wiki-html-dir "/usr/share/doc/arch-wiki/html/en"
  "Directory holding the offline ArchWiki HTML copies.
Populated by the arch-wiki-docs package on Arch systems.")

(defun cj/--arch-wiki-topics (dir)
  "Return an alist of (BASENAME . FULLPATH) for ArchWiki topics under DIR.

Returns nil when DIR does not exist rather than signaling.  This is the
whole point of the helper: `directory-files' raises file-missing on an
absent directory, and the caller's \"is arch-wiki-docs installed?\" hint
sat below that call, so on the one machine state the hint was written for
it could never be reached."
  (when (file-directory-p dir)
    (mapcar (lambda (f) (cons (file-name-base f) f))
            (directory-files dir t "\\.html\\'"))))

(defun cj/local-arch-wiki-search ()
  "Prompt for an ArchWiki topic and open its local HTML copy in EWW.

Looks for “*.html” files under `cj/arch-wiki-html-dir', lets you complete
on their basenames, and displays the chosen file with `eww-browse-url'.
If the directory is missing or empty, reminds you to install
arch-wiki-docs."
  (interactive)
  (let ((topics (cj/--arch-wiki-topics cj/arch-wiki-html-dir)))
    (if (null topics)
        (message "No ArchWiki topics in %s. Is arch-wiki-docs installed?"
                 cj/arch-wiki-html-dir)
      (let* ((chosen (completing-read "Choose an ArchWiki Topic: "
                                      (mapcar #'car topics)))
             (fullname (cdr (assoc chosen topics))))
        (if fullname
            (eww-browse-url (concat "file://" fullname))
          (message "No ArchWiki topic named %s" chosen))))))
(keymap-global-set "C-h A" #'cj/local-arch-wiki-search)

(provide 'help-utils)
;;; help-utils.el ends here
