;;; test-dirvish-config-quick-access.el --- Regression for quick-access key shape -*- lexical-binding: t; -*-

;;; Commentary:
;; `dirvish-quick-access' uses transient under the hood.  Entries are a list
;; of (KEY PATH DOC) cells, and transient auto-groups multi-character keys
;; into nested prefix keymaps.  When one entry's KEY is also the prefix of a
;; longer entry's KEY (e.g. "pw" as a leaf and "pwk" as a deeper key), the
;; transient build falls over: the shorter-key slot resolves to the auto-
;; generated sub-prefix keymap rather than to the intended leaf command.
;; Pressing the dirvish `g' binding then raises:
;;
;;   Wrong type argument: command, (keymap (107 . transient:...::N)), command
;;
;; That happened on 2026-05-10 after the `pcr' -> `pwk' rename in commit
;; 4ece1eb made `pw' (wallpaper, leaf) collide with `pwk' (project work).
;; This test guards against any future occurrence by walking the entries
;; list and asserting that no key is a prefix of any other key.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(defun test-dirvish-config--key-collisions (entries)
  "Return a list of (SHORTER . LONGER) pairs where SHORTER is a prefix of LONGER.
ENTRIES is the same shape as `dirvish-quick-access-entries' -- a list of
(KEY PATH DOC) cells."
  (let ((keys (mapcar #'car entries))
        collisions)
    (dolist (a keys)
      (dolist (b keys)
        (when (and (not (string= a b))
                   (string-prefix-p a b))
          (push (cons a b) collisions))))
    collisions))

(ert-deftest test-dirvish-config--key-collisions-detects-prefix-of-leaf ()
  "Normal: a leaf KEY that prefixes a longer KEY is reported as a collision."
  (let ((entries '(("pw" "/wallpaper" "wallpaper")
                   ("pwk" "/work" "project work")
                   ("h" "~/" "home"))))
    (should (member '("pw" . "pwk")
                    (test-dirvish-config--key-collisions entries)))))

(ert-deftest test-dirvish-config--key-collisions-clean-list-empty ()
  "Boundary: a list with no overlapping prefixes returns no collisions."
  (let ((entries '(("h"  "~/"   "home")
                   ("dx" "/dx"  "documents")
                   ("dl" "/dl"  "downloads")
                   ("pwk" "/wk" "project work")
                   ("wp" "/wp"  "wallpaper"))))
    (should-not (test-dirvish-config--key-collisions entries))))

(ert-deftest test-dirvish-config--key-collisions-empty-list-empty ()
  "Boundary: an empty entries list has no collisions."
  (should-not (test-dirvish-config--key-collisions '())))

(ert-deftest test-dirvish-config--key-collisions-single-entry-empty ()
  "Boundary: a single-entry list has no collisions."
  (should-not (test-dirvish-config--key-collisions
               '(("h" "~/" "home")))))

(ert-deftest test-dirvish-config-quick-access-entries-have-no-collisions ()
  "Regression: the live `dirvish-quick-access-entries' list contains no key
that is a prefix of another key.  See commentary for why this matters."
  (should-not (test-dirvish-config--key-collisions
               dirvish-quick-access-entries)))

(provide 'test-dirvish-config-quick-access)
;;; test-dirvish-config-quick-access.el ends here
