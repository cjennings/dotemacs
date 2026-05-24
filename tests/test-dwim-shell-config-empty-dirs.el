;;; test-dwim-shell-config-empty-dirs.el --- Tests for empty-dirs command builder -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers cj/dwim-shell--empty-dirs-command, which builds the find command for
;; cj/dwim-shell-commands-remove-empty-directories.  The command is scoped to an
;; explicit root (shell-quoted) rather than the ambient current directory.

;;; Code:

(require 'ert)
(require 'dwim-shell-config)

(ert-deftest test-dwim-empty-dirs-command-shape ()
  "Normal: builds a find ... -type d -empty -delete command for the root."
  (let ((root "/home/me/proj"))
    (should (equal (cj/dwim-shell--empty-dirs-command root)
                   (format "find %s -type d -empty -delete  # <<*>>"
                           (shell-quote-argument root))))))

(ert-deftest test-dwim-empty-dirs-command-quotes-spaces ()
  "Boundary: a root with spaces is shell-quoted, not left to word-split."
  (let ((cmd (cj/dwim-shell--empty-dirs-command "/home/me/my dir")))
    (should (string-match-p (regexp-quote (shell-quote-argument "/home/me/my dir"))
                            cmd))
    (should-not (string-match-p "find /home/me/my dir " cmd))))

(provide 'test-dwim-shell-config-empty-dirs)
;;; test-dwim-shell-config-empty-dirs.el ends here
