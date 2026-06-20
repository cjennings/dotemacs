;;; test-dirvish-config-hard-delete-command.el --- Tests for cj/--dirvish-hard-delete-command -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--dirvish-hard-delete-command' is the pure string builder behind the
;; forced `sudo rm -rf' hard-delete bound to D in dirvish.  It shell-quotes
;; every path and guards the list with `--' so a leading-dash or space-bearing
;; filename can't be misread.  The interactive command (prompt + shell-command)
;; is verified live, not here.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dirvish-config)

(ert-deftest test-dirvish-config-hard-delete-command-multiple ()
  "Normal: two paths are quoted and joined behind `sudo rm -rf -- '."
  (should (equal (cj/--dirvish-hard-delete-command '("/tmp/a.txt" "/tmp/b.txt"))
                 "sudo rm -rf -- /tmp/a.txt /tmp/b.txt")))

(ert-deftest test-dirvish-config-hard-delete-command-single ()
  "Boundary: a single path still carries the `--' option terminator."
  (should (equal (cj/--dirvish-hard-delete-command '("/tmp/report.pdf"))
                 "sudo rm -rf -- /tmp/report.pdf")))

(ert-deftest test-dirvish-config-hard-delete-command-spaces-and-dash ()
  "Boundary: a path with spaces is shell-quoted, and `--' protects a
leading-dash filename from being read as an option."
  (let ((cmd (cj/--dirvish-hard-delete-command
              '("/tmp/my file.txt" "/tmp/-rf"))))
    ;; `--' precedes the paths so `-rf' is a target, not an option.
    (should (string-prefix-p "sudo rm -rf -- " cmd))
    ;; the space-bearing path is quoted (not a bare " " splitting the args).
    (should (string-match-p (regexp-quote (shell-quote-argument "/tmp/my file.txt"))
                            cmd))
    (should (string-match-p (regexp-quote (shell-quote-argument "/tmp/-rf"))
                            cmd))))

(ert-deftest test-dirvish-config-hard-delete-command-empty ()
  "Error: an empty list yields just the prefix (no targets) -- the
interactive command never reaches here, guarding `No file at point' first."
  (should (equal (cj/--dirvish-hard-delete-command '())
                 "sudo rm -rf -- ")))

(provide 'test-dirvish-config-hard-delete-command)
;;; test-dirvish-config-hard-delete-command.el ends here
