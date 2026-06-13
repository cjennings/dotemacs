;;; test-dwim-shell-config-command-fixes.el --- zip/backup command builders -*- lexical-binding: t; -*-

;;; Commentary:
;; Two audit fixes, extracted into top-level command-string builders so they're
;; testable without loading the dwim-shell-command package (the command defuns
;; that call them live inside its use-package :config, which the batch test
;; harness doesn't instantiate):
;;  - cj/dwim-shell--zip-single-file-command names the archive <fne>.zip
;;  - cj/dwim-shell--dated-backup-command carries a real timestamp, not "$(date)"
;; The third fix (dired menu key M-S-d -> M-D) is a keybinding inside the same
;; :config block; it's verified in the live daemon, not here.

;;; Code:

(require 'ert)
(require 'dwim-shell-config)

(ert-deftest test-dwim-zip-single-file-command-names-archive-dot-zip ()
  "Normal: the single-file zip template names the archive <fne>.zip, with no
leftover <<e>> that would rebuild the input filename."
  (let ((cmd (cj/dwim-shell--zip-single-file-command)))
    (should (string-match-p "'<<fne>>\\.zip'" cmd))
    (should-not (string-match-p "<<e>>" cmd))))

(ert-deftest test-dwim-dated-backup-command-carries-real-timestamp ()
  "Normal: the dated-backup template interpolates a real YYYYMMDD_HHMMSS stamp,
so the substitution can't sit dead inside single quotes."
  (let ((cmd (cj/dwim-shell--dated-backup-command)))
    (should (string-match-p "\\.[0-9]\\{8\\}_[0-9]\\{6\\}\\.bak'" cmd))
    (should-not (string-match-p "\\$(date" cmd))))

(provide 'test-dwim-shell-config-command-fixes)
;;; test-dwim-shell-config-command-fixes.el ends here
