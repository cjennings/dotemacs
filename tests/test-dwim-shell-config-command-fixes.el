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

;;; ----------------------- tar-gzip command builder --------------------------

(ert-deftest test-dwim-tar-gzip-command-single-names-after-file ()
  "Normal: a single marked file names the archive <fne>.tar.gz over <<f>>."
  (let ((cmd (cj/dwim-shell--tar-gzip-command t)))
    (should (string-match-p "'<<fne>>\\.tar\\.gz'" cmd))
    (should (string-match-p "'<<f>>'" cmd))))

(ert-deftest test-dwim-tar-gzip-command-multi-uses-shared-archive ()
  "Boundary: multiple files tar into a shared archive.tar.gz over <<*>>."
  (let ((cmd (cj/dwim-shell--tar-gzip-command nil)))
    (should (string-match-p "archive\\.tar\\.gz" cmd))
    (should (string-match-p "'<<\\*>>'" cmd))))

;;; --------------------- text-to-speech command builder ----------------------

(ert-deftest test-dwim-text-to-speech-command-darwin-uses-say-voice ()
  "Normal: on darwin the command uses `say' with the chosen voice."
  (let ((cmd (cj/dwim-shell--text-to-speech-command 'darwin "Samantha")))
    (should (string-match-p "\\`say -v Samantha " cmd))
    (should (string-match-p "'<<fne>>\\.aiff'" cmd))))

(ert-deftest test-dwim-text-to-speech-command-linux-uses-espeak ()
  "Boundary: a non-darwin system uses `espeak' and ignores the voice."
  (let ((cmd (cj/dwim-shell--text-to-speech-command 'gnu/linux "ignored")))
    (should (string-match-p "\\`espeak " cmd))
    (should (string-match-p "'<<fne>>\\.wav'" cmd))
    (should-not (string-match-p "ignored" cmd))))

;;; ----------------------- video-trim command builder ------------------------

(ert-deftest test-dwim-video-trim-command-beginning-uses-ss ()
  "Normal: trimming the beginning emits a leading -ss with the start seconds."
  (let ((cmd (cj/dwim-shell--video-trim-command "Beginning" 7 0)))
    (should (string-match-p "-ss 7 " cmd))
    (should-not (string-match-p "-sseof" cmd))))

(ert-deftest test-dwim-video-trim-command-end-uses-sseof ()
  "Normal: trimming the end emits -sseof with the end seconds, no -ss."
  (let ((cmd (cj/dwim-shell--video-trim-command "End" 0 9)))
    (should (string-match-p "-sseof -9 " cmd))
    (should-not (string-match-p "-ss [0-9]" cmd))))

(ert-deftest test-dwim-video-trim-command-both-uses-ss-and-sseof ()
  "Normal: trimming both ends emits both -ss start and -sseof end."
  (let ((cmd (cj/dwim-shell--video-trim-command "Both" 3 4)))
    (should (string-match-p "-ss 3 " cmd))
    (should (string-match-p "-sseof -4 " cmd))))

(ert-deftest test-dwim-video-trim-command-negative-seconds-errors ()
  "Error: a negative second count for the used side signals a user-error."
  (should-error (cj/dwim-shell--video-trim-command "Beginning" -1 0) :type 'user-error)
  (should-error (cj/dwim-shell--video-trim-command "End" 0 -1) :type 'user-error)
  (should-error (cj/dwim-shell--video-trim-command "Both" 0 -2) :type 'user-error))

(provide 'test-dwim-shell-config-command-fixes)
;;; test-dwim-shell-config-command-fixes.el ends here
