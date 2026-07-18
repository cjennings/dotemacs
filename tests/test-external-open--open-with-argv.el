;;; test-external-open--open-with-argv.el --- Tests for cj/--open-with-argv -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--open-with-argv, the pure builder that turns the
;; user-typed "open with" command plus a file path into an argv list.
;; The argv shape is the hardening: the file is one list element, so paths
;; with spaces or shell metacharacters never meet a shell.
;;
;; Test organization:
;; - Normal Cases: bare program, program with args, quoted argument
;; - Boundary Cases: file with spaces and metacharacters stays one element
;; - Error Cases: empty and whitespace-only commands
;;
;;; Code:

(require 'ert)
(require 'external-open)

;;; Normal Cases

(ert-deftest test-external-open--open-with-argv-normal-bare-program ()
  "Normal: a bare program name yields (PROGRAM FILE)."
  (should (equal (cj/--open-with-argv "vlc" "/tmp/foo.mp4")
                 '("vlc" "/tmp/foo.mp4"))))

(ert-deftest test-external-open--open-with-argv-normal-program-with-args ()
  "Normal: a command typed with arguments splits into argv words."
  (should (equal (cj/--open-with-argv "mpv --fs --loop" "/tmp/foo.mp4")
                 '("mpv" "--fs" "--loop" "/tmp/foo.mp4"))))

(ert-deftest test-external-open--open-with-argv-normal-quoted-arg-survives ()
  "Normal: a double-quoted argument stays one word."
  (should (equal (cj/--open-with-argv "player \"two words\"" "/tmp/foo.mp4")
                 '("player" "two words" "/tmp/foo.mp4"))))

;;; Boundary Cases

(ert-deftest test-external-open--open-with-argv-boundary-file-with-spaces ()
  "Boundary: a path with spaces is one argv element, untouched."
  (let ((file "/tmp/my file (draft).mp4"))
    (should (equal (car (last (cj/--open-with-argv "vlc" file))) file))))

(ert-deftest test-external-open--open-with-argv-boundary-file-with-metacharacters ()
  "Boundary: shell metacharacters in the path arrive verbatim."
  (let ((file "/tmp/a;b&c$(d)'e.mp4"))
    (should (equal (car (last (cj/--open-with-argv "vlc" file))) file))))

;;; Error Cases

(ert-deftest test-external-open--open-with-argv-error-empty-command ()
  "Error: an empty command signals user-error."
  (should-error (cj/--open-with-argv "" "/tmp/foo.mp4") :type 'user-error))

(ert-deftest test-external-open--open-with-argv-error-whitespace-command ()
  "Error: a whitespace-only command signals user-error."
  (should-error (cj/--open-with-argv "   " "/tmp/foo.mp4") :type 'user-error))

(provide 'test-external-open--open-with-argv)
;;; test-external-open--open-with-argv.el ends here
