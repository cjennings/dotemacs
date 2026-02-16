;;; test-config-utilities-format-build-time.el --- Tests for cj/emacs-build--format-build-time -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/emacs-build--format-build-time from config-utilities.el.
;;
;; Pure function that converts various time value representations to a
;; human-readable string. Branches on input type: nil, string, cons of
;; integers (Emacs time value), number (epoch seconds), and fallback.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

;;; Normal Cases

(ert-deftest test-config-utilities-format-build-time-normal-nil-returns-unknown ()
  "Nil input should return \"unknown\"."
  (should (equal (cj/emacs-build--format-build-time nil) "unknown")))

(ert-deftest test-config-utilities-format-build-time-normal-string-returned-as-is ()
  "String input should be returned unchanged."
  (should (equal (cj/emacs-build--format-build-time "2026-01-15 10:30:00 CST")
                 "2026-01-15 10:30:00 CST")))

(ert-deftest test-config-utilities-format-build-time-normal-cons-formats-timestamp ()
  "Cons of integers (Emacs time value) should produce formatted date string."
  (let* ((time-val (encode-time 0 30 14 15 2 2026))
         (result (cj/emacs-build--format-build-time time-val)))
    (should (string-match-p "2026-02-15" result))
    (should (string-match-p "14:30:00" result))))

(ert-deftest test-config-utilities-format-build-time-normal-number-formats-timestamp ()
  "Numeric epoch seconds should produce formatted date string."
  (let* ((time-val (float-time (encode-time 0 30 14 15 2 2026)))
         (result (cj/emacs-build--format-build-time time-val)))
    (should (string-match-p "2026-02-15" result))
    (should (string-match-p "14:30:00" result))))

(ert-deftest test-config-utilities-format-build-time-normal-fallback-formats-with-format ()
  "Unrecognized types should be stringified via `format'."
  (should (equal (cj/emacs-build--format-build-time 'some-symbol) "some-symbol")))

;;; Boundary Cases

(ert-deftest test-config-utilities-format-build-time-boundary-empty-string ()
  "Empty string input should be returned as empty string."
  (should (equal (cj/emacs-build--format-build-time "") "")))

(ert-deftest test-config-utilities-format-build-time-boundary-zero-epoch ()
  "Zero epoch (Unix epoch start) should produce a valid formatted date string."
  (let ((result (cj/emacs-build--format-build-time 0)))
    (should (stringp result))
    (should (string-match-p "19[67][0-9]" result))))

(ert-deftest test-config-utilities-format-build-time-boundary-integer-epoch ()
  "Integer (not float) epoch should be handled by numberp branch."
  (let* ((time-val (truncate (float-time (encode-time 0 0 12 1 1 2025))))
         (result (cj/emacs-build--format-build-time time-val)))
    (should (string-match-p "2025-01-01" result))
    (should (string-match-p "12:00:00" result))))

(ert-deftest test-config-utilities-format-build-time-boundary-cons-not-integers ()
  "Cons where car is not integer should hit the fallback branch."
  (let ((result (cj/emacs-build--format-build-time '("not" "integers"))))
    (should (equal result "(not integers)"))))

(ert-deftest test-config-utilities-format-build-time-boundary-vector-fallback ()
  "Vector input should hit the fallback branch."
  (should (equal (cj/emacs-build--format-build-time [1 2 3]) "[1 2 3]")))

(ert-deftest test-config-utilities-format-build-time-boundary-cons-and-number-agree ()
  "Cons time value and its float-time equivalent should produce the same output."
  (let* ((cons-time (encode-time 0 30 14 15 2 2026))
         (num-time (float-time cons-time))
         (result-cons (cj/emacs-build--format-build-time cons-time))
         (result-num (cj/emacs-build--format-build-time num-time)))
    (should (equal result-cons result-num))))

(provide 'test-config-utilities-format-build-time)
;;; test-config-utilities-format-build-time.el ends here
