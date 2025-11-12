;;; test-transcription-counter.el --- Tests for active transcription counting -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--count-active-transcriptions and modeline integration
;; Categories: Normal cases, Boundary cases

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'transcription-config)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-cj/--count-active-transcriptions-empty ()
  "Test count when no transcriptions are active."
  (let ((cj/transcriptions-list '()))
    (should (= 0 (cj/--count-active-transcriptions)))))

(ert-deftest test-cj/--count-active-transcriptions-one-running ()
  "Test count with one running transcription."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running))))
    (should (= 1 (cj/--count-active-transcriptions)))))

(ert-deftest test-cj/--count-active-transcriptions-multiple-running ()
  "Test count with multiple running transcriptions."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running)
           (proc2 "file2.m4a" nil running)
           (proc3 "file3.m4a" nil running))))
    (should (= 3 (cj/--count-active-transcriptions)))))

(ert-deftest test-cj/--count-active-transcriptions-mixed-status ()
  "Test count excludes completed/errored transcriptions."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running)
           (proc2 "file2.m4a" nil complete)
           (proc3 "file3.m4a" nil running)
           (proc4 "file4.m4a" nil error))))
    (should (= 2 (cj/--count-active-transcriptions)))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-cj/--count-active-transcriptions-only-complete ()
  "Test count when all transcriptions are complete."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil complete)
           (proc2 "file2.m4a" nil complete))))
    (should (= 0 (cj/--count-active-transcriptions)))))

(ert-deftest test-cj/--count-active-transcriptions-only-error ()
  "Test count when all transcriptions errored."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil error)
           (proc2 "file2.m4a" nil error))))
    (should (= 0 (cj/--count-active-transcriptions)))))

;; ----------------------------- Modeline Tests --------------------------------

(ert-deftest test-cj/--transcription-modeline-string-none-active ()
  "Test modeline string when no transcriptions active."
  (let ((cj/transcriptions-list '()))
    (should-not (cj/--transcription-modeline-string))))

(ert-deftest test-cj/--transcription-modeline-string-one-active ()
  "Test modeline string with one active transcription."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running))))
    (let ((result (cj/--transcription-modeline-string)))
      (should result)
      (should (string-match-p "⏺1" result)))))

(ert-deftest test-cj/--transcription-modeline-string-multiple-active ()
  "Test modeline string with multiple active transcriptions."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running)
           (proc2 "file2.m4a" nil running)
           (proc3 "file3.m4a" nil running))))
    (let ((result (cj/--transcription-modeline-string)))
      (should result)
      (should (string-match-p "⏺3" result)))))

(ert-deftest test-cj/--transcription-modeline-string-has-help-echo ()
  "Test that modeline string has help-echo property."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running))))
    (let ((result (cj/--transcription-modeline-string)))
      (should (get-text-property 0 'help-echo result)))))

(ert-deftest test-cj/--transcription-modeline-string-has-face ()
  "Test that modeline string has warning face."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running))))
    (let ((result (cj/--transcription-modeline-string)))
      (should (eq 'warning (get-text-property 0 'face result))))))

(provide 'test-transcription-counter)
;;; test-transcription-counter.el ends here
