;;; test-custom-line-paragraph-join-paragraph.el --- Tests for cj/join-paragraph -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/join-paragraph function from custom-line-paragraph.el
;;
;; IMPORTANT NOTE ON REGION ACTIVATION IN BATCH MODE:
;; When testing functions that use (use-region-p) in batch mode, you must
;; explicitly activate the region. Unlike interactive Emacs, batch mode does
;; not automatically activate regions when you set mark and point.
;;
;; To properly test region-based behavior in batch mode:
;; 1. Enable transient-mark-mode: (transient-mark-mode 1)
;; 2. Set mark and point as needed
;; 3. Explicitly activate the mark: (activate-mark)
;;
;; Without these steps, (use-region-p) will return nil even when mark and
;; point are set, causing the function to take the no-region code path.
;; This is a common pitfall that junior developers may miss when writing
;; ERT tests for region-aware commands.
;;
;; The cj/join-paragraph function uses er/mark-paragraph which sets a region,
;; so we need to ensure transient-mark-mode is enabled in our tests.

;;; Code:

;; Add tests directory to load path for testutil-general
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))

(require 'ert)
(require 'testutil-general)

;; Initialize package system to load expand-region
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Add expand-region to load path explicitly
(add-to-list 'load-path (expand-file-name "elpa/expand-region-1.0.0" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Load expand-region for real (needed by cj/join-paragraph)
(require 'expand-region)
(require 'the-org-mode-expansions)

;; Now load the actual production module
(require 'custom-line-paragraph)

;; -------------------------------- Test Fixtures ------------------------------

(defun test-join-paragraph-setup ()
  "Set up test environment."
  (cj/create-test-base-dir))

(defun test-join-paragraph-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;; ---------------------------- Normal Cases -----------------------------------

(ert-deftest test-join-paragraph-simple-multiline-cursor-at-start ()
  "Join a simple 3-line paragraph with cursor at start."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "line one line two line three\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-simple-multiline-cursor-in-middle ()
  "Join a simple 3-line paragraph with cursor in middle line."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (forward-line 1)
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "line one line two line three\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-simple-multiline-cursor-at-end ()
  "Join a simple 3-line paragraph with cursor at end."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "line one\nline two\nline three")
        (goto-char (point-max))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "line one line two line three\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-surrounded-by-other-paragraphs ()
  "Join only the current paragraph when surrounded by others."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "para one line one\npara one line two\n\n")
        (insert "para two line one\npara two line two\n\n")
        (insert "para three line one\npara three line two")
        ;; Position in middle paragraph
        (goto-char (point-min))
        (forward-line 3)
        (cj/join-paragraph)
        (should (string-match-p "para one line one\npara one line two"
                               (buffer-string)))
        (should (string-match-p "para two line one para two line two"
                               (buffer-string)))
        (should (string-match-p "para three line one\npara three line two"
                               (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-with-leading-whitespace ()
  "Join paragraph with indented lines, preserving appropriate spacing."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "  indented line one\n  indented line two\n  indented line three")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "  indented line one indented line two indented line three\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-cursor-position-after ()
  "Verify cursor moves forward one line after joining."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "line one\nline two\nline three\n")
        (goto-char (point-min))
        (let ((initial-line (line-number-at-pos)))
          (cj/join-paragraph)
          (should (= (line-number-at-pos) (1+ initial-line)))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-multiple-paragraphs-first ()
  "Join only first paragraph when three paragraphs exist."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "first one\nfirst two\n\nsecond one\nsecond two\n\nthird one\nthird two")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string-match-p "^first one first two\n" (buffer-string)))
        (should (string-match-p "second one\nsecond two" (buffer-string)))
        (should (string-match-p "third one\nthird two" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-multiple-paragraphs-middle ()
  "Join only middle paragraph when three paragraphs exist."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "first one\nfirst two\n\nsecond one\nsecond two\n\nthird one\nthird two")
        (goto-char (point-min))
        (forward-line 3)
        (cj/join-paragraph)
        (should (string-match-p "first one\nfirst two" (buffer-string)))
        (should (string-match-p "second one second two" (buffer-string)))
        (should (string-match-p "third one\nthird two" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-multiple-paragraphs-last ()
  "Join only last paragraph when three paragraphs exist."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "first one\nfirst two\n\nsecond one\nsecond two\n\nthird one\nthird two")
        (goto-char (point-max))
        (cj/join-paragraph)
        (should (string-match-p "first one\nfirst two" (buffer-string)))
        (should (string-match-p "second one\nsecond two" (buffer-string)))
        (should (string-match-p "third one third two" (buffer-string))))
    (test-join-paragraph-teardown)))

;; ---------------------------- Boundary Cases ---------------------------------

(ert-deftest test-join-paragraph-single-line-paragraph ()
  "Handle paragraph with only one line gracefully."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "single line paragraph")
        (goto-char (point-min))
        (cj/join-paragraph)
        ;; Should still work, even if nothing to join
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "single line paragraph\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-at-buffer-start ()
  "Join paragraph at very beginning of buffer."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "first line\nsecond line\nthird line\n\nother paragraph")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string-match-p "^first line second line third line\n" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-at-buffer-end ()
  "Join paragraph at very end of buffer."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "other paragraph\n\nfirst line\nsecond line\nthird line")
        (goto-char (point-max))
        (cj/join-paragraph)
        (should (string-match-p "first line second line third line\n$" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-very-long ()
  "Join paragraph with many lines (20+ lines)."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (dotimes (i 25)
          (insert (format "line %d\n" (1+ i))))
        (goto-char (point-min))
        (cj/join-paragraph)
        ;; Should have all 25 "line X" strings joined with spaces
        (should (string-match-p "line 1 line 2 line 3.*line 24 line 25" (buffer-string)))
        ;; Should not have multiple newlines in sequence
        (should-not (string-match-p "\n.*\n.*\n" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-with-blank-lines-within ()
  "Test behavior when expand-region might see internal structure."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        ;; This tests how er/mark-paragraph handles the content
        (insert "line one\n\nline two\n\nother para")
        (goto-char (point-min))
        (cj/join-paragraph)
        ;; er/mark-paragraph should mark just the first line in this case
        (should (string-match-p "^line one\n" (buffer-string))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-cursor-on-blank-line ()
  "Handle cursor positioned on blank line between paragraphs."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "para one\npara one line two\n\npara two\npara two line two")
        (goto-char (point-min))
        (forward-line 2)  ;; Position on blank line
        (cj/join-paragraph)
        ;; Behavior depends on how er/mark-paragraph handles blank lines
        ;; At minimum, should not error
        (should (bufferp (current-buffer))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-no-trailing-newline ()
  "Handle paragraph at end of buffer with no trailing newline."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "line one line two line three\n")))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-only-whitespace-lines ()
  "Handle paragraph where lines contain only spaces/tabs."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "   \n\t\t\n  \t  ")
        (goto-char (point-min))
        (cj/join-paragraph)
        ;; Should handle without error
        (should (bufferp (current-buffer))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-unicode-content ()
  "Handle paragraph with emoji and special Unicode characters."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "Hello 👋 world\nこんにちは 世界\n🎉 celebration 🎊")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "Hello 👋 world こんにちは 世界 🎉 celebration 🎊\n")))
    (test-join-paragraph-teardown)))

;; ---------------------------- Error Cases ------------------------------------

(ert-deftest test-join-paragraph-empty-buffer ()
  "Handle empty buffer without error."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        ;; Empty buffer - should handle gracefully without error
        (cj/join-paragraph)
        (should (bufferp (current-buffer))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-buffer-only-whitespace ()
  "Handle buffer containing only whitespace."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "   \n\n\t\t\n  ")
        (goto-char (point-min))
        (cj/join-paragraph)
        ;; Should handle without error
        (should (bufferp (current-buffer))))
    (test-join-paragraph-teardown)))

(ert-deftest test-join-paragraph-buffer-single-character ()
  "Handle buffer with minimal content."
  (test-join-paragraph-setup)
  (unwind-protect
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "x")
        (goto-char (point-min))
        (cj/join-paragraph)
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        "x\n")))
    (test-join-paragraph-teardown)))

(provide 'test-custom-line-paragraph-join-paragraph)
;;; test-custom-line-paragraph-join-paragraph.el ends here
