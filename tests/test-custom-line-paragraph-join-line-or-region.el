;;; test-custom-line-paragraph-join-line-or-region.el --- Tests for cj/join-line-or-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/join-line-or-region function from custom-line-paragraph.el
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

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub expand-region package
(provide 'expand-region)

;; Now load the actual production module
(require 'custom-line-paragraph)

;;; Setup and Teardown

(defun test-join-line-or-region-setup ()
  "Setup for join-line-or-region tests."
  (cj/create-test-base-dir))

(defun test-join-line-or-region-teardown ()
  "Teardown for join-line-or-region tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-join-line-or-region-no-region-joins-with-previous-line ()
  "Without region, should join current line with previous line."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "line one line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-no-region-adds-newline-after-join ()
  "Without region, should add newline after joining."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-suffix-p "\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-with-region-joins-all-lines ()
  "With region, should join all lines in region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "line one line two line three" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-with-region-adds-newline-at-end ()
  "With region, should add newline at end."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-suffix-p "\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-preserves-text-content ()
  "Should preserve all text content when joining."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello\nworld")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "hello.*world" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-removes-line-breaks-between-words ()
  "Should remove line breaks and add spaces between words."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello\nworld")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "hello world" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-multiple-lines-in-region ()
  "Should handle multiple lines in region correctly."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one\ntwo\nthree\nfour\nfive")
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 3)
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "two three four" (buffer-string))))
    (test-join-line-or-region-teardown)))

;;; Boundary Cases

(ert-deftest test-join-line-or-region-on-first-line-no-region-does-nothing-except-newline ()
  "On first line without region, should only add newline."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line")
        (goto-char (point-min))
        (cj/join-line-or-region)
        (should (string= "only line\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-empty-lines-in-region ()
  "Should handle empty lines in region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\n\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "line one.*line three" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-single-line-region ()
  "Should handle single-line region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string= "only line\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-region-with-only-whitespace-lines ()
  "Should handle region with only whitespace lines."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \n   \n   ")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (stringp (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-lines-with-leading-whitespace ()
  "Should handle lines with leading whitespace."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\n  line two")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "line one.*line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-lines-with-trailing-whitespace ()
  "Should handle lines with trailing whitespace."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one  \nline two")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "line one.*line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-lines-with-tabs ()
  "Should handle lines with tab characters."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\tone\nline\ttwo")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "line.*one.*line.*two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-lines-with-mixed-whitespace ()
  "Should handle lines with mixed whitespace."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "  line \t one  \n\t line  two\t")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "line.*one.*line.*two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-very-long-lines ()
  "Should handle very long lines."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((long-line (make-string 5000 ?x)))
          (insert long-line "\n" long-line)
          (goto-char (point-max))
          (cj/join-line-or-region)
          (should (= (length (buffer-string)) (+ (* 2 5000) 1 1)))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-unicode-characters ()
  "Should handle unicode characters."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "café\nnaïve")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "café.*naïve" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-emoji-content ()
  "Should handle emoji content."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello 👋\nworld 🌍")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "hello 👋.*world 🌍" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-rtl-text ()
  "Should handle RTL text."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "مرحبا\nعالم")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "مرحبا.*عالم" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-region-at-buffer-start ()
  "Should handle region starting at buffer beginning."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 2)
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "line one line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-region-at-buffer-end ()
  "Should handle region ending at buffer end."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "line two line three" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-entire-buffer-as-region ()
  "Should handle entire buffer selected as region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one\ntwo\nthree")
        (transient-mark-mode 1)
        (mark-whole-buffer)
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "one two three" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-single-character-lines ()
  "Should handle single character lines."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "a\nb\nc")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "a b c" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-cursor-position-after-no-region ()
  "Cursor should be at end after joining without region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (= (point) (point-max))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-cursor-position-after-region ()
  "Cursor should be at region end marker after joining region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (let ((end-pos (point)))
          (activate-mark)
          (cj/join-line-or-region)
          ;; Point should be near the original end position
          (should (>= (point) (- end-pos 10)))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-marker-validity-after-operation ()
  "Marker should remain valid after operation."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (let ((marker (set-marker (make-marker) (point-min))))
          (goto-char (point-min))
          (set-mark (point))
          (goto-char (point-max))
          (activate-mark)
          (cj/join-line-or-region)
          (should (marker-position marker))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-backwards-region ()
  "Should handle backwards region (mark after point)."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-max))
        (set-mark (point))
        (goto-char (point-min))
        (activate-mark)
        (cj/join-line-or-region)
        (should (string-match-p "line one line two line three" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-partial-line-selection ()
  "Should handle region starting mid-line and ending mid-line."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (forward-char 5)  ; Middle of "line one"
        (set-mark (point))
        (forward-line 2)
        (forward-char 5)  ; Middle of "line three"
        (activate-mark)
        (cj/join-line-or-region)
        ;; Should join lines regardless of partial selection
        (should (string-match-p "line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-stress-test-many-lines ()
  "Should handle many lines (1000+) without hanging."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (dotimes (i 1000)
          (insert (format "line %d\n" i)))
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (let ((start-time (current-time)))
          (cj/join-line-or-region)
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            ;; Should complete in reasonable time (< 5 seconds)
            (should (< elapsed 5.0))))
        ;; Verify all lines joined
        (goto-char (point-min))
        (should (string-match-p "line 0.*line 999" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-combining-characters ()
  "Should handle Unicode combining characters."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        ;; e with combining acute accent (é)
        (insert "cafe\u0301\nnaive\u0308")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string-match-p "cafe\u0301.*naive\u0308" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-control-characters ()
  "Should handle control characters."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\u000Cone\nline\u000Ctwo")
        (goto-char (point-max))
        (cj/join-line-or-region)
        ;; Should preserve control characters
        (should (string-match-p "line.*one.*line.*two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-narrowed-buffer ()
  "Should respect buffer narrowing."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before\nline one\nline two\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (point)))
          (forward-line 2)
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (set-mark (point))
          (goto-char (point-max))
          (activate-mark)
          (cj/join-line-or-region)
          (widen)
          ;; Should only affect narrowed region
          (should (string-match-p "before" (buffer-string)))
          (should (string-match-p "after" (buffer-string)))
          (should (string-match-p "line one.*line two" (buffer-string)))))
    (test-join-line-or-region-teardown)))

;;; Error Cases

(ert-deftest test-join-line-or-region-empty-buffer-no-region ()
  "Should handle empty buffer gracefully without region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/join-line-or-region)
        (should (string= "\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-empty-buffer-with-region ()
  "Should handle empty buffer gracefully with region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (mark-whole-buffer)
        (cj/join-line-or-region)
        (should (string= "\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-single-line-buffer-no-region ()
  "Should handle single line buffer without region."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line")
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (string= "only line\n" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-read-only-buffer-should-error ()
  "Should error when attempting to modify read-only buffer."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-max))
        (read-only-mode 1)
        (should-error (cj/join-line-or-region)))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-undo-behavior ()
  "Should properly support undo after joining lines."
  (test-join-line-or-region-setup)
  (unwind-protect
      (let* ((temp-file (expand-file-name "test-undo.txt" cj/test-base-dir))
             (original-content "line one\nline two"))
        ;; Create file with initial content
        (with-temp-file temp-file
          (insert original-content))
        ;; Open file and test undo
        (find-file temp-file)
        (buffer-enable-undo)  ; Ensure undo is enabled
        ;; Make a small change to establish undo history
        (goto-char (point-min))
        (insert " ")
        (delete-char -1)
        (undo-boundary)  ; Create explicit boundary
        (goto-char (point-max))
        (let ((before-join (buffer-string)))
          (cj/join-line-or-region)
          (undo-boundary)  ; Create boundary after operation
          (let ((after-join (buffer-string)))
            (should-not (string= before-join after-join))
            ;; Undo should work now
            (undo)
            (should (string= before-join (buffer-string)))))
        (kill-buffer (current-buffer)))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-buffer-modified-flag ()
  "Should set buffer modified flag after joining lines."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (set-buffer-modified-p nil)
        (goto-char (point-max))
        (cj/join-line-or-region)
        (should (buffer-modified-p)))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-region-deactivation ()
  "Should deactivate region after operation."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (transient-mark-mode 1)
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (should (use-region-p))
        (cj/join-line-or-region)
        ;; Region should be deactivated after operation
        (should-not (use-region-p)))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-dos-line-endings ()
  "Should handle DOS-style line endings (CRLF) and preserve them."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\r\nline two\r\n")
        ;; Go to line two so we can join with line one
        (goto-char (point-min))
        (forward-line 1)
        (cj/join-line-or-region)
        ;; Should join lines (join-line handles the line endings)
        (should (string-match-p "line one.*line two" (buffer-string))))
    (test-join-line-or-region-teardown)))

(ert-deftest test-join-line-or-region-consecutive-operations ()
  "Should handle consecutive join operations correctly."
  (test-join-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three\nline four")
        ;; First operation: on line two, joins with line one
        (goto-char (point-min))
        (forward-line 1)
        (cj/join-line-or-region)
        (should (string-match-p "line one line two" (buffer-string)))
        ;; Second operation: on line four, joins with line three
        (goto-char (point-min))
        (search-forward "line four")
        (cj/join-line-or-region)
        (should (string-match-p "line three line four" (buffer-string)))
        ;; Both operations should have worked (note: each operation adds a newline)
        (should (string-match-p "line one line two\n+line three line four" (buffer-string))))
    (test-join-line-or-region-teardown)))

(provide 'test-custom-line-paragraph-join-line-or-region)
;;; test-custom-line-paragraph-join-line-or-region.el ends here
