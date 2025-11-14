;;; test-custom-line-paragraph-duplicate-line-or-region.el --- Tests for cj/duplicate-line-or-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/duplicate-line-or-region function from custom-line-paragraph.el
;;
;; This function duplicates the current line or active region below the original.
;; When called with a prefix argument, the duplicated text is commented out.
;;
;; IMPORTANT NOTE ON REGION ACTIVATION IN BATCH MODE:
;; When testing functions that use (region-active-p) in batch mode, you must
;; explicitly activate the region. Unlike interactive Emacs, batch mode does
;; not automatically activate regions when you set mark and point.
;;
;; To properly test region-based behavior in batch mode:
;; 1. Enable transient-mark-mode: (transient-mark-mode 1)
;; 2. Set mark and point as needed
;; 3. Explicitly activate the mark: (activate-mark)

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

(defun test-duplicate-line-or-region-setup ()
  "Setup for duplicate-line-or-region tests."
  (cj/create-test-base-dir))

(defun test-duplicate-line-or-region-teardown ()
  "Teardown for duplicate-line-or-region tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-duplicate-line-or-region-single-line-without-comment ()
  "Should duplicate single line below original without commenting."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "line one\nline one" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-single-line-with-comment ()
  "Should duplicate single line and comment the duplicate."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (emacs-lisp-mode)  ; Enable comment syntax
        (insert "line one")
        (goto-char (point-min))
        (cj/duplicate-line-or-region t)  ; Pass comment argument
        (should (string-match-p "line one\n;; line one" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-multi-line-region-without-comment ()
  "Should duplicate entire region below without commenting."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (transient-mark-mode 1)
        (activate-mark)
        (cj/duplicate-line-or-region)
        (should (string-match-p "line one\nline two\nline three\nline one\nline two\nline three" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-multi-line-region-with-comment ()
  "Should duplicate region and comment all duplicated lines."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (transient-mark-mode 1)
        (activate-mark)
        (cj/duplicate-line-or-region t)
        ;; All duplicated lines should be commented
        (should (string-match-p ";; line one\n;; line two\n;; line three" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-cursor-position-unchanged ()
  "Should keep cursor at original position (save-excursion)."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-min))
        (forward-char 5)  ; Position in middle of first line
        (let ((original-pos (point)))
          (cj/duplicate-line-or-region)
          (should (= (point) original-pos))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-original-content-preserved ()
  "Should preserve original text unchanged."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "original line")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        ;; Original should still be there
        (goto-char (point-min))
        (should (looking-at "original line")))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-preserves-text-content ()
  "Should exactly duplicate text content."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "exact text")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        ;; Count occurrences of "exact text"
        (should (= 2 (how-many "exact text" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

;;; Boundary Cases

(ert-deftest test-duplicate-line-or-region-at-buffer-start ()
  "Should handle duplication from beginning of buffer."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first line\nsecond line")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "first line\nfirst line\nsecond line" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-at-buffer-end ()
  "Should handle duplication at end of buffer."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first line\nlast line")
        (goto-char (point-max))
        (cj/duplicate-line-or-region)
        (should (string-match-p "last line\nlast line$" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-empty-line ()
  "Should duplicate empty line."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "")
        (cj/duplicate-line-or-region)
        ;; Should have duplicated the empty content
        (should (string= "\n" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-only-whitespace ()
  "Should preserve whitespace in duplicate."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   ")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string= "   \n   " (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-very-long-line ()
  "Should handle very long lines (5000+ chars)."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((long-line (make-string 5000 ?x)))
          (insert long-line)
          (goto-char (point-min))
          (cj/duplicate-line-or-region)
          (should (= 2 (how-many long-line (point-min) (point-max))))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-region-with-empty-lines ()
  "Should duplicate empty lines within region."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\n\nline three")
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (transient-mark-mode 1)
        (activate-mark)
        (cj/duplicate-line-or-region)
        ;; Should have empty line duplicated
        (should (string-match-p "line one\n\nline three\nline one\n\nline three" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-single-character ()
  "Should handle minimal single character content."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "x")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string= "x\nx" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-unicode-emoji ()
  "Should handle Unicode and emoji characters."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello 👋 café")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "hello 👋 café\nhello 👋 café" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-with-tabs ()
  "Should preserve tab characters."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\twith\ttabs")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "line\twith\ttabs\nline\twith\ttabs" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-mixed-whitespace ()
  "Should preserve exact whitespace."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "  line \t text  ")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string= "  line \t text  \n  line \t text  " (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-narrowed-buffer ()
  "Should respect buffer narrowing."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before\ntarget\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (point)))
          (forward-line 1)
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (cj/duplicate-line-or-region)
          (widen)
          ;; Should still have before and after
          (should (string-match-p "before" (buffer-string)))
          (should (string-match-p "after" (buffer-string)))
          ;; Target should be duplicated
          (should (= 2 (how-many "target" (point-min) (point-max))))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-backwards-region ()
  "Should handle backwards region (mark after point)."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two")
        (goto-char (point-max))
        (set-mark (point))
        (goto-char (point-min))
        (transient-mark-mode 1)
        (activate-mark)
        (cj/duplicate-line-or-region)
        (should (= 2 (how-many "line one" (point-min) (point-max))))
        (should (= 2 (how-many "line two" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-entire-buffer ()
  "Should handle entire buffer selected as region."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one\ntwo\nthree")
        (transient-mark-mode 1)
        (mark-whole-buffer)
        (activate-mark)
        (cj/duplicate-line-or-region)
        (should (= 2 (how-many "one" (point-min) (point-max))))
        (should (= 2 (how-many "two" (point-min) (point-max))))
        (should (= 2 (how-many "three" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-ending-mid-line ()
  "Should handle region ending mid-line."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (goto-char (point-min))
        (forward-char 5)  ; Middle of first line
        (set-mark (point))
        (forward-line 2)
        (forward-char 5)  ; Middle of third line
        (transient-mark-mode 1)
        (activate-mark)
        (cj/duplicate-line-or-region)
        ;; Should duplicate the selected portion
        (should (> (length (buffer-string)) (length "line one\nline two\nline three"))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-trailing-whitespace ()
  "Should preserve trailing whitespace."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line with trailing   ")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string= "line with trailing   \nline with trailing   " (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-rtl-text ()
  "Should handle RTL text."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "مرحبا")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (= 2 (how-many "مرحبا" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-combining-characters ()
  "Should handle Unicode combining characters."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "cafe\u0301")  ; e with combining acute
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "cafe\u0301\ncafe\u0301" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-at-point-min ()
  "Should handle duplication at point-min edge case."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (= 2 (how-many "first" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-at-point-max ()
  "Should handle duplication at point-max edge case."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "last")
        (goto-char (point-max))
        (cj/duplicate-line-or-region)
        (should (= 2 (how-many "last" (point-min) (point-max)))))
    (test-duplicate-line-or-region-teardown)))

;;; Error Cases

(ert-deftest test-duplicate-line-or-region-read-only-buffer ()
  "Should error when attempting to modify read-only buffer."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "read only line")
        (goto-char (point-min))
        (read-only-mode 1)
        (should-error (cj/duplicate-line-or-region)))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-buffer-modified-flag ()
  "Should set buffer modified flag."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line")
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (buffer-modified-p)))
    (test-duplicate-line-or-region-teardown)))

(ert-deftest test-duplicate-line-or-region-undo-behavior ()
  "Should support undo after duplication."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (let* ((temp-file (expand-file-name "test-undo-dup.txt" cj/test-base-dir))
             (original-content "line one"))
        ;; Create file with initial content
        (with-temp-file temp-file
          (insert original-content))
        ;; Open file and test undo
        (find-file temp-file)
        (buffer-enable-undo)
        ;; Establish undo history
        (goto-char (point-min))
        (insert " ")
        (delete-char -1)
        (undo-boundary)
        (goto-char (point-min))
        (let ((before-dup (buffer-string)))
          (cj/duplicate-line-or-region)
          (undo-boundary)
          (let ((after-dup (buffer-string)))
            (should-not (string= before-dup after-dup))
            (undo)
            (should (string= before-dup (buffer-string)))))
        (kill-buffer (current-buffer)))
    (test-duplicate-line-or-region-teardown)))


(ert-deftest test-duplicate-line-or-region-special-characters ()
  "Should handle control characters."
  (test-duplicate-line-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\u000Cwith\u000Dcontrol")
        (goto-char (point-min))
        (cj/duplicate-line-or-region)
        (should (string-match-p "line\u000Cwith\u000Dcontrol\nline\u000Cwith\u000Dcontrol" (buffer-string))))
    (test-duplicate-line-or-region-teardown)))

(provide 'test-custom-line-paragraph-duplicate-line-or-region)
;;; test-custom-line-paragraph-duplicate-line-or-region.el ends here
