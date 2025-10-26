;;; test-custom-line-paragraph-remove-duplicate-lines-region-or-buffer.el --- Tests for cj/remove-duplicate-lines-region-or-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/remove-duplicate-lines-region-or-buffer function from custom-line-paragraph.el
;;
;; This function removes duplicate lines in the region or buffer, keeping the first occurrence.
;; Operates on the active region when one exists; otherwise operates on the whole buffer.
;;
;; The implementation uses a regex to find duplicate lines: "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n"
;; This pattern matches a line, then any number of lines in between, then the same line again.
;;
;; IMPORTANT NOTE ON REGION ACTIVATION IN BATCH MODE:
;; When testing functions that use (use-region-p) in batch mode, you must
;; explicitly activate the region. Unlike interactive Emacs, batch mode does
;; not automatically activate regions when you set mark and point.

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

(defun test-remove-duplicate-lines-setup ()
  "Setup for remove-duplicate-lines tests."
  (cj/create-test-base-dir))

(defun test-remove-duplicate-lines-teardown ()
  "Teardown for remove-duplicate-lines tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-remove-duplicate-lines-adjacent-duplicates ()
  "Should remove adjacent duplicate lines."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline one\nline two")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "line one\nline two" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-keep-first-occurrence ()
  "Should keep first occurrence and remove subsequent duplicates."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first\nsecond\nfirst\nthird")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "first\nsecond\nthird" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-multiple-sets ()
  "Should remove multiple different duplicated lines."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "alpha\nbeta\nalpha\ngamma\nbeta\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "alpha\nbeta\ngamma\n" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-no-duplicates ()
  "Should leave buffer unchanged when no duplicates exist."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (let ((original (buffer-string)))
          (cj/remove-duplicate-lines-region-or-buffer)
          (should (string= original (buffer-string)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-region-only ()
  "Should only affect active region, leaving rest of buffer unchanged."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before\ndup\ndup\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 2)
        (transient-mark-mode 1)
        (activate-mark)
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; Should have removed one "dup" but kept before and after
        (should (string-match-p "before" (buffer-string)))
        (should (string-match-p "after" (buffer-string)))
        (should (= 1 (how-many "^dup$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-whole-buffer ()
  "Should operate on entire buffer when no region active."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one\ntwo\none\nthree\ntwo\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "one\ntwo\nthree\n" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-preserve-unique ()
  "Should preserve all unique lines intact."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "unique1\ndup\nunique2\ndup\nunique3")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string-match-p "unique1" (buffer-string)))
        (should (string-match-p "unique2" (buffer-string)))
        (should (string-match-p "unique3" (buffer-string)))
        (should (= 1 (how-many "^dup$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-separated-by-content ()
  "Should remove duplicate lines even when separated by other content."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "target\nother1\nother2\ntarget\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "^target$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

;;; Boundary Cases

(ert-deftest test-remove-duplicate-lines-empty-buffer ()
  "Should handle empty buffer gracefully."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-single-line ()
  "Should handle single line buffer (no duplicates possible)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "only line" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-two-identical-lines ()
  "Should handle minimal duplicate case of two identical lines."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "same\nsame\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "same\n" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-all-identical ()
  "Should keep only one line when all lines are identical."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "same\nsame\nsame\nsame\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "same\n" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-at-buffer-start ()
  "Should handle duplicates at beginning of buffer."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first\nfirst\nsecond\nthird")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "first\nsecond\nthird" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-at-buffer-end ()
  "Should handle duplicates at end of buffer."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "first\nsecond\nlast\nlast\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "first\nsecond\nlast\n" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-empty-lines ()
  "Should handle duplicate empty lines."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line1\n\n\nline2")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "line1\n\nline2" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-whitespace-only ()
  "Should handle duplicate whitespace-only lines."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \n   \ntext")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (string= "   \ntext" (buffer-string))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-very-long ()
  "Should handle very long duplicate lines (5000+ chars)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((long-line (make-string 5000 ?x)))
          (insert long-line "\n" long-line "\nshort")
          (cj/remove-duplicate-lines-region-or-buffer)
          (should (= 1 (how-many (regexp-quote long-line) (point-min) (point-max))))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-unicode-emoji ()
  "Should handle Unicode and emoji duplicates."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello 👋\nhello 👋\nother")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "hello 👋" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-with-tabs ()
  "Should preserve and match tab characters."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\twith\ttabs\nline\twith\ttabs\nother")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "line\twith\ttabs" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-mixed-whitespace ()
  "Should do exact whitespace matching."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "  line \t text  \n  line \t text  \nother")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "  line \t text  " (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-rtl-text ()
  "Should handle RTL text duplicates."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "مرحبا\nمرحبا\nعالم")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "مرحبا" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-case-insensitive ()
  "Should treat different cases as same line (case-insensitive by default)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line\nline\nLINE\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; Case-insensitive matching, so duplicates removed
        (should (= 1 (how-many "^[Ll][Ii][Nn][Ee]$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-trailing-whitespace-matters ()
  "Should treat trailing whitespace as significant."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line \nline\nline \n")
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; "line " appears twice, one should be removed
        (should (= 1 (how-many "line $" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-leading-whitespace-matters ()
  "Should treat leading whitespace as significant."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert " line\nline\n line\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; " line" appears twice, one should be removed
        (should (= 1 (how-many "^ line$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-narrowed-buffer ()
  "Should respect buffer narrowing."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before\ndup\ndup\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (point)))
          (forward-line 2)
          (narrow-to-region beg (point))
          (cj/remove-duplicate-lines-region-or-buffer)
          (widen)
          ;; Should still have before and after
          (should (string-match-p "before" (buffer-string)))
          (should (string-match-p "after" (buffer-string)))
          ;; Should have removed one dup
          (should (= 1 (how-many "^dup$" (point-min) (point-max))))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-backwards-region ()
  "Should handle backwards region (mark after point)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "dup\ndup\nother")
        (goto-char (point-max))
        (set-mark (point))
        (goto-char (point-min))
        (transient-mark-mode 1)
        (activate-mark)
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "^dup$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-entire-buffer-as-region ()
  "Should handle entire buffer selected as region."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one\ntwo\none\nthree")
        (transient-mark-mode 1)
        (mark-whole-buffer)
        (activate-mark)
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "^one$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-region-no-duplicates ()
  "Should leave region unchanged when no duplicates exist."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before\nunique1\nunique2\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 2)
        (transient-mark-mode 1)
        (activate-mark)
        (let ((original (buffer-string)))
          (cj/remove-duplicate-lines-region-or-buffer)
          (should (string= original (buffer-string)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-three-or-more ()
  "Should keep first and remove all other duplicates."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "dup\nother1\ndup\nother2\ndup\nother3\ndup\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (= 1 (how-many "^dup$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-alternating-pattern ()
  "Should handle alternating duplicate pattern (A B A B)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "A\nB\nA\nB\n")
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; Should keep first A and first B, remove duplicates
        (should (= 1 (how-many "^A$" (point-min) (point-max))))
        (should (= 1 (how-many "^B$" (point-min) (point-max)))))
    (test-remove-duplicate-lines-teardown)))

;;; Error Cases

(ert-deftest test-remove-duplicate-lines-read-only-buffer ()
  "Should error when attempting to modify read-only buffer."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "dup\ndup\n")
        (read-only-mode 1)
        (should-error (cj/remove-duplicate-lines-region-or-buffer)))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-buffer-modified-flag ()
  "Should set buffer modified flag when duplicates removed."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "dup\ndup\n")
        (set-buffer-modified-p nil)
        (cj/remove-duplicate-lines-region-or-buffer)
        (should (buffer-modified-p)))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-undo-behavior ()
  "Should support undo after removing duplicates."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (let* ((temp-file (expand-file-name "test-undo-rmdup.txt" cj/test-base-dir))
             (original-content "dup\ndup\nother"))
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
        (let ((before-remove (buffer-string)))
          (cj/remove-duplicate-lines-region-or-buffer)
          (undo-boundary)
          (let ((after-remove (buffer-string)))
            (should-not (string= before-remove after-remove))
            (undo)
            (should (string= before-remove (buffer-string)))))
        (kill-buffer (current-buffer)))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-cursor-position-preserved ()
  "Should preserve cursor position (save-excursion)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line1\ndup\nline2\ndup\nline3")
        (goto-char (point-min))
        (forward-char 3)  ; Position in middle of first line
        (let ((original-pos (point)))
          (cj/remove-duplicate-lines-region-or-buffer)
          (should (= (point) original-pos))))
    (test-remove-duplicate-lines-teardown)))

(ert-deftest test-remove-duplicate-lines-region-preserved ()
  "Should preserve region state (save-excursion maintains mark)."
  (test-remove-duplicate-lines-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "dup\ndup\nother\n")
        (transient-mark-mode 1)
        (mark-whole-buffer)
        (activate-mark)
        (should (use-region-p))
        (cj/remove-duplicate-lines-region-or-buffer)
        ;; save-excursion preserves mark, so region stays active
        (should (use-region-p)))
    (test-remove-duplicate-lines-teardown)))

(provide 'test-custom-line-paragraph-remove-duplicate-lines-region-or-buffer)
;;; test-custom-line-paragraph-remove-duplicate-lines-region-or-buffer.el ends here
