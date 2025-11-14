;;; test-custom-line-paragraph-remove-lines-containing.el --- Tests for cj/remove-lines-containing -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/remove-lines-containing function from custom-line-paragraph.el
;;
;; This function removes all lines containing TEXT.
;; If region is active, operate only on the region, otherwise on entire buffer.
;; The operation is undoable and reports the count of removed lines.
;;
;; The function uses (regexp-quote text) to treat special regex characters literally.
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

(defun test-remove-lines-containing-setup ()
  "Setup for remove-lines-containing tests."
  (cj/create-test-base-dir))

(defun test-remove-lines-containing-teardown ()
  "Teardown for remove-lines-containing tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-remove-lines-containing-single-match ()
  "Should remove single line containing text."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (cj/remove-lines-containing "two")
        (should-not (string-match-p "two" (buffer-string)))
        (should (string-match-p "one" (buffer-string)))
        (should (string-match-p "three" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-multiple-matches ()
  "Should remove multiple lines containing text."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "alpha test\nbeta\ngamma test\ndelta")
        (cj/remove-lines-containing "test")
        (should-not (string-match-p "alpha" (buffer-string)))
        (should-not (string-match-p "gamma" (buffer-string)))
        (should (string-match-p "beta" (buffer-string)))
        (should (string-match-p "delta" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-preserve-non-matching ()
  "Should preserve lines not containing text."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "keep this\nremove BAD this\nkeep that\nremove BAD that")
        (cj/remove-lines-containing "BAD")
        (should (string= "keep this\nkeep that\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-region-only ()
  "Should only affect active region."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before target\ntarget middle\ntarget end\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 2)
        (transient-mark-mode 1)
        (activate-mark)
        (cj/remove-lines-containing "target")
        ;; Should keep "before target" and "after"
        (should (string-match-p "before target" (buffer-string)))
        (should (string-match-p "after" (buffer-string)))
        ;; Should remove middle and end
        (should-not (string-match-p "target middle" (buffer-string)))
        (should-not (string-match-p "target end" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-whole-buffer ()
  "Should operate on entire buffer when no region active."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "one X\ntwo\nthree X\nfour")
        (cj/remove-lines-containing "X")
        (should (string= "two\nfour" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-partial-match ()
  "Should match text appearing anywhere in line."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "prefix MATCH suffix\nno match here\nMATCH at start\nat end MATCH")
        (cj/remove-lines-containing "MATCH")
        (should (string= "no match here\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-multiple-occurrences-per-line ()
  "Should remove line with text appearing multiple times."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "FOO and FOO and FOO\nbar\nFOO again")
        (cj/remove-lines-containing "FOO")
        (should (string= "bar\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-no-matches ()
  "Should leave buffer unchanged when text not found."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (let ((original (buffer-string)))
          (cj/remove-lines-containing "NOTFOUND")
          (should (string= original (buffer-string)))))
    (test-remove-lines-containing-teardown)))

;;; Boundary Cases

(ert-deftest test-remove-lines-containing-empty-buffer ()
  "Should handle empty buffer gracefully."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/remove-lines-containing "anything")
        (should (string= "" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-single-line-with-match ()
  "Should remove only line when it matches."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line with TARGET")
        (cj/remove-lines-containing "TARGET")
        (should (string= "" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-single-line-without-match ()
  "Should keep only line when it doesn't match."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "only line")
        (cj/remove-lines-containing "NOTHERE")
        (should (string= "only line" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-text-at-beginning ()
  "Should match text at beginning of line."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "TARGET at start\nmiddle TARGET middle\nkeep this")
        (cj/remove-lines-containing "TARGET")
        (should (string= "keep this" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-text-at-end ()
  "Should match text at end of line."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "at end TARGET\nkeep this\nanother TARGET")
        (cj/remove-lines-containing "TARGET")
        (should (string= "keep this\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-text-in-middle ()
  "Should match text in middle of line."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "prefix TARGET suffix\nkeep\nanother TARGET here")
        (cj/remove-lines-containing "TARGET")
        (should (string= "keep\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-empty-string ()
  "Should handle empty string gracefully without hanging."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nline two\nline three")
        (let ((original (buffer-string)))
          ;; Should not hang or remove anything
          (cj/remove-lines-containing "")
          ;; Buffer should be unchanged
          (should (string= original (buffer-string)))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-whitespace-only-text ()
  "Should remove lines with specific whitespace."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "has  double space\nsingle space\nhas  double space again")
        (cj/remove-lines-containing "  ")
        (should (string= "single space\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-very-long-line ()
  "Should handle very long lines (5000+ chars)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((long-line (concat (make-string 2500 ?x) "TARGET" (make-string 2500 ?y))))
          (insert long-line "\nshort line\n" long-line)
          (cj/remove-lines-containing "TARGET")
          (should (string= "short line\n" (buffer-string)))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-unicode-emoji ()
  "Should handle Unicode and emoji text."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "hello 👋 world\nno emoji\nbye 👋 friend")
        (cj/remove-lines-containing "👋")
        (should (string= "no emoji\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-special-regex-chars ()
  "Should treat regex special characters literally (regexp-quote)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line with .\nanother line\nline with * here\nkeep")
        (cj/remove-lines-containing ".")
        ;; Should remove only the line with literal ".", not all lines (which . would match)
        (should-not (string-match-p "line with \\." (buffer-string)))
        (should (string-match-p "another line" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-case-sensitive ()
  "Should perform case-sensitive matching."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((case-fold-search nil))  ; Ensure case-sensitive
          (insert "Line with Target\nLine with target\nLine with TARGET")
          (cj/remove-lines-containing "target")
          ;; Only lowercase "target" should match
          (should (string-match-p "Target" (buffer-string)))
          (should (string-match-p "TARGET" (buffer-string)))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-all-lines-match ()
  "Should remove all lines when every line contains text."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "X one\nX two\nX three")
        (cj/remove-lines-containing "X")
        (should (string= "" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-first-line-matches ()
  "Should handle match at buffer start."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "REMOVE first\nkeep second\nkeep third")
        (cj/remove-lines-containing "REMOVE")
        (should (string= "keep second\nkeep third" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-last-line-matches ()
  "Should handle match at buffer end."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "keep first\nkeep second\nREMOVE last")
        (cj/remove-lines-containing "REMOVE")
        (should (string= "keep first\nkeep second\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-alternating-matches ()
  "Should handle alternating matching lines."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "keep\nREMOVE\nkeep\nREMOVE\nkeep")
        (cj/remove-lines-containing "REMOVE")
        (should (string= "keep\nkeep\nkeep" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-narrowed-buffer ()
  "Should respect buffer narrowing (save-restriction)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "before TARGET\nmiddle TARGET\nend TARGET\nafter")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (point)))
          (forward-line 2)
          (narrow-to-region beg (point))
          (cj/remove-lines-containing "TARGET")
          (widen)
          ;; Should keep "before TARGET" and "after"
          (should (string-match-p "before TARGET" (buffer-string)))
          (should (string-match-p "after" (buffer-string)))
          ;; Should remove middle and end
          (should-not (string-match-p "middle TARGET" (buffer-string)))
          (should-not (string-match-p "end TARGET" (buffer-string)))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-backwards-region ()
  "Should handle backwards region (mark after point)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "keep\nREMOVE\nREMOVE\nkeep")
        (goto-char (point-max))
        (set-mark (point))
        (goto-char (point-min))
        (forward-line 1)
        (transient-mark-mode 1)
        (activate-mark)
        (cj/remove-lines-containing "REMOVE")
        ;; Should work same as forward region
        (should (= 2 (how-many "keep" (point-min) (point-max)))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-entire-buffer-as-region ()
  "Should handle entire buffer selected as region."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "keep\nREMOVE\nkeep\nREMOVE")
        (transient-mark-mode 1)
        (mark-whole-buffer)
        (activate-mark)
        (cj/remove-lines-containing "REMOVE")
        (should (string= "keep\nkeep\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-tab-characters ()
  "Should match lines with tab characters."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line\twith\ttab\nno tabs here\nanother\ttab")
        (cj/remove-lines-containing "\t")
        (should (string= "no tabs here\n" (buffer-string))))
    (test-remove-lines-containing-teardown)))

;;; Error Cases

(ert-deftest test-remove-lines-containing-read-only-buffer ()
  "Should error when attempting to modify read-only buffer."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line to remove\nline to keep")
        (read-only-mode 1)
        (should-error (cj/remove-lines-containing "remove")))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-buffer-modified-flag ()
  "Should set buffer modified flag when lines removed."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "REMOVE this\nkeep this")
        (set-buffer-modified-p nil)
        (cj/remove-lines-containing "REMOVE")
        (should (buffer-modified-p)))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-undo-behavior ()
  "Should support undo after removing lines."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (let* ((temp-file (expand-file-name "test-undo-rmlines.txt" cj/test-base-dir))
             (original-content "REMOVE this\nkeep this"))
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
          (cj/remove-lines-containing "REMOVE")
          (undo-boundary)
          (let ((after-remove (buffer-string)))
            (should-not (string= before-remove after-remove))
            (undo)
            (should (string= before-remove (buffer-string)))))
        (kill-buffer (current-buffer)))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-cursor-position-preserved ()
  "Should preserve cursor position (save-excursion)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nREMOVE\nline three")
        (goto-char (point-min))
        (forward-char 5)  ; Position in middle of first line
        (let ((original-pos (point)))
          (cj/remove-lines-containing "REMOVE")
          (should (= (point) original-pos))))
    (test-remove-lines-containing-teardown)))

(ert-deftest test-remove-lines-containing-widen-after-narrowing ()
  "Should restore narrowing state (save-restriction)."
  (test-remove-lines-containing-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line one\nREMOVE\nline three\nline four")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (point)))
          (forward-line 2)
          (narrow-to-region beg (point))
          (cj/remove-lines-containing "REMOVE")
          ;; After function, narrowing should be restored
          (should (= (point-min) beg))
          (should (< (point-max) (buffer-size)))))
    (test-remove-lines-containing-teardown)))

(provide 'test-custom-line-paragraph-remove-lines-containing)
;;; test-custom-line-paragraph-remove-lines-containing.el ends here
