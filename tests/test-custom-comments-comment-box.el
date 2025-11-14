;;; test-custom-comments-comment-box.el --- Tests for cj/comment-box -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-box function from custom-comments.el
;;
;; This function generates a 3-line box comment:
;; - Top border: comment-start + full decoration line
;; - Text line: comment-start + decoration + spaces + text + spaces + decoration
;; - Bottom border: comment-start + full decoration line
;;
;; The text is centered within the box with decoration characters on the sides.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-box)
;; to avoid mocking user prompts. This follows our testing best practice
;; of separating business logic from UI interaction.
;;
;; Cross-Language Testing Strategy:
;; - Comprehensive testing in Emacs Lisp (our primary language)
;; - Representative testing in Python and C (hash-based and C-style comments)
;; - Function handles comment syntax generically, so testing 3 syntaxes
;;   proves cross-language compatibility
;; - See test-custom-comments-delete-buffer-comments.el for detailed rationale

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-comments)

;;; Test Helpers

(defun test-comment-box-at-column (column-pos comment-start comment-end decoration-char text length)
  "Test cj/--comment-box at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-box with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, and LENGTH.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-box comment-start comment-end decoration-char text length)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-comment-box-elisp-basic ()
  "Should generate 3-line box in emacs-lisp style."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Section Header" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; First line should start with ;; and have decoration
    (should (string-match-p "^;; -" result))
    ;; Middle line should contain text with side borders
    (should (string-match-p ";; - .* Section Header .* - ;;" result))
    ;; Should have top and bottom borders
    (should (string-match-p "^;; -" result))))

(ert-deftest test-comment-box-elisp-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-comment-box-at-column 0 ";;" "" "*" "Header" 70)))
    (should (string-match-p ";; \\*" result))
    (should-not (string-match-p "-" result))))

(ert-deftest test-comment-box-elisp-custom-text ()
  "Should include custom text centered in box."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Custom Text Here" 70)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-comment-box-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "" 70)))
    ;; Should still generate 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should have side borders
    (should (string-match-p "- .*-" result))))

(ert-deftest test-comment-box-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Header" 70)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-comment-box-elisp-indented ()
  "Should work when indented."
  (let ((result (test-comment-box-at-column 4 ";;" "" "-" "Header" 70)))
    ;; First line should start with spaces
    (should (string-prefix-p "    ;;" result))
    ;; Other lines should be indented
    (let ((lines (split-string result "\n" t)))
      (should (string-prefix-p "    " (nth 1 lines)))
      (should (string-prefix-p "    " (nth 2 lines))))))

(ert-deftest test-comment-box-elisp-short-text ()
  "Should center short text properly."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "X" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present and centered
    (should (string-match-p "- .* X .* -" result))))

(ert-deftest test-comment-box-elisp-long-text ()
  "Should handle longer text."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "This is a longer header text" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "This is a longer header text" result))))

;;; Boundary Cases

(ert-deftest test-comment-box-elisp-minimum-length ()
  "Should work with minimum viable length."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "X" 15)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "X" result))))

(ert-deftest test-comment-box-elisp-very-long-length ()
  "Should handle very long length."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Header" 200)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Border lines should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-comment-box-elisp-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-comment-box-at-column 0 ";;" "" "═" "Header" 70)))
    (should (string-match-p "═" result))))

(ert-deftest test-comment-box-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Hello 👋 مرحبا café" 70)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-comment-box-elisp-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (make-string 100 ?x))
         (result (test-comment-box-at-column 0 ";;" "" "-" long-text 70)))
    ;; Should still generate output
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-comment-box-elisp-comment-end-symmetric ()
  "Should use symmetric comment syntax when comment-end is empty."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "Header" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should use ;; on both sides for symmetry
    (should (string-match-p ";;.*;;$" result))))

(ert-deftest test-comment-box-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-comment-box-at-column 60 ";;" "" "-" "Header" 100)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; First line should start with 60 spaces
    (should (string-prefix-p (make-string 60 ?\s) result))))

(ert-deftest test-comment-box-elisp-text-centering-even ()
  "Should center text properly with even length."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "EVEN" 70)))
    ;; Text should be centered (roughly equal padding on both sides)
    (should (string-match-p "- .* EVEN .* -" result))))

(ert-deftest test-comment-box-elisp-text-centering-odd ()
  "Should center text properly with odd length."
  (let ((result (test-comment-box-at-column 0 ";;" "" "-" "ODD" 70)))
    ;; Text should be centered (roughly equal padding on both sides)
    (should (string-match-p "- .* ODD .* -" result))))

;;; Error Cases

(ert-deftest test-comment-box-elisp-length-too-small ()
  "Should error when length is too small."
  (should-error
   (test-comment-box-at-column 0 ";;" "" "-" "Header" 5)
   :type 'error))

(ert-deftest test-comment-box-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-comment-box-at-column 0 ";;" "" "-" "Header" -10)
   :type 'error))

(ert-deftest test-comment-box-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-comment-box-at-column 0 ";;" "" "-" "Header" 0)
   :type 'error))

(ert-deftest test-comment-box-elisp-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-comment-box-at-column 0 ";;" "" nil "Header" 70)
   :type 'wrong-type-argument))

(ert-deftest test-comment-box-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-comment-box-at-column 0 ";;" "" "-" "Header" "not-a-number")
   :type 'wrong-type-argument))

;;; Python Tests (Hash-based comments)

(ert-deftest test-comment-box-python-basic ()
  "Should generate box with Python comment syntax."
  (let ((result (test-comment-box-at-column 0 "#" "" "-" "Section" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^# -" result))
    (should (string-match-p "Section" result))))

(ert-deftest test-comment-box-python-indented ()
  "Should handle indented Python comments."
  (let ((result (test-comment-box-at-column 4 "#" "" "#" "Function Section" 70)))
    (should (string-prefix-p "    #" result))
    (should (string-match-p "Function Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-comment-box-c-block-comments ()
  "Should generate box with C block comment syntax."
  (let ((result (test-comment-box-at-column 0 "/*" "*/" "-" "Section" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* -" result))
    (should (string-match-p "Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(provide 'test-custom-comments-comment-box)
;;; test-custom-comments-comment-box.el ends here
