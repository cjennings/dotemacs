;;; test-custom-comments-comment-heavy-box.el --- Tests for cj/comment-heavy-box -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-heavy-box function from custom-comments.el
;;
;; This function generates a 5-line heavy box comment:
;; - Top border: comment-start + full decoration line
;; - Empty line: decoration char + spaces + decoration char
;; - Centered text: decoration char + spaces + text + spaces + decoration char
;; - Empty line: decoration char + spaces + decoration char
;; - Bottom border: comment-start + full decoration line
;;
;; The text is centered within the box with padding on both sides.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-heavy-box)
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

(defun test-heavy-box-at-column (column-pos comment-start comment-end decoration-char text length)
  "Test cj/--comment-heavy-box at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-heavy-box with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, and LENGTH.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-heavy-box comment-start comment-end decoration-char text length)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-heavy-box-elisp-basic ()
  "Should generate 5-line heavy box in emacs-lisp style."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Section Header" 70)))
    ;; Should have 5 lines
    (should (= 5 (length (split-string result "\n" t))))
    ;; First line should start with ;; and have decoration
    (should (string-match-p "^;; \\*" result))
    ;; Middle line should contain centered text
    (should (string-match-p "Section Header" result))
    ;; Should have side borders
    (should (string-match-p "^\\*.*\\*$" result))))

(ert-deftest test-heavy-box-elisp-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "#" "Header" 70)))
    (should (string-match-p ";; #" result))
    (should-not (string-match-p "\\*" result))))

(ert-deftest test-heavy-box-elisp-custom-text ()
  "Should include custom text centered in box."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Custom Text Here" 70)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-heavy-box-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "" 70)))
    ;; Should still generate 5 lines
    (should (= 5 (length (split-string result "\n" t))))
    ;; Middle line should just have side borders and spaces
    (should (string-match-p "^\\*.*\\*$" result))))

(ert-deftest test-heavy-box-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Header" 70)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-heavy-box-elisp-indented ()
  "Should work when indented."
  (let ((result (test-heavy-box-at-column 4 ";;" "" "*" "Header" 70)))
    ;; First line should start with spaces
    (should (string-prefix-p "    ;;" result))
    ;; Other lines should be indented
    (let ((lines (split-string result "\n" t)))
      (should (string-prefix-p "    " (nth 1 lines)))
      (should (string-prefix-p "    " (nth 2 lines))))))

(ert-deftest test-heavy-box-elisp-short-text ()
  "Should center short text properly."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "X" 70)))
    ;; Should have 5 lines
    (should (= 5 (length (split-string result "\n" t))))
    ;; Text should be present and centered
    (should (string-match-p "\\* .* X .* \\*" result))))

(ert-deftest test-heavy-box-elisp-long-text ()
  "Should handle longer text."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "This is a longer header text" 70)))
    ;; Should have 5 lines
    (should (= 5 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "This is a longer header text" result))))

;;; Boundary Cases

(ert-deftest test-heavy-box-elisp-minimum-length ()
  "Should work with minimum viable length."
  ;; Minimum for a box: comment + spaces + borders + minimal content
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "X" 15)))
    (should (= 5 (length (split-string result "\n" t))))
    (should (string-match-p "X" result))))

(ert-deftest test-heavy-box-elisp-very-long-length ()
  "Should handle very long length."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Header" 200)))
    (should (= 5 (length (split-string result "\n" t))))
    ;; Border lines should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-heavy-box-elisp-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "═" "Header" 70)))
    (should (string-match-p "═" result))))

(ert-deftest test-heavy-box-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Hello 👋 مرحبا café" 70)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-heavy-box-elisp-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (make-string 100 ?x))
         (result (test-heavy-box-at-column 0 ";;" "" "*" long-text 70)))
    ;; Should still generate output
    (should (= 5 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-heavy-box-elisp-comment-end-empty ()
  "Should handle empty comment-end by using symmetric comment syntax."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "Header" 70)))
    (should (= 5 (length (split-string result "\n" t))))
    ;; When comment-end is empty, function uses comment-char for symmetry
    ;; So border lines will have ";; ... ;;" for visual balance
    (should (string-match-p ";;.*;;$" result))))

(ert-deftest test-heavy-box-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-heavy-box-at-column 60 ";;" "" "*" "Header" 100)))
    (should (= 5 (length (split-string result "\n" t))))
    ;; First line should start with 60 spaces
    (should (string-prefix-p (make-string 60 ?\s) result))))

(ert-deftest test-heavy-box-elisp-text-centering-even ()
  "Should center text properly with even length."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "EVEN" 70)))
    ;; Text should be centered (roughly equal padding on both sides)
    (should (string-match-p "\\* .* EVEN .* \\*" result))))

(ert-deftest test-heavy-box-elisp-text-centering-odd ()
  "Should center text properly with odd length."
  (let ((result (test-heavy-box-at-column 0 ";;" "" "*" "ODD" 70)))
    ;; Text should be centered (roughly equal padding on both sides)
    (should (string-match-p "\\* .* ODD .* \\*" result))))

;;; Error Cases

(ert-deftest test-heavy-box-elisp-length-too-small ()
  "Should error when length is too small."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" "*" "Header" 5)
   :type 'error))

(ert-deftest test-heavy-box-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" "*" "Header" -10)
   :type 'error))

(ert-deftest test-heavy-box-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" "*" "Header" 0)
   :type 'error))

(ert-deftest test-heavy-box-elisp-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" nil "Header" 70)
   :type 'wrong-type-argument))

(ert-deftest test-heavy-box-elisp-nil-text ()
  "Should error when text is nil."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" "*" nil 70)
   :type 'wrong-type-argument))

(ert-deftest test-heavy-box-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-heavy-box-at-column 0 ";;" "" "*" "Header" "not-a-number")
   :type 'wrong-type-argument))

;;; Python Tests (Hash-based comments)

(ert-deftest test-heavy-box-python-basic ()
  "Should generate heavy box with Python comment syntax."
  (let ((result (test-heavy-box-at-column 0 "#" "" "*" "Section" 70)))
    (should (= 5 (length (split-string result "\n" t))))
    (should (string-match-p "^# \\*" result))
    (should (string-match-p "Section" result))))

(ert-deftest test-heavy-box-python-indented ()
  "Should handle indented Python comments."
  (let ((result (test-heavy-box-at-column 4 "#" "" "#" "Function Section" 70)))
    (should (string-prefix-p "    #" result))
    (should (string-match-p "Function Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-heavy-box-c-block-comments ()
  "Should generate heavy box with C block comment syntax."
  (let ((result (test-heavy-box-at-column 0 "/*" "*/" "*" "Section" 70)))
    (should (= 5 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* \\*" result))
    (should (string-match-p "Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(provide 'test-custom-comments-comment-heavy-box)
;;; test-custom-comments-comment-heavy-box.el ends here
