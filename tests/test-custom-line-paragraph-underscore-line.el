;;; test-custom-line-paragraph-underscore-line.el --- Tests for cj/underscore-line -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/underscore-line function from custom-line-paragraph.el
;;
;; This function underlines the current line by inserting a row of characters below it.
;; If the line is empty or contains only whitespace, it aborts with a message.
;;
;; The function uses (read-char) to get the underline character from the user.
;; In tests, we mock this using cl-letf.

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

(defun test-underscore-line-setup ()
  "Setup for underscore-line tests."
  (cj/create-test-base-dir))

(defun test-underscore-line-teardown ()
  "Teardown for underscore-line tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-underscore-line-simple-text ()
  "Should underline simple text line."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello World")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "Hello World\n-----------" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-preserve-original ()
  "Should preserve original line text."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Original Text")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?=)))
          (cj/underscore-line)
          (goto-char (point-min))
          (should (looking-at "Original Text"))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-use-specified-character ()
  "Should use the character provided by user."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Test")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?*)))
          (cj/underscore-line)
          (should (string-match-p "\\*\\*\\*\\*" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-match-width ()
  "Should create underline matching line width."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "12345")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          ;; Should have exactly 5 dashes
          (should (string-match-p "12345\n-----$" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-insert-newline ()
  "Should insert newline before underline."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (= 2 (count-lines (point-min) (point-max))))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-cursor-preserved ()
  "Should preserve cursor position."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Some text here")
        (goto-char (point-min))
        (forward-char 5)  ; Position in middle
        (let ((original-pos (point)))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
            (cj/underscore-line)
            (should (= (point) original-pos)))))
    (test-underscore-line-teardown)))

;;; Boundary Cases

(ert-deftest test-underscore-line-empty-line-aborts ()
  "Should abort on empty line."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "")
        (cj/underscore-line)
        ;; Buffer should remain empty
        (should (string= "" (buffer-string))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-whitespace-only-aborts ()
  "Should abort on whitespace-only line."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \t  ")
        (goto-char (point-min))
        (let ((original (buffer-string)))
          (cj/underscore-line)
          ;; Buffer should be unchanged
          (should (string= original (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-single-character ()
  "Should underline single character line."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "X")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string= "X\n-" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-very-long-line ()
  "Should handle very long lines (5000+ chars)."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((long-line (make-string 5000 ?x)))
          (insert long-line)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
            (cj/underscore-line)
            ;; Should have 5000 dashes
            (should (= 5000 (how-many "-" (point-min) (point-max)))))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-with-tabs ()
  "Should account for tab expansion in column width."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "a\tb")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          ;; Tab expands to column, underline should match visual width
          (let ((underline-length (save-excursion
                                    (goto-char (point-min))
                                    (forward-line 1)
                                    (- (line-end-position) (line-beginning-position)))))
            (should (> underline-length 2)))))  ; More than just "a" and "b"
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-leading-whitespace ()
  "Should include leading whitespace in width calculation."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "  text")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          ;; Should have 6 dashes (2 spaces + 4 chars)
          (should (string= "  text\n------" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-trailing-whitespace ()
  "Should include trailing whitespace in width calculation."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "text  ")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          ;; Should have 6 dashes (4 chars + 2 spaces)
          (should (string= "text  \n------" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-unicode-emoji ()
  "Should handle Unicode and emoji characters."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          ;; Should create underline
          (should (string-match-p "Hello 👋\n-" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-rtl-text ()
  "Should handle RTL text."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "مرحبا")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "مرحبا\n-" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-combining-characters ()
  "Should handle Unicode combining characters."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "cafe\u0301")  ; e with combining acute
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "cafe\u0301\n-" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-at-buffer-start ()
  "Should work on first line in buffer."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "First line\nSecond line")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?=)))
          (cj/underscore-line)
          (should (string-match-p "First line\n==========" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-at-buffer-end ()
  "Should work on last line in buffer."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "First line\nLast line")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?=)))
          (cj/underscore-line)
          (should (string-match-p "Last line\n=========$" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-different-characters ()
  "Should work with various underline characters."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Test")
        (goto-char (point-min))
        (dolist (char '(?- ?= ?* ?# ?~ ?_))
          (goto-char (point-min))
          (delete-region (point-min) (point-max))
          (insert "Test")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) char)))
            (cj/underscore-line)
            (should (string-match-p (regexp-quote (make-string 4 char)) (buffer-string))))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-special-characters ()
  "Should work with special non-alphanumeric characters."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Text")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?@)))
          (cj/underscore-line)
          (should (string-match-p "@@@@" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-cursor-in-middle ()
  "Should work regardless of cursor position on line."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello World")
        (goto-char (point-min))
        (forward-char 6)  ; Position after "Hello "
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "Hello World\n-----------" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-cursor-at-start ()
  "Should work when cursor at line beginning."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Text")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "Text\n----" (buffer-string)))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-cursor-at-end ()
  "Should work when cursor at line end."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Text")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (string-match-p "Text\n----" (buffer-string)))))
    (test-underscore-line-teardown)))

;;; Error Cases

(ert-deftest test-underscore-line-read-only-buffer ()
  "Should error when attempting to modify read-only buffer."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read only text")
        (goto-char (point-min))
        (read-only-mode 1)
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (should-error (cj/underscore-line))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-buffer-modified-flag ()
  "Should set buffer modified flag."
  (test-underscore-line-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Text")
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
          (cj/underscore-line)
          (should (buffer-modified-p))))
    (test-underscore-line-teardown)))

(ert-deftest test-underscore-line-undo-behavior ()
  "Should support undo after underlining."
  (test-underscore-line-setup)
  (unwind-protect
      (let* ((temp-file (expand-file-name "test-undo-underline.txt" cj/test-base-dir))
             (original-content "Test line"))
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
        (let ((before-underline (buffer-string)))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?-)))
            (cj/underscore-line))
          (undo-boundary)
          (let ((after-underline (buffer-string)))
            (should-not (string= before-underline after-underline))
            (undo)
            (should (string= before-underline (buffer-string)))))
        (kill-buffer (current-buffer)))
    (test-underscore-line-teardown)))

(provide 'test-custom-line-paragraph-underscore-line)
;;; test-custom-line-paragraph-underscore-line.el ends here
