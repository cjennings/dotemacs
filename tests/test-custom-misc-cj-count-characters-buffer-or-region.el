;;; test-custom-misc-cj-count-characters-buffer-or-region.el --- Tests for cj/count-characters-buffer-or-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/count-characters-buffer-or-region function from custom-misc.el
;;
;; This function counts characters in the active region or the entire buffer
;; if no region is active. It displays the count in the minibuffer.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-misc)

;;; Setup and Teardown

(defun test-count-characters-buffer-or-region-setup ()
  "Set up test environment."
  ;; No setup needed
  nil)

(defun test-count-characters-buffer-or-region-teardown ()
  "Clean up test environment."
  ;; Clear any active region
  (when (use-region-p)
    (deactivate-mark)))

;;; Normal Cases

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-normal-whole-buffer-counts-all ()
  "Should count all characters in buffer when no region is active."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        ;; Ensure no region is active
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            (should (string-match-p "13 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-normal-active-region-counts-region ()
  "Should count characters in active region."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        ;; Select "Hello" (positions 1-6)
        (goto-char 1)
        (push-mark 1)
        (goto-char 6)
        (activate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            (should (string-match-p "5 characters.*region" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-normal-multiline-buffer-counts-all ()
  "Should count characters including newlines in buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            ;; 6 + 1 + 6 + 1 + 6 = 20 characters
            (should (string-match-p "20 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-normal-multiline-region-counts-region ()
  "Should count characters including newlines in region."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        ;; Select first two lines including newlines
        (goto-char 1)
        (push-mark 1)
        (goto-char 14) ; After "Line 1\nLine 2"
        (activate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            ;; "Line 1\nLine 2" = 6 + 1 + 6 = 13 characters
            (should (string-match-p "13 characters.*region" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

;;; Boundary Cases

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-empty-buffer-returns-zero ()
  "Should return 0 for empty buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            (should (string-match-p "0 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-empty-region-counts-buffer ()
  "Should count whole buffer when region is empty (point equals mark).
When mark and point are at the same position, use-region-p returns nil,
so the function correctly falls back to counting the entire buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        ;; Create empty region (point equals mark)
        ;; Even with activate-mark, use-region-p returns nil when mark == point
        (goto-char 5)
        (push-mark 5)
        (activate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            ;; Should count the whole buffer (13 characters) not the empty region
            (should (string-match-p "13 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-large-buffer-counts-all ()
  "Should handle very large buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((large-content (make-string 100000 ?x)))
          (insert large-content)
          (deactivate-mark)
          (let ((message-output nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq message-output (apply #'format format-string args)))))
              (cj/count-characters-buffer-or-region)
              (should (string-match-p "100000 characters.*buffer" message-output))))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-unicode-counts-correctly ()
  "Should count unicode characters (emoji, RTL text) correctly."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋 مرحبا")
        (deactivate-mark)
        (let ((message-output nil)
              (expected-count (- (point-max) (point-min))))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            (should (string-match-p (format "%d characters.*buffer" expected-count)
                                    message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-whitespace-only-counts-whitespace ()
  "Should count whitespace characters."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \t\n  ")
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            ;; 3 spaces + 1 tab + 1 newline + 2 spaces = 7 characters
            (should (string-match-p "7 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-single-character-returns-one ()
  "Should return 1 for single character buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "x")
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            (should (string-match-p "1 character.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(ert-deftest test-custom-misc-cj-count-characters-buffer-or-region-boundary-narrowed-buffer-counts-visible ()
  "Should count only visible characters in narrowed buffer."
  (test-count-characters-buffer-or-region-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3\n")
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (progn (forward-line 1) (point)))
        (deactivate-mark)
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (cj/count-characters-buffer-or-region)
            ;; "Line 2\n" = 7 characters
            (should (string-match-p "7 characters.*buffer" message-output)))))
    (test-count-characters-buffer-or-region-teardown)))

(provide 'test-custom-misc-cj-count-characters-buffer-or-region)
;;; test-custom-misc-cj-count-characters-buffer-or-region.el ends here
