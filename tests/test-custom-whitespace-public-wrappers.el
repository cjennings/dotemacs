;;; test-custom-whitespace-public-wrappers.el --- Tests for the interactive whitespace dispatchers -*- lexical-binding: t; -*-

;;; Commentary:
;; The internal `cj/--*' helpers are exhaustively covered in the
;; sibling test files.  This file covers the six interactive
;; dispatchers that route a current-line / region / whole-buffer
;; selection into the appropriate helper:
;;
;;   cj/remove-leading-trailing-whitespace
;;   cj/collapse-whitespace-line-or-region
;;   cj/ensure-single-blank-line
;;   cj/delete-blank-lines-region-or-buffer
;;   cj/delete-all-whitespace
;;   cj/hyphenate-whitespace-in-region
;;
;; `yes-or-no-p' is stubbed for the two prompts; `use-region-p' and
;; the region delimiters are exercised via real `with-temp-buffer'
;; state.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-whitespace)

;;; cj/remove-leading-trailing-whitespace

(ert-deftest test-custom-whitespace-wrapper-remove-line-only ()
  "Normal: with no region and no prefix, the wrapper trims the current line only."
  (with-temp-buffer
    (insert "   hello   \n   keep   \n")
    (goto-char (point-min))
    (cj/remove-leading-trailing-whitespace)
    (should (equal (buffer-string) "hello\n   keep   \n"))))

(ert-deftest test-custom-whitespace-wrapper-remove-region ()
  "Normal: with an active region, the wrapper trims inside the region."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "   alpha   \n   beta   \n")
      (push-mark (point-min) t t)
      (goto-char (line-end-position 2))
      (cj/remove-leading-trailing-whitespace))
    (should (string-match-p "alpha" (buffer-string)))
    (should-not (string-match-p "   alpha   " (buffer-string)))))

(ert-deftest test-custom-whitespace-wrapper-remove-whole-buffer-with-prefix ()
  "Normal: a prefix argument widens the operation to the whole buffer."
  (with-temp-buffer
    (insert "   alpha   \n   beta   \n")
    (let ((current-prefix-arg '(4)))
      (cj/remove-leading-trailing-whitespace))
    (should (equal (buffer-string) "alpha\nbeta\n"))))

;;; cj/collapse-whitespace-line-or-region

(ert-deftest test-custom-whitespace-wrapper-collapse-line-only ()
  "Normal: with no region, the wrapper collapses whitespace on the current line."
  (with-temp-buffer
    (insert "  alpha    beta  \n  keep    here  \n")
    (goto-char (point-min))
    (cj/collapse-whitespace-line-or-region)
    (should (equal (buffer-string) "alpha beta\n  keep    here  \n"))))

(ert-deftest test-custom-whitespace-wrapper-collapse-region ()
  "Normal: with an active region, the wrapper collapses inside it only."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "  alpha    beta  \n  outside    \n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (line-end-position))
      (cj/collapse-whitespace-line-or-region))
    ;; First line collapsed, second untouched.
    (should (string-match-p "^alpha beta\n" (buffer-string)))
    (should (string-match-p "  outside    \n" (buffer-string)))))

;;; cj/ensure-single-blank-line

(ert-deftest test-custom-whitespace-wrapper-single-blank-region ()
  "Normal: with an active region, the wrapper collapses blank-line runs in it."
  (with-temp-buffer
    (insert "a\n\n\n\nb\n")
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (cj/ensure-single-blank-line (region-beginning) (region-end))
    (should (equal (buffer-string) "a\n\nb\n"))))

(ert-deftest test-custom-whitespace-wrapper-single-blank-whole-buffer-yes ()
  "Normal: outside a region, an affirmative prompt operates on the whole buffer."
  (with-temp-buffer
    (insert "a\n\n\n\nb\n\n\nc\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (call-interactively #'cj/ensure-single-blank-line))
    (should (equal (buffer-string) "a\n\nb\n\nc\n"))))

(ert-deftest test-custom-whitespace-wrapper-single-blank-decline-aborts ()
  "Error: outside a region, declining the prompt raises `user-error'."
  (with-temp-buffer
    (insert "a\n\n\nb\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (call-interactively #'cj/ensure-single-blank-line)
                    :type 'user-error))))

;;; cj/delete-blank-lines-region-or-buffer

(ert-deftest test-custom-whitespace-wrapper-delete-blank-region ()
  "Normal: with a region, blank lines inside it are deleted."
  (with-temp-buffer
    (insert "a\n\n\nb\n")
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (cj/delete-blank-lines-region-or-buffer (region-beginning) (region-end))
    (should (equal (buffer-string) "a\nb\n"))))

(ert-deftest test-custom-whitespace-wrapper-delete-blank-whole-buffer-yes ()
  "Normal: outside a region, an affirmative prompt deletes blank lines buffer-wide."
  (with-temp-buffer
    (insert "a\n\n\nb\n\n\nc\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (call-interactively #'cj/delete-blank-lines-region-or-buffer))
    (should (equal (buffer-string) "a\nb\nc\n"))))

(ert-deftest test-custom-whitespace-wrapper-delete-blank-decline-aborts ()
  "Error: outside a region, declining the prompt raises `user-error'."
  (with-temp-buffer
    (insert "a\n\n\nb\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (call-interactively #'cj/delete-blank-lines-region-or-buffer)
                    :type 'user-error))))

;;; cj/delete-all-whitespace

(ert-deftest test-custom-whitespace-wrapper-delete-all-region ()
  "Normal: with an active region, all whitespace inside it is removed."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "a b\nc\td  \n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/delete-all-whitespace (region-beginning) (region-end)))
    (should (equal (buffer-string) "abcd"))))

(ert-deftest test-custom-whitespace-wrapper-delete-all-no-region-messages ()
  "Boundary: without an active region, the wrapper messages and no-ops."
  (with-temp-buffer
    (insert "a b\n")
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/delete-all-whitespace (point-min) (point-max)))
      (should (string-match-p "No region" msg)))
    (should (equal (buffer-string) "a b\n"))))

;;; cj/hyphenate-whitespace-in-region

(ert-deftest test-custom-whitespace-wrapper-hyphenate-region ()
  "Normal: with an active region, whitespace runs are replaced by hyphens."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "hello world\tfoo  bar")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/hyphenate-whitespace-in-region (region-beginning) (region-end)))
    (should (equal (buffer-string) "hello-world-foo-bar"))))

(ert-deftest test-custom-whitespace-wrapper-hyphenate-no-region-messages ()
  "Boundary: without an active region, the wrapper messages and no-ops."
  (with-temp-buffer
    (insert "no region here")
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/hyphenate-whitespace-in-region (point-min) (point-max)))
      (should (string-match-p "No region" msg)))
    (should (equal (buffer-string) "no region here"))))

(provide 'test-custom-whitespace-public-wrappers)
;;; test-custom-whitespace-public-wrappers.el ends here
