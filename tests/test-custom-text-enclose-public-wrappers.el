;;; test-custom-text-enclose-public-wrappers.el --- Tests for the interactive enclose wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; The internal `cj/--*' helpers are covered in the sibling test
;; files.  The seven public wrappers were uncovered:
;;
;;   cj/surround-word-or-region
;;   cj/wrap-word-or-region
;;   cj/unwrap-word-or-region
;;   cj/append-to-lines-in-region-or-buffer
;;   cj/prepend-to-lines-in-region-or-buffer
;;   cj/indent-lines-in-region-or-buffer
;;   cj/dedent-lines-in-region-or-buffer
;;
;; Tests cover the region-active branch, the thing-at-point-word
;; branch, and the no-word-no-region message branch for the first
;; three; the region-vs-whole-buffer branches for the line-oriented
;; four.  `transient-mark-mode' is let-bound so `use-region-p' returns
;; t in batch.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-text-enclose)

(defmacro test-cte--with-region (content &rest body)
  "Insert CONTENT in a temp buffer, activate region over all of it, eval BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((transient-mark-mode t))
       (insert ,content)
       (goto-char (point-min))
       (push-mark (point) t t)
       (goto-char (point-max))
       ,@body)))

;;; cj/surround-word-or-region

(ert-deftest test-cte-surround-region-wraps-with-string ()
  "Normal: an active region is replaced with surround+text+surround."
  (test-cte--with-region "hello"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "*")))
      (cj/surround-word-or-region))
    (should (equal (buffer-string) "*hello*"))))

(ert-deftest test-cte-surround-word-at-point ()
  "Normal: with no region but a word at point, the word is surrounded."
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char (point-min))      ; point on "alpha"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "**")))
      (cj/surround-word-or-region))
    (should (equal (buffer-string) "**alpha** beta"))))

(ert-deftest test-cte-surround-no-word-no-region-messages ()
  "Boundary: no region and no word at point => message, buffer unchanged."
  (with-temp-buffer
    (insert "   ")                ; whitespace-only, no word
    (goto-char (point-min))
    (let ((msg nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "*"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/surround-word-or-region))
      (should (string-match-p "No word at point" msg))
      (should (equal (buffer-string) "   ")))))

;;; cj/wrap-word-or-region

(ert-deftest test-cte-wrap-region-uses-opening-and-closing ()
  "Normal: region is replaced with opening+text+closing."
  (test-cte--with-region "body"
    (let ((calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf calls) (if (= calls 1) "<" ">"))))
        (cj/wrap-word-or-region)))
    (should (equal (buffer-string) "<body>"))))

(ert-deftest test-cte-wrap-word-at-point ()
  "Normal: with no region, the word at point is wrapped."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (let ((calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf calls) (if (= calls 1) "[" "]"))))
        (cj/wrap-word-or-region)))
    (should (equal (buffer-string) "[foo] bar"))))

(ert-deftest test-cte-wrap-no-word-no-region-messages ()
  "Boundary: no region and no word at point => message, buffer unchanged."
  (with-temp-buffer
    (insert "")
    (let ((msg nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/wrap-word-or-region))
      (should (string-match-p "No word at point" msg)))))

;;; cj/unwrap-word-or-region

(ert-deftest test-cte-unwrap-region-strips-delimiters ()
  "Normal: region's opening/closing delimiters are stripped."
  (test-cte--with-region "<body>"
    (let ((calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf calls) (if (= calls 1) "<" ">"))))
        (cj/unwrap-word-or-region)))
    (should (equal (buffer-string) "body"))))

(ert-deftest test-cte-unwrap-word-at-point-unchanged-when-no-match ()
  "Boundary: word at point without the wrapping delimiters stays unchanged."
  (with-temp-buffer
    (insert "bare")
    (goto-char (point-min))
    (let ((calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf calls) (if (= calls 1) "<" ">"))))
        (cj/unwrap-word-or-region)))
    (should (equal (buffer-string) "bare"))))

(ert-deftest test-cte-unwrap-no-word-no-region-messages ()
  "Boundary: no region and no word at point => message, buffer unchanged."
  (with-temp-buffer
    (let ((msg nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/unwrap-word-or-region))
      (should (string-match-p "No word at point" msg)))))

;;; cj/append-to-lines-in-region-or-buffer

(ert-deftest test-cte-append-to-lines-region ()
  "Normal: appends to each line inside the active region only."
  (test-cte--with-region "a\nb\n"
    (cj/append-to-lines-in-region-or-buffer ";"))
  ;; The macro doesn't preserve the buffer outside; the assertion happens
  ;; inline below via a manual setup so we can read the buffer.
  )

(ert-deftest test-cte-append-to-lines-region-active ()
  "Normal: appends suffix to each line within the active region."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "a\nb\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/append-to-lines-in-region-or-buffer ";"))
    (should (equal (buffer-string) "a;\nb;\n"))))

(ert-deftest test-cte-append-to-lines-whole-buffer ()
  "Normal: without an active region, appends to every line of the buffer."
  (with-temp-buffer
    (insert "x\ny\nz")
    (cj/append-to-lines-in-region-or-buffer "!")
    (should (equal (buffer-string) "x!\ny!\nz!"))))

;;; cj/prepend-to-lines-in-region-or-buffer

(ert-deftest test-cte-prepend-to-lines-region-active ()
  "Normal: prepends prefix to each line within the active region."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "a\nb\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/prepend-to-lines-in-region-or-buffer "// "))
    (should (equal (buffer-string) "// a\n// b\n"))))

(ert-deftest test-cte-prepend-to-lines-whole-buffer ()
  "Normal: without a region, prepends to every line."
  (with-temp-buffer
    (insert "x\ny\nz")
    (cj/prepend-to-lines-in-region-or-buffer "> ")
    (should (equal (buffer-string) "> x\n> y\n> z"))))

;;; cj/indent-lines-in-region-or-buffer

(ert-deftest test-cte-indent-lines-region-spaces ()
  "Normal: indent N spaces per line inside the active region."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "a\nb\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/indent-lines-in-region-or-buffer 2 nil))
    (should (equal (buffer-string) "  a\n  b\n"))))

(ert-deftest test-cte-indent-lines-whole-buffer-tabs ()
  "Boundary: use-tabs non-nil swaps spaces for tabs."
  (with-temp-buffer
    (insert "a\nb")
    (cj/indent-lines-in-region-or-buffer 1 t)
    (should (equal (buffer-string) "\ta\n\tb"))))

;;; cj/dedent-lines-in-region-or-buffer

(ert-deftest test-cte-dedent-lines-region ()
  "Normal: remove up to N leading whitespace chars per line in the region."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "    a\n    b\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/dedent-lines-in-region-or-buffer 4))
    (should (equal (buffer-string) "a\nb\n"))))

(ert-deftest test-cte-dedent-lines-whole-buffer-keeps-extra-whitespace ()
  "Boundary: only up to COUNT leading whitespace chars are removed."
  (with-temp-buffer
    (insert "      a\n      b")
    (cj/dedent-lines-in-region-or-buffer 2)
    (should (equal (buffer-string) "    a\n    b"))))

(provide 'test-custom-text-enclose-public-wrappers)
;;; test-custom-text-enclose-public-wrappers.el ends here
