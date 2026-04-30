;;; test-config-utilities--benchmark-method.el --- Tests for cj/--benchmark-method -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--benchmark-method'.  The internal takes a title
;; string and a method symbol, validates fboundp, and runs the
;; symbol's funcall through `with-timer'.  Returns whatever the
;; funcall returns; signals `user-error' on invalid input.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(ert-deftest test-config-utilities-benchmark-method-runs-valid-symbol ()
  "Normal: a valid fboundp symbol is called once and its return value
is propagated through the timer wrapper."
  (let ((called 0)
        (inhibit-message t))
    (defalias 'test-cu-benchmark-target (lambda () (cl-incf called) 42))
    (unwind-protect
        (should (= 42 (cj/--benchmark-method "x" 'test-cu-benchmark-target)))
      (fmakunbound 'test-cu-benchmark-target))
    (should (= 1 called))))

(ert-deftest test-config-utilities-benchmark-method-nil-symbol-raises ()
  "Error: a nil method symbol raises `user-error'."
  (let ((inhibit-message t))
    (should-error (cj/--benchmark-method "x" nil) :type 'user-error)))

(ert-deftest test-config-utilities-benchmark-method-unbound-symbol-raises ()
  "Error: a symbol that is not fboundp raises `user-error' naming the symbol."
  (let* ((sym (make-symbol "test-cu-not-bound"))
         (err (let ((inhibit-message t))
                (should-error (cj/--benchmark-method "x" sym)
                              :type 'user-error))))
    (should (string-match-p (symbol-name sym)
                            (error-message-string err)))))

(ert-deftest test-config-utilities-benchmark-method-emits-timer-messages ()
  "Boundary: the announce and done messages from with-timer fire."
  (let (messages)
    (defalias 'test-cu-benchmark-noisy (lambda () nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (cj/--benchmark-method "ZZZ" 'test-cu-benchmark-noisy))
      (fmakunbound 'test-cu-benchmark-noisy))
    (let ((all (string-join (nreverse messages) "\n")))
      (should (string-match-p "^ZZZ\\.\\.\\.$" all))
      (should (string-match-p "^ZZZ\\.\\.\\. done (" all)))))

(provide 'test-config-utilities--benchmark-method)
;;; test-config-utilities--benchmark-method.el ends here
