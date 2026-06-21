;;; test-keybindings--jump-open-var.el --- Tests for cj/jump-open-var -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/jump-open-var' in keybindings.el.  The helper takes a
;; variable symbol, validates that the variable is bound, holds a
;; non-empty string, and points at an existing file, then opens it via
;; `find-file'.  Each failure path raises `user-error'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)

(defvar test-keybindings--fixture-var nil
  "Shared fixture variable for `cj/jump-open-var' tests.
Declared at top level so `let'-binding inside tests sees it as special
under lexical-binding.")

(defmacro test-keybindings--with-find-file-mock (capture-var &rest body)
  "Run BODY with `find-file' replaced by a mock that captures its arg.
CAPTURE-VAR is set to the path passed to `find-file', or stays nil if
the mock is never called."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'find-file)
              (lambda (path &rest _) (setq ,capture-var path))))
     ,@body))

(defmacro test-keybindings--with-fixture (value &rest body)
  "Bind `test-keybindings--fixture-var' to VALUE for BODY, then reset."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn (setq test-keybindings--fixture-var ,value) ,@body)
     (setq test-keybindings--fixture-var nil)))

;;; Normal cases

(ert-deftest test-keybindings-jump-open-var-existing-file-calls-find-file ()
  "Normal: bound var holding a path to an existing file calls `find-file'."
  (let ((tmp (make-temp-file "jump-test-")))
    (unwind-protect
        (test-keybindings--with-fixture tmp
          (let (captured)
            (test-keybindings--with-find-file-mock captured
              (cj/jump-open-var 'test-keybindings--fixture-var)
              (should (equal captured tmp)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; Boundary / Error cases

(ert-deftest test-keybindings-jump-open-var-unbound-raises ()
  "Error: an unbound variable raises `user-error' naming the variable."
  (let ((sym (make-symbol "test-keybindings--unbound")))
    (let ((err (should-error (cj/jump-open-var sym) :type 'user-error)))
      (should (string-match-p (symbol-name sym)
                              (error-message-string err))))))

(ert-deftest test-keybindings-jump-open-var-nil-value-raises ()
  "Error: a variable bound to nil raises `user-error'."
  (test-keybindings--with-fixture nil
    (should-error (cj/jump-open-var 'test-keybindings--fixture-var)
                  :type 'user-error)))

(ert-deftest test-keybindings-jump-open-var-non-string-raises ()
  "Error: a variable bound to a non-string raises `user-error'."
  (test-keybindings--with-fixture 42
    (should-error (cj/jump-open-var 'test-keybindings--fixture-var)
                  :type 'user-error)))

(ert-deftest test-keybindings-jump-open-var-empty-string-raises ()
  "Error: a variable bound to the empty string raises `user-error'."
  (test-keybindings--with-fixture ""
    (should-error (cj/jump-open-var 'test-keybindings--fixture-var)
                  :type 'user-error)))

(ert-deftest test-keybindings-jump-open-var-missing-file-raises ()
  "Error: a path to a non-existent file raises `user-error' naming the path."
  (let ((missing-path (concat temporary-file-directory
                              "definitely-not-here-"
                              (number-to-string (random 1000000)) ".org")))
    (should-not (file-exists-p missing-path))
    (test-keybindings--with-fixture missing-path
      (let ((err (should-error
                  (cj/jump-open-var 'test-keybindings--fixture-var)
                  :type 'user-error)))
        (should (string-match-p (regexp-quote missing-path)
                                (error-message-string err)))))))

(provide 'test-keybindings--jump-open-var)
;;; test-keybindings--jump-open-var.el ends here
