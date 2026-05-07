;;; test-nerd-icons-config--apply-tint.el --- Tests for cj/nerd-icons-apply-tint -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the bulk-tint helper. Mocks `set-face-foreground' and `facep'
;; at the framework boundary so the tests don't depend on nerd-icons being
;; loaded — only on the symbol list and the dispatch logic.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nerd-icons-config)

(defmacro test-nerd-icons-config--capture-set-face-foreground (calls-var &rest body)
  "Run BODY with `set-face-foreground' and `facep' stubbed.
Each (face color) pair gets pushed onto CALLS-VAR. `facep' returns t
for every symbol so all faces in the list count as defined."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'set-face-foreground)
              (lambda (face color &rest _) (push (cons face color) ,calls-var)))
             ((symbol-function 'facep)
              (lambda (_) t)))
     ,@body))

(ert-deftest test-nerd-icons-config--apply-tint-covers-every-face ()
  "Normal: apply-tint calls set-face-foreground once per face in the list."
  (let ((calls nil))
    (test-nerd-icons-config--capture-set-face-foreground calls
      (cj/nerd-icons-apply-tint "test-color"))
    (should (= (length calls) (length cj/--nerd-icons-color-faces)))
    (dolist (face cj/--nerd-icons-color-faces)
      (should (assq face calls)))))

(ert-deftest test-nerd-icons-config--apply-tint-passes-color-arg ()
  "Normal: apply-tint forwards COLOR to every set-face-foreground call."
  (let ((calls nil))
    (test-nerd-icons-config--capture-set-face-foreground calls
      (cj/nerd-icons-apply-tint "rebeccapurple"))
    (dolist (call calls)
      (should (equal (cdr call) "rebeccapurple")))))

(ert-deftest test-nerd-icons-config--apply-tint-defaults-to-customvar ()
  "Normal: with no COLOR arg, uses `cj/nerd-icons-tint-color'."
  (let ((calls nil))
    (test-nerd-icons-config--capture-set-face-foreground calls
      (let ((cj/nerd-icons-tint-color "default-test-color"))
        (cj/nerd-icons-apply-tint)))
    (should (cl-every (lambda (call) (equal (cdr call) "default-test-color")) calls))))

(ert-deftest test-nerd-icons-config--apply-tint-skips-undefined-faces ()
  "Boundary: faces that fail `facep' are silently skipped, not errored."
  (let ((calls nil))
    (cl-letf (((symbol-function 'set-face-foreground)
               (lambda (face color &rest _) (push (cons face color) calls)))
              ((symbol-function 'facep)
               (lambda (_) nil)))
      (cj/nerd-icons-apply-tint "any"))
    (should (null calls))))

(provide 'test-nerd-icons-config--apply-tint)
;;; test-nerd-icons-config--apply-tint.el ends here
