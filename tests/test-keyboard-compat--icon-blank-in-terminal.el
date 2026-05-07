;;; test-keyboard-compat--icon-blank-in-terminal.el --- Tests for cj/--icon-blank-in-terminal -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the per-call icon-rendering gate that replaced the load-time
;; defun-redefinition. The helper must dispatch on the current frame's
;; `display-graphic-p' so the same daemon can serve real icons to GUI frames
;; and blanks to terminal frames.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keyboard-compat)

(defmacro test-keyboard-compat--with-graphic-p (graphic-p &rest body)
  "Run BODY with `display-graphic-p' stubbed to GRAPHIC-P."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional _) ,graphic-p)))
     ,@body))

(ert-deftest test-keyboard-compat--icon-blank-in-terminal-passes-through-on-gui ()
  "Normal: returns ORIG's value when display-graphic-p is non-nil."
  (test-keyboard-compat--with-graphic-p t
    (let ((orig (lambda (&rest _) "ICON")))
      (should (equal (cj/--icon-blank-in-terminal orig "foo.txt") "ICON")))))

(ert-deftest test-keyboard-compat--icon-blank-in-terminal-blank-on-terminal ()
  "Boundary: returns empty string when display-graphic-p is nil."
  (test-keyboard-compat--with-graphic-p nil
    (let ((orig (lambda (&rest _) "ICON")))
      (should (equal (cj/--icon-blank-in-terminal orig "foo.txt") "")))))

(ert-deftest test-keyboard-compat--icon-blank-in-terminal-forwards-args ()
  "Normal: forwards all ARGS to ORIG when graphical."
  (test-keyboard-compat--with-graphic-p t
    (let ((received nil))
      (cj/--icon-blank-in-terminal
       (lambda (&rest args) (setq received args) "ok")
       'a 'b 'c)
      (should (equal received '(a b c))))))

(ert-deftest test-keyboard-compat--icon-blank-in-terminal-does-not-call-orig-on-terminal ()
  "Error: ORIG must not be invoked when display-graphic-p is nil."
  (test-keyboard-compat--with-graphic-p nil
    (let ((called nil))
      (cj/--icon-blank-in-terminal
       (lambda (&rest _) (setq called t) "ICON"))
      (should-not called))))

(provide 'test-keyboard-compat--icon-blank-in-terminal)
;;; test-keyboard-compat--icon-blank-in-terminal.el ends here
