;;; test-keybindings--jump-commands.el --- Smoke tests for the auto-generated jump commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for the `cj/jump-to-NAME' commands that the `dolist' over
;; `cj/jump--specs' generates in keybindings.el.  Each spec entry is
;; (KEY NAME-SYM VAR-SYM); the loop creates a defalias and binds it
;; under `cj/jump-map'.  These tests assert the wiring without exercising
;; the underlying I/O.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)

(ert-deftest test-keybindings-jump-commands-each-spec-has-fbound-command ()
  "Smoke: each `cj/jump--specs' entry has an fbound `cj/jump-to-NAME'."
  (dolist (spec cj/jump--specs)
    (pcase-let ((`(,_key ,name ,_var) spec))
      (let ((fn (intern (format "cj/jump-to-%s" name))))
        (should (fboundp fn))))))

(ert-deftest test-keybindings-jump-commands-each-spec-bound-in-jump-map ()
  "Smoke: each `cj/jump--specs' entry's key is bound to its command in
`cj/jump-map'."
  (dolist (spec cj/jump--specs)
    (pcase-let ((`(,key ,name ,_var) spec))
      (let ((fn (intern (format "cj/jump-to-%s" name))))
        (should (eq fn (lookup-key cj/jump-map (kbd key))))))))

(ert-deftest test-keybindings-jump-commands-each-calls-jump-open-var-with-spec-var ()
  "Smoke: each `cj/jump-to-NAME' calls `cj/jump-open-var' with the var
from its spec entry, in spec order."
  (let (calls)
    (cl-letf (((symbol-function 'cj/jump-open-var)
               (lambda (var) (push var calls))))
      (dolist (spec cj/jump--specs)
        (pcase-let ((`(,_key ,name ,_var) spec))
          (funcall (intern (format "cj/jump-to-%s" name))))))
    (let ((expected-vars (mapcar (lambda (spec) (nth 2 spec)) cj/jump--specs)))
      (should (equal (nreverse calls) expected-vars)))))

(ert-deftest test-keybindings-jump-map-mounted-under-custom-keymap ()
  "Smoke: `cj/jump-map' is mounted at \"j\" under `cj/custom-keymap'."
  (should (eq cj/jump-map (lookup-key cj/custom-keymap (kbd "j")))))

(provide 'test-keybindings--jump-commands)
;;; test-keybindings--jump-commands.el ends here
