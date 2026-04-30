;;; test-config-utilities--with-timer.el --- Tests for the with-timer macro -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the `with-timer' macro from config-utilities.el. The macro
;; runs FORMS, prints a timing message, and returns the value of FORMS.
;; These tests verify that the macro preserves the result through the
;; timing wrapper and that the announce/done messages fire.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(ert-deftest test-config-utilities-with-timer-returns-forms-value ()
  "Normal: with-timer returns the value of the wrapped FORMS."
  (let ((inhibit-message t))
    (should (= 42 (with-timer "compute"
                    (+ 40 2))))))

(ert-deftest test-config-utilities-with-timer-evaluates-forms-once ()
  "Boundary: with-timer evaluates the body exactly once.
Guards against the macro accidentally re-evaluating FORMS for the
elapsed-time message or similar."
  (let ((counter 0)
        (inhibit-message t))
    (with-timer "count"
      (cl-incf counter))
    (should (= 1 counter))))

(ert-deftest test-config-utilities-with-timer-emits-announce-and-done-messages ()
  "Boundary: with-timer emits both \"TITLE...\" and \"TITLE... done\" messages."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-timer "tag"
        :ignored))
    (let ((all (string-join (nreverse messages) "\n")))
      (should (string-match-p "^tag\\.\\.\\.$" all))
      (should (string-match-p "^tag\\.\\.\\. done (" all)))))

(ert-deftest test-config-utilities-with-timer-multiple-forms-returns-last ()
  "Boundary: with-timer with multiple forms returns the last form's value."
  (let ((inhibit-message t))
    (should (equal "third"
                   (with-timer "multi"
                     "first"
                     "second"
                     "third")))))

(provide 'test-config-utilities--with-timer)
;;; test-config-utilities--with-timer.el ends here
