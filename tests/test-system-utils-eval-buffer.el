;;; test-system-utils-eval-buffer.el --- Tests for cj/eval-buffer-with-confirmation-or-error-message -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/eval-buffer-with-confirmation-or-error-message'
;; in system-utils.el.  The function evaluates the current buffer and
;; reports success or any signaled error via `message', so errors
;; don't propagate to the caller.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

(defmacro test-sueb--capture-message (var &rest body)
  "Run BODY with `message' stubbed to set VAR to the formatted string."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
			  (lambda (fmt &rest args)
				(setq ,var (apply #'format fmt args)))))
	 ,@body))

;;; Normal cases

(ert-deftest test-sueb-valid-elisp-reports-success ()
  "Normal: evaluating valid elisp reports \"Buffer evaluated.\" via message."
  (let (captured)
	(test-sueb--capture-message captured
	  (with-temp-buffer
		(emacs-lisp-mode)
		(insert "(+ 1 2)")
		(cj/eval-buffer-with-confirmation-or-error-message)))
	(should (string-match-p "Buffer evaluated" captured))))

;;; Boundary cases

(ert-deftest test-sueb-empty-buffer-reports-success ()
  "Boundary: an empty buffer evaluates to nil cleanly and reports success."
  (let (captured)
	(test-sueb--capture-message captured
	  (with-temp-buffer
		(emacs-lisp-mode)
		(cj/eval-buffer-with-confirmation-or-error-message)))
	(should (string-match-p "Buffer evaluated" captured))))

;;; Error cases

(ert-deftest test-sueb-error-caught-not-propagated ()
  "Error: evaluation of broken code reports the error via message without signaling.
Proves the condition-case wrapper catches errors rather than propagating them."
  (let (captured)
	(test-sueb--capture-message captured
	  (with-temp-buffer
		(emacs-lisp-mode)
		(insert "(this-function-does-not-exist-for-testing)")
		(cj/eval-buffer-with-confirmation-or-error-message)))
	(should (string-match-p "Error occurred during evaluation" captured))))

(provide 'test-system-utils-eval-buffer)
;;; test-system-utils-eval-buffer.el ends here
