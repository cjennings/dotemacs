;;; test-system-lib-executable-find-or-warn.el --- Tests for cj/executable-find-or-warn -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/executable-find-or-warn' generalizes the mail-specific
;; `cj/mail--executable-or-warn' pattern.  Returns the program's
;; executable path when found.  When the program is missing, returns
;; nil AND emits a `display-warning' naming the feature that's
;; unavailable, so the user gets a clear hint about what won't work.
;; The optional GROUP argument controls `display-warning' grouping so
;; per-feature filters keep working.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-lib)

(defmacro test-system-lib-find-or-warn--with-stubs (executable-fn warning-capture &rest body)
  "Run BODY with `executable-find' and `display-warning' stubbed.

EXECUTABLE-FN replaces `executable-find' (takes a string program name,
returns path or nil).  WARNING-CAPTURE is a symbol bound in BODY's scope
to a list of (TYPE MESSAGE LEVEL) entries collected from
`display-warning' calls."
  (declare (indent 2))
  `(let ((,warning-capture nil))
     (cl-letf (((symbol-function 'executable-find) ,executable-fn)
               ((symbol-function 'display-warning)
                (lambda (type message &optional level)
                  (push (list type message level) ,warning-capture))))
       ,@body)))

(ert-deftest test-cj-executable-find-or-warn-program-found-returns-path ()
  "Normal: when the program exists, return its path and emit no warning."
  (test-system-lib-find-or-warn--with-stubs
      (lambda (prog) (when (string= prog "mbsync") "/usr/bin/mbsync"))
      warnings
    (let ((result (cj/executable-find-or-warn "mbsync" "mu4e mail sync")))
      (should (equal result "/usr/bin/mbsync"))
      (should-not warnings))))

(ert-deftest test-cj-executable-find-or-warn-program-missing-returns-nil ()
  "Normal: when the program is missing, return nil."
  (test-system-lib-find-or-warn--with-stubs
      (lambda (_prog) nil)
      _warnings
    (should-not (cj/executable-find-or-warn "missing-prog" "fake feature"))))

(ert-deftest test-cj-executable-find-or-warn-missing-emits-warning ()
  "Normal: a missing program produces a warning that names PROGRAM and FEATURE."
  (test-system-lib-find-or-warn--with-stubs
      (lambda (_prog) nil)
      warnings
    (cj/executable-find-or-warn "missing-prog" "fake feature")
    (should (= 1 (length warnings)))
    (let ((entry (car warnings)))
      (should (string-match-p "missing-prog" (nth 1 entry)))
      (should (string-match-p "fake feature" (nth 1 entry)))
      (should (eq :warning (nth 2 entry))))))

(ert-deftest test-cj-executable-find-or-warn-default-group ()
  "Boundary: with no GROUP arg, the warning uses the default `cj/system-lib' symbol."
  (test-system-lib-find-or-warn--with-stubs
      (lambda (_prog) nil)
      warnings
    (cj/executable-find-or-warn "missing-prog" "feature")
    (should (eq 'cj/system-lib (car (car warnings))))))

(ert-deftest test-cj-executable-find-or-warn-explicit-group ()
  "Normal: an explicit GROUP arg flows through to `display-warning'."
  (test-system-lib-find-or-warn--with-stubs
      (lambda (_prog) nil)
      warnings
    (cj/executable-find-or-warn "missing-prog" "feature" 'mail-config)
    (should (eq 'mail-config (car (car warnings))))))

(provide 'test-system-lib-executable-find-or-warn)
;;; test-system-lib-executable-find-or-warn.el ends here
