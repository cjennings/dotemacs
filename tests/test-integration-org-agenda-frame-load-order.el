;;; test-integration-org-agenda-frame-load-order.el --- Frame allowlist survives load order -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression test for a load-order bug in the Full Agenda frame's read-only
;; shadow.
;;
;; Components integrated:
;; - org-agenda-frame (real, loaded in a subprocess)
;; - org-agenda (real, loaded BEFORE the frame module to reproduce the bug)
;;
;; The bug: cj/--agenda-frame-shadow-mutations walks org-agenda-mode-map and
;; keeps a key only when the frame map already binds it to a `commandp' value.
;; The view/redo handlers (day-view, week-view, safe-redo) were defined LOWER in
;; the file than the `with-eval-after-load' that ran the walk.  When org-agenda
;; was already loaded at frame-load time -- the normal startup order and every
;; reload -- the walk fired before those defuns existed, read them as not-yet
;; commands, and denied d/w/g/r, the very keys the allowlist grants.  Moving the
;; walk to the end of the file (after the defuns) fixed it.
;;
;; This test can't reproduce the ordering in-process (the module is already
;; loaded), so it drives a fresh Emacs that requires org-agenda first, then the
;; frame module, and inspects the resulting keymap.
;;
;; Validates:
;; - d/w/g/r keep their allowlist commands in the org-first load order
;; - a real mutation key (t) is still denied (the read-only guarantee holds)
;;
;;; Code:

(require 'ert)

(defconst test-oaf--repo-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name))))
  "Repo root, one level up from tests/.")

(defun test-oaf--lookup-in-subprocess (keys)
  "Load org-agenda then org-agenda-frame in a fresh Emacs, return KEYS' bindings.
Returns an alist of (KEY . BINDING-SYMBOL-NAME-OR-nil)."
  (let* ((root test-oaf--repo-root)
         (form
          (prin1-to-string
           `(progn
              (setq load-prefer-newer t)
              (package-initialize)
              (require 'org-agenda)          ; the bad order: org first
              (require 'org-agenda-frame)
              (princ (prin1-to-string
                      (mapcar
                       (lambda (k)
                         (cons k (let ((b (lookup-key cj/agenda-frame-mode-map (kbd k))))
                                   (and (symbolp b) (symbol-name b)))))
                       ',keys))))))
         (out (with-output-to-string
                (with-current-buffer standard-output
                  (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil
                   "--batch" "--no-site-file" "--no-site-lisp"
                   "-L" root
                   "-L" (expand-file-name "modules" root)
                   "-L" (expand-file-name "themes" root)
                   "--eval" form)))))
    (car (read-from-string out))))

(ert-deftest test-integration-org-agenda-frame-allowlist-survives-org-first-load ()
  "Integration: with org-agenda loaded before the frame module, the allowlisted
view/redo keys keep their commands and a mutation key stays denied."
  (skip-unless (file-exists-p (expand-file-name "modules/org-agenda-frame.el"
                                                test-oaf--repo-root)))
  (let ((got (test-oaf--lookup-in-subprocess '("d" "w" "g" "r" "t"))))
    (should (equal "cj/--agenda-frame-day-view"  (cdr (assoc "d" got))))
    (should (equal "cj/--agenda-frame-week-view" (cdr (assoc "w" got))))
    (should (equal "cj/--agenda-frame-safe-redo" (cdr (assoc "g" got))))
    (should (equal "cj/--agenda-frame-safe-redo" (cdr (assoc "r" got))))
    ;; t is a real org mutation key; it must be denied, not allowlisted.
    (should (equal "cj/--agenda-frame-denied-readonly" (cdr (assoc "t" got))))))

(provide 'test-integration-org-agenda-frame-load-order)
;;; test-integration-org-agenda-frame-load-order.el ends here
