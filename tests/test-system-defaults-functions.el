;;; test-system-defaults-functions.el --- Tests for the helper functions in system-defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; system-defaults.el is mostly `setq' configuration but ships a handful
;; of small interactive / hook helpers:
;;
;;   cj/disabled                  -- no-op stub used by `defalias' for
;;                                   commands we don't want surfaced
;;   cj/minibuffer-setup-hook     -- inflate gc-cons-threshold while
;;                                   typing in the minibuffer
;;   cj/minibuffer-exit-hook      -- restore gc-cons-threshold on exit
;;   unpropertize-kill-ring       -- strip text properties from
;;                                   kill-ring at shutdown
;;   cj/log-comp-warning          -- route native-comp warnings to a
;;                                   file rather than the *Warnings*
;;                                   buffer
;;
;; Loaded via the sandbox in test-system-defaults-vc-follow-symlinks.el
;; -- the module has startup side effects that we stub there.

;;; Code:

(require 'cl-lib)
(require 'autorevert)
(require 'bookmark)
(require 'ert)
(require 'server)
(require 'vc-hooks)

(defvar org-dir nil)
(defvar user-home-dir nil)
(defvar use-package-always-ensure nil)

(defconst test-system-defaults-functions--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for system-defaults function tests.")

(defmacro test-system-defaults-functions--with-load-environment (&rest body)
  "Load system-defaults.el under a sandbox, then evaluate BODY."
  `(let ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "system-defaults-fn-emacs-" t)))
         (user-home-dir (file-name-as-directory
                         (make-temp-file "system-defaults-fn-home-" t)))
         (org-dir (file-name-as-directory
                   (make-temp-file "system-defaults-fn-org-" t)))
         (use-package-always-ensure nil))
     (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
               ((symbol-function 'server-start) #'ignore)
               ((symbol-function 'set-locale-environment) #'ignore)
               ((symbol-function 'prefer-coding-system) #'ignore)
               ((symbol-function 'set-default-coding-systems) #'ignore)
               ((symbol-function 'set-terminal-coding-system) #'ignore)
               ((symbol-function 'set-keyboard-coding-system) #'ignore)
               ((symbol-function 'set-selection-coding-system) #'ignore)
               ((symbol-function 'set-charset-priority) #'ignore)
               ((symbol-function 'global-auto-revert-mode) #'ignore)
               ((symbol-function 'recentf-mode) #'ignore))
       (unless (fboundp 'use-package)
         (defmacro use-package (&rest _args) nil))
       (load (expand-file-name "modules/system-defaults.el"
                               test-system-defaults-functions--repo-root)
             nil t)
       ,@body)))

;;; cj/disabled

(ert-deftest test-system-defaults-disabled-normal-returns-nil ()
  "Normal: `cj/disabled' is a silent interactive no-op."
  (test-system-defaults-functions--with-load-environment
   (should (eq (cj/disabled) nil))
   (should (commandp #'cj/disabled))))

;;; cj/minibuffer-setup-hook / cj/minibuffer-exit-hook

(ert-deftest test-system-defaults-minibuffer-setup-inflates-gc-threshold ()
  "Normal: entering the minibuffer raises `gc-cons-threshold' to most-positive-fixnum."
  (test-system-defaults-functions--with-load-environment
   (let ((gc-cons-threshold 800000))
     (cj/minibuffer-setup-hook)
     (should (= gc-cons-threshold most-positive-fixnum)))))

(ert-deftest test-system-defaults-minibuffer-exit-restores-gc-threshold ()
  "Normal: leaving the minibuffer restores `gc-cons-threshold' to 800000."
  (test-system-defaults-functions--with-load-environment
   (let ((gc-cons-threshold most-positive-fixnum))
     (cj/minibuffer-exit-hook)
     (should (= gc-cons-threshold 800000)))))

;;; unpropertize-kill-ring

(ert-deftest test-system-defaults-unpropertize-kill-ring-strips-properties ()
  "Normal: every kill-ring entry comes back with no text properties."
  (test-system-defaults-functions--with-load-environment
   (let ((kill-ring (list (propertize "alpha" 'face 'bold)
                          (propertize "beta" 'face 'underline))))
     (unpropertize-kill-ring)
     (should (equal kill-ring '("alpha" "beta")))
     (should-not (text-properties-at 0 (nth 0 kill-ring)))
     (should-not (text-properties-at 0 (nth 1 kill-ring))))))

(ert-deftest test-system-defaults-unpropertize-kill-ring-boundary-empty-ring ()
  "Boundary: an empty `kill-ring' stays empty after the strip pass."
  (test-system-defaults-functions--with-load-environment
   (let ((kill-ring nil))
     (unpropertize-kill-ring)
     (should (null kill-ring)))))

;;; cj/log-comp-warning

(ert-deftest test-system-defaults-log-comp-warning-writes-log-line ()
  "Normal: a TYPE containing `comp' writes a timestamped line to the log."
  (test-system-defaults-functions--with-load-environment
   (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
     (unwind-protect
         (progn
           (cj/log-comp-warning 'comp "hello %s" "world")
           (with-temp-buffer
             (insert-file-contents comp-warnings-log)
             (let ((contents (buffer-string)))
               (should (string-match-p "hello world" contents))
               ;; Bracketed timestamp prefix.
               (should (string-match-p "^\\[" contents)))))
       (delete-file comp-warnings-log)))))

(ert-deftest test-system-defaults-log-comp-warning-list-type-includes-comp ()
  "Boundary: a list TYPE matches when `comp' is one of its elements."
  (test-system-defaults-functions--with-load-environment
   (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
     (unwind-protect
         (progn
           (cj/log-comp-warning '(comp warning) "alpha")
           (with-temp-buffer
             (insert-file-contents comp-warnings-log)
             (should (string-match-p "alpha" (buffer-string)))))
       (delete-file comp-warnings-log)))))

(ert-deftest test-system-defaults-log-comp-warning-non-comp-type-is-noop ()
  "Boundary: a TYPE that doesn't include `comp' returns nil and writes nothing.

`display-warning' interprets nil as \"I didn't handle it\" -- so the
default *Warnings* buffer path keeps working for unrelated warnings."
  (test-system-defaults-functions--with-load-environment
   (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
     (unwind-protect
         (progn
           (should (null (cj/log-comp-warning 'unrelated "ignored")))
           (with-temp-buffer
             (insert-file-contents comp-warnings-log)
             (should (string-empty-p (buffer-string)))))
       (delete-file comp-warnings-log)))))

(provide 'test-system-defaults-functions)
;;; test-system-defaults-functions.el ends here
