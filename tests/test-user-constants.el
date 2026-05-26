;;; test-user-constants.el --- Tests for user-constants path init -*- lexical-binding: t; -*-

;;; Commentary:

;; user-constants defines the config's path constants and creates the
;; configured directories/files.  After the split, loading the module is
;; side-effect-free: creation happens only when
;; cj/initialize-user-directories-and-files is called (from init.el on real
;; startup).  These tests sandbox user-emacs-directory and user-home-dir so the
;; path defconsts resolve into temp dirs, then check that loading creates
;; nothing, the initializer creates what it should, and the verify-or-create
;; helpers report required vs optional failures differently.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Declare special so the macro's let-bindings are dynamic — the module
;; `defvar's user-home-dir at load, which errors if the var is lexically bound.
;; user-emacs-directory is already special (built-in).
(defvar user-home-dir)

(defconst test-user-constants--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root, derived from this file's location under tests/.")

(defun test-user-constants--load ()
  "Load user-constants.el fresh from the repo."
  (load (expand-file-name "modules/user-constants.el"
                          test-user-constants--repo-root)
        nil t))

(defmacro test-user-constants--with-sandbox (&rest body)
  "Load user-constants with paths redirected to temp dirs, run BODY, restore.
The path defconsts are recomputed against temp `user-home-dir' /
`user-emacs-directory' so BODY can create into the sandbox.  Afterwards the
module is reloaded against the real paths so the globals are not left pointing
at deleted temp directories."
  (declare (indent 0))
  `(let ((home (file-name-as-directory (make-temp-file "uc-home-" t)))
         (emacs (file-name-as-directory (make-temp-file "uc-emacs-" t))))
     (unwind-protect
         (let ((user-home-dir home)
               (user-emacs-directory emacs))
           (test-user-constants--load)
           ,@body)
       (test-user-constants--load)
       (ignore-errors (delete-directory home t))
       (ignore-errors (delete-directory emacs t)))))

;;; Loading is side-effect-free

(ert-deftest test-user-constants-loading-creates-no-files ()
  "Normal: loading the module creates no directories or files.
The whole point of the split — a bare require must not touch the filesystem."
  (test-user-constants--with-sandbox
    (should-not (file-exists-p (expand-file-name "sync" home)))
    (should-not (file-exists-p (expand-file-name "org" sync-dir)))
    (should-not (file-exists-p gcal-file))
    (should-not (file-exists-p schedule-file))))

;;; The initializer creates the configured paths

(ert-deftest test-user-constants-initialize-creates-dirs-and-files ()
  "Normal: the initializer creates the backbone dirs and the configured files."
  (test-user-constants--with-sandbox
    (cj/initialize-user-directories-and-files)
    (should (file-directory-p sync-dir))
    (should (file-directory-p org-dir))
    (should (file-directory-p roam-dir))
    (should (file-exists-p gcal-file))
    (should (file-exists-p pcal-file))
    (should (file-exists-p dcal-file))
    (should (file-exists-p schedule-file))
    (should (file-exists-p inbox-file))))

;;; verify-or-create failure reporting (required vs optional)

;; A path under a nonexistent root makes file-directory-p / file-exists-p return
;; nil naturally, so the only stub needed is make-directory failing — no need to
;; redefine core predicates (which trips native-comp trampolines).

(ert-deftest test-user-constants-verify-dir-optional-failure-logs ()
  "Error: an optional directory failure is logged, never warned or signalled."
  (test-user-constants--load)
  (let ((warned nil) (messaged nil))
    (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) (error "boom")))
              ((symbol-function 'display-warning) (lambda (&rest _) (setq warned t)))
              ((symbol-function 'message) (lambda (&rest _) (setq messaged t))))
      (cj/verify-or-create-dir "/nonexistent-uc-test/optional")
      (should messaged)
      (should-not warned))))

(ert-deftest test-user-constants-verify-dir-required-failure-warns ()
  "Error: a required directory failure raises a prominent user-constants warning."
  (test-user-constants--load)
  (let ((warn-args nil))
    (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) (error "boom")))
              ((symbol-function 'display-warning)
               (lambda (group _msg &optional level) (setq warn-args (list group level)))))
      (cj/verify-or-create-dir "/nonexistent-uc-test/required" t)
      (should (eq (nth 0 warn-args) 'user-constants))
      (should (eq (nth 1 warn-args) :error)))))

(ert-deftest test-user-constants-verify-file-required-failure-warns ()
  "Error: a required file failure raises a prominent user-constants warning."
  (test-user-constants--load)
  (let ((dir (make-temp-file "uc-reqfile-" t))
        (warn-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region) (lambda (&rest _) (error "boom")))
                  ((symbol-function 'display-warning)
                   (lambda (group _msg &optional level) (setq warn-args (list group level)))))
          ;; dir exists so the failure is the write, not the parent directory
          (cj/verify-or-create-file (expand-file-name "required.org" dir) t)
          (should (eq (nth 0 warn-args) 'user-constants))
          (should (eq (nth 1 warn-args) :error)))
      (delete-directory dir t))))

(provide 'test-user-constants)
;;; test-user-constants.el ends here
