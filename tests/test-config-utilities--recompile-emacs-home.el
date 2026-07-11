;;; test-config-utilities--recompile-emacs-home.el --- Tests for cj/--recompile-emacs-home -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--recompile-emacs-home'.  The internal takes DIR and
;; an optional NATIVE-P flag, deletes every .elc/.eln file under DIR,
;; removes the eln (native) or elc (byte) cache directory if present,
;; then triggers the appropriate recompile.  Returns \\='native or
;; \\='byte.
;;
;; Tests run against real temp directories.  Only the actual compile
;; invocations (native-compile-async, byte-recompile-directory) are
;; mocked at the boundary so the test suite doesn't spin up real
;; compilation jobs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(defvar comp-async-report-warnings-errors)

(defun test-config-utilities--make-recompile-fixture ()
  "Create a temp dir with a mix of .el / .elc / .eln files for recompile tests.
Returns the temp dir path."
  (let ((dir (make-temp-file "recompile-test-" t)))
    (dolist (rel '("a.el" "a.elc" "b.el" "b.eln" "sub/c.el" "sub/c.elc"))
      (let ((full (expand-file-name rel dir)))
        (let ((parent (file-name-directory full)))
          (when (and parent (not (file-exists-p parent)))
            (make-directory parent t)))
        (with-temp-file full (insert ""))))
    dir))

(ert-deftest test-config-utilities-recompile-native-path-returns-native ()
  "Normal: NATIVE-P non-nil dispatches to native-compile-async and returns 'native."
  (let* ((dir (test-config-utilities--make-recompile-fixture))
         (native-called-with nil)
         (byte-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'native-compile-async)
                   (lambda (target &rest _) (setq native-called-with target)))
                  ((symbol-function 'byte-recompile-directory)
                   (lambda (&rest _) (setq byte-called t))))
          (let ((result (cj/--recompile-emacs-home dir t)))
            (should (eq result 'native))
            (should (equal native-called-with dir))
            (should-not byte-called)))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-recompile-byte-path-returns-byte ()
  "Normal: NATIVE-P nil dispatches to byte-recompile-directory and returns 'byte."
  (let* ((dir (test-config-utilities--make-recompile-fixture))
         (byte-called-with nil)
         (native-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'byte-recompile-directory)
                   (lambda (target &rest _) (setq byte-called-with target)))
                  ((symbol-function 'native-compile-async)
                   (lambda (&rest _) (setq native-called t))))
          (let ((result (cj/--recompile-emacs-home dir nil)))
            (should (eq result 'byte))
            (should (equal byte-called-with dir))
            (should-not native-called)))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-recompile-deletes-all-compiled-files-recursively ()
  "Normal: every .elc and .eln under DIR (including subdirs) is deleted before compile."
  (let ((dir (test-config-utilities--make-recompile-fixture)))
    (unwind-protect
        (cl-letf (((symbol-function 'native-compile-async) (lambda (&rest _) nil))
                  ((symbol-function 'byte-recompile-directory) (lambda (&rest _) nil)))
          (cj/--recompile-emacs-home dir nil)
          (should (file-exists-p (expand-file-name "a.el" dir)))
          (should (file-exists-p (expand-file-name "b.el" dir)))
          (should (file-exists-p (expand-file-name "sub/c.el" dir)))
          (should-not (file-exists-p (expand-file-name "a.elc" dir)))
          (should-not (file-exists-p (expand-file-name "b.eln" dir)))
          (should-not (file-exists-p (expand-file-name "sub/c.elc" dir))))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-recompile-removes-eln-cache-dir-on-native-path ()
  "Boundary: the native path removes the eln-cache directory when present.
The native cache is eln-cache/, not eln/, so that is the directory to clear."
  (let ((dir (test-config-utilities--make-recompile-fixture))
        (eln-cache-dir nil))
    (unwind-protect
        (progn
          (setq eln-cache-dir (expand-file-name "eln-cache" dir))
          (make-directory eln-cache-dir)
          (with-temp-file (expand-file-name "stale.eln" eln-cache-dir) (insert ""))
          (cl-letf (((symbol-function 'native-compile-async) (lambda (&rest _) nil)))
            (cj/--recompile-emacs-home dir t))
          (should-not (file-exists-p eln-cache-dir)))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-native-comp-detection-not-boundp ()
  "Regression: native-comp detection must not test `boundp' of the async
function -- native-compile-async is a function, so `boundp' is always nil and
native compilation would never be selected.  On a native-comp build, detection
returns non-nil."
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (should (cj/--native-comp-p))))

(ert-deftest test-config-utilities-recompile-removes-elc-dir-on-byte-path ()
  "Boundary: the byte path removes the elc cache directory when present."
  (let ((dir (test-config-utilities--make-recompile-fixture))
        (elc-dir nil))
    (unwind-protect
        (progn
          (setq elc-dir (expand-file-name "elc" dir))
          (make-directory elc-dir)
          (with-temp-file (expand-file-name "stale.elc" elc-dir) (insert ""))
          (cl-letf (((symbol-function 'byte-recompile-directory) (lambda (&rest _) nil)))
            (cj/--recompile-emacs-home dir nil))
          (should-not (file-exists-p elc-dir)))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-recompile-cache-dir-absent-no-error ()
  "Boundary: missing eln/elc cache directory is not an error."
  (let ((dir (test-config-utilities--make-recompile-fixture)))
    (unwind-protect
        (cl-letf (((symbol-function 'native-compile-async) (lambda (&rest _) nil)))
          ;; No eln/elc subdir created; should still succeed.
          (should (eq 'native (cj/--recompile-emacs-home dir t))))
      (delete-directory dir t))))

(provide 'test-config-utilities--recompile-emacs-home)
;;; test-config-utilities--recompile-emacs-home.el ends here
