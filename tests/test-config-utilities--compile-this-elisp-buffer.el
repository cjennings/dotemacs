;;; test-config-utilities--compile-this-elisp-buffer.el --- Tests for cj/compile-this-elisp-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/compile-this-elisp-buffer' and its helper
;; `cj/--compile-elisp-file'.  The helper dispatches among
;; native-compile-async, native-compile (sync), and byte-compile-file based
;; on an AVAILABLE-P predicate that defaults to `fboundp'.  Tests force each
;; branch by passing the predicate, never by redefining `fboundp': an `fset'
;; on that subr autoloads comp-run, which requires bytecomp, whose `defun' of
;; `byte-compile-file' replaces any test double installed earlier in the same
;; `cl-letf' (Emacs 30.2 hid this because ert happened to preload bytecomp).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(defun test-config-utilities--available (&rest syms)
  "Return a predicate that reports only SYMS as available compilers."
  (lambda (sym) (memq sym syms)))

(defmacro test-config-utilities--with-elisp-buffer (path &rest body)
  "Run BODY in a temp buffer visiting PATH (a .el file path).
Skips the interactive `save-buffer' so tests stay free of disk side
effects."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name ,path)
     (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
       ,@body)))

;; -- the interactive wrapper -------------------------------------------------

(ert-deftest test-config-utilities-compile-buffer-not-elisp-raises ()
  "Error: a buffer whose file isn't .el raises `user-error'."
  (test-config-utilities--with-elisp-buffer "/tmp/not-elisp.txt"
    (should-error (cj/compile-this-elisp-buffer) :type 'user-error)))

(ert-deftest test-config-utilities-compile-buffer-no-file-raises ()
  "Boundary: a buffer visiting no file raises `user-error' rather than
passing nil to the compiler."
  (with-temp-buffer
    (should-error (cj/compile-this-elisp-buffer) :type 'user-error)))

(ert-deftest test-config-utilities-compile-buffer-saves-then-delegates ()
  "Normal: the wrapper saves the buffer and hands its file to the helper."
  (let (saved compiled)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/some.el")
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (setq saved t)))
                ((symbol-function 'cj/--compile-elisp-file)
                 (lambda (file &optional _) (setq compiled file))))
        (cj/compile-this-elisp-buffer)))
    (should saved)
    (should (equal compiled "/tmp/some.el"))))

;; -- the helper's dispatch ---------------------------------------------------

(ert-deftest test-config-utilities-compile-buffer-prefers-native-async ()
  "Normal: `native-compile-async' is preferred when available."
  (let (called-with)
    (cl-letf (((symbol-function 'native-compile-async)
               (lambda (file) (setq called-with file)))
              ((symbol-function 'native-compile)
               (lambda (_) (error "should not call sync native-compile")))
              ((symbol-function 'byte-compile-file)
               (lambda (&rest _) (error "should not call byte-compile-file"))))
      (cj/--compile-elisp-file
       "/tmp/some.el"
       (test-config-utilities--available 'native-compile-async 'native-compile
                                         'byte-compile-file))
      (should (equal called-with "/tmp/some.el")))))

(ert-deftest test-config-utilities-compile-buffer-falls-back-to-sync-native ()
  "Normal: `native-compile' is used when async isn't available."
  (let (called-with)
    (cl-letf (((symbol-function 'native-compile)
               (lambda (file) (setq called-with file)))
              ((symbol-function 'byte-compile-file)
               (lambda (&rest _) (error "should not call byte-compile-file"))))
      (cj/--compile-elisp-file
       "/tmp/some.el"
       (test-config-utilities--available 'native-compile 'byte-compile-file))
      (should (equal called-with "/tmp/some.el")))))

(ert-deftest test-config-utilities-compile-buffer-falls-back-to-byte-compile ()
  "Normal: `byte-compile-file' is used when neither native option is available."
  (let (called-with)
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (file &rest _) (setq called-with file) "/tmp/some.elc")))
      (cj/--compile-elisp-file
       "/tmp/some.el"
       (test-config-utilities--available 'byte-compile-file))
      (should (equal called-with "/tmp/some.el")))))

(ert-deftest test-config-utilities-compile-buffer-reports-when-nothing-available ()
  "Boundary: with no compiler available the helper only messages, calling none."
  (let (captured)
    (cl-letf (((symbol-function 'native-compile-async)
               (lambda (&rest _) (error "should not call native-compile-async")))
              ((symbol-function 'native-compile)
               (lambda (&rest _) (error "should not call native-compile")))
              ((symbol-function 'byte-compile-file)
               (lambda (&rest _) (error "should not call byte-compile-file")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
      (cj/--compile-elisp-file "/tmp/some.el" (test-config-utilities--available)))
    (should (string-match-p "No compilation available" captured))))

(ert-deftest test-config-utilities-compile-buffer-handles-sync-native-error ()
  "Error: a sync `native-compile' that signals is caught and reported.
Asserts no error escapes by running the helper and checking that the
message captured contains the failure prefix."
  (let (captured)
    (cl-letf (((symbol-function 'native-compile)
               (lambda (_) (error "boom")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
      (cj/--compile-elisp-file
       "/tmp/some.el"
       (test-config-utilities--available 'native-compile 'byte-compile-file)))
    (should (string-match-p "Native compile failed" captured))))

(ert-deftest test-config-utilities-compile-buffer-default-predicate-is-fboundp ()
  "Normal: with no predicate the helper consults `fboundp', so on a real
Emacs it reaches whichever compiler exists rather than the no-compiler
message."
  (let (captured)
    (cl-letf (((symbol-function 'native-compile-async) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
      (cj/--compile-elisp-file "/tmp/some.el"))
    (should (string-match-p "Queued native compilation" captured))))

(provide 'test-config-utilities--compile-this-elisp-buffer)
;;; test-config-utilities--compile-this-elisp-buffer.el ends here
