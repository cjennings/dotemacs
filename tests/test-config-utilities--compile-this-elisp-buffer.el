;;; test-config-utilities--compile-this-elisp-buffer.el --- Tests for cj/compile-this-elisp-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/compile-this-elisp-buffer'. The function dispatches
;; among native-compile-async, native-compile (sync), and
;; byte-compile-file based on which is fboundp. Tests force each
;; branch by mocking fboundp at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(defmacro test-config-utilities--with-elisp-buffer (path &rest body)
  "Run BODY in a temp buffer visiting PATH (a .el file path).
Skips the interactive `save-buffer' so tests stay free of disk side
effects."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name ,path)
     (cl-letf (((symbol-function 'save-buffer) (lambda () nil)))
       ,@body)))

(ert-deftest test-config-utilities-compile-buffer-not-elisp-raises ()
  "Error: a buffer whose file isn't .el raises `user-error'."
  (test-config-utilities--with-elisp-buffer "/tmp/not-elisp.txt"
    (should-error (cj/compile-this-elisp-buffer) :type 'user-error)))

(ert-deftest test-config-utilities-compile-buffer-no-buffer-file-name-raises ()
  "Error: a buffer with no `buffer-file-name' raises `user-error'."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (should-error (cj/compile-this-elisp-buffer) :type 'user-error)))

(ert-deftest test-config-utilities-compile-buffer-prefers-native-async ()
  "Normal: `native-compile-async' is preferred when available."
  (let (called-with)
    (test-config-utilities--with-elisp-buffer "/tmp/some.el"
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (memq sym '(native-compile-async native-compile byte-compile-file))))
                ((symbol-function 'native-compile-async)
                 (lambda (file) (setq called-with file)))
                ((symbol-function 'native-compile)
                 (lambda (_) (error "should not call sync native-compile")))
                ((symbol-function 'byte-compile-file)
                 (lambda (_) (error "should not call byte-compile-file"))))
        (cj/compile-this-elisp-buffer)
        (should (equal called-with "/tmp/some.el"))))))

(ert-deftest test-config-utilities-compile-buffer-falls-back-to-sync-native ()
  "Normal: `native-compile' is used when async isn't available."
  (let (called-with)
    (test-config-utilities--with-elisp-buffer "/tmp/some.el"
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym) (memq sym '(native-compile byte-compile-file))))
                ((symbol-function 'native-compile)
                 (lambda (file) (setq called-with file)))
                ((symbol-function 'byte-compile-file)
                 (lambda (_) (error "should not call byte-compile-file"))))
        (cj/compile-this-elisp-buffer)
        (should (equal called-with "/tmp/some.el"))))))

(ert-deftest test-config-utilities-compile-buffer-falls-back-to-byte-compile ()
  "Normal: `byte-compile-file' is used when neither native option is available."
  (let (called-with)
    (test-config-utilities--with-elisp-buffer "/tmp/some.el"
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'byte-compile-file)))
                ((symbol-function 'byte-compile-file)
                 (lambda (file) (setq called-with file) "/tmp/some.elc")))
        (cj/compile-this-elisp-buffer)
        (should (equal called-with "/tmp/some.el"))))))

(ert-deftest test-config-utilities-compile-buffer-handles-sync-native-error ()
  "Error: a sync `native-compile' that signals is caught and reported.
Asserts no error escapes by running the function and checking that the
message captured contains the failure prefix."
  (test-config-utilities--with-elisp-buffer "/tmp/some.el"
    (let (captured)
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym) (memq sym '(native-compile byte-compile-file))))
                ((symbol-function 'native-compile)
                 (lambda (_) (error "boom")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured (apply #'format fmt args)))))
        (cj/compile-this-elisp-buffer))
      (should (string-match-p "Native compile failed" captured)))))

(provide 'test-config-utilities--compile-this-elisp-buffer)
;;; test-config-utilities--compile-this-elisp-buffer.el ends here
