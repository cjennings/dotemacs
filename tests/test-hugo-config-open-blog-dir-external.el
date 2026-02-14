;;; test-hugo-config-open-blog-dir-external.el --- Tests for cj/hugo-open-blog-dir-external -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/hugo-open-blog-dir-external function from hugo-config.el
;;
;; This function opens the blog source directory in the system file manager,
;; selecting the command based on platform:
;; - macOS: "open"
;; - Windows: "explorer.exe"
;; - Linux/other: "xdg-open"
;;
;; We mock the platform detection functions and start-process to verify
;; the correct command is dispatched per platform.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(unless (boundp 'website-dir)
  (defvar website-dir "/tmp/test-website/"))
(unless (fboundp 'env-macos-p)
  (defun env-macos-p () nil))
(unless (fboundp 'env-windows-p)
  (defun env-windows-p () nil))

(require 'hugo-config)

;;; Test Helpers

(defvar test-hugo--captured-process-cmd nil
  "Captures the command passed to start-process during tests.")

(defmacro with-platform (macos-p windows-p &rest body)
  "Execute BODY with mocked platform detection.
MACOS-P and WINDOWS-P control env-macos-p and env-windows-p return values.
Mocks start-process to capture the command and file-directory-p to avoid
filesystem checks."
  (declare (indent 2))
  `(let ((test-hugo--captured-process-cmd nil))
     (cl-letf (((symbol-function 'env-macos-p) (lambda () ,macos-p))
               ((symbol-function 'env-windows-p) (lambda () ,windows-p))
               ((symbol-function 'file-directory-p) (lambda (_d) t))
               ((symbol-function 'start-process)
                (lambda (_name _buf cmd &rest _args)
                  (setq test-hugo--captured-process-cmd cmd))))
       ,@body)))

;;; Normal Cases

(ert-deftest test-hugo-config-open-blog-dir-external-normal-linux ()
  "Should use xdg-open on Linux."
  (with-platform nil nil
    (cj/hugo-open-blog-dir-external)
    (should (string= test-hugo--captured-process-cmd "xdg-open"))))

(ert-deftest test-hugo-config-open-blog-dir-external-normal-macos ()
  "Should use open on macOS."
  (with-platform t nil
    (cj/hugo-open-blog-dir-external)
    (should (string= test-hugo--captured-process-cmd "open"))))

(ert-deftest test-hugo-config-open-blog-dir-external-normal-windows ()
  "Should use explorer.exe on Windows."
  (with-platform nil t
    (cj/hugo-open-blog-dir-external)
    (should (string= test-hugo--captured-process-cmd "explorer.exe"))))

;;; Boundary Cases

(ert-deftest test-hugo-config-open-blog-dir-external-boundary-macos-takes-precedence ()
  "When both macos and windows return true, macOS should take precedence."
  (with-platform t t
    (cj/hugo-open-blog-dir-external)
    (should (string= test-hugo--captured-process-cmd "open"))))

(ert-deftest test-hugo-config-open-blog-dir-external-boundary-creates-missing-dir ()
  "Should create the directory if it doesn't exist."
  (let ((mkdir-called nil))
    (cl-letf (((symbol-function 'env-macos-p) (lambda () nil))
              ((symbol-function 'env-windows-p) (lambda () nil))
              ((symbol-function 'file-directory-p) (lambda (_d) nil))
              ((symbol-function 'make-directory)
               (lambda (_dir &rest _args) (setq mkdir-called t)))
              ((symbol-function 'start-process) #'ignore))
      (cj/hugo-open-blog-dir-external)
      (should mkdir-called))))

(ert-deftest test-hugo-config-open-blog-dir-external-boundary-skips-mkdir-when-exists ()
  "Should not call make-directory if directory already exists."
  (let ((mkdir-called nil))
    (cl-letf (((symbol-function 'env-macos-p) (lambda () nil))
              ((symbol-function 'env-windows-p) (lambda () nil))
              ((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'make-directory)
               (lambda (_dir &rest _args) (setq mkdir-called t)))
              ((symbol-function 'start-process) #'ignore))
      (cj/hugo-open-blog-dir-external)
      (should-not mkdir-called))))

(provide 'test-hugo-config-open-blog-dir-external)
;;; test-hugo-config-open-blog-dir-external.el ends here
