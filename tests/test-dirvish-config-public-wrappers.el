;;; test-dirvish-config-public-wrappers.el --- Tests for the interactive dirvish wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling test files cover the pure helpers (`cj/--html-file-p`,
;; `cj/--duplicate-file-name`, `cj/--dired-line-is-directory-p`,
;; `cj/--dired-resolve-display-path`, etc.).  This file covers
;; interactive wrappers that build on them:
;;
;;   cj/dirvish-open-html-in-eww
;;   cj/dirvish-duplicate-file
;;   cj/dired-mark-all-visible-files
;;   cj/dired-copy-path-as-kill
;;   cj/dirvish-open-file-manager-here
;;
;; Dired primitives (`dired-get-file-for-visit', `dired-get-filename',
;; `dired-mark', etc.) are stubbed so the tests don't depend on a real
;; dired buffer; `eww-open-file', `copy-file', `revert-buffer',
;; `kill-new', and the external-open dispatch are stubbed too.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dirvish-config)

;;; cj/dirvish-open-html-in-eww

(ert-deftest test-dirvish-open-html-in-eww-html-file-opens ()
  "Normal: an HTML file at point gets handed to `eww-open-file'."
  (let ((opened nil))
    (cl-letf (((symbol-function 'dired-get-file-for-visit)
               (lambda () "/tmp/report.html"))
              ((symbol-function 'eww-open-file)
               (lambda (f) (setq opened f))))
      (cj/dirvish-open-html-in-eww))
    (should (equal opened "/tmp/report.html"))))

(ert-deftest test-dirvish-open-html-in-eww-non-html-messages ()
  "Boundary: a non-HTML file is rejected with a message; eww isn't called."
  (let ((opened nil)
        (msg nil))
    (cl-letf (((symbol-function 'dired-get-file-for-visit)
               (lambda () "/tmp/notes.txt"))
              ((symbol-function 'eww-open-file)
               (lambda (f) (setq opened f)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/dirvish-open-html-in-eww))
    (should (null opened))
    (should (string-match-p "Not an HTML file" msg))))

;;; cj/dirvish-duplicate-file

(ert-deftest test-dirvish-duplicate-file-normal-copies ()
  "Normal: a regular file is copied with the -copy suffix; user sees a message."
  (let ((copied nil)
        (msg nil))
    (cl-letf (((symbol-function 'dired-get-filename)
               (lambda (&rest _) "/tmp/report.pdf"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'copy-file)
               (lambda (src dst &rest _) (setq copied (cons src dst))))
              ((symbol-function 'revert-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/dirvish-duplicate-file))
    (should (equal (car copied) "/tmp/report.pdf"))
    (should (string-suffix-p "report-copy.pdf" (cdr copied)))
    (should (string-match-p "Duplicated" msg))))

(ert-deftest test-dirvish-duplicate-file-error-no-file ()
  "Error: nil file at point signals user-error."
  (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) nil)))
    (should-error (cj/dirvish-duplicate-file) :type 'user-error)))

(ert-deftest test-dirvish-duplicate-file-error-directory ()
  "Error: refusing to duplicate a directory."
  (cl-letf (((symbol-function 'dired-get-filename)
             (lambda (&rest _) "/tmp/somedir"))
            ((symbol-function 'file-directory-p) (lambda (_) t)))
    (should-error (cj/dirvish-duplicate-file) :type 'user-error)))

(ert-deftest test-dirvish-duplicate-file-boundary-decline-overwrite-cancels ()
  "Boundary: declining the overwrite y-or-n-p signals user-error.

Uses real temp files so `file-directory-p' and `file-exists-p' don't
need to be `cl-letf'-redefined -- native-comp's trampoline cache gets
confused when several built-ins are overridden in the same test."
  (let* ((src (make-temp-file "test-dirvish-src-" nil ".pdf"))
         (dst (cj/--duplicate-file-name src)))
    (with-temp-file dst (insert "pre-existing"))
    (unwind-protect
        (cl-letf (((symbol-function 'dired-get-filename)
                   (lambda (&rest _) src))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (should-error (cj/dirvish-duplicate-file) :type 'user-error))
      (when (file-exists-p src) (delete-file src))
      (when (file-exists-p dst) (delete-file dst)))))

;;; cj/dired-mark-all-visible-files

(ert-deftest test-dirvish-mark-all-visible-skips-directories ()
  "Normal: directory lines are skipped, file lines are marked."
  (let ((marks 0))
    (with-temp-buffer
      ;; Real dired listing has lines like "  drwxr... dir/" or "  -rw... file".
      ;; The helper `cj/--dired-line-is-directory-p' matches "<space>d".
      (insert "  drwxr-xr-x  subdir\n"
              "  -rw-r--r--  file1.txt\n"
              "  -rw-r--r--  file2.txt\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'dired-mark)
                 (lambda (&rest _) (cl-incf marks))))
        (cj/dired-mark-all-visible-files)))
    ;; 2 file lines marked; the directory line + the trailing empty line skipped.
    (should (= marks 2))))

;;; cj/dired-copy-path-as-kill

(ert-deftest test-dirvish-copy-path-as-kill-not-in-dired-errors ()
  "Error: invoking outside a dired-mode buffer signals user-error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (not (memq 'dired-mode modes)))))
      (should-error (cj/dired-copy-path-as-kill) :type 'user-error))))

(ert-deftest test-dirvish-copy-path-as-kill-normal-copies-resolved-path ()
  "Normal: the resolved path is placed on the kill ring."
  (let ((killed nil))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'dired-mode modes)))
              ((symbol-function 'dired-get-filename)
               (lambda (&rest _) "/tmp/foo.txt"))
              ((symbol-function 'cj/get-project-root)
               (lambda () nil))
              ((symbol-function 'kill-new)
               (lambda (s) (setq killed s)))
              ((symbol-function 'message) #'ignore))
      (cj/dired-copy-path-as-kill))
    (should (stringp killed))
    (should (string-match-p "foo.txt" killed))))

(ert-deftest test-dirvish-copy-path-as-kill-org-link-format ()
  "Normal: with AS-ORG-LINK non-nil the kill is an org-link string."
  (let ((killed nil))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'dired-mode modes)))
              ((symbol-function 'dired-get-filename)
               (lambda (&rest _) "/tmp/foo.txt"))
              ((symbol-function 'cj/get-project-root) (lambda () nil))
              ((symbol-function 'kill-new)
               (lambda (s) (setq killed s)))
              ((symbol-function 'message) #'ignore))
      (cj/dired-copy-path-as-kill t))
    (should (string-prefix-p "[[file:" killed))
    (should (string-match-p "foo.txt" killed))))

;;; cj/dirvish-open-file-manager-here

(ert-deftest test-dirvish-open-file-manager-here-uses-external-open-program ()
  "Normal: a directory and a program from `cj/external-open-command' triggers call-process."
  (let ((called-prog nil)
        (called-dir nil))
    (cl-letf (((symbol-function 'dired-current-directory) (lambda () "/tmp"))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'cj/external-open-command)
               (lambda () "/usr/bin/xdg-open"))
              ((symbol-function 'call-process)
               (lambda (prog _infile _buf _display dir &rest _)
                 (setq called-prog prog)
                 (setq called-dir dir)
                 0))
              ((symbol-function 'message) #'ignore))
      (cj/dirvish-open-file-manager-here))
    (should (equal called-prog "/usr/bin/xdg-open"))
    (should (equal called-dir "/tmp"))))

(ert-deftest test-dirvish-open-file-manager-here-no-program-falls-back-to-xdg-shell ()
  "Boundary: when `cj/external-open-command' returns nil, falls back to a shell xdg-open."
  (let ((shell-called nil))
    (cl-letf (((symbol-function 'dired-current-directory) (lambda () "/tmp"))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'cj/external-open-command) (lambda () nil))
              ((symbol-function 'shell-command)
               (lambda (cmd &rest _) (setq shell-called cmd) 0))
              ((symbol-function 'message) #'ignore))
      (cj/dirvish-open-file-manager-here))
    (should (string-match-p "xdg-open" shell-called))))

(ert-deftest test-dirvish-open-file-manager-here-missing-directory-messages ()
  "Boundary: an unresolvable directory just messages; no external program runs."
  (let ((msg nil)
        (called nil))
    (cl-letf (((symbol-function 'dired-current-directory) (lambda () nil))
              ((symbol-function 'cj/external-open-command)
               (lambda () (setq called 'external) nil))
              ((symbol-function 'shell-command)
               (lambda (&rest _) (setq called 'shell) 0))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/dirvish-open-file-manager-here))
    (should (string-match-p "Could not determine" msg))
    (should-not called)))

(provide 'test-dirvish-config-public-wrappers)
;;; test-dirvish-config-public-wrappers.el ends here
