;;; test-coverage-elisp-helpers.el --- Tests for coverage-elisp helpers + runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-coverage-elisp--detect.el` covers the detect heuristic.
;; This file fills in the rest:
;;
;;   cj/--coverage-elisp-project-root
;;   cj/--coverage-elisp-report-path
;;   cj/--coverage-elisp-run
;;
;; projectile / compilation-start are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-elisp)

;;; cj/--coverage-elisp-project-root

(ert-deftest test-coverage-elisp-project-root-uses-explicit-arg ()
  "Normal: an explicit ROOT argument wins over the fallbacks."
  (should (equal (cj/--coverage-elisp-project-root "/proj/root/")
                 "/proj/root/")))

(ert-deftest test-coverage-elisp-project-root-falls-back-to-projectile ()
  "Normal: with no ROOT, projectile's root is returned."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/proj/from-projectile/")))
    (should (equal (cj/--coverage-elisp-project-root)
                   "/proj/from-projectile/"))))

(ert-deftest test-coverage-elisp-project-root-falls-back-to-default-directory ()
  "Boundary: with neither arg nor projectile, falls back to `default-directory'."
  (let ((default-directory "/proj/from-default/"))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (not (eq sym 'projectile-project-root)))))
      (should (equal (cj/--coverage-elisp-project-root)
                     "/proj/from-default/")))))

;;; cj/--coverage-elisp-report-path

(ert-deftest test-coverage-elisp-report-path-resolves-relative-to-root ()
  "Normal: report path appends `.coverage/simplecov.json' to ROOT."
  (should (equal (cj/--coverage-elisp-report-path "/proj/root/")
                 "/proj/root/.coverage/simplecov.json")))

(ert-deftest test-coverage-elisp-report-path-no-root-uses-fallback ()
  "Boundary: with no ROOT, expands against the fallback root."
  (let ((default-directory "/proj/from-default/"))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (not (eq sym 'projectile-project-root)))))
      (should (equal (cj/--coverage-elisp-report-path)
                     "/proj/from-default/.coverage/simplecov.json")))))

;;; cj/--coverage-elisp-run

(ert-deftest test-coverage-elisp-run-starts-make-coverage-compilation ()
  "Normal: run launches `make coverage' via `compilation-start' with a
named buffer and registers a finish hook on the compilation buffer."
  (let ((cmd nil)
        (registered-hook nil)
        (buf (generate-new-buffer "*coverage-run*")))
    (unwind-protect
        (cl-letf (((symbol-function 'compilation-start)
                   (lambda (c &rest _) (setq cmd c) buf))
                  ((symbol-function 'projectile-project-root)
                   (lambda () "/proj/root/")))
          (cj/--coverage-elisp-run (lambda (_) nil))
          (with-current-buffer buf
            (setq registered-hook compilation-finish-functions)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should (equal cmd "make coverage"))
    (should registered-hook)))

(ert-deftest test-coverage-elisp-run-callback-fires-on-success ()
  "Normal: when the compilation finishes with `finished', the callback gets
the report path."
  (let ((reported nil)
        (buf (generate-new-buffer "*coverage-run*")))
    (unwind-protect
        (cl-letf (((symbol-function 'compilation-start)
                   (lambda (&rest _) buf))
                  ((symbol-function 'projectile-project-root)
                   (lambda () "/proj/root/")))
          (cj/--coverage-elisp-run (lambda (path) (setq reported path)))
          ;; Fire the hook installed by run().
          (with-current-buffer buf
            (run-hook-with-args 'compilation-finish-functions
                                buf "finished\n")))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should (equal reported "/proj/root/.coverage/simplecov.json"))))

(ert-deftest test-coverage-elisp-run-callback-skipped-on-failure ()
  "Boundary: a non-`finished' status doesn't fire the callback."
  (let ((reported nil)
        (buf (generate-new-buffer "*coverage-run*")))
    (unwind-protect
        (cl-letf (((symbol-function 'compilation-start)
                   (lambda (&rest _) buf))
                  ((symbol-function 'projectile-project-root)
                   (lambda () "/proj/root/")))
          (cj/--coverage-elisp-run (lambda (path) (setq reported path)))
          (with-current-buffer buf
            (run-hook-with-args 'compilation-finish-functions
                                buf "exited abnormally\n")))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should-not reported)))

(provide 'test-coverage-elisp-helpers)
;;; test-coverage-elisp-helpers.el ends here
