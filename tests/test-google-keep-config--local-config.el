;;; test-google-keep-config--local-config.el --- Tests for the Keep machine-local config loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/keep--load-local-config, the loader for the gitignored
;; machine-local google-keep.local.el (venv interpreter path, account email) —
;; the same shape calendar-sync uses for calendar-sync.local.el.

;;; Code:

(require 'ert)
(require 'google-keep-config)

(ert-deftest test-google-keep-local-config-loads-readable-file ()
  "Normal: a readable local config file is loaded and its settings apply."
  (let ((file (make-temp-file "keep-local-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(setq test-google-keep--local-marker 'loaded)"))
          (defvar test-google-keep--local-marker nil)
          (setq test-google-keep--local-marker nil)
          (let ((cj/keep-local-config-file file))
            (should (cj/keep--load-local-config))
            (should (eq test-google-keep--local-marker 'loaded))))
      (delete-file file))))

(ert-deftest test-google-keep-local-config-missing-file-is-quiet ()
  "Boundary: an absent local config file is a silent no-op, no error."
  (let ((cj/keep-local-config-file "/nonexistent/google-keep.local.el"))
    (should-not (cj/keep--load-local-config))))

(ert-deftest test-google-keep-local-config-broken-file-does-not-signal ()
  "Error: a local config file with a broken form is caught and reported,
never propagated as a load-time error."
  (let ((file (make-temp-file "keep-local-broken-" nil ".el"))
        (messages nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(error \"deliberately broken local config\")"))
          (let ((cj/keep-local-config-file file))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages)
                         nil)))
              (should-not (cj/keep--load-local-config)))
            (should (seq-find (lambda (m) (string-match-p "google-keep.*local config" m))
                              messages))))
      (delete-file file))))

(provide 'test-google-keep-config--local-config)
;;; test-google-keep-config--local-config.el ends here
