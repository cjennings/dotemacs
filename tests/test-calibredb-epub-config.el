;;; test-calibredb-epub-config.el --- Tests for ebook config helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Focuses on project-owned helpers in calibredb-epub-config rather than
;; CalibreDB/Nov internals.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

(ert-deftest test-calibredb-epub-nov-text-width-default-window ()
  "Normal: text width uses the configured margins against the current window."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width)
               (lambda (_) 120)))
      (should (= 60 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-nov-text-width-clamps-large-margin ()
  "Boundary: excessive margins are clamped to keep a readable text column."
  (let ((cj/nov-margin-percent 80)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width)
               (lambda (_) 120)))
      (should (= 40 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-nov-text-width-fallback-without-window ()
  "Boundary: a buffer without a visible window still gets a usable width."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) nil)))
      (should (= 40 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-open-external-uses-zathura ()
  "Normal: named Nov external-open command delegates to zathura."
  (let (command)
    (cl-letf (((symbol-function 'cj/open-file-with-command)
               (lambda (cmd) (setq command cmd))))
      (cj/nov-open-external)
      (should (equal command "zathura")))))

(provide 'test-calibredb-epub-config)
;;; test-calibredb-epub-config.el ends here
