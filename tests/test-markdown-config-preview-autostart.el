;;; test-markdown-config-preview-autostart.el --- preview server autostart -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--markdown-preview-ensure-server': F2 preview starts the
;; simple-httpd listener itself when it isn't running (per the 2026-07-01
;; cj comment), and leaves a running server alone.  simple-httpd isn't
;; installed in batch, so the tests provide a stub feature with the two
;; functions the helper touches.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'markdown-config)

;; Stand in for the elpa package: `require' in the helper becomes a
;; no-op.  Tests mock httpd-running-p explicitly (works whether the
;; real simple-httpd or this stub is loaded), so the stub bodies never
;; matter.
(require 'cl-lib)
(unless (featurep 'simple-httpd)
  (defun httpd-running-p () nil)
  (defun httpd-start () nil)
  (provide 'simple-httpd))

(ert-deftest test-markdown-config-ensure-server-starts-when-down ()
  "Normal: a stopped server gets started."
  (let ((started nil))
    (cl-letf (((symbol-function 'httpd-running-p) (lambda () nil))
              ((symbol-function 'cj/markdown-preview-server-start)
               (lambda () (setq started t))))
      (cj/--markdown-preview-ensure-server)
      (should started))))

(ert-deftest test-markdown-config-ensure-server-noop-when-running ()
  "Boundary: a running server is left alone."
  (let ((started nil))
    (cl-letf (((symbol-function 'httpd-running-p) (lambda () t))
              ((symbol-function 'cj/markdown-preview-server-start)
               (lambda () (setq started t))))
      (cj/--markdown-preview-ensure-server)
      (should-not started))))

(ert-deftest test-markdown-config-preview-no-longer-signals-user-error ()
  "Error-path regression: the old user-error on a stopped server is gone."
  (should-not (string-match-p "user-error"
                              (format "%S" (symbol-function 'cj/markdown-preview)))))

(provide 'test-markdown-config-preview-autostart)
;;; test-markdown-config-preview-autostart.el ends here
