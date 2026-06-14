;;; test-slack-config-close-all.el --- cj/slack-close-all-buffers guard -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/slack-close-all-buffers iterates every buffer.  It must not signal
;; void-variable when `slack-current-buffer' has no binding in a buffer (slack
;; not loaded), and must kill only buffers where it is set non-nil.  The original
;; read it with `buffer-local-value' (which errors on buffers without the local
;; binding) instead of guarding like its sibling cj/slack-mark-read-and-bury.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'slack-config)

(ert-deftest test-slack-close-all-buffers-skips-unbound-kills-slack ()
  "Error/Normal: no signal on buffers without `slack-current-buffer'; only
buffers that have it set non-nil are killed."
  (let ((plain (generate-new-buffer " *plain*"))
        (slackish (generate-new-buffer " *slackish*")))
    (with-current-buffer slackish (setq-local slack-current-buffer t))
    (unwind-protect
        (progn
          (cj/slack-close-all-buffers)
          (should (buffer-live-p plain))
          (should-not (buffer-live-p slackish)))
      (when (buffer-live-p plain) (kill-buffer plain))
      (when (buffer-live-p slackish) (kill-buffer slackish)))))

(provide 'test-slack-config-close-all)
;;; test-slack-config-close-all.el ends here
