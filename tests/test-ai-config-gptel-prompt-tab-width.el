;;; test-ai-config-gptel-prompt-tab-width.el --- Tests for gptel prompt-buffer tab-width fix -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the `org-element--list-struct: Tab width in Org
;; files must be 8, not 4' error that fires when `gptel-magit' is
;; invoked from a `COMMIT_EDITMSG' buffer where `git-commit-major-mode'
;; is `org-mode'.
;;
;; Root cause: gptel's `gptel--with-buffer-copy-internal'
;; (gptel-request.el:945) sets the prompt buffer's `major-mode' to the
;; source buffer's mode as a *symbol*, without running mode hooks.  So
;; an inherited `org-mode' prompt buffer keeps the global default
;; `tab-width' (4 in this config) — `org-mode-hook' never runs to set
;; 8.  When gptel later parses the prompt buffer with `org-element',
;; Org's `tab-width=8' guard raises.
;;
;; Fix: an `:around' advice on `gptel--with-buffer-copy-internal' in
;; ai-config.el sets `tab-width=8' inside the prompt buffer when its
;; inherited `major-mode' is `org-mode'.
;;
;; These tests verify the advice fires for org-mode source buffers and
;; stays out of the way for other modes.  Each test pins
;; `default-value' of `tab-width' to 4 — matching Craig's real config
;; — so the advice's effect is observable in batch tests (where the
;; default would otherwise be Emacs's built-in 8).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; testutil-ai-config provides the gptel--with-buffer-copy-internal stub
;; and the gptel stubs ai-config needs to load.
(require 'testutil-ai-config)
(require 'ai-config)

(defmacro test-ai-config--with-default-tab-width (sentinel &rest body)
  "Bind `default-value' of `tab-width' to SENTINEL during BODY.
Restores the prior default on unwind.  New buffers created inside BODY
inherit SENTINEL as their `tab-width'."
  (declare (indent 1))
  `(let ((saved (default-value 'tab-width)))
     (unwind-protect
         (progn (setq-default tab-width ,sentinel)
                ,@body)
       (setq-default tab-width saved))))

;;; Normal Cases

(ert-deftest test-ai-config-gptel-prompt-tab-width-org-mode-source-sets-8 ()
  "Normal: source buffer in `org-mode' → prompt buffer has `tab-width=8'.
Regression net for the `org-element--list-struct: Tab width must be 8'
error when gptel-magit runs from COMMIT_EDITMSG with
`git-commit-major-mode=org-mode'."
  (test-ai-config--with-default-tab-width 4
    (let ((recorded nil))
      (with-temp-buffer
        (setq major-mode 'org-mode)
        (gptel--with-buffer-copy-internal
         (current-buffer) nil nil
         (lambda () (setq recorded tab-width))))
      (should (= recorded 8)))))

;;; Boundary Cases

(ert-deftest test-ai-config-gptel-prompt-tab-width-text-mode-source-not-overridden ()
  "Boundary: source buffer in `text-mode' → advice does NOT set `tab-width=8'.
Only `org-mode' source triggers the override; other modes keep the
inherited default."
  (test-ai-config--with-default-tab-width 4
    (let ((recorded nil))
      (with-temp-buffer
        (setq major-mode 'text-mode)
        (gptel--with-buffer-copy-internal
         (current-buffer) nil nil
         (lambda () (setq recorded tab-width))))
      (should (= recorded 4)))))

(ert-deftest test-ai-config-gptel-prompt-tab-width-fundamental-mode-source-not-overridden ()
  "Boundary: source buffer in `fundamental-mode' → no override.
Confirms the advice's `(eq major-mode 'org-mode)' gate is strict."
  (test-ai-config--with-default-tab-width 4
    (let ((recorded nil))
      (with-temp-buffer
        (setq major-mode 'fundamental-mode)
        (gptel--with-buffer-copy-internal
         (current-buffer) nil nil
         (lambda () (setq recorded tab-width))))
      (should (= recorded 4)))))

(ert-deftest test-ai-config-gptel-prompt-tab-width-advice-does-not-leak ()
  "Boundary: advice's `setq-local' stays buffer-local to the prompt buffer.
After body-thunk completes and the prompt buffer is killed, the source
buffer's `tab-width' is unchanged."
  (with-temp-buffer
    (setq major-mode 'org-mode)
    (setq-local tab-width 4)
    (gptel--with-buffer-copy-internal
     (current-buffer) nil nil
     (lambda () nil))
    (should (= tab-width 4))))

(provide 'test-ai-config-gptel-prompt-tab-width)
;;; test-ai-config-gptel-prompt-tab-width.el ends here
