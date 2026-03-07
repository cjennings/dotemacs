;;; testutil-ai-config.el --- Test stubs for ai-config.el tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides gptel and dependency stubs so ai-config.el can be loaded in
;; batch mode without the real gptel package.  Must be required BEFORE
;; ai-config so stubs are in place when use-package :config runs.

;;; Code:

;; Pre-cache API keys so auth-source is never consulted
(defvar cj/anthropic-api-key-cached "test-anthropic-key")
(defvar cj/openai-api-key-cached "test-openai-key")

;; Stub gptel variables (must exist before use-package :custom runs)
(defvar gptel-backend nil)
(defvar gptel-model nil)
(defvar gptel-mode nil)
(defvar gptel-prompt-prefix-alist nil)
(defvar gptel--debug nil)
(defvar gptel-default-mode nil)
(defvar gptel-expert-commands nil)
(defvar gptel-track-media nil)
(defvar gptel-include-reasoning nil)
(defvar gptel-log-level nil)
(defvar gptel-confirm-tool-calls nil)
(defvar gptel-directives nil)
(defvar gptel--system-message nil)
(defvar gptel-context--alist nil)
(defvar gptel-mode-map (make-sparse-keymap))
(defvar gptel-post-response-functions nil)

;; Stub gptel functions
(defun gptel-make-anthropic (name &rest _args)
  "Stub: return a vector mimicking a gptel backend struct."
  (vector 'cl-struct-gptel-backend name))

(defun gptel-make-openai (name &rest _args)
  "Stub: return a vector mimicking a gptel backend struct."
  (vector 'cl-struct-gptel-backend name))

(defun gptel-send (&rest _) "Stub." nil)
(defun gptel-menu (&rest _) "Stub." nil)
(defun gptel (&rest _) "Stub." nil)
(defun gptel-system-prompt (&rest _) "Stub." nil)
(defun gptel-rewrite (&rest _) "Stub." nil)
(defun gptel-add-file (&rest _) "Stub." nil)
(defun gptel-add (&rest _) "Stub." nil)
(defun gptel-backend-models (_backend) "Stub." nil)

(provide 'gptel)
(provide 'gptel-context)

;; Stub custom keymap (defined in user's keybinding config)
(defvar cj/custom-keymap (make-sparse-keymap))

;; Stub which-key
(unless (fboundp 'which-key-add-key-based-replacements)
  (defun which-key-add-key-based-replacements (&rest _) "Stub." nil))
(provide 'which-key)

;; Stub gptel-prompts
(defun gptel-prompts-update (&rest _) "Stub." nil)
(defun gptel-prompts-add-update-watchers (&rest _) "Stub." nil)
(provide 'gptel-prompts)

;; NOTE: gptel-magit is NOT stubbed here.  ai-config.el now uses
;; with-eval-after-load 'magit instead of use-package gptel-magit,
;; so the magit integration only activates when magit is provided.
;; See test-ai-config-gptel-magit-lazy-loading.el for magit stub tests.

;; Stub ai-conversations
(provide 'ai-conversations)

(provide 'testutil-ai-config)
;;; testutil-ai-config.el ends here
