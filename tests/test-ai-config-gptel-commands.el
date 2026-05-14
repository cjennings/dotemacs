;;; test-ai-config-gptel-commands.el --- Tests for ai-config gptel command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Second pass on ai-config.  The first batch covered the helpers
;; (auth-source, api-key caching, add-file, clear-buffer, context-
;; clear, insert-model-heading).  This file covers the gptel command
;; wrappers and a few small pure helpers:
;;
;;   cj/gptel--refresh-org-prefix
;;   cj/gptel-backend-and-model
;;   cj/gptel-switch-backend
;;   cj/gptel-add-buffer-file
;;   cj/gptel-add-this-buffer
;;   cj/toggle-gptel
;;
;; The gptel/projectile primitives are stubbed throughout.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

;; Dynamic vars gptel would normally own.
(defvar gptel-backend nil)
(defvar gptel-model nil)
(defvar gptel-prompt-prefix-alist nil)

;;; cj/gptel--refresh-org-prefix

(ert-deftest test-ai-config-refresh-org-prefix-updates-alist-entry ()
  "Normal: the advice refreshes the org-mode entry in the prefix alist."
  (let ((gptel-prompt-prefix-alist '((org-mode . "stale\n"))))
    (cj/gptel--refresh-org-prefix)
    (let ((entry (alist-get 'org-mode gptel-prompt-prefix-alist)))
      (should (stringp entry))
      ;; Fresh prefix includes the user-login-name + a timestamp bracket.
      (should (string-match-p "\\[" entry)))))

;;; cj/gptel-backend-and-model

(ert-deftest test-ai-config-backend-and-model-formats-with-vector-backend ()
  "Normal: a vector backend's name element comes through formatted."
  (let ((gptel-backend [unused-slot "Claude" other])
        (gptel-model 'claude-opus-4-6))
    (let ((s (cj/gptel-backend-and-model)))
      (should (string-match-p "Claude" s))
      (should (string-match-p "claude-opus-4-6" s)))))

(ert-deftest test-ai-config-backend-and-model-falls-back-to-ai-when-no-backend ()
  "Boundary: with no backend bound, the string starts with the AI fallback."
  (let ((gptel-backend nil)
        (gptel-model nil))
    (should (string-prefix-p "AI:" (cj/gptel-backend-and-model)))))

;;; cj/gptel-switch-backend

(ert-deftest test-ai-config-switch-backend-sets-backend-and-model ()
  "Normal: switch picks a backend + model, then updates the gptel vars."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (msg nil))
    (cl-letf (((symbol-function 'cj/gptel--available-backends)
               (lambda ()
                 '(("Anthropic - Claude" . anthropic-backend))))
              ((symbol-function 'gptel-backend-models)
               (lambda (_b) '(claude-opus claude-sonnet)))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 ;; First call -> backend choice; second -> model.
                 (cond
                  ((string-match-p "backend" prompt) "Anthropic - Claude")
                  (t "claude-opus"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-switch-backend))
    (should (eq gptel-backend 'anthropic-backend))
    (should (equal gptel-model "claude-opus"))
    (should (string-match-p "Anthropic - Claude" msg))))

(ert-deftest test-ai-config-switch-backend-error-invalid-choice ()
  "Error: an unrecognized backend name signals user-error."
  (cl-letf (((symbol-function 'cj/gptel--available-backends)
             (lambda () '(("Anthropic - Claude" . backend-a))))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "Something Else")))
    (should-error (cj/gptel-switch-backend) :type 'user-error)))

;;; cj/gptel-add-buffer-file

(ert-deftest test-ai-config-add-buffer-file-adds-when-buffer-has-file ()
  "Normal: a buffer that visits a file -> the file is added to context."
  (let ((added nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/sample.org")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (buffer-name)))
                ((symbol-function 'cj/gptel--add-file-to-context)
                 (lambda (f) (setq added f) t))
                ((symbol-function 'message) #'ignore))
        (cj/gptel-add-buffer-file))
      (setq buffer-file-name nil))
    (should (equal added "/tmp/sample.org"))))

(ert-deftest test-ai-config-add-buffer-file-messages-when-no-file ()
  "Boundary: a buffer not visiting a file -> message, no add call."
  (let ((added nil)
        (msg nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (buffer-name)))
                ((symbol-function 'cj/gptel--add-file-to-context)
                 (lambda (f) (setq added f) t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/gptel-add-buffer-file)))
    (should-not added)
    (should (string-match-p "not visiting" msg))))

;;; cj/gptel-add-this-buffer

(ert-deftest test-ai-config-add-this-buffer-calls-gptel-add-with-prefix ()
  "Normal: `cj/gptel-add-this-buffer' calls `gptel-add' with the (4) prefix arg."
  (let ((arg nil))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'gptel-add)
               (lambda (a) (setq arg a)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (cj/gptel-add-this-buffer)))
    (should (equal arg '(4)))))

;;; cj/toggle-gptel

(ert-deftest test-ai-config-toggle-gptel-closes-when-window-shown ()
  "Normal: with a window already displaying *AI-Assistant*, toggle deletes it."
  (let* ((buf (generate-new-buffer "*AI-Assistant*"))
         (deleted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_b) 'fake-window))
                  ((symbol-function 'delete-window)
                   (lambda (w) (setq deleted w))))
          (cj/toggle-gptel))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (should (eq deleted 'fake-window))))

(provide 'test-ai-config-gptel-commands)
;;; test-ai-config-gptel-commands.el ends here
