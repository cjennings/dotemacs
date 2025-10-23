;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Configuration for AI integrations in Emacs, focused on GPTel.
;;
;; Main Features:
;; - Quick toggle for AI assistant window (F9 or M-a t)
;; - Custom keymap (M-a prefix, overrides 'backwards-sentence') for AI-related commands.
;; - Enhanced org-mode conversation formatting with timestamps
;;   allows switching models and easily compare and track responses.
;; - Various specialized AI directives (coder, reviewer, etc.)
;; - Context management for adding files/buffers to conversations
;; - Conversation persistence with save/load functionality
;; - Integration with Magit for code review
;;
;; Basic Workflow
;;
;; Using a side-chat window:
;; - Launch GPTel via F9 or M-a t, and chat in the AI-Assistant side window (C-<return> to send)
;; - Change system prompt (expertise, personalities) with M-a p
;; - Add context from files (M-a f) or buffers (M-a b)
;; - Save conversations with M-a s, load previous ones with M-a l
;; - Clear the conversation and start over with M-a x
;; Or in any buffer:
;; - Add directive as above, and select a region to rewrite with M-a r.
;;

;;; Code:

(autoload 'cj/gptel-save-conversation "ai-conversations" "Save the AI conversation to a file." t)
(autoload 'cj/gptel-load-conversation "ai-conversations" "Load a saved AI conversation." t)
(autoload 'cj/gptel-delete-conversation "ai-conversations" "Delete a saved AI conversation." t)



(with-eval-after-load 'gptel
  (require 'ai-conversations))

;;; ------------------------- AI Config Helper Functions ------------------------

;; Define variables upfront
(defvar cj/anthropic-api-key-cached nil "Cached Anthropic API key.")
(defvar cj/openai-api-key-cached nil "Cached OpenAI API key.")
(defvar gptel-claude-backend nil "Claude backend, lazy-initialized.")
(defvar gptel-chatgpt-backend nil "ChatGPT backend, lazy-initialized.")


(defun cj/auth-source-secret (host user)
  "Fetch a secret from auth-source for HOST and USER.

HOST and USER must be strings that identify the credential to return."
  (let* ((found (auth-source-search :host host :user user :require '(:secret) :max 1))
         (secret (plist-get (car found) :secret)))
    (cond
     ((functionp secret) (funcall secret))
     ((stringp secret) secret)
     (t (error "No usable secret found for host %s and user %s" host user)))))

(defun cj/anthropic-api-key ()
  "Return the Anthropic API key, caching the result after first retrieval."
  (or cj/anthropic-api-key-cached
      (setq cj/anthropic-api-key-cached
            (cj/auth-source-secret "api.anthropic.com" "apikey"))))

(defun cj/openai-api-key ()
  "Return the OpenAI API key, caching the result after first retrieval."
  (or cj/openai-api-key-cached
      (setq cj/openai-api-key-cached
            (cj/auth-source-secret "api.openai.com" "apikey"))))

(defun cj/ensure-gptel-backends ()
  "Initialize GPTel backends if they are not already available.
Call this only after loading 'gptel' so the backend constructors exist."
  (unless gptel-claude-backend
    (setq gptel-claude-backend
          (gptel-make-anthropic
              "Claude"
            :key (cj/anthropic-api-key)
            :models '(
                      "claude-opus-4-1-20250805"
                      "claude-3-5-sonnet-20241022"
                      "claude-3-opus-20240229"
                      "claude-3-5-haiku-20241022"
                      )
            :stream t)))
  (unless gptel-chatgpt-backend
    (setq gptel-chatgpt-backend
          (gptel-make-openai
              "ChatGPT"
            :key (cj/openai-api-key)
            :models '(
                      "gpt-4o"
                      "gpt-5"
                      "gpt-4.1"
                      "o1"
                      )
            :stream t)))
  ;; Set default backend
  (unless gptel-backend
    (setq gptel-backend (or gptel-chatgpt-backend gptel-claude-backend))))

(autoload 'cj/toggle-gptel "ai-config" "Toggle the AI-Assistant window" t)

;; ------------------ GPTel Conversation And Utility Commands ------------------

(defun cj/gptel--available-backends ()
  "Return an alist of (NAME . BACKEND), ensuring gptel and backends are initialized."
  (unless (featurep 'gptel)
    (require 'gptel))
  (cj/ensure-gptel-backends)
  (delq nil
        (list (and (bound-and-true-p gptel-claude-backend)
                   (cons "Anthropic - Claude" gptel-claude-backend))
              (and (bound-and-true-p gptel-chatgpt-backend)
                   (cons "OpenAI - ChatGPT" gptel-chatgpt-backend)))))

(defun cj/gptel--model->string (m)
  (cond
   ((stringp m) m)
   ((symbolp m) (symbol-name m))
   (t (format "%s" m))))

;; Backend/model switching commands (moved out of use-package so they are commandp)
(defun cj/gptel-change-model ()
  "Change the GPTel backend and select a model from that backend.
Present all available models from every backend, switching backends when
necessary. Prompt for whether to apply the selection globally or buffer-locally."
  (interactive)
  (let* ((backends (cj/gptel--available-backends))
         (all-models
          (mapcan
           (lambda (pair)
             (let* ((backend-name (car pair))
                    (backend (cdr pair))
                    (models (when (fboundp 'gptel-backend-models)
                              (gptel-backend-models backend))))
               (mapcar (lambda (m)
                         (list (format "%s: %s" backend-name (cj/gptel--model->string m))
                               backend
                               (cj/gptel--model->string m)
                               backend-name))
                       models)))
           backends))
         (current-backend-name (car (rassoc (bound-and-true-p gptel-backend) backends)))
         (current-selection (format "%s: %s"
                                    (or current-backend-name "AI")
                                    (cj/gptel--model->string (bound-and-true-p gptel-model))))
         (scope (completing-read "Set model for: " '("buffer" "global") nil t))
         (selected (completing-read
                    (format "Select model (current: %s): " current-selection)
                    (mapcar #'car all-models) nil t nil nil current-selection)))
    (let* ((model-info (assoc selected all-models))
           (backend (nth 1 model-info))
           (model (intern (nth 2 model-info)))  ;; Convert string to symbol
           (backend-name (nth 3 model-info)))
      (if (string= scope "global")
          (progn
            (setq gptel-backend backend)
            (setq gptel-model model)
            (message "Changed to %s model: %s (global)" backend-name model))
        (setq-local gptel-backend backend)
        (setq-local gptel-model (if (stringp model) (intern model) model))
        (message "Changed to %s model: %s (buffer-local)" backend-name model)))))

(defun cj/gptel-switch-backend ()
  "Switch the GPTel backend and then choose one of its models."
  (interactive)
  (let* ((backends (cj/gptel--available-backends))
         (choice (completing-read "Select GPTel backend: " (mapcar #'car backends) nil t))
         (backend (cdr (assoc choice backends))))
    (unless backend
      (user-error "Invalid GPTel backend: %s" choice))
    (let* ((models (when (fboundp 'gptel-backend-models)
                     (gptel-backend-models backend)))
           (model (completing-read (format "Select %s model: " choice)
                                   (mapcar #'cj/gptel--model->string models)
                                   nil t nil nil (cj/gptel--model->string (bound-and-true-p gptel-model)))))
      (setq gptel-backend backend
            gptel-model model)
      (message "Switched to %s with model: %s" choice model))))

;; Clear assistant buffer (moved out so it's always available)
(defun cj/gptel-clear-buffer ()
  "Erase the current GPTel buffer while preserving the initial Org heading.

Operate only when `gptel-mode' is active in an Org buffer so the heading
can be reinserted."
  (interactive)
  (let ((is-gptel (bound-and-true-p gptel-mode))
        (is-org (derived-mode-p 'org-mode)))
    (if (and is-gptel is-org)
        (progn
          (erase-buffer)
          (when (fboundp 'cj/gptel--fresh-org-prefix)
            (insert (cj/gptel--fresh-org-prefix)))
          (message "GPTel buffer cleared and heading reset"))
      (message "Not a GPTel buffer in org-mode. Nothing cleared."))))

;; ----------------------------- Context Management ----------------------------

(defun cj/gptel--add-file-to-context (file-path)
  "Add FILE-PATH to the GPTel context.

Returns t on success, nil on failure.
Provides consistent user feedback about the context state."
  (when (and file-path (file-exists-p file-path))
    (gptel-add-file file-path)
    (let ((context-count (if (boundp 'gptel-context--alist)
                             (length gptel-context--alist)
                           0)))
      (message "Added %s to GPTel context (%d sources total)"
               (file-name-nondirectory file-path)
               context-count))
    t))

(defun cj/gptel-add-file ()
  "Add a file to the GPTel context.

If inside a Projectile project, prompt from that project's file list.
Otherwise, prompt with `read-file-name'."
  (interactive)
  (let* ((in-proj (and (featurep 'projectile)
                       (fboundp 'projectile-project-p)
                       (projectile-project-p)))
         (file-name (if in-proj
                        (let ((cands (projectile-current-project-files)))
                          (if (fboundp 'projectile-completing-read)
                              (projectile-completing-read "GPTel add file: " cands)
                            (completing-read "GPTel add file: " cands nil t)))
                      (read-file-name "GPTel add file: ")))
         (file-path (if in-proj
                        (expand-file-name file-name (projectile-project-root))
                      file-name)))
    (unless (cj/gptel--add-file-to-context file-path)
      (error "Failed to add file: %s" file-path))))

(defun cj/gptel-add-buffer-file ()
  "Select a buffer and add its associated file to the GPTel context.

Lists all open buffers for selection. If the selected buffer is visiting
a file, that file is added to the GPTel context. Otherwise, an error
message is displayed."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (selected-buffer-name (completing-read "Add file from buffer: " buffers nil t))
         (selected-buffer (get-buffer selected-buffer-name))
         (file-path (and selected-buffer
                         (buffer-file-name selected-buffer))))
    (if file-path
        (cj/gptel--add-file-to-context file-path)
      (message "Buffer '%s' is not visiting a file" selected-buffer-name))))

(defun cj/gptel-add-this-buffer ()
  "Add the current buffer to the GPTel context.

Works for any buffer, whether it's visiting a file or not."
  (interactive)
  ;; Load gptel-context if needed
  (unless (featurep 'gptel-context)
    (require 'gptel-context))
  ;; Use gptel-add with prefix arg '(4) to add current buffer
  (gptel-add '(4))
  (message "Added buffer '%s' to GPTel context" (buffer-name)))

;;; ---------------------------- GPTel Configuration ----------------------------

(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind
  (("<f9>" . cj/toggle-gptel)
   :map gptel-mode-map
   ("C-<return>" . gptel-send))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-track-media t)
  ;; TODO: add reasoning to a buffer. See docstring.
  (gptel-include-reasoning 'ignore)
  (gptel-log-level 'info)
  (gptel--debug nil)
  :config
  (cj/ensure-gptel-backends)
  ;; Set ChatGPT as default after initialization
  (setq gptel-backend gptel-chatgpt-backend)

  ;; Named backend list for switching
  (defvar cj/gptel-backends
    `(("Anthropic - Claude" . ,gptel-claude-backend)
      ("OpenAI - ChatGPT" . ,gptel-chatgpt-backend))
    "Alist of GPTel backends for interactive switching.")

  (setq gptel-confirm-tool-calls nil) ;; allow tool access by default
  ;;; ---------------------------- Backend Management ---------------------------

  (setq gptel-backend gptel-chatgpt-backend) ;; use ChatGPT as default
  ;; (setq gptel-backend gptel-claude-backend) ;; use Claude as default

;;; -------------------------- Org Header Construction --------------------------

  ;;  Dynamic user prefix for org-mode heading (string, refreshed just before send)
  (defun cj/gptel--fresh-org-prefix ()
    "Generate a fresh org-mode header with current timestamp for user messages."
    (concat "* " user-login-name " " (format-time-string "[%Y-%m-%d %H:%M:%S]") "\n"))

  ;; Initialize as a string (GPTel expectation)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist)
        (cj/gptel--fresh-org-prefix))

  ;; Refresh immediately before each send for accurate timestamp
  (defun cj/gptel--refresh-org-prefix (&rest _)
    "Update the org-mode prefix with fresh timestamp before sending message."
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist)
          (cj/gptel--fresh-org-prefix)))
  (advice-add 'gptel-send :before #'cj/gptel--refresh-org-prefix)

  ;; AI header on each reply: (e.g. "*** AI: <model> [timestamp]")
  (defun cj/gptel-backend-and-model ()
    "Return backend, model, and timestamp as a single string."
    (let* ((backend (pcase (bound-and-true-p gptel-backend)
                      ((and v (pred vectorp)) (aref v 1))  ;; display name if vector
                      (_ "AI")))
           (model   (format "%s" (or (bound-and-true-p gptel-model) "")))
           (ts      (format-time-string "[%Y-%m-%d %H:%M:%S]")))
      (format "%s: %s %s" backend model ts)))

  (defun cj/gptel-insert-model-heading (response-begin-pos _response-end-pos)
    "Insert an Org heading for the AI reply at RESPONSE-BEGIN-POS."
    (save-excursion
      (goto-char response-begin-pos)
      (insert (format "* %s\n" (cj/gptel-backend-and-model)))))

  (add-hook 'gptel-post-response-functions #'cj/gptel-insert-model-heading))

;;; ---------------------------- Toggle GPTel Window ----------------------------

(defun cj/toggle-gptel ()
  "Toggle the visibility of the AI-Assistant buffer, and place point at its end."
  (interactive)
  (let* ((buf-name "*AI-Assistant*")
         (buffer   (get-buffer buf-name))
         (win      (and buffer (get-buffer-window buffer))))
    (if win
        (delete-window win)
      ;; Ensure GPTel and our backends are initialized before creating the buffer
      (unless (featurep 'gptel)
        (require 'gptel))
      (cj/ensure-gptel-backends)
      (unless buffer
        ;; Pass backend, not model
        (gptel buf-name gptel-backend))
      (setq buffer (get-buffer buf-name))
      (setq win
            (display-buffer-in-side-window
             buffer
             '((side . right)
               (window-width . 0.4))))
      (select-window win)
      (with-current-buffer buffer
        (goto-char (point-max))))))

;; ------------------------------- Clear Context -------------------------------

(defun cj/gptel-context-clear ()
  "Clear all GPTel context sources, with compatibility across GPTel versions."
  (interactive)
  (cond
   ((fboundp 'gptel-context-remove-all)
    (call-interactively 'gptel-context-remove-all)
    (message "GPTel context cleared"))
   ((fboundp 'gptel-context-clear)
    (call-interactively 'gptel-context-clear)
    (message "GPTel context cleared"))
   ((boundp 'gptel-context--alist)
    (setq gptel-context--alist nil)
    (message "GPTel context cleared"))
   (t
    (message "No known GPTel context clearing function available"))))

;;; -------------------------------- GPTel-Magit --------------------------------

(use-package gptel-magit
  :defer t
  :hook (magit-mode . gptel-magit-install))

;; ------------------------------ GPTel Directives -----------------------------

(use-package gptel-prompts
  :load-path (lambda () (expand-file-name "custom/" user-emacs-directory))
  :after gptel
  :if (file-exists-p (expand-file-name "custom/gptel-prompts.el" user-emacs-directory))
  :custom
  (gptel-prompts-directory (concat user-emacs-directory "ai-prompts"))
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

;;; --------------------------------- AI Keymap ---------------------------------

(defvar-keymap cj/ai-keymap
  :doc "Keymap for gptel and other AI operations."
  "B" #'cj/gptel-switch-backend      ;; change the backend (OpenAI, Anthropic, etc.
  "M" #'gptel-menu                   ;; gptel's transient menu
  "d" #'cj/gptel-delete-conversation ;; delete conversation
  "." #'cj/gptel-add-this-buffer     ;; add buffer to context
  "f" #'cj/gptel-add-file            ;; add a file to context
  "l" #'cj/gptel-load-conversation   ;; load and continue conversation
  "m" #'cj/gptel-change-model        ;; change the LLM model
  "p" #'gptel-system-prompt          ;; change prompt
  "&" #'gptel-rewrite                ;; rewrite a region of code/text
  "r" #'cj/gptel-context-clear       ;; remove all context
  "s" #'cj/gptel-save-conversation   ;; save conversation
  "t" #'cj/toggle-gptel              ;; toggles the ai-assistant window
  "x" #'cj/gptel-clear-buffer)        ;; clears the assistant buffer
(keymap-set cj/custom-keymap "a" cj/ai-keymap)

(provide 'ai-config)
;;; ai-config.el ends here.
