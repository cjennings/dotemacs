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
;; Uses AI directives from ai-directives.el for specialized AI behaviors.

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules/"))

;;; ------------------------- AI Config Helper Functions ------------------------

;; Define all our variables upfront
(defvar cj/anthropic-api-key-cached nil "Cached Anthropic API key.")
(defvar cj/openai-api-key-cached nil "Cached OpenAI API key.")
(defvar gptel-claude-backend nil "Claude backend, lazy-initialized.")
(defvar gptel-chatgpt-backend nil "ChatGPT backend, lazy-initialized.")


(defun cj/auth-source-secret (host user)
  "Fetch secret from auth-source for HOST and USER."
  (let* ((found (auth-source-search :host host :user user :require '(:secret) :max 1))
		 (secret (plist-get (car found) :secret)))
	(cond
	 ((functionp secret) (funcall secret))
	 ((stringp secret) secret)
	 (t (error "No usable secret found for host %s and user %s" host user)))))

(defun cj/anthropic-api-key ()
  "Get Anthropic API key, caching result after first retrieval."
  (or cj/anthropic-api-key-cached
	  (setq cj/anthropic-api-key-cached
			(cj/auth-source-secret "api.anthropic.com" "apikey"))))

(defun cj/openai-api-key ()
  "Get OpenAI API key, caching result after first retrieval."
  (or cj/openai-api-key-cached
	  (setq cj/openai-api-key-cached
			(cj/auth-source-secret "api.openai.com" "apikey"))))

(defun cj/ensure-gptel-backends ()
  "Initialize GPTel backends if not already done.
This function should only be called AFTER gptel is loaded."
  (unless gptel-claude-backend
	(setq gptel-claude-backend
		  (gptel-make-anthropic
			  "Claude"
			:key (cj/anthropic-api-key)
			:models '("claude-opus-4-1-20250805")
			:stream t)))
  (unless gptel-chatgpt-backend
	(setq gptel-chatgpt-backend
		  (gptel-make-openai
			  "OpenAI - ChatGPT"
			:key (cj/openai-api-key)
			:models '("gpt-5")
			:stream t)))
  ;; Set default backend
  (setq gptel-backend (or gptel-claude-backend gptel-chatgpt-backend)))

;; Since cj/toggle-gptel is bound to F9 but defined in :config
(autoload 'cj/toggle-gptel "ai-config" "Toggle the AI-Assistant window" t)

;; ------------------ Gptel Conversation And Utility Commands ------------------

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
  "Change the AI model and backend for gptel.
Presents all available models from all backends, automatically switching
backend when needed. Prompts for scope (global or buffer-local)."
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
		   (model (nth 2 model-info))
		   (backend-name (nth 3 model-info)))
	  (if (string= scope "global")
		  (progn
			(setq gptel-backend backend)
			(setq gptel-model model)
			(message "Changed to %s model: %s (global)" backend-name model))
		(setq-local gptel-backend backend)
		(setq-local gptel-model model)
		(message "Changed to %s model: %s (buffer-local)" backend-name model)))))

(defun cj/gptel-switch-backend ()
  "Switch GPTel backend and select a model from that backend."
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
  "Erase the contents of the current GPTel buffer leaving initial org heading.
Only works in buffers with gptel-mode active."
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

;; Add a file to GPTel context (made resilient to Projectile not being loaded)
(defun cj/gptel-add-file ()
  "Add a file to the GPTel context.
If inside a Projectile project, prompt from the project's file list;
otherwise use `read-file-name'."
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
	(gptel-add-file file-path)
	(when (boundp 'gptel-context--alist)
	  (message "Current context has %d sources" (length gptel-context--alist)))))

;;; Conversation Management (moved out of use-package)

(defcustom cj/gptel-conversations-directory
  (expand-file-name "ai-conversations" user-emacs-directory)
  "Directory where GPTel conversations are stored."
  :type 'directory
  :group 'gptel)

(defun cj/gptel--save-buffer-to-file (buffer filepath)
  "Save the BUFFER content to FILEPATH with org visibility properties."
  (with-current-buffer buffer
	(let ((content (buffer-string)))
	  (with-temp-buffer
		(insert "#+STARTUP: showeverything\n")
		(insert "#+VISIBILITY: all\n\n")
		(insert content)
		(write-region (point-min) (point-max) filepath nil 'silent))))
  filepath)

(defun cj/gptel-save-conversation ()
  "Save the current AI-Assistant buffer to a .gptel file.
Also enables autosave for subsequent AI responses to this same file."
  (interactive)
  (let ((buf (get-buffer "*AI-Assistant*")))
	(unless buf
	  (user-error "No AI-Assistant buffer found"))
	;; Ensure directory exists
	(unless (file-exists-p cj/gptel-conversations-directory)
	  (make-directory cj/gptel-conversations-directory t)
	  (message "Created directory: %s" cj/gptel-conversations-directory))
	;; Get existing topic names (without timestamps)
	(let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$"))
		   (topics (delete-dups
					(mapcar (lambda (f)
							  (replace-regexp-in-string "_[0-9]\\{8\\}-[0-9]\\{6\\}\\.gptel$" "" f))
							files)))
		   (topic (completing-read "Conversation topic: " topics nil nil))
		   (clean-topic (replace-regexp-in-string "[^a-zA-Z0-9-_]" "-" topic))
		   (existing-files (directory-files cj/gptel-conversations-directory nil
											(format "^%s_[0-9]\\{8\\}-[0-9]\\{6\\}\\.gptel$"
													(regexp-quote clean-topic))))
		   (newest-file (car (sort existing-files #'string>)))
		   (use-existing (and newest-file
							  (y-or-n-p (format "Update existing file %s? " newest-file))))
		   (filepath (if use-existing
						 (expand-file-name newest-file cj/gptel-conversations-directory)
					   (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
							  (filename (format "%s_%s.gptel" clean-topic timestamp)))
						 (expand-file-name filename cj/gptel-conversations-directory)))))
	  ;; Save the buffer
	  (cj/gptel--save-buffer-to-file buf filepath)
	  ;; Enable/refresh autosave target in this buffer
	  (with-current-buffer buf
		(setq-local cj/gptel-autosave-filepath filepath)
		(setq-local cj/gptel-autosave-enabled t))
	  (message "Conversation saved to: %s" filepath))))

(defun cj/gptel-delete-conversation ()
  "Delete a saved GPTel conversation file."
  (interactive)
  (unless (file-exists-p cj/gptel-conversations-directory)
	(user-error "Conversations directory doesn't exist: %s"
				cj/gptel-conversations-directory))
  (let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$")))
	(unless files
	  (user-error "No saved conversations found in %s"
				  cj/gptel-conversations-directory))
	(let* ((files-with-dates
			(mapcar (lambda (f)
					  (let* ((full-path (expand-file-name f cj/gptel-conversations-directory))
							 (mod-time (nth 5 (file-attributes full-path)))
							 (time-str (format-time-string "%Y-%m-%d %H:%M" mod-time)))
						(cons (format "%-40s [%s]" f time-str) f)))
					files))
		   (selection (completing-read "Delete conversation: " files-with-dates nil t))
		   (filename (cdr (assoc selection files-with-dates)))
		   (filepath (expand-file-name filename cj/gptel-conversations-directory)))
	  (when (y-or-n-p (format "Really delete %s? " filename))
		(delete-file filepath)
		(message "Deleted conversation: %s" filename)))))

(defun cj/gptel-load-conversation ()
  "Load a saved GPTel conversation into the AI-Assistant buffer.
Prompts to save the current one if present, and enables autosave."
  (interactive)
  (let ((ai-buffer (get-buffer-create "*AI-Assistant*")))
	;; Offer to save existing conversation
	(when (and (with-current-buffer ai-buffer (> (buffer-size) 0))
			   (with-current-buffer ai-buffer (bound-and-true-p gptel-mode)))
	  (when (y-or-n-p "Save current conversation before loading new one? ")
		(with-current-buffer ai-buffer
		  (call-interactively #'cj/gptel-save-conversation))))
	;; Ensure directory exists and has files
	(unless (file-exists-p cj/gptel-conversations-directory)
	  (user-error "Conversations directory doesn't exist: %s"
				  cj/gptel-conversations-directory))
	(let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$")))
	  (unless files
		(user-error "No saved conversations found in %s"
					cj/gptel-conversations-directory))
	  (let* ((files-with-dates
			  (mapcar (lambda (f)
						(let* ((full-path (expand-file-name f cj/gptel-conversations-directory))
							   (mod-time (nth 5 (file-attributes full-path)))
							   (time-str (format-time-string "%Y-%m-%d %H:%M" mod-time)))
						  (cons (format "%-40s [%s]" f time-str) f)))
					  files))
			 (selection (completing-read "Load conversation: " files-with-dates nil t))
			 (filename (cdr (assoc selection files-with-dates)))
			 (filepath (expand-file-name filename cj/gptel-conversations-directory)))
		;; Ensure gptel is initialized in the target buffer
		(with-current-buffer ai-buffer
		  (unless (bound-and-true-p gptel-mode)
			(gptel "*AI-Assistant*")
			(org-mode)
			(gptel-mode 1))
		  (erase-buffer)
		  (insert-file-contents filepath)
		  (goto-char (point-min))
		  (when (looking-at "^#\\+STARTUP:.*\n#\\+VISIBILITY:.*\n\n")
			(delete-region (point) (match-end 0)))
		  (goto-char (point-max))
		  (set-buffer-modified-p t)
		  (setq-local cj/gptel-autosave-filepath filepath)
		  (setq-local cj/gptel-autosave-enabled t))
		;; Show buffer in a side window if not visible
		(unless (get-buffer-window ai-buffer)
		  (if (fboundp 'cj/toggle-gptel)
			  (cj/toggle-gptel)
			(display-buffer-in-side-window
			 ai-buffer '((side . right) (window-width . 0.4)))))
		(select-window (get-buffer-window ai-buffer))
		(message "Loaded conversation from: %s" filepath)))))

;;; Autosave after responses (define early, add hook after gptel loads)

(defvar-local cj/gptel-autosave-enabled nil
  "When non-nil in a GPTel conversation buffer, auto-save after each AI response.")

(defvar-local cj/gptel-autosave-filepath nil
  "File path used for auto-saving the conversation buffer.")

(defun cj/gptel--autosave-after-response (&rest _args)
  "Auto-save the current GPTel buffer when enabled."
  (when (and (bound-and-true-p gptel-mode)
			 cj/gptel-autosave-enabled
			 (stringp cj/gptel-autosave-filepath)
			 (> (length cj/gptel-autosave-filepath) 0))
	(condition-case err
		(cj/gptel--save-buffer-to-file (current-buffer) cj/gptel-autosave-filepath)
	  (error (message "cj/gptel autosave failed: %s" (error-message-string err))))))

(with-eval-after-load 'gptel
  (add-hook 'gptel-post-response-functions #'cj/gptel--autosave-after-response))

;;; ---------------------------- GPTel Configuration ----------------------------

(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind ("<f9>" . cj/toggle-gptel)
  (:map gptel-mode-map
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

  ;; Named backend list for switching
  (defvar cj/gptel-backends
	`(("Anthropic - Claude" . ,gptel-claude-backend)
	  ("OpenAI - ChatGPT" . ,gptel-chatgpt-backend))
    "Alist of GPTel backends for interactive switching.")

  ;;; ---------------------------- Backend Management ---------------------------

  ;; (setq gptel-backend gptel-claude-backend) ;; use Claude as default
  (setq gptel-backend gptel-chatgpt-backend) ;; use ChatGPT as default

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

  (add-hook 'gptel-post-response-functions #'cj/gptel-insert-model-heading)

;;; ----------------------- GPTel Conversation Management -----------------------

  (defun cj/gptel--save-buffer-to-file (buffer filepath)
	"Save the BUFFER content to FILEPATH with org visibility properties.
Adds org-mode startup properties to ensure content is visible when reopened."
	(with-current-buffer buffer
	  (let ((content (buffer-string)))
		;; Create temp buffer to add properties
		(with-temp-buffer
		  ;; Add org properties to ensure everything is shown on load
		  (insert "#+STARTUP: showeverything\n")
		  (insert "#+VISIBILITY: all\n\n")
		  (insert content)
		  (write-region (point-min) (point-max) filepath nil 'silent))))
	filepath)
)
;;; ---------------------------- Toggle GPTel Window ----------------------------

(defun cj/toggle-gptel ()
  "Toggle the visibility of the AI-Assistant buffer, and place point at its end."
  (interactive)
  (let* ((buf-name "*AI-Assistant*")
		 (buffer   (get-buffer buf-name))
		 (win      (and buffer (get-buffer-window buffer))))
	(if win
		;; If it's already visible, just close it
		(delete-window win)
	  ;; Otherwise ensure the buffer exists
	  (unless buffer
		(gptel buf-name gptel-model))
	  (setq buffer (get-buffer buf-name))
	  ;; Display in a side window, select it, and move point to end
	  (setq win
			(display-buffer-in-side-window
			 buffer
			 '((side . right)
			   (window-width . 0.4))))
	  (select-window win)
	  (with-current-buffer buffer
		(goto-char (point-max))))))

;;; -------------------------------- GPTel-Magit --------------------------------

(use-package gptel-magit
  :defer t
  :hook (magit-mode . gptel-magit-install))

;; ------------------------------ GPTel Directives -----------------------------

(use-package gptel-prompts
  :load-path "custom/gptel-prompts.el"
  :after (gptel)
  :custom
  (gptel-prompts-directory (concat user-emacs-directory "ai-prompts"))
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))

;;; --------------------------------- AI Keymap ---------------------------------

(defvar ai-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "B") #'cj/gptel-switch-backend)    ;; Change the backend (OpenAI, Anthropic, etc.)
	(define-key map (kbd "M") #'gptel-menu)                 ;; gptel's transient menu
	(define-key map (kbd "d") #'cj/gptel-delete-conversation) ;; delete conversation
	(define-key map (kbd "f") #'cj/gptel-add-file)          ;; add a file to context
	(define-key map (kbd "l") #'cj/gptel-load-conversation) ;; load and continue conversation
	(define-key map (kbd "m") #'cj/gptel-change-model)      ;; change the LLM model
	(define-key map (kbd "p") #'gptel-system-prompt)        ;; change prompt
	(define-key map (kbd "&") #'gptel-rewrite)              ;; rewrite a region of code/text
	(define-key map (kbd "r") #'gptel-context-remove-all)   ;; remove all context
	(define-key map (kbd "s") #'cj/gptel-save-conversation) ;; save conversation
	(define-key map (kbd "t") #'cj/toggle-gptel)            ;; toggles the ai-assistant window
	(define-key map (kbd "x") #'cj/gptel-clear-buffer)      ;; clears the assistant buffer
	map)
  "Keymap for AI-related commands (prefix \\<ai-keymap>).
Binds global M-a (overriding default \='backward-sentence\=').")
(global-set-key (kbd "M-a") ai-keymap)

(provide 'ai-config)
;;; ai-config.el ends here
