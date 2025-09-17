;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

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
(require 'ai-directives)

;;; ------------------------- GPTel Config And AI-Keymap ------------------------

(defvar ai-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "B") #'cj/gptel-switch-backend)    ;; Change the backend (OpenAI, Anthropic, etc.)
	(define-key map (kbd "M") #'gptel-menu)                 ;; open the gptel transient menu
	(define-key map (kbd "b") #'cj/gptel-add-buffer)        ;; add a buffer to context
	(define-key map (kbd "d") #'cj/gptel-delete-conversation) ;; save conversation
	(define-key map (kbd "f") #'cj/gptel-add-file)          ;; add a file to context
	(define-key map (kbd "l") #'cj/gptel-load-conversation) ;; load and continue conversation
	(define-key map (kbd "m") #'cj/gptel-change-model)      ;; change the LLM model
	(define-key map (kbd "p") #'gptel-system-prompt)        ;; change prompt
	(define-key map (kbd "r") #'gptel-rewrite)              ;; rewrite a region of code/text
	(define-key map (kbd "s") #'cj/gptel-save-conversation) ;; save conversation
	(define-key map (kbd "t") #'cj/toggle-gptel)            ;; toggles the ai-assistant window
	(define-key map (kbd "x") #'cj/gptel-clear-buffer)      ;; clears the assistant buffer
    map)
  "Keymap for AI-related commands (prefix \\<ai-keymap>).
Binds global M-a (overriding default \='backward-sentence\=').")
(global-set-key (kbd "M-a") ai-keymap)

(use-package gptel
  :defer 0.5
  :commands (gptel gptel-send)
  :bind ("<f9>"  . cj/toggle-gptel)
  (:map gptel-mode-map
        ("C-<return>" . gptel-send))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-track-media t)
  ;; TODO: recommend adding to a buffer. See docstring.
  (gptel-include-reasoning 'ignore)
  (gptel-log-level 'info)
  (gptel--debug nil)
  :init
  ;; Define a helper to retrieve secrets from .authinfo.gpg
  (defun cj/auth-source-secret (host user)
    "Fetch secret from auth-source for HOST and USER."
    (let* ((found (auth-source-search :host host :user user :require '(:secret) :max 1))
           (secret (plist-get (car found) :secret)))
      (cond
       ((functionp secret) (funcall secret))
       ((stringp secret) secret)
       (t (error "No usable secret found for host %s and user %s" host user)))))

  ;; Load API keys securely
  (defvar cj/anthropic-api-key nil "Cached Anthropic API key from authinfo.gpg.")
  (defvar cj/openai-api-key nil "Cached OpenAI API key from authinfo.gpg.")

  ;; Default backend: Claude via Anthropic
  (setq cj/anthropic-api-key (cj/auth-source-secret "api.anthropic.com" "apikey"))
  (setq gptel-claude-backend
        (gptel-make-anthropic
            "Claude"
		  :key cj/anthropic-api-key
		  :models '("claude-opus-4-1-20250805")
          :stream t))

  ;; Optional OpenAI backend
  (setq cj/openai-api-key (cj/auth-source-secret "api.openai.com" "apikey"))
  (defvar gptel-chatgpt-backend
    (gptel-make-openai
		"OpenAI - ChatGPT"
      :key cj/openai-api-key
	  :models '("gpt-5")
      :stream t))

  :config
  ;; Named backend list for switching
  (defvar cj/gptel-backends
	`(("Anthropic - Claude" . ,gptel-claude-backend)
	  ("OpenAI - ChatGPT" . ,gptel-chatgpt-backend))
    "Alist of GPTel backends for interactive switching.")

  (defun cj/gptel-switch-backend ()
	"Switch GPTel backend and select a model from that backend.
This is a backend-first approach, whereas `cj/gptel-change-model'
is model-first."
	(interactive)
	(let* ((choice (completing-read "Select GPTel backend: "
									(mapcar #'car cj/gptel-backends)
									nil t))
           (backend (cdr (assoc choice cj/gptel-backends))))
	  (if backend
		  (let* ((models (gptel-backend-models backend))
				 (selected-model
				  (intern (completing-read
						   (format "Select %s model: " choice)
						   (mapcar #'symbol-name models)
						   nil t))))
			(setq gptel-backend backend)
			(setq gptel-model selected-model)
			(message "Switched to %s with model: %s" choice selected-model))
		(user-error "Invalid GPTel backend: %s" choice))))


  (setq gptel-backend gptel-claude-backend) ;; use Claude as default
  ;; (setq gptel-backend gptel-chatgpt-backend) ;; use ChatGPT as default

  (setq gptel-directives
		`((default     . ,default-directive)
		  (accountant  . ,accountant-directive)
		  (coder       . ,coder-directive)
		  (contractor  . ,contractor-directive)
		  (emacs       . ,emacs-directive)
		  (package-pm  . ,package-pm-directive)
		  (email       . ,email-directive)
		  (historian   . ,historian-directive)
		  (proofreader . ,proofreader-directive)
		  (llm-prompt  . ,prompt-directive)
		  (qa          . ,qa-directive)
		  (reviewer    . ,reviewer-directive)))
  (setq gptel-default-directive default-directive))


;;; -------------------- User And Model Names In Org Headers --------------------

(with-eval-after-load 'gptel
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

  ;; Hook is now at the proper level - will be executed when with-eval-after-load runs
  (add-hook 'gptel-post-response-functions #'cj/gptel-insert-model-heading))

;;; ---------------------------- Toggle GPTel Window ----------------------------

(with-eval-after-load 'gptel
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
          (goto-char (point-max)))))))

;;; ----------------------------- Clear Gptel Buffer ----------------------------

(with-eval-after-load 'gptel
  (defun cj/gptel-clear-buffer ()
    "Erase the contents of the current GPTel buffer leaving initial org heading.
Only works in buffers with gptel-mode active."
    (interactive)
    (let ((is-gptel (bound-and-true-p gptel-mode))
          (is-org (derived-mode-p 'org-mode)))
      ;; debug info to Messages
      ;; (message "Debug: gptel-mode: %s, org-mode: %s, major-mode: %s"
      ;;          is-gptel is-org major-mode)

      (if (and is-gptel is-org)
          (progn
            (erase-buffer)
            ;; re-insert the user heading with fresh timestamp
            (insert (cj/gptel--fresh-org-prefix))
            (message "GPTel buffer cleared and heading reset"))
        (message "Not a GPTel buffer in org-mode. Nothing cleared.")))))

;;; ---------------------------- Context Manipulation ---------------------------

(with-eval-after-load 'gptel
  (defun cj/gptel-add-buffer ()
    "Add a buffer to the GPTel context.
Prompts for a buffer name and adds its entire content as context.
By default shows only regular buffers (not special buffers), but
allows access to all buffers via completion."
    (interactive)
    (let* ((buffers (mapcar #'buffer-name (buffer-list)))
           ;; Filter out special buffers by default (those starting with space or *)
           ;; But keep them in the collection for completion
           (default-buffers (cl-remove-if (lambda (name)
                                            (or (string-prefix-p " " name)
                                                (string-prefix-p "*" name)))
                                          buffers))
           (buffer-name (completing-read
                         "Buffer to add as context: "
                         buffers
                         nil  ; No predicate
                         t    ; Require match
                         nil  ; No initial input
                         nil  ; No history
                         (car default-buffers))) ; Default to first regular buffer
           (buffer (get-buffer buffer-name)))
      (when buffer
        (if (and (buffer-file-name buffer)
                 (y-or-n-p "This buffer is associated with a file. Add file instead? "))
            ;; If it's a file buffer and user confirms, add the file instead
            (gptel-add-file (buffer-file-name buffer))
          ;; Otherwise add the buffer content
          (gptel-context--add-region
           buffer
           (with-current-buffer buffer (point-min))
           (with-current-buffer buffer (point-max))
           t)
          (message "Buffer '%s' added as context." buffer-name)))))

  (defun cj/gptel-change-model ()
	"Change the AI model and backend for gptel.
Presents all available models from all backends, automatically switching
backend when needed. Prompts for scope (global or buffer-local)."
	(interactive)
	(let* ((all-models '())
		   ;; Collect all models from all backends
		   (backends-models
			(mapcar (lambda (backend-cons)
					  (let* ((backend-name (car backend-cons))
							 (backend (cdr backend-cons))
							 (models (gptel-backend-models backend)))
						(dolist (model models)
						  (push (list (format "%s: %s" backend-name model)
									  backend
									  model
									  backend-name)
								all-models))
						backend-cons))
					cj/gptel-backends))
		   ;; Current selection for default
		   (current-backend-name
			(car (rassoc gptel-backend cj/gptel-backends)))
		   (current-selection
			(format "%s: %s" current-backend-name gptel-model))
		   ;; Prompt user
		   (scope (completing-read "Set model for: " '("buffer" "global") nil t))
		   (selected (completing-read
					  (format "Select model (current: %s): " current-selection)
					  (mapcar #'car all-models)
					  nil t nil nil
					  current-selection)))

	  ;; Find the selected model info
	  (let* ((model-info (assoc selected all-models))
			 (backend (nth 1 model-info))
			 (model (nth 2 model-info))
			 (backend-name (nth 3 model-info)))

		;; Apply changes based on scope
		(if (string= scope "global")
			(progn
			  (setq gptel-backend backend)
			  (setq gptel-model model)
			  (message "Changed to %s model: %s (global)" backend-name model))
		  (setq-local gptel-backend backend)
		  (setq-local gptel-model model)
		  (message "Changed to %s model: %s (buffer-local)" backend-name model))))))

(with-eval-after-load 'gptel
  (with-eval-after-load 'projectile
	(defun cj/gptel-add-file ()
	  "Add a file to the GPTel context.
If inside a Projectile project, prompt from the project's file list;
otherwise use `read-file-name'."
	  (interactive)
	  (let* ((in-proj (and (fboundp 'projectile-project-p)
						   (projectile-project-p)))
			 (file-name (if in-proj
							(projectile-completing-read
							 "GPTel add file: "
							 (projectile-current-project-files))
						  (read-file-name "GPTel add file: ")))
			 ;; Ensure we have a full path when using projectile
			 (file-path (if in-proj
							(expand-file-name file-name (projectile-project-root))
						  file-name)))
		;; Debug output
		(message "Adding file to context: %s" file-path)

		;; Call the gptel built-in function directly
		(gptel-add-file file-path)

		;; Verify context was added
		(message "Current context has %d sources"
				 (length gptel-context--alist))))))

;;; ----------------------- GPTel Conversation Management -----------------------

(defcustom cj/gptel-conversations-directory
  (expand-file-name "ai-conversations" user-emacs-directory)
  "Directory where GPTel conversations are stored.
Defaults to ~/.emacs.d/ai-conversations/"
  :type 'directory
  :group 'gptel)

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

(with-eval-after-load 'gptel
  (defun cj/gptel-save-conversation ()
	"Save the current AI-Assistant buffer to a file with .gptel extension.
Offers existing conversation topics as options but allows entering new topics."
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
						 ;; Create new file with timestamp
						 (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
								(filename (format "%s_%s.gptel" clean-topic timestamp)))
						   (expand-file-name filename cj/gptel-conversations-directory)))))

		;; Save the buffer
		(cj/gptel--save-buffer-to-file buf filepath)
		(message "Conversation saved to: %s" filepath)))))


(with-eval-after-load 'gptel
  (defun cj/gptel-delete-conversation ()
	"Delete a saved GPTel conversation file.
Presents a list of .gptel files for selection and confirms before deletion."
	(interactive)

	;; Check directory exists
	(unless (file-exists-p cj/gptel-conversations-directory)
	  (user-error "Conversations directory doesn't exist: %s"
				  cj/gptel-conversations-directory))

	;; Get all .gptel files
	(let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$"))
		   (files-with-dates
			(mapcar (lambda (f)
					  (let* ((full-path (expand-file-name f cj/gptel-conversations-directory))
							 (mod-time (nth 5 (file-attributes full-path)))
							 (time-str (format-time-string "%Y-%m-%d %H:%M" mod-time)))
						(cons (format "%-40s [%s]" f time-str) f)))
					files)))

	  (unless files
		(user-error "No saved conversations found in %s"
					cj/gptel-conversations-directory))

	  ;; Let user select a file
	  (let* ((selection (completing-read "Delete conversation: " files-with-dates nil t))
			 (filename (cdr (assoc selection files-with-dates)))
			 (filepath (expand-file-name filename cj/gptel-conversations-directory)))

		;; Confirm deletion
		(when (y-or-n-p (format "Really delete %s? " filename))
		  (delete-file filepath)
		  (message "Deleted conversation: %s" filename))))))

(with-eval-after-load 'gptel
  (defun cj/gptel-load-conversation ()
	"Load a saved GPTel conversation into the AI-Assistant buffer.
If the current buffer has content, prompts to save it first.
Presents a list of .gptel files for selection and loads the chosen file."
	(interactive)

	;; Check if AI-Assistant buffer exists, create if needed
	(let ((ai-buffer (get-buffer-create "*AI-Assistant*")))

	  ;; If buffer has content and gptel-mode is active, offer to save
	  (when (and (with-current-buffer ai-buffer
				   (> (buffer-size) 0))
				 (with-current-buffer ai-buffer
				   (bound-and-true-p gptel-mode)))
		(when (y-or-n-p "Save current conversation before loading new one? ")
		  (with-current-buffer ai-buffer
			(call-interactively #'cj/gptel-save-conversation))))

	  ;; Check directory exists
	  (unless (file-exists-p cj/gptel-conversations-directory)
		(user-error "Conversations directory doesn't exist: %s"
					cj/gptel-conversations-directory))

	  ;; Get all .gptel files
	  (let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$"))
			 (files-with-dates
			  (mapcar (lambda (f)
						(let* ((full-path (expand-file-name f cj/gptel-conversations-directory))
							   (mod-time (nth 5 (file-attributes full-path)))
							   (time-str (format-time-string "%Y-%m-%d %H:%M" mod-time)))
						  (cons (format "%-40s [%s]" f time-str) f)))
					  files)))

		(unless files
		  (user-error "No saved conversations found in %s"
					  cj/gptel-conversations-directory))

		;; Let user select a file
		(let* ((selection (completing-read "Load conversation: " files-with-dates nil t))
			   (filename (cdr (assoc selection files-with-dates)))
			   (filepath (expand-file-name filename cj/gptel-conversations-directory)))

		  ;; Clear buffer and insert file contents
		  (with-current-buffer ai-buffer
			;; Ensure gptel-mode is active
			(unless (bound-and-true-p gptel-mode)
			  (gptel "*AI-Assistant*")  ;; Initialize gptel if not already active
			  (org-mode)
			  (gptel-mode 1))

			;; Clear and insert the conversation
			(erase-buffer)
			(insert-file-contents filepath)

			;; Remove the org properties if present at the beginning
			(goto-char (point-min))
			(when (looking-at "^#\\+STARTUP:.*\n#\\+VISIBILITY:.*\n\n")
			  (delete-region (point) (match-end 0)))

			;; Position at end and mark as modified
			(goto-char (point-max))
			(set-buffer-modified-p t))

		  ;; Show buffer in a side window if not already visible
		  (unless (get-buffer-window ai-buffer)
			(if (fboundp 'cj/toggle-gptel)
				(cj/toggle-gptel)
			  ;; Fallback to display in side window
			  (display-buffer-in-side-window
			   ai-buffer
			   '((side . right)
				 (window-width . 0.4)))))

		  ;; Select the window
		  (select-window (get-buffer-window ai-buffer))
		  (message "Loaded conversation from: %s" filepath))))))

;;; -------------------------------- GPTel-Magit --------------------------------

(use-package gptel-magit
  :defer t
  :hook (magit-mode . gptel-magit-install))

(provide 'ai-config)
;;; ai-config.el ends here
