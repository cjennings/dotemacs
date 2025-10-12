;;; ai-conversations.el --- GPTel conversation persistence and autosave -*- lexical-binding: t; coding: utf-8; -*-
;; Author: Craig Jennings <c@cjennings.net>
;; Maintainer: Craig Jennings <c@cjennings.net>
;; Version 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;;
;;; Commentary:
;; Provides conversation save/load/delete, autosave after responses, and
;; org-visibility headers for GPTel-powered assistant buffers.
;;
;; Loads lazily via autoloads for the interactive entry points.

;;; Code:

(defgroup cj/ai-conversations nil
  "Conversation persistence for GPTel (save/load/delete, autosave)."
  :group 'gptel
  :prefix "cj/")

(defcustom cj/gptel-conversations-directory
  (expand-file-name "ai-conversations" user-emacs-directory)
  "Directory where GPTel conversations are stored."
  :type 'directory
  :group 'cj/ai-conversations)

(defcustom cj/gptel-conversations-window-side 'right
  "Side to display the AI-Assistant buffer when loading a conversation."
  :type '(choice (const :tag "Right" right)
				 (const :tag "Left" left)
				 (const :tag "Bottom" bottom)
				 (const :tag "Top" top))
  :group 'cj/ai-conversations)

(defcustom cj/gptel-conversations-window-width 0.4
  "Set the side window width when loading a conversation.

If displaying on the top or bottom, treat this value as a height fraction."
  :type 'number
  :group 'cj/ai-conversations)

(defcustom cj/gptel-conversations-sort-order 'newest-first
  "Sort order for conversation selection prompts."
  :type '(choice (const :tag "Newest first" newest-first)
				 (const :tag "Oldest first" oldest-first))
  :group 'cj/ai-conversations)

(defvar-local cj/gptel-autosave-enabled nil
  "Non-nil means auto-save after each AI response in GPTel buffers.")

(defvar-local cj/gptel-autosave-filepath nil
  "File path used for auto-saving the conversation buffer.")

(defcustom cj/gptel-conversations-autosave-on-send t
  "Non-nil means auto-save the conversation immediately after `gptel-send'."
  :type 'boolean
  :group 'cj/ai-conversations)

(defun cj/gptel--autosave-after-send (&rest _args)
  "Auto-save current GPTel buffer right after `gptel-send' if enabled."
  (when (and cj/gptel-conversations-autosave-on-send
			 (bound-and-true-p gptel-mode)
			 cj/gptel-autosave-enabled
			 (stringp cj/gptel-autosave-filepath)
			 (> (length cj/gptel-autosave-filepath) 0))
	(condition-case err
		(cj/gptel--save-buffer-to-file (current-buffer) cj/gptel-autosave-filepath)
	  (error (message "cj/gptel autosave-on-send failed: %s" (error-message-string err))))))

(with-eval-after-load 'gptel
  (unless (advice-member-p #'cj/gptel--autosave-after-send #'gptel-send)
	(advice-add 'gptel-send :after #'cj/gptel--autosave-after-send)))

(defun cj/gptel--slugify-topic (s)
  "Return a filesystem-friendly slug for topic string S."
  (let* ((down (downcase (or s "")))
		 (repl (replace-regexp-in-string "[^a-z0-9]+" "-" down))
		 (trim (replace-regexp-in-string "^-+\\|-+$" "" repl)))
	(or (and (> (length trim) 0) trim) "conversation")))

(defun cj/gptel--existing-topics ()
  "Return topic slugs, without timestamps, present in the conversations directory."
  (when (file-exists-p cj/gptel-conversations-directory)
	(let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$")))
	  (delete-dups
	   (mapcar
		(lambda (f)
		  (replace-regexp-in-string "_[0-9]\\{8\\}-[0-9]\\{6\\}\\.gptel$" "" f))
		files)))))

(defun cj/gptel--latest-file-for-topic (topic-slug)
  "Return the newest saved conversation filename for TOPIC-SLUG, or nil."
  (let* ((rx (format "^%s_[0-9]\\{8\\}-[0-9]\\{6\\}\\.gptel$"
					 (regexp-quote topic-slug)))
		 (files (and (file-exists-p cj/gptel-conversations-directory)
					 (directory-files cj/gptel-conversations-directory nil rx))))
	(car (sort files #'string>))))

(defun cj/gptel--timestamp-from-filename (filename)
  "Return an Emacs timestamp extracted from FILENAME, or nil.

Expect FILENAME to match _YYYYMMDD-HHMMSS.gptel."
  (when (string-match "_\\([0-9]\\{8\\}\\)-\\([0-9]\\{6\\}\\)\\.gptel\\'" filename)
	(let* ((date (match-string 1 filename))
		   (time (match-string 2 filename))
		   (Y (string-to-number (substring date 0 4)))
		   (M (string-to-number (substring date 4 6)))
		   (D (string-to-number (substring date 6 8)))
		   (h (string-to-number (substring time 0 2)))
		   (m (string-to-number (substring time 2 4)))
		   (s (string-to-number (substring time 4 6))))
	  (encode-time s m h D M Y))))

(defun cj/gptel--conversation-candidates ()
  "Return conversation candidates sorted per `cj/gptel-conversations-sort-order'."
  (unless (file-exists-p cj/gptel-conversations-directory)
	(user-error "Conversations directory doesn't exist: %s" cj/gptel-conversations-directory))
  (let* ((files (directory-files cj/gptel-conversations-directory nil "\\.gptel$"))
		 (enriched
		  (mapcar
		   (lambda (f)
			 (let* ((full (expand-file-name f cj/gptel-conversations-directory))
					(ptime (or (cj/gptel--timestamp-from-filename f)
							   (nth 5 (file-attributes full))))
					(disp (format "%s [%s]" f (format-time-string "%Y-%m-%d %H:%M" ptime))))
			   (list :file f :time ptime :display disp)))
		   files))
		 (sorted
		  (sort enriched
				(lambda (a b)
				  (let ((ta (plist-get a :time))
						(tb (plist-get b :time)))
					(if (eq cj/gptel-conversations-sort-order 'newest-first)
						(time-less-p tb ta)  ;; tb earlier than ta => a first
					  (time-less-p ta tb))))))
		 (cands (mapcar (lambda (pl)
						  (cons (plist-get pl :display)
								(plist-get pl :file)))
						sorted)))
    cands))

(defun cj/gptel--save-buffer-to-file (buffer filepath)
  "Save BUFFER content to FILEPATH with Org visibility properties."
  (with-current-buffer buffer
	(let ((content (buffer-string)))
	  (with-temp-buffer
		(insert "#+STARTUP: showeverything\n")
		(insert "#+VISIBILITY: all\n\n")
		(insert content)
		(write-region (point-min) (point-max) filepath nil 'silent))))
  filepath)

(defun cj/gptel--ensure-ai-buffer ()
  "Return the *AI-Assistant* buffer, creating it via `gptel' if needed."
  (let* ((buf-name "*AI-Assistant*")
		 (buffer (get-buffer buf-name)))
	(unless buffer
	  (gptel buf-name))
	(or (get-buffer buf-name)
		(user-error "Could not create or find *AI-Assistant* buffer"))))

;;;###autoload
(defun cj/gptel-save-conversation ()
  "Save the current AI-Assistant buffer to a .gptel file.

Enable autosave for subsequent AI responses to the same file."
  (interactive)
  (let ((buf (get-buffer "*AI-Assistant*")))
	(unless buf
	  (user-error "No AI-Assistant buffer found"))
	(unless (file-exists-p cj/gptel-conversations-directory)
	  (make-directory cj/gptel-conversations-directory t)
	  (message "Created directory: %s" cj/gptel-conversations-directory))
	(let* ((topics (or (cj/gptel--existing-topics) '()))
		   (input (completing-read "Conversation topic: " topics nil nil))
		   (topic-slug (cj/gptel--slugify-topic input))
		   (latest (cj/gptel--latest-file-for-topic topic-slug))
		   (use-existing (and latest
							  (y-or-n-p (format "Update existing file %s? " latest))))
		   (filepath (if use-existing
						 (expand-file-name latest cj/gptel-conversations-directory)
					   (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
							  (filename (format "%s_%s.gptel" topic-slug timestamp)))
						 (expand-file-name filename cj/gptel-conversations-directory)))))
	  (cj/gptel--save-buffer-to-file buf filepath)
	  (with-current-buffer buf
		(setq-local cj/gptel-autosave-filepath filepath)
		(setq-local cj/gptel-autosave-enabled t))
	  (message "Conversation saved to: %s" filepath))))

;;;###autoload
(defun cj/gptel-delete-conversation ()
  "Delete a saved GPTel conversation file (chronologically sorted candidates)."
  (interactive)
  (unless (file-exists-p cj/gptel-conversations-directory)
	(user-error "Conversations directory doesn't exist: %s" cj/gptel-conversations-directory))
  (let* ((cands (cj/gptel--conversation-candidates)))
	(unless cands
	  (user-error "No saved conversations found in %s" cj/gptel-conversations-directory))
	(let* ((completion-extra-properties '(:display-sort-function identity
																 :cycle-sort-function identity))
           (selection (completing-read "Delete conversation: " cands nil t))
		   (filename (cdr (assoc selection cands)))
		   (filepath (and filename
						  (expand-file-name filename cj/gptel-conversations-directory))))
	  (unless filename
		(user-error "No conversation selected"))
	  (when (y-or-n-p (format "Really delete %s? " filename))
		(delete-file filepath)
		(message "Deleted conversation: %s" filename)))))

(defun cj/gptel--strip-visibility-headers ()
  "Strip org visibility headers at the top of the current buffer if present."
  (save-excursion
	(goto-char (point-min))
	(while (looking-at "^#\\+\\(STARTUP\\|VISIBILITY\\):.*\n")
	  (delete-region (match-beginning 0) (match-end 0)))
	(when (looking-at "^\n+")
	  (delete-region (point) (match-end 0)))))

;;;###autoload
(defun cj/gptel-load-conversation ()
  "Load a saved GPTel conversation into the AI-Assistant buffer.

Prompt to save the current conversation first when appropriate, then enable autosave."
  (interactive)
  (let ((ai-buffer (get-buffer-create "*AI-Assistant*")))
	(when (and (with-current-buffer ai-buffer (> (buffer-size) 0))
			   (with-current-buffer ai-buffer (bound-and-true-p gptel-mode)))
	  (when (y-or-n-p "Save current conversation before loading new one? ")
		(with-current-buffer ai-buffer
		  (call-interactively #'cj/gptel-save-conversation)))))
  (unless (file-exists-p cj/gptel-conversations-directory)
	(user-error "Conversations directory doesn't exist: %s" cj/gptel-conversations-directory))
  (let* ((cands (cj/gptel--conversation-candidates)))
	(unless cands
	  (user-error "No saved conversations found in %s" cj/gptel-conversations-directory))
	(let* ((completion-extra-properties '(:display-sort-function identity
																 :cycle-sort-function identity))
           (selection (completing-read "Load conversation: " cands nil t))
		   (filename (cdr (assoc selection cands)))
		   (filepath (and filename
						  (expand-file-name filename cj/gptel-conversations-directory))))
	  (unless filename
		(user-error "No conversation selected"))
	  (with-current-buffer (cj/gptel--ensure-ai-buffer)
		(erase-buffer)
		(insert-file-contents filepath)
		(cj/gptel--strip-visibility-headers)
		(goto-char (point-max))
		(set-buffer-modified-p t)
		(setq-local cj/gptel-autosave-filepath filepath)
		(setq-local cj/gptel-autosave-enabled t))
	  (let ((buf (get-buffer "*AI-Assistant*")))
		(unless (get-buffer-window buf)
		  (display-buffer-in-side-window
		   buf `((side . ,cj/gptel-conversations-window-side)
				 (window-width . ,cj/gptel-conversations-window-width)))))
	  (select-window (get-buffer-window "*AI-Assistant*"))
	  (message "Loaded conversation from: %s" filepath))))

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
  (unless (member #'cj/gptel--autosave-after-response gptel-post-response-functions)
	(add-hook 'gptel-post-response-functions #'cj/gptel--autosave-after-response)))

(provide 'ai-conversations)
;;; ai-conversations.el ends here
