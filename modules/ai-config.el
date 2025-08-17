;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Here is my basic workflow:

;; - Launch GPTel via F9 or C-h g t, and chat with the AI in the side window.
;;   Remember that sending the message requires C-<return>.

;; Note that you can save a file, then turn on gptel-mode to resume your
;; conversation.


;;; ------------------------------- Directives ---------------------------------

(defvar default-directive
  "You are a large language model living in Emacs. You understand philosophy, critical theory, and comparative
  literature at a university graduate student level. You are concise and always provide references to source material.")

(defvar code-only-directive
  "You are a large language model living in Emacs. You are an expert in emacs-lisp, Python, Golang, and Shell scripting.
  You encourage unit testing and always provide unit tests when you provide code.")

(defvar writing-directive
  "You are a large language model and a writing assistant. Respond concisely.")

(defvar chat-directive
  "You are a large language model and a funny conversation partner who asks good questions.")

;;; ------------------------------ Toggle GPTel --------------------------------


(defun cj/toggle-gptel ()
  "Toggle the visibility of the ChatGPT buffer, and when shown place point at its end."
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

;; ------------------------- GPTel Config And AI-Keymap ------------------------

(defvar ai-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "t") #'cj/toggle-gptel)
	(define-key map (kbd "c") #'cj/gptel-clear-buffer)
	map)
  "Keymap for AI commands, bound to C-h g…")
(global-set-key (kbd "C-h g") ai-keymap)

(use-package gptel
  :defer 0.5
  :commands (gptel gptel-send)
  :bind
  ("<f9>"  . cj/toggle-gptel)
  (:map gptel-mode-map
		("C-<return>" . gptel-send))
  :custom
  (gptel-default-directive 'code-only)
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-track-media t)
  (gptel-include-reasoning 'ignore)
  (gptel-model 'o4-mini)     ;; keep or change to your preferred model
  (gptel-log-level 'info)
  (gptel--debug nil)
  :config
  ;; Directives
  (setq gptel-directives
		`((default   . ,default-directive)
		  (code-only . ,code-only-directive)
		  (writing   . ,writing-directive)
		  (chat      . ,chat-directive)))

  ;;  Dynamic user prefix for org-mode heading (string, refreshed just before send)
  (defun cj/gptel--fresh-org-prefix ()
	(concat "*** cj " (format-time-string "[%Y-%m-%d %H:%M:%S]") "\n"))

  ;; Initialize as a string (GPTel expectation)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist)
		(cj/gptel--fresh-org-prefix))

  ;; Refresh immediately before each send for accurate timestamp
  (defun cj/gptel--refresh-org-prefix (&rest _)
	(setf (alist-get 'org-mode gptel-prompt-prefix-alist)
		  (cj/gptel--fresh-org-prefix)))
  (advice-add 'gptel-send :before #'cj/gptel--refresh-org-prefix)

  ;; AI header on each reply: (e.g. "*** ChatGPT: <model> [timestamp]")
  (defun cj/gptel-backend-and-model ()
	"Return backend, model, and timestamp as a single string."
	(let* ((backend (pcase (bound-and-true-p gptel-backend)
					  ((and v (pred vectorp)) (aref v 1))  ;; display name if vector
					  (_ "ChatGPT")))
		   (model   (format "%s" (or (bound-and-true-p gptel-model) "")))
		   (ts      (format-time-string "[%Y-%m-%d %H:%M:%S]")))
	  (format "%s: %s %s" backend model ts)))

  (defun cj/gptel-insert-model-heading (response-begin-pos _response-end-pos)
	"Insert an Org heading for the AI reply at RESPONSE-BEGIN-POS."
	(save-excursion
	  (goto-char response-begin-pos)
	  (insert (format "*** %s\n" (cj/gptel-backend-and-model)))))

  (defun cj/gptel-clear-buffer ()
	"Erase the contents of the *AI-Assistant* buffer, re-insert the org heading, and message."
	(interactive)
	(let ((buf (get-buffer "*AI-Assistant*")))
	  (if (not buf)
		  (message "No AI buffer found")
		(with-current-buffer buf
		  (erase-buffer)
		  ;; re-insert the user heading with fresh timestamp
		  (insert (cj/gptel--fresh-org-prefix))
		  (message "AI buffer cleared and heading reset")))))

  ;; Hook is called with (BEG END); add our per-reply heading
  (add-hook 'gptel-post-response-functions #'cj/gptel-insert-model-heading)

  ;; ---- Auth: pick the API key from your auth source
  (setq auth-sources `((:source ,authinfo-file)))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))

;; -------------------------------- GPTel-Magit --------------------------------

(use-package gptel-magit
  :defer .5
  :hook (magit-mode . gptel-magit-install))

(provide 'ai-config)
;;; ai-config.el ends here
