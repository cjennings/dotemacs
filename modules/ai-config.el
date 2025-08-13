;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; There are several workflows available. Here are the ones I use most.

;; - Launch GPTel and chat with the AI in a separate buffer.
;;   Chatting is fine, but it can mean cutting and pasting code back and forth
;;   between buffers.

;; - Select a region and launch GPTel.
;;   The region is automatically inserted into the buffer making it easy to
;;   simply ask a question after it's read the code.

;; Note that you can save a file, then turn on gptel-mode to resume your
;; conversation.

;; Remember that sending the message requires C-<return>.

;;; Code:

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


;; -------------------------------- Toggle GPTel -------------------------------
;; Toggle GPTel's buffer in a side window

(defun cj/toggle-gptel ()
  "Toggle the visibility of the ChatGPT buffer without prompting for a name."
  (interactive)
  (let ((buffer (get-buffer "*AI-Assistant*")))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (if buffer
          (display-buffer-in-side-window buffer '((side . right) (window-width . 0.4)))
        ;; Call gptel with a fixed buffer name to skip prompt
        (gptel "*AI-Assistant*" gptel-model)))))

;; ;; defer prefixing the prompt until gptel is actually loaded
;; (with-eval-after-load 'gptel
;;   ;; Define the var if the package hasn't yet
;;   (unless (boundp 'gptel-prompt-prefix-alist)
;;     (defvar gptel-prompt-prefix-alist nil
;;       "Alist mapping major modes to prompt prefixes for gptel.")
;;     (add-to-list 'gptel-prompt-prefix-alist
;;                  (cons 'org-mode
;;                        (concat "*** cj "
;;                                (format-time-string "[%Y-%m-%d %H:%M:%S]")
;;                                "\n")))))

;; ----------------------------------- GPTel -----------------------------------
;; Emacs integration with large language models

(use-package gptel
  :defer .5
  :commands (gptel gptel-send)
  :bind
  (("C-h G" . cj/toggle-gptel)
   ("<f9>" . cj/toggle-gptel)
   (:map gptel-mode-map
         ("C-<return>" . gptel-send)))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-track-media t)
  (gptel-include-reasoning 'ignore)
  (gptel-model 'gpt-4o)
  (gptel-log-level 'info)
  (gptel--debug nil)
  :config
  (setq gptel-directives
        `((default   . ,default-directive)
          (code-only . ,code-only-directive)
          (writing   . ,writing-directive)
          (chat      . ,chat-directive)))

  ;; fancy prompt
  (add-to-list 'gptel-prompt-prefix-alist
               (cons 'org-mode
                     (concat "*** cj "
                             (format-time-string "[%Y-%m-%d %H:%M:%S]")
                             "\n")))

  ;; Grab the secret from the auth file
  (setq auth-sources `((:source ,authinfo-file)))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))

;; -------------------------- GPTel Fancy Org Headers --------------------------
;; each response will be generated under it's own org mode header with backend
;; and model indicated. This is useful for leveraging several LLMs and comparing
;; and saving responses between them.

(defun cj/gptel-backend-and-model ()
  "Return gptel backend and model with timestamp in the desired format."
  (let ((backend (if (boundp 'gptel-backend) (aref gptel-backend 1)))
		(model (if (boundp 'gptel-model) gptel-model))
		(timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]")))
	(format "%s: %s %s" backend model timestamp)))

(defun cj/gptel-insert-model-in-non-gptel-buffers ()
  "Add the backend, model, and timestamp in non-dedicated GPTel buffers.
To be used in `gptel-pre-response-hook`."
  (unless (member 'gptel-mode local-minor-modes)
	(goto-char (point-max))
	(insert (format "\n%s: " (cj/gptel-backend-and-model)))
	(goto-char (point-max))))

(defun cj/gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
  "Add the backend, model, and timestamp in dedicated chat buffers.
Can be used with the `gptel-post-response-functions` hook."
  (let* ((gptel-org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
		 (inserted-string (format "%s %s\n"
								  (substring gptel-org-prefix 0 (string-match " " gptel-org-prefix))
								  (cj/gptel-backend-and-model)))
		 (len-inserted (length inserted-string)))
	(goto-char response-begin-pos)
	(insert inserted-string)
	(goto-char (+ response-end-pos len-inserted))))

;; (defun cj/gptel-backend-and-model ()
;;   "Return gptel backend and model (if any)."
;;   (let ((backend (if  (boundp 'gptel-backend)  (aref gptel-backend 1)))
;;         (model (if  (boundp 'gptel-model) gptel-model)))
;;     (format "(%s %s)" backend model)))

;; (defun cj/gptel-insert-model-in-non-gptel-buffers ()
;;   "This function will add the backend and model in the \"dynamic\" buffers, not in dedicated chat buffers.
;; To be used in `gptel-pre-response-hook'."
;;   (unless (member 'gptel-mode local-minor-modes)
;;     (goto-char (point-max))
;;     (insert (format "\n%s: " (cj/gptel-backend-and-model)))
;;     (goto-char (point-max))))
;; (add-hook 'gptel-pre-response-hook 'cj/gptel-insert-model-in-non-gptel-buffers)

;; (defun cj/gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
;;   "This function adds the backend and model in dedicated chat buffers.
;; Can be used with the `gptel-post-response-functions' hook."
;;   (let* ((gptel-org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
;;          (inserted-string (format "%s %s\n"
;;                                   (substring gptel-org-prefix 0 (string-match " " gptel-org-prefix))
;;                                   (cj/gptel-backend-and-model)))
;;          (len-inserted (length inserted-string )))
;;     (goto-char response-begin-pos)
;;     (insert inserted-string)
;;     (goto-char (+ response-end-pos len-inserted))))
;; (add-hook 'gptel-post-response-functions 'cj/gptel-insert-model-in-chat-buffers)

(provide 'ai-config)
;;; ai-config.el ends here
