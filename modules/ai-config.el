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

;; ----------------------------------- GPTel -----------------------------------
;; integration with ChatGPT and other large language models.

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


(defun toggle-gptel ()
  "Toggle the visibility of the ChatGPT buffer."
  (interactive)
  (let ((buffer (get-buffer "*ChatGPT*")))
	(if (and buffer (get-buffer-window buffer))
		(delete-window (get-buffer-window buffer))
	  (if buffer
		  (display-buffer-in-side-window buffer '((side . right) (window-width . 0.4)))
		(gptel)))))

(use-package gptel
  :defer t
  :commands (gptel gptel-send)
  :bind
  (("C-h G" . gptel)
   ("<f9>" . toggle-gptel)
   (:map gptel-mode-map
		 ("C-<return>" . gptel-send)))
  :custom
  (gptel-model "gpt-4")
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-directives
		`((default   . ,default-directive)
		  (code-only . ,code-only-directive)
		  (writing   . ,writing-directive)
		  (chat      . ,chat-directive)))

  ;; Grab the secret from the auth file
  (setq auth-sources `((:source ,authinfo-file)))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))


(provide 'ai-config)
;;; ai-config.el ends here
