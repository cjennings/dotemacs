;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; -*-

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

(use-package gptel
  :defer t
  :commands (gptel gptel-send)
  :bind
  ("C-h G" . gptel)
  (:map gptel-mode-map
        ("C-<return>" . gptel-send))
  :custom
  ;;  (gptel-model "gpt-3.5-turbo-16k") ;; next best alternative
  (gptel-model "gpt-4")
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-directives
		'((default
		   . "You are a large language model living in Emacs. You are an expert
  in emacs-lisp, Python, Golang, and Shell scripting. You encourage unit testing
  code and propose unit tests when you provide code. Please be accurate and
  concise in your responses.")
		  (code-only
		   . "You are a large language model and a careful programmer. Provide code
  and only code as output without any additional text, prompt or note.")
		  (writing
		   . "You are a large language model and a writing assistant. Respond
  concisely.")
		  (chat
		   . "You are a large language model and a conversation partner. Respond
  concisely.")))

  ;; grab the secret from the auth file
  (setq auth-sources `((:source ,authinfo-file)))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))

(provide 'ai-config)
;;; ai-config.el ends here
