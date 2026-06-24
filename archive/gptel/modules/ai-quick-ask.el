;;; ai-quick-ask.el --- One-shot GPTel quick-ask -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Provides `cj/gptel-quick-ask': read a single prompt in the
;; minibuffer, stream the response into a transient *GPTel-Quick*
;; buffer.  The transient buffer is dismissible with q or escape and
;; can be escalated with c into a full *AI-Assistant* conversation
;; seeded with the prompt + response.
;;
;; Designed for impromptu help where the conversation thread doesn't
;; matter.  Doesn't touch the *AI-Assistant* side window unless the
;; user explicitly escalates, doesn't autosave anywhere.

;;; Code:

(require 'cj-window-toggle-lib)  ;; cj/side-window-display

;; Shared *AI-Assistant* panel-width state, owned by ai-config.el.  Forward-
;; declared here so the escalation reopens the panel at the same remembered
;; width as the F-key toggle without a circular require.
(defvar cj/ai-assistant-window-width)
(defvar cj/--ai-assistant-width)

(defvar-local cj/gptel-quick--prompt nil
  "Buffer-local: the prompt used for the current *GPTel-Quick* session.")

(defconst cj/gptel-quick--buffer-name "*GPTel-Quick*"
  "Buffer used for one-shot quick-ask Q&A.")

(defconst cj/gptel-quick--response-marker "A: "
  "String inserted before the response in the quick-ask buffer.")

(defvar-keymap cj/gptel-quick-mode-map
  :doc "Keymap for `cj/gptel-quick-mode'."
  "q"        #'cj/gptel-quick-dismiss
  "<escape>" #'cj/gptel-quick-dismiss
  "c"        #'cj/gptel-quick-continue)

(define-derived-mode cj/gptel-quick-mode special-mode "GPTel-Quick"
  "Major mode for the one-shot *GPTel-Quick* buffer."
  ;; Allow gptel-request to stream into the buffer despite the
  ;; special-mode read-only default.
  (setq-local buffer-read-only nil))

(defun cj/gptel-quick--initial-text (prompt)
  "Return the initial buffer body for a quick-ask of PROMPT.
The result is \"Q: <prompt>\\n\\nA: \", with the response marker at
the end so the streamed response lands right after it."
  (format "Q: %s\n\n%s" prompt cj/gptel-quick--response-marker))

(defun cj/gptel-quick--extract-response (text)
  "Return the response portion of TEXT, or nil if not found.
TEXT is the contents of a *GPTel-Quick* buffer.  The response is
everything after the first occurrence of `cj/gptel-quick--response-marker'
on its own line.  Returns nil when the marker is absent."
  (when (string-match
         (concat "^" (regexp-quote cj/gptel-quick--response-marker))
         text)
    (substring text (match-end 0))))

(defun cj/gptel-quick--seed-text (prompt response)
  "Format a *AI-Assistant* seed from PROMPT and RESPONSE.
Matches the org-heading shape that `cj/gptel--fresh-org-prefix' and
`cj/gptel-insert-model-heading' produce: a user heading followed by
the prompt body, followed by an AI heading followed by the response."
  (let ((ts (format-time-string "[%Y-%m-%d %H:%M:%S]")))
    (format "* %s %s\n%s\n\n* AI %s\n%s\n"
            user-login-name ts prompt
            ts (or response ""))))

;;;###autoload
(defun cj/gptel-quick-ask (prompt)
  "Read a one-shot PROMPT in the minibuffer and stream the answer.
The response lands in a transient *GPTel-Quick* buffer.  Press q or
escape to dismiss, or c to escalate into a full *AI-Assistant*
conversation seeded with the prompt and response."
  (interactive (list (read-string "Quick ask: ")))
  (when (string-empty-p prompt)
    (user-error "Empty prompt"))
  (let ((buf (get-buffer-create cj/gptel-quick--buffer-name)))
    (with-current-buffer buf
      (cj/gptel-quick-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (cj/gptel-quick--initial-text prompt))
        (setq-local cj/gptel-quick--prompt prompt)))
    (unless (featurep 'gptel)
      (require 'gptel))
    (when (fboundp 'cj/ensure-gptel-backends)
      (cj/ensure-gptel-backends))
    (gptel-request prompt
      :buffer buf
      :position (with-current-buffer buf (point-max))
      :stream t)
    (display-buffer buf
                    '((display-buffer-reuse-window
                       display-buffer-pop-up-window)
                      (window-height . 0.3)))
    buf))

(defun cj/gptel-quick-dismiss ()
  "Kill the *GPTel-Quick* buffer if it exists."
  (interactive)
  (when-let ((buf (get-buffer cj/gptel-quick--buffer-name)))
    (when-let ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf)))

(defun cj/gptel-quick-continue ()
  "Escalate the current quick-ask into a full *AI-Assistant* conversation.
Reads the prompt and response from the *GPTel-Quick* buffer, seeds
them into *AI-Assistant* under proper org headings, displays the
side window, then dismisses the quick buffer."
  (interactive)
  (unless (eq major-mode 'cj/gptel-quick-mode)
    (user-error "Not in a *GPTel-Quick* buffer"))
  (let* ((prompt cj/gptel-quick--prompt)
         (response (cj/gptel-quick--extract-response (buffer-string)))
         (seed (cj/gptel-quick--seed-text prompt response)))
    (unless prompt
      (user-error "No prompt recorded in this buffer"))
    ;; Ensure *AI-Assistant* exists in gptel-mode.
    (unless (featurep 'gptel)
      (require 'gptel))
    (let ((ai-buf (get-buffer "*AI-Assistant*")))
      (unless ai-buf
        (when (fboundp 'cj/ensure-gptel-backends)
          (cj/ensure-gptel-backends))
        (gptel "*AI-Assistant*")
        (setq ai-buf (get-buffer "*AI-Assistant*")))
      (with-current-buffer ai-buf
        (goto-char (point-max))
        (insert seed))
      (cj/side-window-display
       ai-buf 'right 'cj/--ai-assistant-width cj/ai-assistant-window-width)
      (cj/gptel-quick-dismiss))))

(provide 'ai-quick-ask)
;;; ai-quick-ask.el ends here
