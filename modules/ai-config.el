;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Here is my basic workflow:

;; - Launch GPTel via F9 or M-a t, and chat with the AI in the side window.
;;   Remember that sending the message requires C-<return>.
;; ... or ...
;; - Select a region to rewrite, key M-a r, and add the directive in the menu.

;; Note;
;; - you can save a file for a later session. Just open the file and change the buffer to gptel-mode to resume work.
;; - add files to the context with M-a f
;; - add buffers to the context with M-a b.

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'ai-directives)

;;; ------------------------------ Toggle GPTel --------------------------------

(defun cj/toggle-gptel ()
  "Toggle the visibility of the ChatGPT buffer, and place point at its end."
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
    (define-key map (kbd "x") #'cj/gptel-clear-buffer)
    (define-key map (kbd "m") #'gptel-menu)
    (define-key map (kbd "r") #'gptel-rewrite)
    (define-key map (kbd "f") #'gptel-add-file)
    (define-key map (kbd "b") #'gptel-add-buffer)
    (define-key map (kbd "p") #'gptel-system-prompt)
    map)
  "Keymap for AI-related commands (prefix \\<ai-keymap>).")
(global-set-key (kbd "M-a") ai-keymap)

(use-package gptel
  :defer t
  :commands (gptel gptel-send)
  :bind
  ("<f9>"  . cj/toggle-gptel)
  (:map gptel-mode-map
        ("C-<return>" . gptel-send))
  :custom
  (gptel-default-directive 'default-directive)
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-track-media t)
  (gptel-include-reasoning 'ignore)
  (gptel-model 'o4-mini)     ;; default only - may change in config below
  (gptel-log-level 'info)
  (gptel--debug nil)
  :config
  ;; Directives -- see ai-directives.el in modules directory.
  (setq gptel-directives
        `((default     . ,default-directive)
          (accountant  . ,accountant-directive)
          (coder       . ,coder-directive)
          (chat        . ,chat-directive)
          (contractor  . ,contractor-directive)
		  (emacs       . ,emacs-directive)
		  (package-pm  . ,package-pm-directive)
          (email       . ,email-directive)
          (historian   . ,historian-directive)
          (proofreader . ,proofreader-directive)
          (llm-prompt  . ,prompt-directive)
          (qa          . ,qa-directive)
          (reviewer    . ,reviewer-directive)))

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
      (insert (format "*** %s\n" (cj/gptel-backend-and-model)))))

  (defun cj/gptel-clear-buffer ()
    "Erase the contents of the *AI-Assistant* buffer leaving initial org heading."
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
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
  (setq anthropic-api-key (auth-source-pick-first-password :host "api.anthropic.com"))

  ;; Setup Anthropic's Claude
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t :key anthropic-api-key))
  (setq gptel-model 'claude-3-opus-4-20250514)
  ) ;; end use-package declaration


(with-eval-after-load 'projectile
  (defun cj/gptel-add-file ()
    "Add a file to the GPTel context.
If inside a Projectile project, prompt from the project's file list;
otherwise use =read-file-name'."
    (interactive)
	(let* ((in-proj (and (fboundp 'projectile-project-p)
                        (projectile-project-p)))
          (file    (if in-proj
                       (projectile-completing-read
                        "GPTel add file: "
                        (projectile-current-project-files))
                     (read-file-name "GPTel add file: "))))
      (gptel-add-file file)))
  (define-key ai-keymap (kbd "f") #'cj/gptel-add-file))

;; -------------------------------- GPTel-Magit --------------------------------

(use-package gptel-magit
  :defer t
  :hook (magit-mode . gptel-magit-install))



(provide 'ai-config)
;;; ai-config.el ends here
