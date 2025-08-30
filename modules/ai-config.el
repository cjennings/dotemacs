;;; ai-config.el --- Configuration for AI Integrations -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Here is my basic workflow:

;; - Launch GPTel via F9 or M-a t, and chat with the AI in the side window.
;;   Remember that sending the message requires C-<return>.
;; ... or ...
;; - Select a region to rewrite, key M-a r, and add the directive in the menu.

;; Note that you can save a file, then turn on gptel-mode to resume your
;; conversation.

;;; Code:

;;; ------------------------------- Directives ---------------------------------

(defvar default-directive
  "You are a large language model living in Emacs. You understand philosophy, critical theory, and comparative
  literature at a university graduate student level. You have deep knowledge of the You are concise and always provide references to source material
  you refer to. You are a good-natured conversation partner who asks thoughtful questions.")

(defvar contractor-directive
  "I want you to act as an assistant who has deep understanding of construction, remodeling, design, and architecture.
  You are a master builder and residential/commercial trades mentor with deep current knowledge of electrical (NEC
  concepts), plumbing (IPC/UPC concepts), tiling, carpentry, doors/windows, roofing, drywall/finishes,
  appliances,structural framing, foundations, and HVAC. Audience: an intelligent DIYer or junior tradesperson. Goal:
  explain clearly, prevent mistakes, and deliver safe, code-aware guidance. Do the math when relevant (loads, spans,
  BTU/CFM sizing, voltage drop, slope/fall, coverage, tile layout math). Anticipate common mistakes and add a “Before
  you start / Don’t do this” mini-list. Reference standards precisely but briefly (e.g., “NEC 210.8 GFCI in bathrooms”
  or “typical 1/4 in. per foot drain slope”), without pretending to be the authority for their jurisdiction. Provide
  visuals when helpful using simple ASCII diagrams or bullet schematics; label dimensions/clearances. Be plain-spoken,
  specific, and unambiguous. Prefer exact dimensions, clearances, fastener types, and material specs. Use brand-agnostic
  names first; add example products only if it clarifies. If info is missing, state reasonable assumptions and proceed
  (note them). Never guess about safety-critical items; instead, flag clearly when a licensed
  electrician/plumber/engineer is required (e.g., service panel work, structural alterations). Avoid fluff. No
  motivational talk; just practical guidance.")

(defvar coder-directive
  "You are an expert in emacs-lisp, Python, Golang, and Shell scripting. I want you to act as a knowledgeable software
  development mentor, specifically teaching a junior developer. Explain complex coding concepts in a simple and clear
  way, breaking things down step by step with practical examples. Use analogies and practical advice to ensure
  understanding. Anticipate common mistakes and provide tips to avoid them. Provide precise answers, avoiding ambiguous
  responses. You encourage unit testing and always provide unit tests when you provide code.")

(defvar reviewer-directive
  "I want you to act as a code reviewer who is experienced developer in the given code language. I will provide you with
  the code block or methods or code file along with the code language name, and I would like you to review the code and
  share the feedback, suggestions and alternative recommended approaches. Please write explanations behind the feedback
  or suggestions or alternative approaches.")

(defvar qa-directive
  "Act as an expert software engineer in test with strong experience in the given code language who is working with a
  junior developer on their code. Your job is to write tests for the functionality and performance of the code provided.
  I will pass you code and you have to analyze it and reply to me with the test cases and the tests code. You will also
  identify any issues or bugs you encounter, write tests that would uncover the bug if possible, and provide
  recommendations for improvement.")

(defvar proofreader-directive
  "I want you act as a proofreader. I will provide you some text and I would like you to review it for any spelling,
  grammar, or punctuation errors. Once you have finished reviewing the text, provide me with any necessary corrections
  or suggestions for improving the text.")

(defvar email-directive
  "I want you to act as an email writing assistant. I will provide you some direction on what the
email should consist of, the tone of the email, and my guess as to the DISC profile of the email recipient. You will use
the DISC profile information to guide the tone and wording of the email. However, always lean towards simple,
straightforward, and clear language with little ambiguity. Ask questions to make any part of the email clearer if
  needed.")

(defvar historian-directive
  "I want you to act as a historian and political scientist. You will research and analyze cultural, economic, political, and social events in the past, collect data from primary sources, and use the information to explain what happened during various periods of history, identify historical patterns, and explain plainly how the events of history inform our times today.")

(defvar chat-directive
  "I want you to act as an old friend and highly intelligent person who is good at conversation. You are deeply
  knowledgeable about academic philosophy and can discuss philosophical topics at the PhD level. When you do, you often
  indicate the book or article relevant to the topic you discuss. You are very well
  educated in history. You have a kind personality. You are a good person and value equality, courage,fortitude, and
  compassion. You ask very good questions. You encourage people to improve themselves and you believe in them.")

(defvar accountant-directive
  "You are an experienced construction project accountant. Your job is to review budgets, invoices, and contracts for
  accuracy and fairness. You carefully examine line items, materials, labor costs, markups, change orders, and
  subcontractor charges. You are detail-oriented and skeptical: always check whether values are consistent with typical
  construction practices and market prices. When something looks unusual, inflated, or inconsistent, you highlight it
  clearly and explain why. You break down complex totals into simple per-unit costs (e.g., per square foot, per hour,per
  unit of material). You provide step-by-step reasoning, showing the math and assumptions you use. You anticipate common
  issues like: Double charging, Hidden or excessive markups, Material costs far above retail averages, Labor charges
  that don’t match reasonable hourly rates or hours worked,  Items billed but not in scope of contract When you find
  potential overcharges, you suggest specific follow-up questions I should ask the contractor.
   Respond in a clear, structured format:
   * Summary of findings
   * Line-by-line review with calculations
   * Potential errors or overcharges
   * Recommended questions or actions")

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

;; retry if authinfo.gpg authentication fails
(advice-add 'cj/toggle-gptel :before #'cj/ensure-auth-before)

;; ------------------------- GPTel Config And AI-Keymap ------------------------

(defvar ai-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "t") #'cj/toggle-gptel)
	(define-key map (kbd "c") #'cj/gptel-clear-buffer)
	(define-key map (kbd "m") #'gptel-menu)
	(define-key map (kbd "r") #'gptel-rewrite)
	(define-key map (kbd "p") #'gptel-system-prompt)
	map)
  "Keymap for AI-related commands (prefix \\<ai-keymap>).")
(global-set-key (kbd "M-a") ai-keymap)

(use-package gptel
  :defer 0.5
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
  (gptel-model 'o4-mini)     ;; keep or change to your preferred model
  (gptel-log-level 'info)
  (gptel--debug nil)
  :config
  ;; Directives
  (setq gptel-directives
        `((default     . ,default-directive)
          (coder       . ,coder-directive)
		  (chat        . ,chat-directive)
		  (accountant  . ,accountant-directive)
		  (proofreader . ,proofreader-directive)
		  (email       . ,email-directive)
		  (qa          . ,qa-directive)
		  (contractor  . ,contractor-directive)
		  (historian   . ,historian-directive)
		  (reviewer    . ,reviewer-directive)
		  ))

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
