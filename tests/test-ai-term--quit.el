;;; test-ai-term--quit.el --- Tests for cj/ai-term-quit -*- lexical-binding: t; -*-

;;; Commentary:
;; Headless teardown of a project's AI-term: kill the aiv-<name> tmux session,
;; then the agent buffer.  Driven by the rulesets Stop hook via emacsclient -e,
;; keyed by project basename.  Must be idempotent (a no-op when already gone).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defmacro test-ai-term-quit--with-tmux (calls-var &rest body)
  "Run BODY with `process-file' mocked to record arg lists into CALLS-VAR (0 exit)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'process-file)
              (lambda (_program _infile _destination _display &rest args)
                (push args ,calls-var) 0)))
     ,@body))

(ert-deftest test-ai-term-quit-kills-session-and-buffer ()
  "Normal: quit kills the project's aiv- session and its agent buffer."
  (let ((buf (get-buffer-create "agent [myproj]"))
        (calls nil))
    (unwind-protect
        (test-ai-term-quit--with-tmux calls
          (cj/ai-term-quit "myproj")
          (should (member '("kill-session" "-t" "aiv-myproj") calls))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ai-term-quit-sanitizes-dotted-basename ()
  "Boundary: a dotted basename maps to the sanitized session tmux really uses."
  (let ((buf (get-buffer-create "agent [.emacs.d]"))
        (calls nil))
    (unwind-protect
        (test-ai-term-quit--with-tmux calls
          (cj/ai-term-quit ".emacs.d")
          (should (member '("kill-session" "-t" "aiv-_emacs_d") calls))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ai-term-quit-idempotent-when-gone ()
  "Error/Boundary: a second quit (session + buffer already gone) does not error."
  (let ((calls nil))
    (test-ai-term-quit--with-tmux calls
      ;; No buffer named "agent [ghost]" exists; session kill is a no-op in tmux.
      (should (stringp (cj/ai-term-quit "ghost")))
      (should (member '("kill-session" "-t" "aiv-ghost") calls)))))

(ert-deftest test-ai-term-quit-leaves-non-agent-buffers ()
  "Error: a same-named-but-non-agent buffer is never killed (prefix guard)."
  (let ((buf (get-buffer-create "notes-myproj"))
        (calls nil))
    (unwind-protect
        (test-ai-term-quit--with-tmux calls
          (cj/ai-term-quit "myproj")
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'test-ai-term--quit)
;;; test-ai-term--quit.el ends here
