;;; test-ai-vterm--live-tmux-sessions.el --- Tests for cj/--ai-vterm-live-tmux-sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Lists the live tmux sessions that carry the AI-vterm prefix so the
;; project picker can surface projects whose Claude session survived an
;; Emacs crash.  tmux being absent or no server running is a normal
;; "nothing to match" outcome, not an error -- the lister returns nil.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(defmacro test-ai-vterm--with-tmux-list (exit-code output &rest body)
  "Run BODY with `process-file' mocked to a tmux list-sessions response.

EXIT-CODE is what `process-file' returns (or the symbol `error' to
make it signal).  OUTPUT is written to the stdout destination buffer."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-file)
              (lambda (_program _infile destination _display &rest _args)
                (when (eq ,exit-code 'error)
                  (error "tmux: command not found"))
                (let ((buffer (cond
                               ((eq destination t) (current-buffer))
                               ((bufferp destination) destination)
                               ((consp destination)
                                (and (eq (car destination) t)
                                     (current-buffer))))))
                  (when (bufferp buffer)
                    (with-current-buffer buffer (insert ,output))))
                ,exit-code)))
     ,@body))

(ert-deftest test-ai-vterm--live-tmux-sessions-filters-to-prefix ()
  "Normal: only sessions starting with the AI-vterm prefix come back."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (test-ai-vterm--with-tmux-list 0 "aiv-foo\nrandom-session\naiv-bar\n"
      (should (equal (cj/--ai-vterm-live-tmux-sessions)
                     '("aiv-foo" "aiv-bar"))))))

(ert-deftest test-ai-vterm--live-tmux-sessions-honors-custom-prefix ()
  "Normal: a non-default prefix is what gets matched."
  (let ((cj/ai-vterm-tmux-session-prefix "em-"))
    (test-ai-vterm--with-tmux-list 0 "em-foo\naiv-bar\nem-baz\n"
      (should (equal (cj/--ai-vterm-live-tmux-sessions)
                     '("em-foo" "em-baz"))))))

(ert-deftest test-ai-vterm--live-tmux-sessions-empty-output-yields-nil ()
  "Boundary: a running server with no matching sessions yields nil."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (test-ai-vterm--with-tmux-list 0 "other-a\nother-b\n"
      (should (null (cj/--ai-vterm-live-tmux-sessions))))))

(ert-deftest test-ai-vterm--live-tmux-sessions-no-server-yields-nil ()
  "Error: tmux exits non-zero (no server running) -> nil, not a signal."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (test-ai-vterm--with-tmux-list 1 "no server running on /tmp/tmux-1000/default\n"
      (should (null (cj/--ai-vterm-live-tmux-sessions))))))

(ert-deftest test-ai-vterm--live-tmux-sessions-tmux-missing-yields-nil ()
  "Error: tmux not installed -> `process-file' signals; lister returns nil."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (test-ai-vterm--with-tmux-list 'error ""
      (should (null (cj/--ai-vterm-live-tmux-sessions))))))

(provide 'test-ai-vterm--live-tmux-sessions)
;;; test-ai-vterm--live-tmux-sessions.el ends here
