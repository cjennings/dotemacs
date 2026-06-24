;;; test-ai-term--live-count.el --- Tests for cj/ai-term-live-count -*- lexical-binding: t; -*-

;;; Commentary:
;; The shutdown safety gate: the integer count of live AI-term (aiv-*) tmux
;; sessions, read by the rulesets wrap-it-up workflow via emacsclient -e.  No
;; server / no sessions is 0, not an error.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defmacro test-ai-term-live-count--with-tmux (exit-code output &rest body)
  "Run BODY with `process-file' mocked to a tmux list-sessions response.
EXIT-CODE is returned (or the symbol `error' to signal); OUTPUT is written to
the stdout destination buffer."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-file)
              (lambda (_program _infile destination _display &rest _args)
                (when (eq ,exit-code 'error) (error "tmux: command not found"))
                (let ((buffer (cond ((eq destination t) (current-buffer))
                                    ((bufferp destination) destination)
                                    ((consp destination)
                                     (and (eq (car destination) t) (current-buffer))))))
                  (when (bufferp buffer)
                    (with-current-buffer buffer (insert ,output))))
                ,exit-code)))
     (let ((cj/ai-term-tmux-session-prefix "aiv-"))
       ,@body)))

(ert-deftest test-ai-term-live-count-counts-matching-sessions ()
  "Normal: two aiv-* sessions among others count as 2."
  (test-ai-term-live-count--with-tmux 0 "aiv-foo\nrandom\naiv-bar\n"
    (should (= (cj/ai-term-live-count) 2))))

(ert-deftest test-ai-term-live-count-single-session ()
  "Boundary: a sole aiv-* session counts as 1."
  (test-ai-term-live-count--with-tmux 0 "aiv-only\nother\n"
    (should (= (cj/ai-term-live-count) 1))))

(ert-deftest test-ai-term-live-count-no-matching-sessions ()
  "Boundary: a running server with no aiv-* sessions is 0."
  (test-ai-term-live-count--with-tmux 0 "other-a\nother-b\n"
    (should (= (cj/ai-term-live-count) 0))))

(ert-deftest test-ai-term-live-count-no-server ()
  "Error: tmux exits non-zero (no server) -> 0, not a signal."
  (test-ai-term-live-count--with-tmux 1 "no server running\n"
    (should (= (cj/ai-term-live-count) 0))))

(ert-deftest test-ai-term-live-count-tmux-missing ()
  "Error: tmux not installed -> 0."
  (test-ai-term-live-count--with-tmux 'error ""
    (should (= (cj/ai-term-live-count) 0))))

(provide 'test-ai-term--live-count)
;;; test-ai-term--live-count.el ends here
