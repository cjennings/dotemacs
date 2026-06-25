;;; test-ai-term--active-agent-dirs.el --- Tests for cj/--ai-term-active-agent-dirs -*- lexical-binding: t; -*-

;;; Commentary:
;; The queue `cj/ai-term-next' steps through: project dirs with an active
;; agent, which is either a live agent buffer (attached) or a live tmux session
;; with no Emacs buffer (detached).  Folding detached sessions in is what lets
;; the step key reach and attach a session that isn't currently on screen.
;; Candidates / buffers / sessions are mocked so the enumeration logic is
;; exercised without a real tmux server.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--active-agent-dirs-includes-attached-and-detached ()
  "Normal: dirs with a live buffer OR a live session are active and sorted by
name; dirs with neither are excluded."
  (let ((buf (get-buffer-create (cj/--ai-term-buffer-name "/p/alpha"))))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-candidates)
                   (lambda (&rest _) '("/p/alpha" "/p/beta" "/p/gamma" "/p/delta")))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda (&rest _) (list buf)))
                  ((symbol-function 'cj/--ai-term-live-tmux-sessions)
                   (lambda (&rest _) (list (cj/--ai-term-tmux-session-name "/p/gamma")))))
          ;; alpha attached (buffer), gamma detached (session); beta/delta neither.
          (should (equal '("/p/alpha" "/p/gamma") (cj/--ai-term-active-agent-dirs))))
      (kill-buffer buf))))

(ert-deftest test-ai-term--active-agent-dirs-detached-only ()
  "Normal: a dir with only a live session (no buffer) is included -- the detached case."
  (cl-letf (((symbol-function 'cj/--ai-term-candidates) (lambda (&rest _) '("/p/solo")))
            ((symbol-function 'cj/--ai-term-agent-buffers) (lambda (&rest _) nil))
            ((symbol-function 'cj/--ai-term-live-tmux-sessions)
             (lambda (&rest _) (list (cj/--ai-term-tmux-session-name "/p/solo")))))
    (should (equal '("/p/solo") (cj/--ai-term-active-agent-dirs)))))

(ert-deftest test-ai-term--active-agent-dirs-empty-when-none-active ()
  "Boundary: no live buffers and no sessions -> an empty queue."
  (cl-letf (((symbol-function 'cj/--ai-term-candidates) (lambda (&rest _) '("/p/a" "/p/b")))
            ((symbol-function 'cj/--ai-term-agent-buffers) (lambda (&rest _) nil))
            ((symbol-function 'cj/--ai-term-live-tmux-sessions) (lambda (&rest _) nil)))
    (should (null (cj/--ai-term-active-agent-dirs)))))

(provide 'test-ai-term--active-agent-dirs)
;;; test-ai-term--active-agent-dirs.el ends here
