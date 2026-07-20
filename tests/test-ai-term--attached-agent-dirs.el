;;; test-ai-term--attached-agent-dirs.el --- Tests for cj/--ai-term-attached-agent-dirs -*- lexical-binding: t; -*-

;;; Commentary:
;; The queue `cj/ai-term-next-attached' (M-SPC) steps through: project dirs
;; with a live agent BUFFER only -- attached sessions.  Unlike
;; `cj/--ai-term-active-agent-dirs', detached tmux sessions with no Emacs
;; buffer are excluded; those are reachable only via `cj/ai-term-next'
;; (M-S-SPC).  Candidates / buffers / sessions are mocked so the enumeration
;; logic is exercised without a real tmux server.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--attached-agent-dirs-excludes-detached ()
  "Normal: only dirs with a live buffer are attached; a detached session is
excluded even though it is active."
  (let ((buf (get-buffer-create (cj/--ai-term-buffer-name "/p/alpha"))))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-candidates)
                   (lambda (&rest _) '("/p/alpha" "/p/beta" "/p/gamma" "/p/delta")))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda (&rest _) (list buf)))
                  ((symbol-function 'cj/--ai-term-live-tmux-sessions)
                   (lambda (&rest _) (list (cj/--ai-term-tmux-session-name "/p/gamma")))))
          ;; alpha attached (buffer), gamma detached (session): only alpha.
          (should (equal '("/p/alpha") (cj/--ai-term-attached-agent-dirs))))
      (kill-buffer buf))))

(ert-deftest test-ai-term--attached-agent-dirs-multiple-sorted ()
  "Normal: multiple attached dirs come back sorted by agent buffer name."
  (let ((a (get-buffer-create (cj/--ai-term-buffer-name "/p/delta")))
        (b (get-buffer-create (cj/--ai-term-buffer-name "/p/alpha"))))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-candidates)
                   (lambda (&rest _) '("/p/alpha" "/p/beta" "/p/delta")))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda (&rest _) (list a b)))
                  ((symbol-function 'cj/--ai-term-live-tmux-sessions)
                   (lambda (&rest _) nil)))
          (should (equal '("/p/alpha" "/p/delta") (cj/--ai-term-attached-agent-dirs))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest test-ai-term--attached-agent-dirs-empty-when-only-detached ()
  "Boundary: sessions exist but no live buffers -> no attached dirs."
  (cl-letf (((symbol-function 'cj/--ai-term-candidates) (lambda (&rest _) '("/p/solo")))
            ((symbol-function 'cj/--ai-term-agent-buffers) (lambda (&rest _) nil))
            ((symbol-function 'cj/--ai-term-live-tmux-sessions)
             (lambda (&rest _) (list (cj/--ai-term-tmux-session-name "/p/solo")))))
    (should (null (cj/--ai-term-attached-agent-dirs)))))

(provide 'test-ai-term--attached-agent-dirs)
;;; test-ai-term--attached-agent-dirs.el ends here
