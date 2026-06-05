;;; test-ai-term--close.el --- Tests for graceful agent close -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/ai-term-close' tears an agent down gracefully: kill its tmux
;; session (stopping the agent process), kill the ghostel buffer, and
;; remove its window.  These tests cover the pure pieces -- the
;; tmux-kill helper, the per-buffer teardown, and the target selection --
;; with `process-file' and the prompt mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--kill-tmux-session-runs-kill-session ()
  "Normal: invokes `tmux kill-session -t <session>'."
  (let (captured)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program &rest args)
                 (setq captured (cons program args))
                 0)))
      (cj/--ai-term-kill-tmux-session "aiv-foo"))
    (should (equal (car captured) "tmux"))
    (should (member "kill-session" captured))
    (should (member "-t" captured))
    (should (member "aiv-foo" captured))))

(ert-deftest test-ai-term--kill-tmux-session-swallows-error ()
  "Error: returns nil when tmux is unavailable (process-file signals)."
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) (error "no tmux"))))
    (should (null (cj/--ai-term-kill-tmux-session "aiv-foo")))))

(ert-deftest test-ai-term--close-buffer-kills-session-and-buffer ()
  "Normal: derives the session from default-directory, kills it and the buffer."
  (let ((buf (get-buffer-create "agent [foo]"))
        captured-session)
    (with-current-buffer buf (setq-local default-directory "/tmp/foo/"))
    (cl-letf (((symbol-function 'cj/--ai-term-kill-tmux-session)
               (lambda (s) (setq captured-session s) 0)))
      (cj/--ai-term-close-buffer buf))
    (should (equal captured-session "aiv-foo"))
    (should-not (buffer-live-p buf))))

(ert-deftest test-ai-term--close-buffer-noop-on-non-agent ()
  "Boundary: does nothing for a buffer that is not an agent buffer."
  (let ((buf (get-buffer-create "*not-an-agent*"))
        (called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'cj/--ai-term-kill-tmux-session)
                     (lambda (_s) (setq called t) 0)))
            (cj/--ai-term-close-buffer buf))
          (should-not called)
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ai-term--close-target-current-agent-buffer ()
  "Normal: returns the current buffer when it is an agent buffer."
  (let ((buf (get-buffer-create "agent [cur]")))
    (unwind-protect
        (with-current-buffer buf
          (should (eq (cj/--ai-term-close-target) buf)))
      (kill-buffer buf))))

(ert-deftest test-ai-term--close-target-sole-agent ()
  "Normal: returns the only live agent buffer when current isn't an agent."
  (let ((buf (get-buffer-create "agent [only]")))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'cj/--ai-term-agent-buffers)
                     (lambda () (list buf))))
            (should (eq (cj/--ai-term-close-target) buf))))
      (kill-buffer buf))))

(ert-deftest test-ai-term--close-target-none-returns-nil ()
  "Boundary: nil when current buffer isn't an agent and none are alive."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/--ai-term-agent-buffers) (lambda () nil)))
      (should (null (cj/--ai-term-close-target))))))

(provide 'test-ai-term--close)
;;; test-ai-term--close.el ends here
