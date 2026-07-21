;;; test-ai-term--session-threading.el --- Session-list threading tests -*- lexical-binding: t; -*-

;;; Commentary:
;; The launch path used to call cj/--ai-term-live-tmux-sessions (a tmux
;; subprocess) once in the project picker, again for the launcher's fresh
;; check, and a third time inside show-or-create.  One fetch per launch,
;; threaded through, is the contract pinned here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term-launch-fetches-tmux-sessions-once ()
  "Normal: one tmux session fetch per launch, threaded to the picker and
show-or-create rather than re-spawned by each."
  (let ((fetches 0) received-sessions)
    (cl-letf (((symbol-function 'cj/--ai-term-live-tmux-sessions)
               (lambda () (setq fetches (1+ fetches)) '("other")))
              ((symbol-function 'cj/--ai-term-candidates)
               (lambda () '("/tmp/proj/")))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (car (all-completions "" table))))
              ((symbol-function 'get-buffer) (lambda (_n) nil))
              ((symbol-function 'cj/--ai-term-pick-runtime) (lambda () 'claude))
              ((symbol-function 'cj/--ai-term-runtime-command) (lambda (_r) "cmd"))
              ((symbol-function 'cj/--ai-term-show-or-create)
               (lambda (_dir _name _cmd &optional sessions)
                 (setq received-sessions sessions)
                 (current-buffer)))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (cj/ai-term-pick-project t)
      (should (= fetches 1))
      (should (equal received-sessions '("other"))))))

(provide 'test-ai-term--session-threading)
;;; test-ai-term--session-threading.el ends here
