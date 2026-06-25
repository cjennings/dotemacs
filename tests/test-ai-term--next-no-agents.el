;;; test-ai-term--next-no-agents.el --- cj/ai-term-next no-agents fallback -*- lexical-binding: t; -*-

;;; Commentary:
;; When no agent buffers are open, `cj/ai-term-next' (bound to M-SPC) launches
;; the project picker (`cj/ai-term-pick-project') to start the first agent,
;; instead of signalling a `user-error'.  The swap key thus doubles as a
;; "start an agent" key when there is nothing to swap to.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term-next-no-agents-launches-picker ()
  "Error: no agents open -> launches the picker instead of erroring."
  (let ((picked 0))
    (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs) (lambda (&rest _) nil))
              ((symbol-function 'cj/--ai-term-displayed-agent-window) (lambda (&rest _) nil))
              ((symbol-function 'cj/ai-term-pick-project) (lambda (&rest _) (setq picked (1+ picked)))))
      (cj/ai-term-next)
      (should (= picked 1)))))

(ert-deftest test-ai-term-next-no-agents-does-not-signal ()
  "Error: no agents open -> returns normally, no user-error raised."
  (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs) (lambda (&rest _) nil))
            ((symbol-function 'cj/--ai-term-displayed-agent-window) (lambda (&rest _) nil))
            ((symbol-function 'cj/ai-term-pick-project) (lambda (&rest _) nil)))
    (should (progn (cj/ai-term-next) t))))

(provide 'test-ai-term--next-no-agents)
;;; test-ai-term--next-no-agents.el ends here
