;;; test-term-toggle--buffer-filter.el --- Tests for F12's buffer filter -*- lexical-binding: t; -*-

;;; Commentary:
;; Three closely-related helpers determine which terminal buffer F12
;; manages: the predicate `cj/--term-toggle-buffer-p', the list
;; `cj/--term-toggle-buffers', and the per-frame window finder
;; `cj/--term-toggle-displayed-window'.  F12 opens eshell (run through EAT via
;; eat-eshell-mode), so it manages eshell-mode buffers.  Standalone eat buffers
;; and ai-term's agent buffers (also eat) are NOT F12-managed.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'eat-config)
(require 'testutil-terminal-buffers)

(defun test-term-toggle--cleanup ()
  "Kill leftover agent- and *test-term- prefixed buffers."
  (cj/test--kill-agent-buffers)
  (cj/test--kill-test-term-buffers))

(ert-deftest test-term-toggle--buffer-p-accepts-eshell-mode ()
  "Normal: an eshell-mode buffer qualifies as the F12 terminal."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-eshell-buffer "*test-term-1*")))
    (unwind-protect
        (should (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-eat ()
  "Boundary: a standalone eat buffer is NOT F12-managed (F12 opens eshell)."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-eat-buffer "*test-term-eat*")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-agent ()
  "Boundary: ai-term agent buffers are excluded from F12's set."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-eat-buffer "agent [project-a]")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-non-terminal ()
  "Boundary: a regular buffer (not eshell-mode) -> nil."
  (test-term-toggle--cleanup)
  (let ((buf (get-buffer-create "*test-term-regular*")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-dead-buffer ()
  "Boundary: nil and dead buffers -> nil."
  (should-not (cj/--term-toggle-buffer-p nil))
  (let ((buf (cj/test--make-fake-eshell-buffer "*test-term-dead*")))
    (kill-buffer buf)
    (should-not (cj/--term-toggle-buffer-p buf))))

(ert-deftest test-term-toggle--buffers-returns-eshell-excludes-others ()
  "Normal: returns the eshell terminal but not eat/agent buffers."
  (test-term-toggle--cleanup)
  (let ((esh (cj/test--make-fake-eshell-buffer "*test-term-esh*"))
        (agent (cj/test--make-fake-eat-buffer "agent [for-test]")))
    (unwind-protect
        (let ((result (cj/--term-toggle-buffers)))
          (should (memq esh result))
          (should-not (memq agent result)))
      (kill-buffer esh)
      (kill-buffer agent))))

(ert-deftest test-term-toggle--displayed-window-finds-terminal ()
  "Normal: the eshell terminal in a window -> returns that window."
  (test-term-toggle--cleanup)
  (let ((esh (cj/test--make-fake-eshell-buffer "*test-term-shown*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win esh)
            (let ((result (cj/--term-toggle-displayed-window)))
              (should (windowp result))
              (should (eq (window-buffer result) esh)))))
      (kill-buffer esh))))

(ert-deftest test-term-toggle--displayed-window-skips-agent ()
  "Boundary: only an agent terminal is displayed -> nil (agent not F12-managed)."
  (test-term-toggle--cleanup)
  (let ((agent (cj/test--make-fake-eat-buffer "agent [skip-test]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win agent)
            (should-not (cj/--term-toggle-displayed-window))))
      (kill-buffer agent))))

(provide 'test-term-toggle--buffer-filter)
;;; test-term-toggle--buffer-filter.el ends here
