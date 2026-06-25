;;; test-term-toggle--buffer-filter.el --- Tests for F12's buffer filter -*- lexical-binding: t; -*-

;;; Commentary:
;; Three closely-related helpers determine which terminal buffer F12
;; manages: the predicate `cj/--term-toggle-buffer-p', the list
;; `cj/--term-toggle-buffers', and the per-frame window finder
;; `cj/--term-toggle-displayed-window'.  F12 manages the EAT terminal;
;; ghostel buffers (including ai-term's agent buffers) are NOT F12-managed --
;; they live on M-SPC.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'term-config)
(require 'testutil-ghostel-buffers)

(defun test-term-toggle--cleanup ()
  "Kill leftover agent- and *test-term- prefixed buffers."
  (cj/test--kill-agent-buffers)
  (cj/test--kill-test-term-buffers))

(ert-deftest test-term-toggle--buffer-p-accepts-eat-mode ()
  "Normal: an eat-mode buffer qualifies as the F12 terminal."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-eat-buffer "*test-term-1*")))
    (unwind-protect
        (should (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-ghostel ()
  "Boundary: a ghostel buffer is NOT F12-managed (ghostel is ai-term's, M-SPC)."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-ghostel-buffer "*test-term-ghostel*")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-agent ()
  "Boundary: ai-term agent buffers are excluded from F12's set."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-ghostel-buffer "agent [project-a]")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-non-terminal ()
  "Boundary: a regular buffer (not eat-mode, no terminal name prefix) -> nil."
  (test-term-toggle--cleanup)
  (let ((buf (get-buffer-create "*test-term-regular*")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-dead-buffer ()
  "Boundary: nil and dead buffers -> nil."
  (should-not (cj/--term-toggle-buffer-p nil))
  (let ((buf (cj/test--make-fake-eat-buffer "*test-term-dead*")))
    (kill-buffer buf)
    (should-not (cj/--term-toggle-buffer-p buf))))

(ert-deftest test-term-toggle--buffers-returns-eat-excludes-others ()
  "Normal: returns the EAT terminal but not ghostel/agent buffers."
  (test-term-toggle--cleanup)
  (let ((eat (cj/test--make-fake-eat-buffer "*test-term-eat*"))
        (agent (cj/test--make-fake-ghostel-buffer "agent [for-test]")))
    (unwind-protect
        (let ((result (cj/--term-toggle-buffers)))
          (should (memq eat result))
          (should-not (memq agent result)))
      (kill-buffer eat)
      (kill-buffer agent))))

(ert-deftest test-term-toggle--displayed-window-finds-terminal ()
  "Normal: the EAT terminal in a window -> returns that window."
  (test-term-toggle--cleanup)
  (let ((eat (cj/test--make-fake-eat-buffer "*test-term-shown*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win eat)
            (let ((result (cj/--term-toggle-displayed-window)))
              (should (windowp result))
              (should (eq (window-buffer result) eat)))))
      (kill-buffer eat))))

(ert-deftest test-term-toggle--displayed-window-skips-agent ()
  "Boundary: only an agent terminal is displayed -> nil (agent not F12-managed)."
  (test-term-toggle--cleanup)
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [skip-test]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win agent)
            (should-not (cj/--term-toggle-displayed-window))))
      (kill-buffer agent))))

(provide 'test-term-toggle--buffer-filter)
;;; test-term-toggle--buffer-filter.el ends here
