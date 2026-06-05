;;; test-term-toggle--buffer-filter.el --- Tests for F12's buffer filter -*- lexical-binding: t; -*-

;;; Commentary:
;; Three closely-related helpers determine which terminal buffers F12
;; manages: the predicate `cj/--term-toggle-buffer-p', the MRU list
;; `cj/--term-toggle-buffers', and the per-frame window finder
;; `cj/--term-toggle-displayed-window'.  All three exclude agent-
;; prefixed buffers so agent has its own F9 surface.

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

(ert-deftest test-term-toggle--buffer-p-accepts-ghostel-mode ()
  "Normal: a ghostel-mode buffer with non-agent name qualifies."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-ghostel-buffer "*test-term-1*")))
    (unwind-protect
        (should (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-agent ()
  "Boundary: agent-prefixed terminal buffers are excluded from F12's set."
  (test-term-toggle--cleanup)
  (let ((buf (cj/test--make-fake-ghostel-buffer "agent [project-a]")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-non-terminal ()
  "Boundary: a regular buffer (not ghostel-mode, no terminal name prefix) -> nil."
  (test-term-toggle--cleanup)
  (let ((buf (get-buffer-create "*test-term-regular*")))
    (unwind-protect
        (should-not (cj/--term-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-term-toggle--buffer-p-rejects-dead-buffer ()
  "Boundary: nil and dead buffers -> nil."
  (should-not (cj/--term-toggle-buffer-p nil))
  (let ((buf (cj/test--make-fake-ghostel-buffer "*test-term-dead*")))
    (kill-buffer buf)
    (should-not (cj/--term-toggle-buffer-p buf))))

(ert-deftest test-term-toggle--buffers-filters-agent ()
  "Normal: returns terminal buffers but excludes agent-prefixed ones."
  (test-term-toggle--cleanup)
  (let ((normal (cj/test--make-fake-ghostel-buffer "*test-term-normal*"))
        (agent (cj/test--make-fake-ghostel-buffer "agent [for-test]")))
    (unwind-protect
        (let ((result (cj/--term-toggle-buffers)))
          (should (memq normal result))
          (should-not (memq agent result)))
      (kill-buffer normal)
      (kill-buffer agent))))

(ert-deftest test-term-toggle--displayed-window-finds-terminal ()
  "Normal: terminal in a window -> returns that window."
  (test-term-toggle--cleanup)
  (let ((vt (cj/test--make-fake-ghostel-buffer "*test-term-shown*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win vt)
            (let ((result (cj/--term-toggle-displayed-window)))
              (should (windowp result))
              (should (eq (window-buffer result) vt)))))
      (kill-buffer vt))))

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
