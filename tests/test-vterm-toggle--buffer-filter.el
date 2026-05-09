;;; test-vterm-toggle--buffer-filter.el --- Tests for F12's buffer filter -*- lexical-binding: t; -*-

;;; Commentary:
;; Three closely-related helpers determine which vterm buffers F12
;; manages: the predicate `cj/--vterm-toggle-buffer-p', the MRU list
;; `cj/--vterm-toggle-buffers', and the per-frame window finder
;; `cj/--vterm-toggle-displayed-window'.  All three exclude claude-
;; prefixed buffers so claude has its own F9 surface.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eshell-vterm-config)

(defun test-vterm-toggle--cleanup ()
  "Kill any test-prefixed vterm-style buffers left behind."
  (dolist (b (buffer-list))
    (let ((name (buffer-name b)))
      (when (or (string-prefix-p "*test-vterm" name)
                (string-prefix-p "claude [" name))
        (kill-buffer b)))))

(defun test-vterm-toggle--make-vterm-buffer (name)
  "Create BUFFER with vterm-mode for testing.
Avoids actually launching a vterm process by manually setting major-mode."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local major-mode 'vterm-mode))
    buf))

(ert-deftest test-vterm-toggle--buffer-p-accepts-vterm-mode ()
  "Normal: a vterm-mode buffer with non-claude name qualifies."
  (test-vterm-toggle--cleanup)
  (let ((buf (test-vterm-toggle--make-vterm-buffer "*test-vterm-1*")))
    (unwind-protect
        (should (cj/--vterm-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-vterm-toggle--buffer-p-rejects-claude ()
  "Boundary: claude-prefixed vterm buffers are excluded from F12's set."
  (test-vterm-toggle--cleanup)
  (let ((buf (test-vterm-toggle--make-vterm-buffer "claude [project-a]")))
    (unwind-protect
        (should-not (cj/--vterm-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-vterm-toggle--buffer-p-rejects-non-vterm ()
  "Boundary: a regular buffer (not vterm-mode, no vterm name prefix) -> nil."
  (test-vterm-toggle--cleanup)
  (let ((buf (get-buffer-create "*test-vterm-regular*")))
    (unwind-protect
        (should-not (cj/--vterm-toggle-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest test-vterm-toggle--buffer-p-rejects-dead-buffer ()
  "Boundary: nil and dead buffers -> nil."
  (should-not (cj/--vterm-toggle-buffer-p nil))
  (let ((buf (test-vterm-toggle--make-vterm-buffer "*test-vterm-dead*")))
    (kill-buffer buf)
    (should-not (cj/--vterm-toggle-buffer-p buf))))

(ert-deftest test-vterm-toggle--buffers-filters-claude ()
  "Normal: returns vterm buffers but excludes claude-prefixed ones."
  (test-vterm-toggle--cleanup)
  (let ((normal (test-vterm-toggle--make-vterm-buffer "*test-vterm-normal*"))
        (claude (test-vterm-toggle--make-vterm-buffer "claude [for-test]")))
    (unwind-protect
        (let ((result (cj/--vterm-toggle-buffers)))
          (should (memq normal result))
          (should-not (memq claude result)))
      (kill-buffer normal)
      (kill-buffer claude))))

(ert-deftest test-vterm-toggle--displayed-window-finds-vterm ()
  "Normal: vterm in a window -> returns that window."
  (test-vterm-toggle--cleanup)
  (let ((vt (test-vterm-toggle--make-vterm-buffer "*test-vterm-shown*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win vt)
            (let ((result (cj/--vterm-toggle-displayed-window)))
              (should (windowp result))
              (should (eq (window-buffer result) vt)))))
      (kill-buffer vt))))

(ert-deftest test-vterm-toggle--displayed-window-skips-claude ()
  "Boundary: only a claude vterm is displayed -> nil (claude not F12-managed)."
  (test-vterm-toggle--cleanup)
  (let ((claude (test-vterm-toggle--make-vterm-buffer "claude [skip-test]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win claude)
            (should-not (cj/--vterm-toggle-displayed-window))))
      (kill-buffer claude))))

(provide 'test-vterm-toggle--buffer-filter)
;;; test-vterm-toggle--buffer-filter.el ends here
