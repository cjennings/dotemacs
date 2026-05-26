;;; test-slack-config-display.el --- Slack buffer placement -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/slack--display-buffer is wired as `slack-buffer-function' so opening a
;; Slack room or thread shows it in another window and never replaces the
;; selected window's buffer.  These exercise the placement directly with plain
;; buffers (no live Slack), using a side-by-side split so the window math is
;; reliable under `emacs --batch'.

;;; Code:

(require 'ert)
(require 'slack-config)

(ert-deftest test-slack-config-display-buffer-uses-other-window-when-split ()
  "Normal: with a split, the buffer lands in a non-selected window."
  (let ((a (get-buffer-create "*slack-disp-a*"))
        (b (get-buffer-create "*slack-disp-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) a)
          (let ((win-a (selected-window)))
            (split-window win-a nil t)   ;; side-by-side, batch-friendly
            (cj/slack--display-buffer b)
            (let ((win-b (get-buffer-window b)))
              (should win-b)
              (should-not (eq win-b win-a)))))
      (ignore-errors (kill-buffer a))
      (ignore-errors (kill-buffer b)))))

(ert-deftest test-slack-config-display-buffer-keeps-selected-window ()
  "Boundary: displaying does not replace the selected window's buffer."
  (let ((a (get-buffer-create "*slack-disp-a*"))
        (b (get-buffer-create "*slack-disp-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) a)
          (let ((win-a (selected-window)))
            (split-window win-a nil t)
            (cj/slack--display-buffer b)
            (should (eq (window-buffer win-a) a))))
      (ignore-errors (kill-buffer a))
      (ignore-errors (kill-buffer b)))))

(provide 'test-slack-config-display)
;;; test-slack-config-display.el ends here
