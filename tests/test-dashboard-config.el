;;; test-dashboard-config.el --- Tests for dashboard-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises `cj/dashboard-only', the F1-bound entry point that displays the
;; *dashboard* buffer and resets the view to the top.  The fix this test
;; locks in: window-start is forced to point-min, not left wherever a prior
;; view had scrolled to.  Without that, a dashboard buffer taller than the
;; window opens with content above the visible area.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)

;; Stub package-level deps that dashboard-config pulls transitively.
(declare-function cj/kill-all-other-buffers-and-windows "undead-buffers")
(unless (fboundp 'cj/kill-all-other-buffers-and-windows)
  (defun cj/kill-all-other-buffers-and-windows () nil))
(unless (fboundp 'cj/make-buffer-undead)
  (defun cj/make-buffer-undead (_name) nil))

(require 'dashboard-config)

(ert-deftest test-dashboard-only-resets-window-start-to-point-min ()
  "Normal: `cj/dashboard-only' forces `window-start' to `point-min'.

The dashboard buffer can be taller than the selected window's height
(banner + 3 navigator rows + sections).  A prior call that left the
window scrolled into the buffer's middle must not leak into the next
display -- point should land at the top AND the window's view should
start at the top.  Without `set-window-start', batch redisplay leaves
`window-start' wherever it was set."
  (let ((dash (get-buffer-create "*dashboard*")))
    (unwind-protect
        (progn
          (with-current-buffer dash
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (make-string 5000 ?x))
              (insert "\n")))
          (save-window-excursion
            (switch-to-buffer dash)
            (let ((win (selected-window)))
              ;; Simulate a prior view that scrolled into the middle.
              (set-window-start win 1000)
              (should (= (window-start win) 1000))
              (cl-letf (((symbol-function 'cj/kill-all-other-buffers-and-windows)
                         #'ignore))
                (cj/dashboard-only))
              (should (= (window-start win) (point-min)))
              (should (= (point) (point-min))))))
      (when (buffer-live-p dash)
        (kill-buffer dash)))))

(provide 'test-dashboard-config)
;;; test-dashboard-config.el ends here
