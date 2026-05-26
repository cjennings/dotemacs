;;; test-mail-config-mu4e-window.el --- mu4e main-view window placement -*- lexical-binding: t; -*-

;;; Commentary:
;; mu4e's main view defaults to a display-buffer-full-frame action that deletes
;; the window split on launch.  mail-config registers a display-buffer-alist
;; entry routing the "*mu4e-main*" buffer to the current window instead.  These
;; verify the entry is registered and that, with it active, displaying the main
;; buffer under mu4e's own full-frame action no longer collapses a split.

;;; Code:

(require 'ert)
(require 'mail-config)

(ert-deftest test-mail-config-mu4e-main-entry-registered ()
  "Normal: the *mu4e-main* alist entry avoids the full-frame action."
  (let ((entry (assoc "\\`\\*mu4e-main\\*\\'" display-buffer-alist)))
    (should entry)
    (should (memq 'display-buffer-same-window (cadr entry)))
    (should-not (memq 'display-buffer-full-frame (cadr entry)))))

(ert-deftest test-mail-config-mu4e-main-keeps-split ()
  "Normal: displaying *mu4e-main* under the full-frame action keeps the split.
The registered alist entry takes precedence over the action passed to
`display-buffer', so the sibling window survives."
  (let ((a (get-buffer-create "*mail-win-test-a*"))
        (main (get-buffer-create "*mu4e-main*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) a)
          (split-window (selected-window) nil t)   ;; side-by-side, batch-friendly
          (let ((n (count-windows)))
            (display-buffer main '(display-buffer-reuse-window
                                   display-buffer-reuse-mode-window
                                   display-buffer-full-frame))
            (should (= (count-windows) n))))
      (ignore-errors (kill-buffer a))
      (ignore-errors (kill-buffer main)))))

(provide 'test-mail-config-mu4e-window)
;;; test-mail-config-mu4e-window.el ends here
