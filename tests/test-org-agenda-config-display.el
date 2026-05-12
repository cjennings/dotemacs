;;; test-org-agenda-config-display.el --- Tests for org agenda display rule -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the display-buffer rule used by the F8 org agenda view.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

(ert-deftest test-org-agenda-config-display-rule-uses-configured-height ()
  "Normal: the agenda display rule uses the configured frame fraction."
  (let ((cj/org-agenda-window-height 0.75))
    (should (equal (cdr (assoc 'window-height
                               (cddr (cj/--org-agenda-display-rule))))
                   0.75))))

(ert-deftest test-org-agenda-config-display-rule-does-not-fit-to-buffer ()
  "Regression: F8 agenda should not shrink to fit compact agenda contents."
  (let ((cj/org-agenda-window-height 0.75))
    (should-not (eq (cdr (assoc 'window-height
                                (cddr (cj/--org-agenda-display-rule))))
                    'fit-window-to-buffer))))

(ert-deftest test-org-agenda-config-display-rule-creates-large-window ()
  "Integration: the agenda rule creates a window near the configured height."
  (let ((cj/org-agenda-window-height 0.75)
        (display-buffer-alist (list (cj/--org-agenda-display-rule)))
        (buffer (get-buffer-create "*Org Agenda*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer buffer
            (erase-buffer)
            (dotimes (_ 3)
              (insert "agenda line\n")))
          (let* ((before-height (window-total-height))
                 (window (display-buffer buffer))
                 (actual-ratio (/ (float (window-total-height window))
                                  before-height)))
            (should (= 2 (length (window-list))))
            (should (> actual-ratio 0.65))
            (should (< actual-ratio 0.85))))
      (kill-buffer buffer))))

(provide 'test-org-agenda-config-display)
;;; test-org-agenda-config-display.el ends here
