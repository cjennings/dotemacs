;;; test-org-agenda-config-display.el --- Tests for org agenda display rule -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the display-buffer rule used by the F8 org agenda view.
;; The agenda takes the whole frame; these pin that, and pin the two ways
;; it previously failed to (a fraction of the frame, or shrunk to fit).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

(defun test-org-agenda-config-display--actions ()
  "Return the display-action function list from the agenda rule."
  (car (cdr (cj/--org-agenda-display-rule))))

(defun test-org-agenda-config-display--alist ()
  "Return the action alist from the agenda rule."
  (cddr (cj/--org-agenda-display-rule)))

(ert-deftest test-org-agenda-config-display-rule-takes-full-frame ()
  "Normal: the agenda display rule claims the whole frame."
  (should (memq 'display-buffer-full-frame
                (test-org-agenda-config-display--actions))))

(ert-deftest test-org-agenda-config-display-rule-reuses-agenda-window ()
  "Normal: an agenda already on screen is reused rather than re-displayed."
  (should (memq 'display-buffer-reuse-mode-window
                (test-org-agenda-config-display--actions))))

(ert-deftest test-org-agenda-config-display-rule-sets-no-window-height ()
  "Regression: no height fraction survives.
The rule used to hand the agenda 0.75 of the frame; a leftover
`window-height' entry would cap the full-frame window right back down."
  (should-not (assoc 'window-height (test-org-agenda-config-display--alist))))

(ert-deftest test-org-agenda-config-display-rule-does-not-fit-to-buffer ()
  "Regression: F8 agenda should not shrink to fit compact agenda contents."
  (should-not (eq (cdr (assoc 'window-height
                              (test-org-agenda-config-display--alist)))
                  'fit-window-to-buffer)))

(ert-deftest test-org-agenda-config-display-rule-window-not-dedicated ()
  "Regression: the agenda window must not be dedicated.
With the agenda owning the only window, a dedicated one leaves RET on an
item (`org-agenda-switch-to') nowhere to put the file, so it splits or
opens a frame instead of replacing the agenda."
  (should-not (cdr (assoc 'dedicated (test-org-agenda-config-display--alist)))))

(ert-deftest test-org-agenda-config-display-rule-creates-sole-window ()
  "Integration: displaying the agenda leaves it as the frame's only window."
  (let ((display-buffer-alist (list (cj/--org-agenda-display-rule)))
        (buffer (get-buffer-create "*Org Agenda*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (split-window-below)
          (with-current-buffer buffer
            (erase-buffer)
            (dotimes (_ 3) (insert "agenda line\n")))
          (let ((window (display-buffer buffer)))
            (should (= 1 (length (window-list))))
            (should (eq window (car (window-list))))
            (should (eq (window-buffer window) buffer))))
      (kill-buffer buffer))))

(provide 'test-org-agenda-config-display)
;;; test-org-agenda-config-display.el ends here
