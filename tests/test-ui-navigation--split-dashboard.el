;;; test-ui-navigation--split-dashboard.el --- Tests for split-with-dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; C-x 2 / C-x 3 split and show the *dashboard* in the new window while point
;; stays in the original.  cj/--split-show-buffer does the placement;
;; cj/split-below/right-with-dashboard wire it to the two split directions.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ui-navigation)

(ert-deftest test-ui-navigation-split-dashboard-keybindings ()
  "Normal: C-x 2 / C-x 3 are bound to the dashboard-split commands."
  (should (eq (key-binding (kbd "C-x 2")) #'cj/split-below-with-dashboard))
  (should (eq (key-binding (kbd "C-x 3")) #'cj/split-right-with-dashboard)))

(ert-deftest test-ui-navigation-split-show-buffer-displays-and-keeps-point ()
  "Normal: the new window shows the buffer; the original stays selected."
  (let ((buf (get-buffer-create " *split-dash-test*"))
        (config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((orig (selected-window))
                 (new (cj/--split-show-buffer #'split-window-below buf)))
            (should (window-live-p new))
            (should (not (eq new orig)))
            (should (eq (window-buffer new) buf))
            (should (eq (selected-window) orig)))) ; point stays put
      (set-window-configuration config)
      (kill-buffer buf))))

(ert-deftest test-ui-navigation-split-below-routes-to-split-window-below ()
  "Normal: cj/split-below-with-dashboard splits below with the dashboard buffer."
  (let (captured)
    (cl-letf (((symbol-function 'cj/--dashboard-buffer) (lambda () 'dashboard))
              ((symbol-function 'cj/--split-show-buffer)
               (lambda (fn buf) (setq captured (list fn buf)) nil)))
      (cj/split-below-with-dashboard))
    (should (eq (car captured) #'split-window-below))
    (should (eq (cadr captured) 'dashboard))))

(ert-deftest test-ui-navigation-split-right-routes-to-split-window-right ()
  "Normal: cj/split-right-with-dashboard splits right with the dashboard buffer."
  (let (captured)
    (cl-letf (((symbol-function 'cj/--dashboard-buffer) (lambda () 'dashboard))
              ((symbol-function 'cj/--split-show-buffer)
               (lambda (fn buf) (setq captured (list fn buf)) nil)))
      (cj/split-right-with-dashboard))
    (should (eq (car captured) #'split-window-right))
    (should (eq (cadr captured) 'dashboard))))

(ert-deftest test-ui-navigation-dashboard-buffer-returns-existing ()
  "Boundary: cj/--dashboard-buffer returns an existing *dashboard* without opening."
  (let ((db (get-buffer-create "*dashboard*"))
        (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'dashboard-open)
                   (lambda (&rest _) (setq opened t))))
          (should (eq (cj/--dashboard-buffer) db))
          (should-not opened))
      (kill-buffer db))))

(provide 'test-ui-navigation--split-dashboard)
;;; test-ui-navigation--split-dashboard.el ends here
