;;; test-org-capture-config-popup-window.el --- Quick-capture popup tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Hyprland Super+Shift+N quick-capture popup.  The popup opens an
;; emacsclient frame named "org-capture" and runs `cj/quick-capture', which
;; captures a single Task into the global inbox with no template menu.  Covered
;; here: the sole-window predicate and display action (the CAPTURE-* buffer
;; fills the frame), the single-Task template builder, frame discovery and focus
;; (the emacsclient focus race), and frame cleanup on every exit path.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-capture)            ; makes `org-capture-templates' a real special var
(require 'user-constants)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

;;; cj/org-capture--popup-sole-window-p

(ert-deftest test-org-capture-config-popup-sole-window-p-select-menu ()
  "Normal: the *Org Select* menu in the popup frame wants the sole window."
  (should (cj/org-capture--popup-sole-window-p "org-capture" "*Org Select*")))

(ert-deftest test-org-capture-config-popup-sole-window-p-capture-buffer ()
  "Normal: a CAPTURE-* buffer in the popup frame wants the sole window."
  (should (cj/org-capture--popup-sole-window-p "org-capture" "CAPTURE-todo.org")))

(ert-deftest test-org-capture-config-popup-sole-window-p-capture-prefix-only ()
  "Boundary: the bare \"CAPTURE-\" prefix still matches."
  (should (cj/org-capture--popup-sole-window-p "org-capture" "CAPTURE-")))

(ert-deftest test-org-capture-config-popup-sole-window-p-other-frame ()
  "Boundary: the same menu in a normal frame is left alone."
  (should-not (cj/org-capture--popup-sole-window-p "emacs" "*Org Select*"))
  (should-not (cj/org-capture--popup-sole-window-p nil "CAPTURE-todo.org")))

(ert-deftest test-org-capture-config-popup-sole-window-p-other-buffer ()
  "Boundary: an unrelated buffer in the popup frame is left alone."
  (should-not (cj/org-capture--popup-sole-window-p "org-capture" "todo.org"))
  (should-not (cj/org-capture--popup-sole-window-p "org-capture" "*scratch*")))

(ert-deftest test-org-capture-config-popup-sole-window-p-nil-buffer ()
  "Error: a nil or non-string buffer name returns nil without raising."
  (should-not (cj/org-capture--popup-sole-window-p "org-capture" nil))
  (should-not (cj/org-capture--popup-sole-window-p "org-capture" 42)))

;;; Integration: the display-buffer-alist entry routes to a sole window

(ert-deftest test-integration-org-capture-popup-display-sole-window ()
  "Integration: in an \"org-capture\"-named frame, displaying a CAPTURE-*
buffer fills the frame's sole window via the registered display-buffer-alist
entry, instead of splitting.

Components integrated:
- cj/org-capture--popup-display-condition (real)
- cj/org-capture--display-sole-window (real)
- display-buffer / display-buffer-alist (real)

Validates the popup frame ends with one window showing the CAPTURE buffer."
  (let ((buf (get-buffer-create "CAPTURE-itest")))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'name "org-capture")
          (delete-other-windows)
          (display-buffer buf)
          (should (= (length (window-list)) 1))
          (should (eq (window-buffer (selected-window)) buf)))
      (set-frame-parameter nil 'name nil)
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; cj/--quick-capture-template  (single Task into the inbox)

(ert-deftest test-org-capture-config-quick-capture-template ()
  "Normal: the quick-capture template is a single Task into INBOX's Inbox."
  (let* ((tmpl (cj/--quick-capture-template "/inbox.org"))
         (task (assoc "t" tmpl)))
    (should (equal (mapcar #'car tmpl) '("t")))
    (should (equal (nth 1 task) "Task"))
    (should (eq (nth 2 task) 'entry))
    (should (equal (nth 3 task) '(file+headline "/inbox.org" "Inbox")))
    (should (equal (nth 4 task) "* TODO %?"))
    (should (memq :prepend task))))

;;; cj/quick-capture  (single Task; stubbed org-capture)

(ert-deftest test-integration-org-capture-quick-capture-binds-task-only ()
  "Integration: cj/quick-capture runs org-capture with a single Task template
targeting the inbox, dispatched by key.

Components integrated:
- cj/quick-capture (real)
- cj/--quick-capture-template (real)
- org-capture (MOCKED — records the bound templates and dispatch key)"
  (let (captured key)
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&optional _goto k) (setq captured org-capture-templates key k))))
      (cj/quick-capture))
    (should (equal (mapcar #'car captured) '("t")))
    (should (equal (nth 3 (assoc "t" captured)) (list 'file+headline inbox-file "Inbox")))
    (should (equal (nth 4 (assoc "t" captured)) "* TODO %?"))
    (should (equal key "t"))))

(ert-deftest test-integration-org-capture-quick-capture-closes-frame-on-abort ()
  "Integration: when capture aborts (org-capture signals), cj/quick-capture
deletes the popup frame instead of leaving it orphaned.

Components integrated:
- cj/quick-capture (real)
- org-capture (MOCKED — signals user-error \"Abort\")
- cj/org-capture--delete-popup-frame (MOCKED — records the call)"
  (let ((deleted 0))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _) (user-error "Abort")))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 1))))

(ert-deftest test-integration-org-capture-quick-capture-closes-frame-on-quit ()
  "Integration: a C-g (quit) during capture also closes the popup frame."
  (let ((deleted 0))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 1))))

(ert-deftest test-integration-org-capture-quick-capture-keeps-frame-on-success ()
  "Integration: a successful capture (no signal) does NOT delete the frame —
the finalize hook owns that."
  (let ((deleted 0))
    (cl-letf (((symbol-function 'org-capture) (lambda (&rest _) nil))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 0))))

;;; cj/org-capture--popup-frame-p

(ert-deftest test-org-capture-config-popup-frame-p ()
  "Normal/Boundary: true only when the selected frame is named \"org-capture\"."
  (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) "org-capture")))
    (should (cj/org-capture--popup-frame-p)))
  (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) "emacs")))
    (should-not (cj/org-capture--popup-frame-p))))

;;; cj/org-capture--popup-frame  (find the popup frame by name)

(ert-deftest test-org-capture-config-popup-frame-found ()
  "Normal: returns the live frame whose name is \"org-capture\"."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fb fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter)
             (lambda (f _p) (if (eq f 'fb) "org-capture" "other"))))
    (should (eq (cj/org-capture--popup-frame) 'fb))))

(ert-deftest test-org-capture-config-popup-frame-none ()
  "Boundary: no popup frame present yields nil."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter) (lambda (_f _p) "other")))
    (should-not (cj/org-capture--popup-frame))))

;;; cj/quick-capture targets the popup frame

(ert-deftest test-integration-org-capture-quick-capture-selects-named-frame ()
  "Integration: cj/quick-capture selects the \"org-capture\" frame found by name,
not whatever frame happens to be selected (the emacsclient -c focus race)."
  (let ((focused nil))
    (cl-letf (((symbol-function 'cj/org-capture--popup-frame) (lambda () 'popup-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f) (setq focused f)))
              ((symbol-function 'org-capture) (lambda (&rest _) nil)))
      (cj/quick-capture))
    (should (eq focused 'popup-frame))))

(ert-deftest test-integration-org-capture-quick-capture-no-frame-still-captures ()
  "Integration: when no popup frame is found, cj/quick-capture skips the focus
call and still runs the capture (no error)."
  (let ((focused 'unset)
        (captured nil))
    (cl-letf (((symbol-function 'cj/org-capture--popup-frame) (lambda () nil))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f) (setq focused f)))
              ((symbol-function 'org-capture) (lambda (&rest _) (setq captured t))))
      (cj/quick-capture))
    (should (eq focused 'unset))
    (should captured)))

(provide 'test-org-capture-config-popup-window)
;;; test-org-capture-config-popup-window.el ends here
