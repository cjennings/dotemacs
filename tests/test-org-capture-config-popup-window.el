;;; test-org-capture-config-popup-window.el --- Quick-capture popup single-window tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pure predicate behind the quick-capture popup single-window
;; fix.  The Hyprland Super+Shift+N popup opens an emacsclient frame named
;; "org-capture"; in that frame the *Org Select* template menu and the
;; CAPTURE-* buffer must fill the frame's sole window instead of splitting it.
;; `cj/org-capture--popup-sole-window-p' is the frame+buffer decision; the
;; display-buffer action that acts on it is exercised by hand (window ops),
;; not here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-capture)            ; makes `org-capture-templates' a real special var
(require 'user-constants)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

(defconst test-org-capture-popup--sample-templates
  '(("t" "Task" entry (function cj/--org-capture-project-location)
     "* TODO %?" :prepend t)
    ("b" "Bug" entry (function cj/--org-capture-project-location)
     "* TODO [#C] %?" :prepend t)
    ("e" "Event" entry (file+headline schedule-file "Scheduled Events")
     "* %?" :prepend t :prepare-finalize cj/org-capture-format-event-headline)
    ("m" "Mu4e Email" entry (file+headline inbox-file "Inbox") "* TODO %?" :prepend t)
    ("L" "Link" entry (file+headline inbox-file "Inbox") "* %?" :immediate-finish t)
    ("d" "Drill Question" entry (file ignore) "* Item :drill:\n%?" :prepend t))
  "A representative org-capture-templates list for popup-subset tests.")

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
  ;; The batch frame is auto-named (\"F1\"), which cannot be restored by name
  ;; (\"F<num> usurped by Emacs\"); reset to nil to return it to auto-naming,
  ;; keeping the test independent of execution order.
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

;;; cj/--org-capture-popup-templates  (pure subset/retarget)

(ert-deftest test-org-capture-config-popup-templates-keeps-tbe ()
  "Normal: only Task, Bug, Event survive, preserving order."
  (should (equal (mapcar #'car (cj/--org-capture-popup-templates
                                test-org-capture-popup--sample-templates "/inbox.org"))
                 '("t" "b" "e"))))

(ert-deftest test-org-capture-config-popup-templates-retargets-task-bug ()
  "Normal: Task and Bug retarget to the inbox \"Inbox\" headline; body + props kept."
  (let* ((result (cj/--org-capture-popup-templates
                  test-org-capture-popup--sample-templates "/inbox.org"))
         (task (assoc "t" result))
         (bug (assoc "b" result)))
    (should (equal (nth 3 task) '(file+headline "/inbox.org" "Inbox")))
    (should (equal (nth 3 bug) '(file+headline "/inbox.org" "Inbox")))
    (should (equal (nth 4 task) "* TODO %?"))
    (should (equal (nth 4 bug) "* TODO [#C] %?"))
    (should (memq :prepend task))))

(ert-deftest test-org-capture-config-popup-templates-event-unchanged ()
  "Boundary: Event passes through untouched, schedule-file target and props intact."
  (let ((event (assoc "e" (cj/--org-capture-popup-templates
                           test-org-capture-popup--sample-templates "/inbox.org"))))
    (should (equal (nth 3 event) '(file+headline schedule-file "Scheduled Events")))
    (should (memq :prepare-finalize event))))

(ert-deftest test-org-capture-config-popup-templates-drops-context-templates ()
  "Boundary: context-dependent templates (mu4e, link, drill) are dropped."
  (let ((result (cj/--org-capture-popup-templates
                 test-org-capture-popup--sample-templates "/inbox.org")))
    (should-not (assoc "m" result))
    (should-not (assoc "L" result))
    (should-not (assoc "d" result))))

(ert-deftest test-org-capture-config-popup-templates-empty ()
  "Error/Boundary: empty or all-dropped input yields nil without raising."
  (should-not (cj/--org-capture-popup-templates nil "/inbox.org"))
  (should-not (cj/--org-capture-popup-templates
               '(("L" "Link" entry (file+headline f "Inbox") "* %?")) "/inbox.org")))

;;; cj/quick-capture  (binds the subset; integration with a stubbed org-capture)

(ert-deftest test-integration-org-capture-quick-capture-binds-subset ()
  "Integration: cj/quick-capture runs org-capture with only Task/Bug/Event,
Task and Bug retargeted to the inbox.

Components integrated:
- cj/quick-capture (real)
- cj/--org-capture-popup-templates (real)
- org-capture (MOCKED — records the dynamically-bound templates)"
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        captured)
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _) (setq captured org-capture-templates))))
      (cj/quick-capture))
    (should (equal (mapcar #'car captured) '("t" "b" "e")))
    (should (equal (nth 3 (assoc "t" captured)) (list 'file+headline inbox-file "Inbox")))
    (should (equal (nth 3 (assoc "b" captured)) (list 'file+headline inbox-file "Inbox")))))

(ert-deftest test-integration-org-capture-quick-capture-closes-frame-on-abort ()
  "Integration: when selection aborts (org-capture signals), cj/quick-capture
deletes the popup frame instead of leaving it orphaned.

Components integrated:
- cj/quick-capture (real)
- org-capture (MOCKED — signals user-error \"Abort\")
- cj/org-capture--delete-popup-frame (MOCKED — records the call)"
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        (deleted 0))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _) (user-error "Abort")))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 1))))

(ert-deftest test-integration-org-capture-quick-capture-closes-frame-on-quit ()
  "Integration: a C-g (quit) during capture also closes the popup frame."
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        (deleted 0))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 1))))

(ert-deftest test-integration-org-capture-quick-capture-keeps-frame-on-success ()
  "Integration: a successful capture (no signal) does NOT delete the frame —
the finalize hook owns that."
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        (deleted 0))
    (cl-letf (((symbol-function 'org-capture) (lambda (&rest _) nil))
              ((symbol-function 'cj/org-capture--delete-popup-frame)
               (lambda () (cl-incf deleted))))
      (cj/quick-capture))
    (should (= deleted 0))))

;;; cj/--org-capture-popup-strip-specials  (drop the Customize menu entry)

(ert-deftest test-org-capture-config-popup-strip-specials-removes-customize ()
  "Normal: the \"C\" Customize entry is removed, \"q\" Abort kept, order intact."
  (should (equal (cj/--org-capture-popup-strip-specials
                  '(("C" "Customize org-capture-templates") ("q" "Abort")))
                 '(("q" "Abort")))))

(ert-deftest test-org-capture-config-popup-strip-specials-no-customize ()
  "Boundary: specials without a \"C\" entry pass through unchanged."
  (should (equal (cj/--org-capture-popup-strip-specials '(("q" "Abort")))
                 '(("q" "Abort")))))

(ert-deftest test-org-capture-config-popup-strip-specials-empty ()
  "Error/Boundary: nil specials yields nil without raising."
  (should-not (cj/--org-capture-popup-strip-specials nil)))

;;; cj/org-capture--popup-frame-p

(ert-deftest test-org-capture-config-popup-frame-p ()
  "Normal/Boundary: true only when the selected frame is named \"org-capture\"."
  (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) "org-capture")))
    (should (cj/org-capture--popup-frame-p)))
  (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) "emacs")))
    (should-not (cj/org-capture--popup-frame-p))))

;;; cj/org-capture--popup-mks-advice  (frame-gated specials stripping)

(ert-deftest test-org-capture-config-popup-mks-advice-strips-in-popup ()
  "Integration: in the popup frame, org-mks receives specials without \"C\"."
  (let (seen)
    (cl-letf (((symbol-function 'cj/org-capture--popup-frame-p) (lambda () t)))
      (cj/org-capture--popup-mks-advice
       (lambda (_table _title _prompt specials) (setq seen specials))
       nil nil nil '(("C" "Customize org-capture-templates") ("q" "Abort"))))
    (should (equal seen '(("q" "Abort"))))))

(ert-deftest test-org-capture-config-popup-mks-advice-keeps-elsewhere ()
  "Integration: in a normal frame, org-mks receives the specials untouched."
  (let (seen)
    (cl-letf (((symbol-function 'cj/org-capture--popup-frame-p) (lambda () nil)))
      (cj/org-capture--popup-mks-advice
       (lambda (_table _title _prompt specials) (setq seen specials))
       nil nil nil '(("C" "Customize org-capture-templates") ("q" "Abort"))))
    (should (equal seen '(("C" "Customize org-capture-templates") ("q" "Abort"))))))

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
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        (focused nil))
    (cl-letf (((symbol-function 'cj/org-capture--popup-frame) (lambda () 'popup-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f) (setq focused f)))
              ((symbol-function 'org-capture) (lambda (&rest _) nil)))
      (cj/quick-capture))
    (should (eq focused 'popup-frame))))

(ert-deftest test-integration-org-capture-quick-capture-no-frame-still-captures ()
  "Integration: when no popup frame is found, cj/quick-capture skips the focus
call and still runs the capture (no error)."
  (let ((org-capture-templates test-org-capture-popup--sample-templates)
        (focused 'unset)
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
