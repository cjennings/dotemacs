;;; test-org-agenda-frame.el --- Tests for the fullscreen agenda frame -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 1 of the org-agenda fullscreen frame (spec:
;; docs/specs/2026-07-17-org-agenda-fullscreen-frame-spec.org).  Frame lookup
;; is mocked (frame-list / frame-live-p / frame-parameter), the house pattern
;; from test-dirvish-config-popup.el, since --batch can't create real frames.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-frame)

;; org-agenda isn't loaded in batch (no package-initialize), so declare the
;; command list special and bound for the registration tests to let-bind.
(defvar org-agenda-custom-commands nil)

;;; cj/--agenda-frame — locate the marked frame

(ert-deftest test-org-agenda-frame-find-returns-marked-live-frame ()
  "Normal: returns the live frame carrying the `cj/agenda-frame' marker."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fb fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter)
             (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'fb)))))
    (should (eq (cj/--agenda-frame) 'fb))))

(ert-deftest test-org-agenda-frame-find-nil-when-none-marked ()
  "Boundary: no frame carries the marker -> nil."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter) (lambda (_f _p) nil)))
    (should (null (cj/--agenda-frame)))))

(ert-deftest test-org-agenda-frame-find-ignores-dead-marked-frame ()
  "Error: a marked but dead frame is not returned."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fb)))
            ((symbol-function 'frame-live-p) (lambda (f) (not (eq f 'fb))))
            ((symbol-function 'frame-parameter)
             (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'fb)))))
    (should (null (cj/--agenda-frame)))))

;;; cj/--agenda-frame-p — is FRAME a live agenda frame

(ert-deftest test-org-agenda-frame-p-true-for-live-marked ()
  "Normal: a live marked frame is an agenda frame."
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter)
             (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'fa)))))
    (should (cj/--agenda-frame-p 'fa))))

(ert-deftest test-org-agenda-frame-p-nil-for-unmarked ()
  "Boundary: a live unmarked frame is not an agenda frame."
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter) (lambda (_f _p) nil)))
    (should (null (cj/--agenda-frame-p 'fa)))))

(ert-deftest test-org-agenda-frame-p-nil-for-dead-marked ()
  "Error: a dead marked frame is not an agenda frame."
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) nil))
            ((symbol-function 'frame-parameter)
             (lambda (_f p) (eq p 'cj/agenda-frame))))
    (should (null (cj/--agenda-frame-p 'fa)))))

;;; cj/--agenda-frame-working-frame — route source files to a non-agenda frame

(ert-deftest test-org-agenda-working-frame-returns-non-agenda-frame ()
  "Normal: with no recorded launch frame, returns the first live non-agenda frame."
  (let ((cj/--agenda-frame-launch-frame nil))
    (cl-letf (((symbol-function 'frame-list) (lambda () '(agenda work)))
              ((symbol-function 'frame-live-p) (lambda (f) (memq f '(agenda work))))
              ((symbol-function 'frame-parameter)
               (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'agenda)))))
      (should (eq (cj/--agenda-frame-working-frame) 'work)))))

(ert-deftest test-org-agenda-working-frame-prefers-live-launch-frame ()
  "Normal: the recorded launch frame wins when live and non-agenda."
  (let ((cj/--agenda-frame-launch-frame 'w2))
    (cl-letf (((symbol-function 'frame-list) (lambda () '(agenda w1 w2)))
              ((symbol-function 'frame-live-p) (lambda (f) (memq f '(agenda w1 w2))))
              ((symbol-function 'frame-parameter)
               (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'agenda)))))
      (should (eq (cj/--agenda-frame-working-frame) 'w2)))))

(ert-deftest test-org-agenda-working-frame-falls-back-when-launch-dead ()
  "Boundary: a dead recorded launch frame falls back to another non-agenda frame."
  (let ((cj/--agenda-frame-launch-frame 'gone))
    (cl-letf (((symbol-function 'frame-list) (lambda () '(agenda work)))
              ((symbol-function 'frame-live-p) (lambda (f) (memq f '(agenda work))))
              ((symbol-function 'frame-parameter)
               (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'agenda)))))
      (should (eq (cj/--agenda-frame-working-frame) 'work)))))

(ert-deftest test-org-agenda-working-frame-nil-when-only-agenda ()
  "Error: the agenda frame is the only live frame -> nil (caller creates one)."
  (let ((cj/--agenda-frame-launch-frame nil))
    (cl-letf (((symbol-function 'frame-list) (lambda () '(agenda)))
              ((symbol-function 'frame-live-p) (lambda (f) (eq f 'agenda)))
              ((symbol-function 'frame-parameter)
               (lambda (f p) (and (eq p 'cj/agenda-frame) (eq f 'agenda)))))
      (should (null (cj/--agenda-frame-working-frame))))))

;;; cj/--agenda-frame-command — the dedicated F seven-day view

(defun test-org-agenda-frame--block-settings ()
  "Return the per-block settings alist of the agenda-frame command."
  (let* ((cmd (cj/--agenda-frame-command))
         (blocks (nth 2 cmd))
         (agenda-block (car blocks)))
    (nth 2 agenda-block)))

(defun test-org-agenda-frame--general-settings ()
  "Return the general (view-wide) settings alist of the agenda-frame command."
  (nth 3 (cj/--agenda-frame-command)))

(ert-deftest test-org-agenda-frame-command-key-is-F ()
  "Normal: the command's key is F (collision-free with the existing d)."
  (should (equal (nth 0 (cj/--agenda-frame-command)) "F")))

(ert-deftest test-org-agenda-frame-command-today-anchored-7-day ()
  "Normal: the span is seven days anchored to today, not Monday."
  (let ((s (test-org-agenda-frame--block-settings)))
    (should (equal (cadr (assq 'org-agenda-span s)) 7))
    (should (equal (cadr (assq 'org-agenda-start-day s)) "0d"))
    ;; start-on-weekday nil is what un-anchors the span from Monday.
    (should (assq 'org-agenda-start-on-weekday s))
    (should (null (cadr (assq 'org-agenda-start-on-weekday s))))))

(ert-deftest test-org-agenda-frame-command-tight-prefix-format ()
  "Normal: the view sets its own prefix format with a narrow category column.
Without it the global agenda format applies, whose 25-char category pad
leaves a wide blank gutter between the source name and the item."
  (let ((s (test-org-agenda-frame--block-settings)))
    (should (assq 'org-agenda-prefix-format s))
    (should (string-match-p "%-10:c"
                            (cadr (assq 'org-agenda-prefix-format s))))))

(ert-deftest test-org-agenda-frame-do-redo-leaves-sticky-alone ()
  "Normal: the redo binds current-window but never touches sticky.
org-agenda-redo handles the in-place rebuild itself (it binds sticky nil
and redirects the buffer name); a sticky t reaching org-agenda-prepare
mid-redo makes it throw \\='exit with no catch and the tick fails."
  (let ((org-agenda-sticky nil)
        seen-sticky seen-setup (params '()))
    (with-temp-buffer
      (insert "agenda line\n")
      (cl-letf (((symbol-function 'org-agenda-redo)
                 (lambda (&rest _)
                   (setq seen-sticky org-agenda-sticky
                         seen-setup org-agenda-window-setup)))
                ((symbol-function 'frame-parameter) (lambda (_f p) (alist-get p params)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (_f p v) (setf (alist-get p params) v))))
        (cj/--agenda-frame-do-redo 'af (current-buffer) nil)
        (should (null seen-sticky))
        (should (eq seen-setup 'current-window))))))

(ert-deftest test-org-agenda-frame-command-follow-mode-off ()
  "Boundary: follow-mode is forced off locally so a global default can't split."
  (let ((s (test-org-agenda-frame--block-settings)))
    (should (assq 'org-agenda-start-with-follow-mode s))
    (should (null (cadr (assq 'org-agenda-start-with-follow-mode s))))))

(ert-deftest test-org-agenda-frame-command-sticky-and-current-window ()
  "Normal: current-window in the settings; sticky deliberately NOT there.
The general settings are baked into the buffer's series-redo-cmd and
re-applied on every redo; a sticky t there makes org-agenda-use-sticky-p
true mid-redo (the buffer exists), and org-agenda-prepare throws \\='exit
with no catch -- every refresh tick fails.  Stickiness belongs only in
the spawn wrapper, where it names the buffer."
  (let ((g (test-org-agenda-frame--general-settings)))
    (should-not (assq 'org-agenda-sticky g))
    ;; org evaluates custom-command setting values via org-let, so the stored
    ;; form is (quote current-window); eval it the way org would.
    (should (eq (eval (cadr (assq 'org-agenda-window-setup g)) t)
                'current-window))))

;;; cj/--agenda-frame-register-command — idempotent registration

(ert-deftest test-org-agenda-frame-register-adds-entry ()
  "Normal: registration inserts the F entry into org-agenda-custom-commands."
  (let ((org-agenda-custom-commands '(("d" "Daily" nil))))
    (cj/--agenda-frame-register-command)
    (should (assoc "F" org-agenda-custom-commands))
    (should (assoc "d" org-agenda-custom-commands))))

(ert-deftest test-org-agenda-frame-register-is-idempotent ()
  "Boundary: registering twice leaves exactly one F entry."
  (let ((org-agenda-custom-commands nil))
    (cj/--agenda-frame-register-command)
    (cj/--agenda-frame-register-command)
    (should (= 1 (seq-count (lambda (e) (equal (car e) "F"))
                            org-agenda-custom-commands)))))

;;; Default-deny policy — denial handlers

(ert-deftest test-org-agenda-frame-denied-readonly-messages ()
  "Normal: the read-only denial shows the read-only message and acts on nothing."
  (let (captured)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
      (cj/--agenda-frame-denied-readonly))
    (should (string-match-p "read-only" captured))
    (should (string-match-p "working frame" captured))))

(ert-deftest test-org-agenda-frame-denied-fixed-view-messages ()
  "Normal: the view-change denial shows the fixed-view message."
  (let (captured)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
      (cj/--agenda-frame-denied-fixed-view))
    (should (string-match-p "7-day view" captured))))

;;; Default-deny policy — the keymap

(ert-deftest test-org-agenda-frame-map-catch-all-is-readonly-deny ()
  "Normal: the [t] default binding denies with the read-only handler."
  (should (eq (lookup-key cj/agenda-frame-mode-map [t])
              'cj/--agenda-frame-denied-readonly)))

(ert-deftest test-org-agenda-frame-map-navigation-allowed ()
  "Normal: navigation keys resolve to their org-agenda commands."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "n")) 'org-agenda-next-line))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "p")) 'org-agenda-previous-line))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-g")) 'keyboard-quit)))

(ert-deftest test-org-agenda-frame-map-point-motion-and-isearch-allowed ()
  "Normal: read-only point motion and isearch work in the frame.
C-a/C-e/C-f/C-b move point and C-s/C-r search; all are read-only and
must not hit the deny catch-all."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-a")) 'move-beginning-of-line))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-e")) 'move-end-of-line))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-f")) 'forward-char))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-b")) 'backward-char))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-s")) 'isearch-forward))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-r")) 'isearch-backward)))

(ert-deftest test-org-agenda-frame-map-engage-routed ()
  "Normal: RET and TAB route to the working-frame engage command."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "RET"))
              'cj/--agenda-frame-engage-open))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "TAB"))
              'cj/--agenda-frame-engage-open)))

(ert-deftest test-org-agenda-frame-map-engage-gui-function-keys ()
  "Boundary: the GUI [return]/[tab] events engage too, not the [t] deny handler.
Without these, the catch-all suppresses their translation to RET/TAB in a
graphical frame and RET would be denied instead of opening the item."
  (should (eq (lookup-key cj/agenda-frame-mode-map [return])
              'cj/--agenda-frame-engage-open))
  (should (eq (lookup-key cj/agenda-frame-mode-map [tab])
              'cj/--agenda-frame-engage-open)))

(ert-deftest test-org-agenda-frame-map-lifecycle-keys ()
  "Normal: q/Q/x close the frame; r takes the safe-redo path."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "q")) 'cj/--agenda-frame-close))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "Q")) 'cj/--agenda-frame-close))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "x")) 'cj/--agenda-frame-close))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "r")) 'cj/--agenda-frame-safe-redo)))

(ert-deftest test-org-agenda-frame-map-view-changers-fixed-view-deny ()
  "Boundary: view-changing keys are explicitly denied with the fixed-view message,
not caught by the read-only catch-all."
  (dolist (key '("w" "d" "y" "f" "b" "j" "g"))
    (should (eq (lookup-key cj/agenda-frame-mode-map (kbd key))
                'cj/--agenda-frame-denied-fixed-view))))

(ert-deftest test-org-agenda-frame-map-frame-controls-bound ()
  "Normal: the frame's own controls work from inside the frame.
S-<f8> must close/toggle and C-M-<f8> must force-rescan; unbound, the
catch-all denies them and the frame can't be closed by its own key."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "S-<f8>"))
              'cj/agenda-frame-toggle))
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-M-<f8>"))
              'cj/org-agenda-refresh-files)))

(ert-deftest test-org-agenda-frame-map-C-x-C-c-closes-frame ()
  "Normal: C-x C-c in the agenda frame closes the frame, not the daemon.
The global save-buffers-kill-terminal would kill Emacs itself here (a
make-frame frame has no client), so the intuitive close gesture must be
remapped to the frame close."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "C-x C-c"))
              'cj/--agenda-frame-close)))

(ert-deftest test-org-agenda-frame-map-machinery-punched-through ()
  "Boundary: input machinery is punched through the [t] catch-all.
switch-frame events, mouse-wheel scrolling, mouse-1 clicks, and the help
prefix must fall through to their global bindings (an explicit nil shadows
the default in this map); otherwise every frame-focus change and every
scroll spams the deny message."
  (dolist (key (list [switch-frame]
                     [wheel-up] [wheel-down] [wheel-left] [wheel-right]
                     [double-wheel-up] [double-wheel-down]
                     [triple-wheel-up] [triple-wheel-down]
                     [mouse-1] [down-mouse-1] [drag-mouse-1]
                     (kbd "C-h")))
    ;; accept-default t: a punched key returns nil (falls through to the
    ;; global map); an unpunched key returns the catch-all deny handler.
    (should (null (lookup-key cj/agenda-frame-mode-map key t)))))

(ert-deftest test-org-agenda-frame-map-unpunched-still-denied ()
  "Normal: an ordinary unbound key still hits the catch-all after the punches."
  (should (eq (lookup-key cj/agenda-frame-mode-map (kbd "t") t)
              'cj/--agenda-frame-denied-readonly)))

(ert-deftest test-org-agenda-frame-maybe-enable-readds-kill-buffer-hook ()
  "Normal: the finalize re-enable also re-adds the buffer-local kill hook.
org-agenda-redo's kill-all-local-variables strips the hook installed at
spawn; without the re-add, killing the buffer after the first refresh tick
orphans the frame."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'agenda))
              ((symbol-function 'get-buffer-window) (lambda (_b _f) 'win)))
      (cj/--agenda-frame-maybe-enable-mode)
      (should (memq 'cj/--agenda-frame-on-kill-buffer
                    (buffer-local-value 'kill-buffer-hook (current-buffer)))))))

(ert-deftest test-org-agenda-frame-overlay-removable-after-local-var-wipe ()
  "Error: the failure overlay is found by property, not a buffer-local var.
kill-all-local-variables (every redo) wipes buffer-local vars while the
overlay object survives erase-buffer, so a var-held overlay could never be
removed after a later success -- the failure banner would stick forever."
  (with-temp-buffer
    (insert "x\n")
    (cj/--agenda-frame-show-failure-overlay (current-buffer))
    ;; Simulate the org-agenda-mode reset between failure and success.
    (kill-all-local-variables)
    (cj/--agenda-frame-remove-overlay (current-buffer))
    ;; The visible banner (a before-string overlay) must be gone.
    (should (= 0 (seq-count (lambda (o) (overlay-get o 'before-string))
                            (overlays-in (point-min) (point-max)))))))

(ert-deftest test-org-agenda-frame-map-mutation-keys-not-explicitly-bound ()
  "Boundary: a mutation key (t = org-agenda-todo) is not explicitly bound, so the
[t] catch-all denies it as read-only."
  (should (null (lookup-key cj/agenda-frame-mode-map (kbd "t")))))

;;; Default-deny policy — the minor mode + finalize re-enable

(ert-deftest test-org-agenda-frame-mode-toggles ()
  "Normal: the minor mode turns on and off in a buffer."
  (with-temp-buffer
    (cj/agenda-frame-mode 1)
    (should cj/agenda-frame-mode)
    (cj/agenda-frame-mode -1)
    (should-not cj/agenda-frame-mode)))

(ert-deftest test-org-agenda-frame-maybe-enable-in-agenda-frame ()
  "Normal: after a build in the agenda frame, the policy is re-enabled."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'agenda))
              ((symbol-function 'get-buffer-window) (lambda (_b _f) 'win)))
      (cj/--agenda-frame-maybe-enable-mode)
      (should cj/agenda-frame-mode))))

(ert-deftest test-org-agenda-frame-maybe-enable-skips-other-buffers ()
  "Boundary: a build not shown in the agenda frame leaves the policy off."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'agenda))
              ((symbol-function 'get-buffer-window) (lambda (_b _f) nil)))
      (cj/--agenda-frame-maybe-enable-mode)
      (should-not cj/agenda-frame-mode))))

(ert-deftest test-org-agenda-frame-maybe-enable-no-frame ()
  "Boundary: no agenda frame at all -> policy stays off, no error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () nil)))
      (cj/--agenda-frame-maybe-enable-mode)
      (should-not cj/agenda-frame-mode))))

;;; Engage routing — frame target + open

(ert-deftest test-org-agenda-frame-target-frame-uses-working-frame ()
  "Normal: the engage target is the working frame when one exists."
  (cl-letf (((symbol-function 'cj/--agenda-frame-working-frame) (lambda () 'work))
            ((symbol-function 'make-frame) (lambda (&rest _) (error "should not create"))))
    (should (eq (cj/--agenda-frame-target-frame) 'work))))

(ert-deftest test-org-agenda-frame-target-frame-creates-when-none ()
  "Boundary: no working frame -> a normal frame is created."
  (cl-letf (((symbol-function 'cj/--agenda-frame-working-frame) (lambda () nil))
            ((symbol-function 'make-frame) (lambda (&rest _) 'new)))
    (should (eq (cj/--agenda-frame-target-frame) 'new))))

(ert-deftest test-org-agenda-frame-engage-open-no-item-errors ()
  "Error: engaging on a line with no source item signals a user-error."
  (cl-letf (((symbol-function 'cj/--agenda-frame-item-marker) (lambda () nil)))
    (should-error (cj/--agenda-frame-engage-open) :type 'user-error)))

(ert-deftest test-org-agenda-frame-engage-open-routes-to-source ()
  "Normal: engage opens the item's source buffer at the item's position."
  (let ((source (generate-new-buffer " *frame-engage-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "line one\nline two\nline three\n"))
          (let ((marker (set-marker (make-marker) 10 source))
                focused opened)
            (cl-letf (((symbol-function 'cj/--agenda-frame-item-marker) (lambda () marker))
                      ((symbol-function 'cj/--agenda-frame-target-frame) (lambda () 'work))
                      ((symbol-function 'select-frame-set-input-focus)
                       (lambda (f &rest _) (setq focused f)))
                      ((symbol-function 'pop-to-buffer-same-window)
                       (lambda (b &rest _) (setq opened b) (set-buffer b)))
                      ((symbol-function 'org-fold-show-context) (lambda (&rest _) nil)))
              (cj/--agenda-frame-engage-open)
              (should (eq focused 'work))
              (should (eq opened source))
              (should (eq (current-buffer) source))
              (should (= (point) (line-beginning-position))))))
      (kill-buffer source))))

;;; Frame lifecycle — sticky buffer, timer-cancel, teardown cleanup

(ert-deftest test-org-agenda-frame-sticky-buffer-name ()
  "Normal: the sticky buffer is *Org Agenda(F)*; nil when absent."
  (should (null (cj/--agenda-frame-sticky-buffer)))
  (let ((buf (get-buffer-create "*Org Agenda(F)*")))
    (unwind-protect
        (should (eq (cj/--agenda-frame-sticky-buffer) buf))
      (kill-buffer buf))))

(ert-deftest test-org-agenda-frame-cancel-timer-safe-when-none ()
  "Boundary: cancelling with no timer set does nothing and does not error."
  (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () nil)))
    (should-not (cj/--agenda-frame-cancel-timer))))

;;; Frame lifecycle — toggle dispatch

(ert-deftest test-org-agenda-frame-toggle-spawns-when-none ()
  "Normal: with no agenda frame, toggle spawns one."
  (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () nil))
            ((symbol-function 'cj/--agenda-frame-spawn) (lambda () 'spawned)))
    (should (eq (cj/--agenda-frame-toggle) 'spawned))))

(ert-deftest test-org-agenda-frame-toggle-deletes-when-selected ()
  "Normal: toggle from within the agenda frame deletes it."
  (let (deleted)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'selected-frame) (lambda () 'af))
              ((symbol-function 'cj/--agenda-frame-delete)
               (lambda () (setq deleted t))))
      (cj/--agenda-frame-toggle)
      (should deleted))))

(ert-deftest test-org-agenda-frame-toggle-raises-when-unfocused ()
  "Normal: toggle from a working frame raises the existing agenda frame."
  (let (raised)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'selected-frame) (lambda () 'work))
              ((symbol-function 'cj/--agenda-frame-raise)
               (lambda (f) (setq raised f))))
      (cj/--agenda-frame-toggle)
      (should (eq raised 'af)))))

;;; Frame lifecycle — delete

(ert-deftest test-org-agenda-frame-delete-deletes-live-frame ()
  "Normal: delete removes the live agenda frame."
  (let (deleted)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'frame-live-p) (lambda (f) (eq f 'af)))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--agenda-frame-delete)
      (should (eq deleted 'af)))))

(ert-deftest test-org-agenda-frame-delete-noop-when-none ()
  "Boundary: delete with no agenda frame does nothing."
  (let (called)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () nil))
              ((symbol-function 'delete-frame) (lambda (_f &rest _) (setq called t))))
      (cj/--agenda-frame-delete)
      (should-not called))))

;;; Frame lifecycle — cleanup on frame death and buffer kill

(ert-deftest test-org-agenda-frame-on-delete-cancels-and-kills-buffer ()
  "Normal: deleting the agenda frame cancels its timer and kills the sticky buffer."
  (let ((buf (get-buffer-create "*Org Agenda(F)*"))
        cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--agenda-frame-p) (lambda (_f) t))
                  ((symbol-function 'cj/--agenda-frame-cancel-timer)
                   (lambda (&optional _f) (setq cancelled t))))
          (cj/--agenda-frame-on-delete-frame 'af)
          (should cancelled)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-org-agenda-frame-on-delete-ignores-non-agenda-frame ()
  "Boundary: a non-agenda frame deletion triggers no cleanup."
  (let (cancelled)
    (cl-letf (((symbol-function 'cj/--agenda-frame-p) (lambda (_f) nil))
              ((symbol-function 'cj/--agenda-frame-cancel-timer)
               (lambda (&optional _f) (setq cancelled t))))
      (cj/--agenda-frame-on-delete-frame 'work)
      (should-not cancelled))))

(ert-deftest test-org-agenda-frame-on-kill-buffer-deletes-frame ()
  "Normal: killing the dedicated buffer deletes the frame."
  (let (deleted)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'frame-live-p) (lambda (f) (eq f 'af)))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--agenda-frame-on-kill-buffer)
      (should (eq deleted 'af)))))

(ert-deftest test-org-agenda-frame-on-kill-buffer-guarded-during-teardown ()
  "Boundary: during a teardown the buffer-kill hook does not re-delete the frame."
  (let (deleted (cj/--agenda-frame-tearing-down t))
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--agenda-frame-on-kill-buffer)
      (should-not deleted))))

;;; Auto-dim suspension while the agenda frame lives

(ert-deftest test-org-agenda-frame-spawn-suspends-auto-dim ()
  "Normal: spawning the frame turns auto-dim off and remembers it was on.
The refresh tick's selection swing marks the working window non-selected;
auto-dim's debounced dim then lands after the tick and the working frame
visibly dims every five minutes."
  (defvar auto-dim-other-buffers-mode)
  (let ((auto-dim-other-buffers-mode t)
        (cj/--agenda-frame-dim-was-on nil)
        calls)
    (cl-letf (((symbol-function 'auto-dim-other-buffers-mode)
               (lambda (arg) (push arg calls)))
              ((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'af))
              ((symbol-function 'select-frame-set-input-focus) (lambda (_f &rest _) nil))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda) (lambda (&rest _) nil))
              ((symbol-function 'delete-other-windows) (lambda (&rest _) nil))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'cj/--agenda-frame-start-timer) (lambda (_f) nil)))
      (cj/--agenda-frame-spawn)
      (should (equal calls '(-1)))
      (should cj/--agenda-frame-dim-was-on))))

(ert-deftest test-org-agenda-frame-spawn-leaves-auto-dim-when-off ()
  "Boundary: auto-dim already off -> spawn doesn't touch it, no restore later."
  (defvar auto-dim-other-buffers-mode)
  (let ((auto-dim-other-buffers-mode nil)
        (cj/--agenda-frame-dim-was-on nil)
        calls)
    (cl-letf (((symbol-function 'auto-dim-other-buffers-mode)
               (lambda (arg) (push arg calls)))
              ((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'af))
              ((symbol-function 'select-frame-set-input-focus) (lambda (_f &rest _) nil))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda) (lambda (&rest _) nil))
              ((symbol-function 'delete-other-windows) (lambda (&rest _) nil))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'cj/--agenda-frame-start-timer) (lambda (_f) nil)))
      (cj/--agenda-frame-spawn)
      (should (null calls))
      (should-not cj/--agenda-frame-dim-was-on))))

(ert-deftest test-org-agenda-frame-on-delete-restores-auto-dim ()
  "Normal: closing the frame restores auto-dim when spawn had turned it off."
  (let ((cj/--agenda-frame-dim-was-on t)
        calls)
    (cl-letf (((symbol-function 'auto-dim-other-buffers-mode)
               (lambda (arg) (push arg calls)))
              ((symbol-function 'cj/--agenda-frame-p) (lambda (_f) t))
              ((symbol-function 'cj/--agenda-frame-cancel-timer)
               (lambda (&optional _f) nil)))
      (cj/--agenda-frame-on-delete-frame 'af)
      (should (equal calls '(1)))
      (should-not cj/--agenda-frame-dim-was-on))))

(ert-deftest test-org-agenda-frame-on-delete-no-dim-restore-when-untouched ()
  "Boundary: closing without a suspended auto-dim doesn't enable it."
  (let ((cj/--agenda-frame-dim-was-on nil)
        calls)
    (cl-letf (((symbol-function 'auto-dim-other-buffers-mode)
               (lambda (arg) (push arg calls)))
              ((symbol-function 'cj/--agenda-frame-p) (lambda (_f) t))
              ((symbol-function 'cj/--agenda-frame-cancel-timer)
               (lambda (&optional _f) nil)))
      (cj/--agenda-frame-on-delete-frame 'af)
      (should (null calls)))))

;;; Frame lifecycle — transactional spawn rollback

(ert-deftest test-org-agenda-frame-spawn-rolls-back-on-failure ()
  "Error: a failure after make-frame deletes the partial frame, restores the
working frame, and signals a user-error."
  (let (deleted focus)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'pf))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f &rest _) (setq focus f)))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda) (lambda (&rest _) (error "boom")))
              ((symbol-function 'frame-live-p) (lambda (f) (memq f '(pf launch))))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (should-error (cj/--agenda-frame-spawn) :type 'user-error)
      (should (eq deleted 'pf))
      (should (eq focus 'launch)))))

;;; Phase 2 — wall-clock alignment

(ert-deftest test-org-agenda-frame-seconds-to-next-mark-aligned ()
  "Normal: a time exactly on a 5-minute boundary yields a full period."
  ;; 1000000200 is divisible by 300 (a :00/:05 wall-clock mark).
  (should (= (cj/--agenda-frame-seconds-to-next-mark 1000000200 300) 300)))

(ert-deftest test-org-agenda-frame-seconds-to-next-mark-midway ()
  "Boundary: partway through a period returns the remainder to the next mark."
  ;; 1000000200 + 120 -> 180 seconds remain to the next 300 mark.
  (should (= (cj/--agenda-frame-seconds-to-next-mark 1000000320 300) 180)))

;;; Phase 2 — deterministic point restoration

(defun test-org-agenda-frame--make-agenda-buffer (lines source)
  "Insert LINES into the current buffer; each is (TEXT . SRC-POS).
A non-nil SRC-POS puts an org-marker into SOURCE at that position on the line."
  (dolist (spec lines)
    (let ((start (point)))
      (insert (car spec) "\n")
      (when (cdr spec)
        (put-text-property start (1+ start) 'org-marker
                           (set-marker (make-marker) (cdr spec) source))))))

(ert-deftest test-org-agenda-frame-restore-point-duplicate-nearest ()
  "Normal: a source marker occurring twice restores the occurrence nearest the old line."
  (let ((src (generate-new-buffer " *rp-src*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer src (insert "aaaaaaaaaa\n"))
          (test-org-agenda-frame--make-agenda-buffer
           '(("header" . nil) ("item @2" . 3) ("filler" . nil)
             ("filler" . nil) ("item @5" . 3))
           src)
          (let ((old (set-marker (make-marker) 3 src)))
            (cj/--agenda-frame-restore-point old 4))
          ;; lines 2 and 5 both point at src pos 3; nearest to old-line 4 is line 5.
          (should (= (line-number-at-pos) 5)))
      (kill-buffer src))))

(ert-deftest test-org-agenda-frame-restore-point-missing-clamps ()
  "Boundary: a gone marker clamps the old line into range and lands on an item."
  (let ((src (generate-new-buffer " *rp-src*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer src (insert "aaaaaaaaaa\n"))
          (test-org-agenda-frame--make-agenda-buffer
           '(("header" . nil) ("item" . 3) ("item" . 5))
           src)
          ;; old-line 99 is past the end; clamp to last line (an item).
          (let ((gone (set-marker (make-marker) 99 src)))
            (cj/--agenda-frame-restore-point gone 99))
          (should (get-text-property (line-beginning-position) 'org-marker)))
      (kill-buffer src))))

(ert-deftest test-org-agenda-frame-restore-point-header-goes-to-first-item ()
  "Boundary: with no marker match, a header line moves to the first item."
  (let ((src (generate-new-buffer " *rp-src*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer src (insert "aaaaaaaaaa\n"))
          (test-org-agenda-frame--make-agenda-buffer
           '(("header" . nil) ("item one" . 3) ("item two" . 5))
           src)
          (cj/--agenda-frame-restore-point nil 1)   ; line 1 is the header
          (should (= (line-number-at-pos) 2))
          (should (get-text-property (line-beginning-position) 'org-marker)))
      (kill-buffer src))))

(ert-deftest test-org-agenda-frame-restore-point-empty-buffer-start ()
  "Error: an empty (item-less) view leaves point at buffer start."
  (with-temp-buffer
    (test-org-agenda-frame--make-agenda-buffer
     '(("only a header" . nil) ("no items here" . nil)) nil)
    (cj/--agenda-frame-restore-point nil 2)
    (should (= (point) (point-min)))))

;;; Phase 2 — snapshot marker cloning

(ert-deftest test-org-agenda-frame-clone-and-reinstall-markers ()
  "Normal: cloned markers survive nulling the originals and reinstall live."
  (let ((src (generate-new-buffer " *clone-src*")))
    (unwind-protect
        (let (clones)
          (with-temp-buffer
            (with-current-buffer src (insert "0123456789\n"))
            (test-org-agenda-frame--make-agenda-buffer '(("item" . 4)) src)
            (setq clones (cj/--agenda-frame-snapshot-markers (current-buffer)))
            ;; Simulate org-agenda-reset-markers nulling the buffer's originals.
            (let ((orig (get-text-property (point-min) 'org-marker)))
              (set-marker orig nil)))
          ;; Reinstall into a fresh buffer copy; the clone must still be live.
          (with-temp-buffer
            (test-org-agenda-frame--make-agenda-buffer '(("item" . nil)) nil)
            (cj/--agenda-frame-reinstall-markers (current-buffer) clones)
            (let ((m (get-text-property (point-min) 'org-marker)))
              (should (markerp m))
              (should (eq (marker-buffer m) src))
              (should (= (marker-position m) 4)))))
      (kill-buffer src))))

;;; Phase 2 — failure latch (report once per consecutive-failure run)

(ert-deftest test-org-agenda-frame-failure-latch-reports-once ()
  "Normal: the first failure of a run reports; subsequent ones stay silent."
  (let ((params '()))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_f p) (alist-get p params)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f p v) (setf (alist-get p params) v))))
      (should (cj/--agenda-frame-record-failure 'af))       ; 0 -> 1, report
      (should-not (cj/--agenda-frame-record-failure 'af))   ; 1 -> 2, silent
      (cj/--agenda-frame-clear-failure 'af)
      (should (cj/--agenda-frame-record-failure 'af)))))    ; reset -> report again

;;; Phase 2 — timer start + duplicate prevention

(ert-deftest test-org-agenda-frame-start-timer-sets-timer ()
  "Normal: start-timer schedules and stores a timer when none exists."
  (let (stored)
    (cl-letf (((symbol-function 'frame-parameter) (lambda (_f _p) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'the-timer))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f _p v) (setq stored v))))
      (should (eq (cj/--agenda-frame-start-timer 'af) 'the-timer))
      (should (eq stored 'the-timer)))))

(ert-deftest test-org-agenda-frame-start-timer-no-duplicate ()
  "Boundary: a frame already carrying a live timer is not given a second one."
  (let (called)
    (cl-letf (((symbol-function 'frame-parameter) (lambda (_f _p) 'existing))
              ((symbol-function 'timerp) (lambda (x) (eq x 'existing)))
              ((symbol-function 'run-at-time) (lambda (&rest _) (setq called t) 'new)))
      (cj/--agenda-frame-start-timer 'af)
      (should-not called))))

;;; Phase 2 — do-redo orchestration (success and failure branches)

(ert-deftest test-org-agenda-frame-do-redo-success-clears-and-releases ()
  "Normal: a successful redo clears the failure latch and releases the snapshot."
  (let ((params (list (cons 'cj/agenda-frame-fail-count 3)))
        released)
    (with-temp-buffer
      (insert "agenda line\n")
      (cl-letf (((symbol-function 'org-agenda-redo) (lambda (&rest _) nil))
                ((symbol-function 'frame-parameter) (lambda (_f p) (alist-get p params)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (_f p v) (setf (alist-get p params) v)))
                ((symbol-function 'cj/--agenda-frame-release-snapshot)
                 (lambda (_s) (setq released t))))
        (cj/--agenda-frame-do-redo 'af (current-buffer) nil)
        (should (equal (alist-get 'cj/agenda-frame-fail-count params) 0))
        (should released)))))

(ert-deftest test-org-agenda-frame-do-redo-error-restores-reenables-reports ()
  "Error: a redo that fails mid-rebuild restores the last-good buffer verbatim,
re-enables the policy, shows one overlay, and reports once."
  (let ((params '()) msgs)
    (with-temp-buffer
      (insert "good agenda content\n")
      (unwind-protect
          (cl-letf (((symbol-function 'org-agenda-redo)
                     (lambda (&rest _) (erase-buffer) (insert "PARTIAL") (error "boom")))
                    ((symbol-function 'frame-parameter) (lambda (_f p) (alist-get p params)))
                    ((symbol-function 'set-frame-parameter)
                     (lambda (_f p v) (setf (alist-get p params) v)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest a) (push (apply #'format fmt a) msgs))))
            (cj/--agenda-frame-do-redo 'af (current-buffer) nil)
            (should cj/agenda-frame-mode)                         ; policy re-enabled
            (should (= 1 (length (cj/--agenda-frame-failure-overlays
                                  (current-buffer)))))            ; overlay shown
            (should (string-match-p "good agenda content" (buffer-string))) ; restored
            (should-not (string-match-p "PARTIAL" (buffer-string)))
            (should (= 1 (seq-count (lambda (m) (string-match-p "refresh failed" m))
                                    msgs))))
        (cj/agenda-frame-mode -1)
        (cj/--agenda-frame-remove-overlay (current-buffer))))))

(ert-deftest test-org-agenda-frame-do-redo-follows-item-across-shift ()
  "Normal: on a successful redo, point follows the same source item even when
lines shift and the buffer's own markers are nulled (the reset-markers case).
Guards against restoring by raw line number after the item moved."
  (let ((src (generate-new-buffer " *shift-src*"))
        (params '()))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer src (insert "0123456789\n"))
          ;; Before: item B (src pos 5) sits on line 3, and point is on it.
          (test-org-agenda-frame--make-agenda-buffer
           '(("header" . nil) ("item A" . 3) ("item B" . 5)) src)
          (goto-char (point-min)) (forward-line 2)   ; line 3, item B
          (cl-letf (((symbol-function 'frame-parameter) (lambda (_f p) (alist-get p params)))
                    ((symbol-function 'set-frame-parameter)
                     (lambda (_f p v) (setf (alist-get p params) v)))
                    ((symbol-function 'org-agenda-redo)
                     (lambda (&rest _)
                       ;; Null the buffer's originals (as org-agenda-reset-markers
                       ;; does), then rebuild with item B shifted to line 4.
                       (save-excursion
                         (goto-char (point-min))
                         (while (not (eobp))
                           (let ((m (get-text-property (line-beginning-position) 'org-marker)))
                             (when (markerp m) (set-marker m nil)))
                           (forward-line 1)))
                       (erase-buffer)
                       (test-org-agenda-frame--make-agenda-buffer
                        '(("header" . nil) ("new item" . 1) ("item A" . 3) ("item B" . 5))
                        src))))
            (cj/--agenda-frame-do-redo 'af (current-buffer) nil))
          ;; Point should be on the rebuilt item B (src pos 5), now line 4 --
          ;; not clamped to old line 3 (which is now item A, src pos 3).
          (let ((m (get-text-property (line-beginning-position) 'org-marker)))
            (should (markerp m))
            (should (= (marker-position m) 5))))
      (kill-buffer src))))

;;; Phase 2 — snapshot round-trip, release, safe-redo window contract, overlay

(ert-deftest test-org-agenda-frame-restore-snapshot-round-trip ()
  "Normal: snapshot then restore reinstates text and a live cloned marker."
  (let ((src (generate-new-buffer " *ss-src*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer src (insert "0123456789\n"))
          (test-org-agenda-frame--make-agenda-buffer '(("item alpha" . 4)) src)
          (let ((snap (cj/--agenda-frame-snapshot (current-buffer) nil)))
            (erase-buffer)
            (insert "CORRUPT")
            (cj/--agenda-frame-restore-snapshot (current-buffer) snap nil)
            (should (string-match-p "item alpha" (buffer-string)))
            (let ((m (get-text-property (point-min) 'org-marker)))
              (should (markerp m))
              (should (eq (marker-buffer m) src))
              (should (= (marker-position m) 4)))
            (cj/--agenda-frame-release-snapshot snap)
            (should-not (marker-buffer (cdr (car (plist-get snap :markers)))))))
      (kill-buffer src))))

(ert-deftest test-org-agenda-frame-safe-redo-selects-and-restores-window ()
  "Normal: the active tick runs the redo and restores the prior window (no focus theft)."
  (let (redone (prev (selected-window)))
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) prev))
              ((symbol-function 'cj/--agenda-frame-do-redo)
               (lambda (&rest _) (setq redone t))))
      (cj/--agenda-frame-safe-redo)
      (should redone)
      (should (eq (selected-window) prev)))))

(ert-deftest test-org-agenda-frame-overlay-idempotent ()
  "Boundary: showing the failure overlay twice keeps a single overlay."
  (with-temp-buffer
    (insert "x\n")
    (unwind-protect
        (progn
          (cj/--agenda-frame-show-failure-overlay (current-buffer))
          (let ((first (car (cj/--agenda-frame-failure-overlays (current-buffer)))))
            (cj/--agenda-frame-show-failure-overlay (current-buffer))
            (should (equal (cj/--agenda-frame-failure-overlays (current-buffer))
                           (list first)))
            (should (= 1 (seq-count (lambda (o) (overlay-get o 'before-string))
                                    (overlays-in (point-min) (point-max)))))))
      (cj/--agenda-frame-remove-overlay (current-buffer)))))

;;; Phase 2 — public command + F8-family rebind

(ert-deftest test-org-agenda-frame-public-toggle-wraps-private ()
  "Normal: the interactive command delegates to the private toggle."
  (let (called)
    (cl-letf (((symbol-function 'cj/--agenda-frame-toggle)
               (lambda () (setq called t))))
      (call-interactively 'cj/agenda-frame-toggle)
      (should called))))

(ert-deftest test-org-agenda-frame-parameters-normal-tiled-frame ()
  "Boundary: the agenda frame is a normal frame (the tiling WM places it
side by side with the working frame), carrying the marker and a distinct name."
  (let ((params (cj/--agenda-frame-make-parameters)))
    (should (assq cj/--agenda-frame-parameter params))
    (should (equal (cdr (assq 'name params)) "Full Agenda"))
    ;; No fullscreen request -- a fullboth frame would cover the whole output
    ;; instead of tiling beside the working frame.
    (should-not (assq 'fullscreen params))))

(ert-deftest test-org-agenda-frame-spawn-binds-sticky-and-current-window ()
  "Normal: spawn dynamically binds sticky + current-window around the render.
The custom command's own settings apply too late to name the buffer, so
without these bindings the buffer is plain *Org Agenda* -- which matches
the 0.75 below-selected display rule and splits the new frame with the
working buffer left on top."
  (let (seen-sticky seen-setup)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'af))
              ((symbol-function 'select-frame-set-input-focus) (lambda (_f &rest _) nil))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda)
               (lambda (&rest _)
                 (setq seen-sticky org-agenda-sticky
                       seen-setup org-agenda-window-setup)))
              ((symbol-function 'delete-other-windows) (lambda (&rest _) nil))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'cj/--agenda-frame-start-timer) (lambda (_f) nil)))
      (cj/--agenda-frame-spawn)
      (should (eq seen-sticky t))
      (should (eq seen-setup 'current-window)))))

(ert-deftest test-org-agenda-frame-spawn-forces-single-window ()
  "Boundary: spawn collapses the frame to one window after rendering.
A display rule that still splits the frame must not leave a second window
showing the launch buffer."
  (let (collapsed)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'af))
              ((symbol-function 'select-frame-set-input-focus) (lambda (_f &rest _) nil))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda) (lambda (&rest _) nil))
              ((symbol-function 'delete-other-windows)
               (lambda (&rest _) (setq collapsed t)))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'cj/--agenda-frame-start-timer) (lambda (_f) nil)))
      (cj/--agenda-frame-spawn)
      (should collapsed))))

(ert-deftest test-org-agenda-frame-spawn-starts-timer ()
  "Normal: a successful spawn starts the refresh timer for the new frame."
  (let (timed)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'launch))
              ((symbol-function 'make-frame) (lambda (&rest _) 'af))
              ((symbol-function 'select-frame-set-input-focus) (lambda (_f &rest _) nil))
              ((symbol-function 'cj/build-org-agenda-list) (lambda (&rest _) nil))
              ((symbol-function 'org-agenda) (lambda (&rest _) nil))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'cj/--agenda-frame-start-timer)
               (lambda (f) (setq timed f))))
      (should (eq (cj/--agenda-frame-spawn) 'af))
      (should (eq timed 'af)))))

(ert-deftest test-org-agenda-frame-safe-redo-inhibits-redisplay ()
  "Normal: the tick runs with redisplay inhibited.
The rebuild takes visible time; without this, the agenda window is the
selected window for the whole rebuild and the user watches their cursor
go hollow every five minutes -- indistinguishable from focus theft."
  (let (seen (prev (selected-window)))
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) prev))
              ((symbol-function 'cj/--agenda-frame-do-redo)
               (lambda (&rest _) (setq seen inhibit-redisplay))))
      (cj/--agenda-frame-safe-redo)
      (should (eq seen t)))))

(ert-deftest test-org-agenda-frame-safe-redo-skips-during-minibuffer ()
  "Boundary: a tick while a minibuffer is active is skipped entirely.
Reselecting windows under an active minibuffer session can break it; the
next tick catches up."
  (let (redone (prev (selected-window)))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mini))
              ((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) prev))
              ((symbol-function 'cj/--agenda-frame-do-redo)
               (lambda (&rest _) (setq redone t))))
      (cj/--agenda-frame-safe-redo)
      (should-not redone))))

(ert-deftest test-org-agenda-frame-safe-redo-noop-when-not-shown ()
  "Boundary: a tick with the buffer not shown in the frame does not redo or error."
  (let (redone)
    (cl-letf (((symbol-function 'cj/--agenda-frame) (lambda () 'af))
              ((symbol-function 'cj/--agenda-frame-sticky-buffer) (lambda () nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
              ((symbol-function 'cj/--agenda-frame-do-redo)
               (lambda (&rest _) (setq redone t))))
      (cj/--agenda-frame-safe-redo)
      (should-not redone))))

(ert-deftest test-org-agenda-frame-install-keys-rebinds-f8-family ()
  "Normal: S-<f8> toggles the frame; the force-rescan moves to C-M-<f8>."
  (let ((map (make-sparse-keymap)))
    (cj/--agenda-frame-install-keys map)
    (should (eq (lookup-key map (kbd "S-<f8>")) 'cj/agenda-frame-toggle))
    (should (eq (lookup-key map (kbd "C-M-<f8>")) 'cj/org-agenda-refresh-files))))

(provide 'test-org-agenda-frame)
;;; test-org-agenda-frame.el ends here
