;;; test-org-capture-config--neutralize.el --- Popup neutralize-guard tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the "org-capture" popup neutralize guards.  The popup frame opens
;; showing the daemon's last buffer; if a capture aborts before its UI paints,
;; the popup lingers on that buffer, and a live eat/vterm terminal shown there
;; clamps the real frame to the popup's rows.  The guards repoint every
;; non-capture-UI window of the popup at *scratch*: one fires on frame creation
;; (after-make-frame-functions), one on any buffer change
;; (window-buffer-change-functions).
;;
;; The tests drive the real batch frame (renamed to "org-capture" and restored
;; in cleanup, the same idiom as the popup-window integration test) rather than
;; mocking frame primitives.  `window-buffer-change-functions' only runs during
;; redisplay, so the module's own hook cannot fire mid-test in batch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-capture)
(require 'user-constants)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

(defmacro test-neutralize--with-popup-frame (buffer-name &rest body)
  "Run BODY with the batch frame named \"org-capture\" showing BUFFER-NAME.
The frame name reverts to auto-naming and the test buffer is killed
afterward.  BODY sees the buffer bound to `buf'."
  (declare (indent 1))
  `(let ((buf (get-buffer-create ,buffer-name)))
     (unwind-protect
         (progn
           (set-frame-parameter nil 'name "org-capture")
           (delete-other-windows)
           (set-window-buffer (selected-window) buf)
           ,@body)
       ;; Restore to nil, not the saved name: the batch frame's auto name is
       ;; "F1", and Emacs refuses to set F<num>-shaped names explicitly.
       ;; nil reverts to auto-naming (same idiom as the popup-window test).
       (set-frame-parameter nil 'name nil)
       (when (buffer-live-p buf) (kill-buffer buf)))))

;;; cj/org-capture--neutralize-frame

(ert-deftest test-org-capture-config-neutralize-frame-evicts-plain-buffer ()
  "Normal: a non-capture-UI buffer in the popup frame is repointed to *scratch*."
  (test-neutralize--with-popup-frame "neutralize-plain.org"
    (cj/org-capture--neutralize-frame (selected-frame))
    (should (equal (buffer-name (window-buffer (selected-window)))
                   "*scratch*"))))

(ert-deftest test-org-capture-config-neutralize-frame-spares-capture-buffer ()
  "Normal: a CAPTURE-* buffer is capture UI and stays put."
  (test-neutralize--with-popup-frame "CAPTURE-neutralize.org"
    (cj/org-capture--neutralize-frame (selected-frame))
    (should (eq (window-buffer (selected-window)) buf))))

(ert-deftest test-org-capture-config-neutralize-frame-spares-select-menu ()
  "Normal: the *Org Select* template menu is capture UI and stays put."
  (test-neutralize--with-popup-frame "*Org Select*"
    (cj/org-capture--neutralize-frame (selected-frame))
    (should (eq (window-buffer (selected-window)) buf))))

(ert-deftest test-org-capture-config-neutralize-frame-scratch-untouched ()
  "Boundary: a window already on *scratch* is left alone (idempotent)."
  (let ((scratch (get-buffer-create "*scratch*")))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'name "org-capture")
          (delete-other-windows)
          (set-window-buffer (selected-window) scratch)
          (cj/org-capture--neutralize-frame (selected-frame))
          (should (eq (window-buffer (selected-window)) scratch))
          ;; Second pass is also a no-op.
          (cj/org-capture--neutralize-frame (selected-frame))
          (should (eq (window-buffer (selected-window)) scratch)))
      (set-frame-parameter nil 'name nil))))

(ert-deftest test-org-capture-config-neutralize-frame-other-frame-untouched ()
  "Boundary: a frame not named \"org-capture\" is never neutralized."
  (let ((buf (get-buffer-create "neutralize-other-frame.org")))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'name "some-other-frame")
          (delete-other-windows)
          (set-window-buffer (selected-window) buf)
          (cj/org-capture--neutralize-frame (selected-frame))
          (should (eq (window-buffer (selected-window)) buf)))
      (set-frame-parameter nil 'name nil)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-org-capture-config-neutralize-frame-dead-input-no-error ()
  "Error: nil and non-frame inputs are ignored without raising."
  (should-not (cj/org-capture--neutralize-frame nil))
  (should-not (cj/org-capture--neutralize-frame 'not-a-frame)))

;;; cj/org-capture--neutralize-new-frame (Guard 1 wrapper)

(ert-deftest test-org-capture-config-neutralize-new-frame-delegates ()
  "Normal: the frame-creation wrapper neutralizes the popup frame."
  (test-neutralize--with-popup-frame "neutralize-new-frame.org"
    (cj/org-capture--neutralize-new-frame (selected-frame))
    (should (equal (buffer-name (window-buffer (selected-window)))
                   "*scratch*"))))

;;; cj/org-capture--neutralize-on-buffer-change (Guard 2 wrapper)

(ert-deftest test-org-capture-config-neutralize-on-buffer-change-window-arg ()
  "Normal: a window argument resolves to its frame and neutralizes it.
`window-buffer-change-functions' passes a window when buffer-local."
  (test-neutralize--with-popup-frame "neutralize-window-arg.org"
    (cj/org-capture--neutralize-on-buffer-change (selected-window))
    (should (equal (buffer-name (window-buffer (selected-window)))
                   "*scratch*"))))

(ert-deftest test-org-capture-config-neutralize-on-buffer-change-frame-arg ()
  "Normal: a frame argument passes straight through.
`window-buffer-change-functions' passes a frame when global."
  (test-neutralize--with-popup-frame "neutralize-frame-arg.org"
    (cj/org-capture--neutralize-on-buffer-change (selected-frame))
    (should (equal (buffer-name (window-buffer (selected-window)))
                   "*scratch*"))))

;;; Hook wiring

(ert-deftest test-org-capture-config-neutralize-hooks-registered ()
  "Normal: both guards are installed on their hooks at module load."
  (should (memq #'cj/org-capture--neutralize-new-frame
                after-make-frame-functions))
  (should (memq #'cj/org-capture--neutralize-on-buffer-change
                window-buffer-change-functions)))

(provide 'test-org-capture-config--neutralize)
;;; test-org-capture-config--neutralize.el ends here
