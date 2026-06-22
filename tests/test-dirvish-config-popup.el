;;; test-dirvish-config-popup.el --- Dirvish Hyprland popup tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Hyprland Super+F dirvish popup.  The launcher opens an
;; emacsclient frame named "dirvish" (window rules float/size/center it by that
;; name) and runs `cj/dirvish-popup', which opens Dirvish rooted at home.  `q'
;; runs `cj/dirvish-popup-quit': in the popup frame it quits Dirvish and deletes
;; the frame; in any other frame it quits Dirvish normally.  Covered here: frame
;; discovery by name, the emacsclient focus race on open, and the quit dispatch
;; on every frame condition.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dirvish-config)

;;; cj/--dirvish-popup-frame  (find the popup frame by name)

(ert-deftest test-dirvish-config-popup-frame-found ()
  "Normal: returns the live frame whose name is \"dirvish\"."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fb fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter)
             (lambda (f _p) (if (eq f 'fb) "dirvish" "other"))))
    (should (eq (cj/--dirvish-popup-frame) 'fb))))

(ert-deftest test-dirvish-config-popup-frame-none ()
  "Boundary: no popup frame present yields nil."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fc)))
            ((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-parameter) (lambda (_f _p) "other")))
    (should-not (cj/--dirvish-popup-frame))))

(ert-deftest test-dirvish-config-popup-frame-skips-dead ()
  "Boundary: a dead frame named \"dirvish\" is skipped."
  (cl-letf (((symbol-function 'frame-list) (lambda () '(fa fb)))
            ((symbol-function 'frame-live-p) (lambda (f) (not (eq f 'fb))))
            ((symbol-function 'frame-parameter) (lambda (_f _p) "dirvish")))
    (should (eq (cj/--dirvish-popup-frame) 'fa))))

;;; cj/dirvish-popup  (open dirvish in the named frame)

(ert-deftest test-dirvish-config-popup-selects-named-frame ()
  "Integration: cj/dirvish-popup focuses the \"dirvish\" frame found by name,
not whatever frame happens to be selected (the emacsclient -c focus race).

Components integrated:
- cj/dirvish-popup (real)
- cj/--dirvish-popup-frame (MOCKED — returns a sentinel frame)
- select-frame-set-input-focus (MOCKED — records the focused frame)
- dirvish (MOCKED — records the path opened)"
  (let ((focused nil) (opened nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f &rest _) (setq focused f)))
              ((symbol-function 'dirvish) (lambda (&optional p) (setq opened (or p t)))))
      (cj/dirvish-popup))
    (should (eq focused 'popup-frame))
    (should opened)))

(ert-deftest test-dirvish-config-popup-no-frame-still-opens ()
  "Integration: with no popup frame found, cj/dirvish-popup skips the focus call
and still opens Dirvish (no error)."
  (let ((focused 'unset) (opened nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () nil))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f &rest _) (setq focused f)))
              ((symbol-function 'dirvish) (lambda (&optional _p) (setq opened t))))
      (cj/dirvish-popup))
    (should (eq focused 'unset))
    (should opened)))

;;; cj/dirvish-popup-quit  (quit; delete the popup frame only when in it)

(ert-deftest test-dirvish-config-popup-quit-in-popup-deletes-frame ()
  "Normal: in the popup frame, q quits Dirvish and deletes the popup frame."
  (let ((quit 0) (deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'selected-frame) (lambda () 'popup))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'dirvish-quit) (lambda () (cl-incf quit)))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/dirvish-popup-quit))
    (should (= quit 1))
    (should (eq deleted 'popup))))

(ert-deftest test-dirvish-config-popup-quit-normal-frame-keeps-frame ()
  "Boundary: with no popup frame, q quits Dirvish and deletes nothing."
  (let ((quit 0) (deleted 'unset))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () nil))
              ((symbol-function 'selected-frame) (lambda () 'main))
              ((symbol-function 'dirvish-quit) (lambda () (cl-incf quit)))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/dirvish-popup-quit))
    (should (= quit 1))
    (should (eq deleted 'unset))))

(ert-deftest test-dirvish-config-popup-quit-popup-not-selected-keeps-frame ()
  "Boundary: the popup exists but a different frame is selected — q quits Dirvish
in that frame and does not delete the popup."
  (let ((quit 0) (deleted 'unset))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'selected-frame) (lambda () 'main))
              ((symbol-function 'dirvish-quit) (lambda () (cl-incf quit)))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/dirvish-popup-quit))
    (should (= quit 1))
    (should (eq deleted 'unset))))

(ert-deftest test-dirvish-config-popup-quit-survives-dirvish-quit-error ()
  "Error: a signal from dirvish-quit in the popup still deletes the frame."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'selected-frame) (lambda () 'popup))
              ((symbol-function 'frame-live-p) (lambda (_f) t))
              ((symbol-function 'dirvish-quit) (lambda () (error "boom")))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/dirvish-popup-quit))
    (should (eq deleted 'popup))))

;;; cj/dirvish-popup-focus-existing  (second-launch re-use guard)

(ert-deftest test-dirvish-config-popup-focus-existing-found ()
  "Normal: an existing popup is focused and t is returned."
  (let ((focused nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f &rest _) (setq focused f))))
      (should (eq (cj/dirvish-popup-focus-existing) t))
      (should (eq focused 'popup)))))

(ert-deftest test-dirvish-config-popup-focus-existing-none ()
  "Boundary: no popup present — returns nil and focuses nothing."
  (let ((focused 'unset))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () nil))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (f &rest _) (setq focused f))))
      (should-not (cj/dirvish-popup-focus-existing))
      (should (eq focused 'unset)))))

;;; cj/--dirvish-popup-selected-p

(ert-deftest test-dirvish-config-popup-selected-p-true ()
  "Normal: true when the selected frame is the popup frame."
  (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
            ((symbol-function 'selected-frame) (lambda () 'popup)))
    (should (cj/--dirvish-popup-selected-p))))

(ert-deftest test-dirvish-config-popup-selected-p-false-other-frame ()
  "Boundary: false when a different frame is selected."
  (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
            ((symbol-function 'selected-frame) (lambda () 'main)))
    (should-not (cj/--dirvish-popup-selected-p))))

(ert-deftest test-dirvish-config-popup-selected-p-false-no-popup ()
  "Boundary: false when no popup frame exists."
  (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () nil))
            ((symbol-function 'selected-frame) (lambda () 'main)))
    (should-not (cj/--dirvish-popup-selected-p))))

;;; cj/dirvish-popup-find-file  (popup = launcher; outside = plain find-file)

(ert-deftest test-dirvish-config-popup-find-file-in-popup-file-launches-external ()
  "Normal: in the popup, a file at point opens via cj/xdg-open, not in-frame."
  (let ((opened nil) (visited nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-selected-p) (lambda () t))
              ((symbol-function 'dired-get-file-for-visit) (lambda () "/tmp/a.mp4"))
              ((symbol-function 'file-directory-p) (lambda (_f) nil))
              ((symbol-function 'cj/xdg-open) (lambda (f) (setq opened f)))
              ((symbol-function 'dired-find-file) (lambda () (setq visited t))))
      (cj/dirvish-popup-find-file))
    (should (equal opened "/tmp/a.mp4"))
    (should-not visited)))

(ert-deftest test-dirvish-config-popup-find-file-in-popup-dir-navigates ()
  "Boundary: in the popup, a directory at point is entered normally."
  (let ((opened nil) (visited nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-selected-p) (lambda () t))
              ((symbol-function 'dired-get-file-for-visit) (lambda () "/tmp/dir/"))
              ((symbol-function 'file-directory-p) (lambda (_f) t))
              ((symbol-function 'cj/xdg-open) (lambda (f) (setq opened f)))
              ((symbol-function 'dired-find-file) (lambda () (setq visited t))))
      (cj/dirvish-popup-find-file))
    (should visited)
    (should-not opened)))

(ert-deftest test-dirvish-config-popup-find-file-outside-popup-is-plain-find-file ()
  "Boundary: outside the popup, behaves exactly like dired-find-file."
  (let ((opened nil) (visited nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-selected-p) (lambda () nil))
              ((symbol-function 'cj/xdg-open) (lambda (f) (setq opened f)))
              ((symbol-function 'dired-find-file) (lambda () (setq visited t))))
      (cj/dirvish-popup-find-file))
    (should visited)
    (should-not opened)))

;;; cj/--dirvish-popup-focus-watch  (dismiss on focus loss, armed after focus)

(ert-deftest test-dirvish-config-popup-focus-watch-focused-arms-flag ()
  "Normal: while the popup is focused, the watch sets the had-focus flag and
deletes nothing."
  (let ((params '()) (deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'frame-focus-state) (lambda (_f) t))
              ((symbol-function 'frame-parameter) (lambda (_f p) (plist-get params p)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f p v) (setq params (plist-put params p v))))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--dirvish-popup-focus-watch))
    (should (plist-get params 'cj-dirvish-popup-had-focus))
    (should-not deleted)))

(ert-deftest test-dirvish-config-popup-focus-watch-unfocused-after-arming-deletes ()
  "Normal: lost focus after having held it — the popup is deleted."
  (let ((params (list 'cj-dirvish-popup-had-focus t)) (deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'frame-focus-state) (lambda (_f) nil))
              ((symbol-function 'frame-parameter) (lambda (_f p) (plist-get params p)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f p v) (setq params (plist-put params p v))))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--dirvish-popup-focus-watch))
    (should (eq deleted 'popup))))

(ert-deftest test-dirvish-config-popup-focus-watch-unfocused-before-arming-keeps ()
  "Boundary: not focused and never armed (the creation race) — NOT deleted."
  (let ((params '()) (deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () 'popup))
              ((symbol-function 'frame-focus-state) (lambda (_f) nil))
              ((symbol-function 'frame-parameter) (lambda (_f p) (plist-get params p)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f p v) (setq params (plist-put params p v))))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--dirvish-popup-focus-watch))
    (should-not deleted)))

(ert-deftest test-dirvish-config-popup-focus-watch-no-popup-is-noop ()
  "Error: with no popup frame, the watch does nothing and doesn't raise."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'cj/--dirvish-popup-frame) (lambda () nil))
              ((symbol-function 'delete-frame) (lambda (f &rest _) (setq deleted f))))
      (cj/--dirvish-popup-focus-watch))
    (should-not deleted)))

(provide 'test-dirvish-config-popup)
;;; test-dirvish-config-popup.el ends here
