;;; test-music-config--playlist-dock.el --- Tests for the F10 playlist dock -*- lexical-binding: t; -*-

;;; Commentary:
;; The F10 playlist always docks at the bottom, whatever the frame's shape.
;; It used to pick `right' on a wide frame via `cj/preferred-dock-direction',
;; which produced an unwanted three-way split on a wide terminal.
;;
;; `cj/side-window-display' is the window-system boundary here, so it is the
;; thing stubbed (an ordinary defun -- safe to `cl-letf', unlike the frame-*
;; subrs).  Stubbing it lets these tests assert what the toggle *asks for*
;; without needing a live frame under `--batch'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'music-config)

(defmacro test-music-dock--with-captured-side (captured &rest body)
  "Run BODY with the playlist display path stubbed, recording its args in CAPTURED.
CAPTURED is set to a plist of :side, :size-var and :default-size."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer " *test-playlist*")))
     (unwind-protect
         (cl-letf (((symbol-function 'cj/emms--setup) (lambda (&rest _) nil))
                   ((symbol-function 'cj/music--ensure-playlist-buffer)
                    (lambda (&rest _) buffer))
                   ((symbol-function 'emms-playlist-mode-center-current)
                    (lambda (&rest _) nil))
                   ((symbol-function 'cj/side-window-display)
                    (lambda (_buf side size-var default-size)
                      (setq ,captured (list :side side :size-var size-var
                                            :default-size default-size))
                      (selected-window))))
           ,@body)
       (kill-buffer buffer))))

(ert-deftest test-music-config-playlist-toggle-docks-bottom-on-wide-frame ()
  "Normal: a wide frame still docks the playlist at the bottom.
A wide frame used to dock right, splitting the frame three ways."
  (let (captured)
    (test-music-dock--with-captured-side captured
      (cl-letf (((symbol-function 'frame-width) (lambda (&rest _) 400)))
        (cj/music-playlist-toggle)))
    (should (eq 'bottom (plist-get captured :side)))))

(ert-deftest test-music-config-playlist-toggle-docks-bottom-on-narrow-frame ()
  "Boundary: a narrow frame docks at the bottom, as it always did."
  (let (captured)
    (test-music-dock--with-captured-side captured
      (cl-letf (((symbol-function 'frame-width) (lambda (&rest _) 40)))
        (cj/music-playlist-toggle)))
    (should (eq 'bottom (plist-get captured :side)))))

(ert-deftest test-music-config-playlist-toggle-uses-height-memory ()
  "Normal: the bottom dock carries the height fraction and its memory var."
  (let (captured)
    (test-music-dock--with-captured-side captured
      (cj/music-playlist-toggle))
    (should (eq 'cj/--music-playlist-height (plist-get captured :size-var)))
    (should (= cj/music-playlist-window-height (plist-get captured :default-size)))))

(ert-deftest test-music-config-playlist-dock-has-no-width-knobs ()
  "Error: the right-dock width variables are gone, not merely unused.
A stale `cj/music-playlist-window-width' would read as a live knob that
silently does nothing."
  (should-not (boundp 'cj/music-playlist-window-width))
  (should-not (boundp 'cj/--music-playlist-width))
  (should-not (fboundp 'cj/--music-playlist-side)))

(ert-deftest test-music-config-playlist-default-height-is-half ()
  "Normal: the dock opens at half the frame height by default (Craig,
2026-07-18 -- a third still read too short for a real playlist)."
  (should (= cj/music-playlist-window-height 0.5)))

(ert-deftest test-music-config-playlist-toggle-off-discards-shrunk-height ()
  "Error: a captured height below the default is discarded.  Window churn
squeezes the dock, and remembering the squeeze reopens it too short on
every later toggle."
  (let ((buffer (generate-new-buffer " *test-playlist-shrink*"))
        (cj/--music-playlist-height nil))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((cj/music-playlist-buffer-name (buffer-name buffer)))
            (cl-letf (((symbol-function 'cj/side-window-capture-size)
                       (lambda (_w _side var) (set var 0.2)))
                      ((symbol-function 'delete-window) #'ignore)
                      ((symbol-function 'message) #'ignore))
              (cj/music-playlist-toggle)))
          (should (null cj/--music-playlist-height)))
      (kill-buffer buffer))))

(ert-deftest test-music-config-playlist-toggle-off-keeps-enlarged-height ()
  "Normal: a captured height at or above the default is remembered, so a
deliberate enlargement sticks for the session."
  (let ((buffer (generate-new-buffer " *test-playlist-grow*"))
        (cj/--music-playlist-height nil))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((cj/music-playlist-buffer-name (buffer-name buffer)))
            (cl-letf (((symbol-function 'cj/side-window-capture-size)
                       (lambda (_w _side var) (set var 0.5)))
                      ((symbol-function 'delete-window) #'ignore)
                      ((symbol-function 'message) #'ignore))
              (cj/music-playlist-toggle)))
          (should (= 0.5 cj/--music-playlist-height)))
      (kill-buffer buffer))))

(provide 'test-music-config--playlist-dock)
;;; test-music-config--playlist-dock.el ends here
