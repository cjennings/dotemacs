;;; test-music-config--playlist-open-position.el --- Tests for playlist landing position -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Opening the playlist lands point by one rule: the beginning of the playing
;; track's line when a song is playing, else the top of the list.  The old
;; behavior keyed off EMMS's selected track, which stays set while stopped, so
;; the playlist opened deep in the list at a stale position.  The decision is
;; a pure helper; the window landing (point + upper-third recenter) is tested
;; with recenter stubbed at the display boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Declare the emms vars special HERE too: the module's bare (defvar
;; emms-player-playing-p) marks them special only for code compiled in
;; that file, so a plain `let' in this lexical-binding test file would
;; bind them lexically and the module would never see the value (the
;; scope-shadowing trap from the testing rules).
(defvar emms-player-playing-p)
(defvar emms-playlist-selected-marker)

(require 'music-config)

(defmacro test-music-open-pos--with-buffer (var &rest body)
  "Run BODY with VAR bound to a temp 3-track playlist-shaped buffer."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *test-open-pos*")))
     (unwind-protect
         (progn
           (with-current-buffer ,var
             (insert "track one\ntrack two\ntrack three\n"))
           ,@body)
       (when (buffer-live-p ,var) (kill-buffer ,var)))))

(defun test-music-open-pos--marker (buffer line offset)
  "Marker in BUFFER at LINE (1-based) plus OFFSET chars."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char offset)
      (point-marker))))

;;; Normal Cases

(ert-deftest test-music-playlist-open-position-playing-lands-on-playing-line-start ()
  "Normal: playing -> the playing track's line, at its beginning (even when
the marker sits mid-line)."
  (test-music-open-pos--with-buffer buf
    (let ((emms-player-playing-p t)
          (emms-playlist-selected-marker (test-music-open-pos--marker buf 2 4)))
      (should (= (cj/music--playlist-open-position buf)
                 (with-current-buffer buf
                   (save-excursion (goto-char (point-min)) (forward-line 1) (point))))))))

(ert-deftest test-music-playlist-open-position-stopped-lands-at-top ()
  "Normal: not playing -> top of the list, even though EMMS still has a
stale selected track."
  (test-music-open-pos--with-buffer buf
    (let ((emms-player-playing-p nil)
          (emms-playlist-selected-marker (test-music-open-pos--marker buf 3 0)))
      (should (= (cj/music--playlist-open-position buf) 1)))))

;;; Boundary Cases

(ert-deftest test-music-playlist-open-position-playing-no-marker-lands-at-top ()
  "Boundary: playing but no usable marker -> top of the list."
  (test-music-open-pos--with-buffer buf
    (let ((emms-player-playing-p t)
          (emms-playlist-selected-marker nil))
      (should (= (cj/music--playlist-open-position buf) 1)))))

(ert-deftest test-music-playlist-open-position-marker-in-other-buffer-lands-at-top ()
  "Boundary: a marker pointing into a different buffer is ignored."
  (test-music-open-pos--with-buffer buf
    (with-temp-buffer
      (insert "elsewhere\n")
      (let ((emms-player-playing-p t)
            (emms-playlist-selected-marker (point-marker)))
        (should (= (cj/music--playlist-open-position buf) 1))))))

(ert-deftest test-music-playlist-open-position-empty-buffer ()
  "Boundary: an empty playlist lands at point-min without error."
  (let ((buf (generate-new-buffer " *test-open-pos-empty*")))
    (unwind-protect
        (let ((emms-player-playing-p nil)
              (emms-playlist-selected-marker nil))
          (should (= (cj/music--playlist-open-position buf) 1)))
      (kill-buffer buf))))

;;; Landing (window boundary stubbed)

(ert-deftest test-music-playlist-land-point-playing-recenter-upper-third ()
  "Normal: landing on a playing row sets window point to its line start and
recenters into the upper third."
  (test-music-open-pos--with-buffer buf
    (let ((emms-player-playing-p t)
          (emms-playlist-selected-marker (test-music-open-pos--marker buf 2 4))
          (recenter-arg 'not-called))
      (save-window-excursion
        (set-window-buffer (selected-window) buf)
        (cl-letf (((symbol-function 'recenter)
                   (lambda (&optional arg &rest _) (setq recenter-arg arg))))
          (cj/music--playlist-land-point (selected-window) buf))
        (should (= (window-point (selected-window))
                   (with-current-buffer buf
                     (save-excursion (goto-char (point-min)) (forward-line 1) (point)))))
        (should (integerp recenter-arg))
        (should (>= recenter-arg 1))))))

(ert-deftest test-music-playlist-land-point-stopped-top-no-recenter ()
  "Normal: landing while stopped puts window point at the top; no recenter."
  (test-music-open-pos--with-buffer buf
    (let ((emms-player-playing-p nil)
          (emms-playlist-selected-marker nil)
          (recenter-called nil))
      (save-window-excursion
        (set-window-buffer (selected-window) buf)
        (cl-letf (((symbol-function 'recenter)
                   (lambda (&rest _) (setq recenter-called t))))
          (cj/music--playlist-land-point (selected-window) buf))
        (should (= (window-point (selected-window)) 1))
        (should-not recenter-called)))))

;;; hl-line in the playlist buffer

(ert-deftest test-music-playlist-ensure-enables-hl-line ()
  "Normal: the playlist buffer gets hl-line-mode so the current row is
findable even when the cursor sits on album art."
  (let (created)
    (cl-letf (((symbol-function 'emms-playlist-mode) #'ignore))
      (unwind-protect
          (progn
            (setq created (cj/music--ensure-playlist-buffer))
            (with-current-buffer created
              (should hl-line-mode)))
        (when (buffer-live-p created) (kill-buffer created))))))

(provide 'test-music-config--playlist-open-position)
;;; test-music-config--playlist-open-position.el ends here
