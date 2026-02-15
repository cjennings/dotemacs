;;; test-music-config-toggle-consume.el --- Tests for consume mode -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music-toggle-consume and cj/music--consume-track.
;;
;; cj/music-toggle-consume toggles consume mode on/off, managing the
;; emms-player-finished-hook to remove tracks after playback.
;;
;; cj/music--consume-track is the hook function that removes the track
;; at the selected marker position from the playlist buffer.
;;
;; Test organization:
;; - Normal Cases: Toggle on/off, track removal during consume
;; - Boundary Cases: Double toggle, single-track playlist, consume off
;; - Error Cases: No selected marker, empty playlist
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Add EMMS elpa directory to load path for batch testing
(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir
    (add-to-list 'load-path emms-dir)))

;; Load EMMS for playlist buffer setup
(require 'emms)
(require 'emms-playlist-mode)

;; Load production code
(require 'music-config)

;;; Test helpers

(defun test-consume--setup-playlist-buffer (track-names)
  "Create an EMMS playlist buffer with TRACK-NAMES and return it.
Each entry in TRACK-NAMES becomes a file track in the playlist.
Uses EMMS's own insertion function for proper text properties.
Selects the first track by default."
  (let ((buf (get-buffer-create "*EMMS-Test-Playlist*")))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (name track-names)
          (emms-playlist-insert-track (emms-track 'file name)))
        ;; Set selected marker to first track
        (when track-names
          (goto-char (point-min))
          (unless emms-playlist-selected-marker
            (setq emms-playlist-selected-marker (make-marker)))
          (set-marker emms-playlist-selected-marker (point)))))
    buf))

(defun test-consume--teardown ()
  "Clean up consume mode state and test buffers."
  (setq cj/music-consume-mode nil)
  (remove-hook 'emms-player-finished-hook #'cj/music--consume-track)
  (when-let ((buf (get-buffer "*EMMS-Test-Playlist*")))
    (kill-buffer buf)))

(defun test-consume--track-count (buf)
  "Return number of tracks in playlist buffer BUF."
  (with-current-buffer buf
    (if (= (point-min) (point-max))
        0
      (count-lines (point-min) (point-max)))))

(defun test-consume--capture-message (fn &rest args)
  "Call FN with ARGS and return the message it produces.
Works in both interactive and batch mode."
  (let ((captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest msg-args)
                 (setq captured (apply #'format fmt msg-args)))))
      (apply fn args))
    captured))

;;; Normal Cases - cj/music-toggle-consume

(ert-deftest test-music-config-toggle-consume-normal-enable-sets-variable ()
  "Validate toggling consume on sets cj/music-consume-mode to t."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode nil)
        (cj/music-toggle-consume)
        (should (eq cj/music-consume-mode t)))
    (test-consume--teardown)))

(ert-deftest test-music-config-toggle-consume-normal-enable-adds-hook ()
  "Validate toggling consume on adds consume-track to finished hook."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode nil)
        (cj/music-toggle-consume)
        (should (memq #'cj/music--consume-track emms-player-finished-hook)))
    (test-consume--teardown)))

(ert-deftest test-music-config-toggle-consume-normal-disable-clears-variable ()
  "Validate toggling consume off sets cj/music-consume-mode to nil."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode t)
        (add-hook 'emms-player-finished-hook #'cj/music--consume-track)
        (cj/music-toggle-consume)
        (should (eq cj/music-consume-mode nil)))
    (test-consume--teardown)))

(ert-deftest test-music-config-toggle-consume-normal-disable-removes-hook ()
  "Validate toggling consume off removes consume-track from finished hook."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode t)
        (add-hook 'emms-player-finished-hook #'cj/music--consume-track)
        (cj/music-toggle-consume)
        (should-not (memq #'cj/music--consume-track emms-player-finished-hook)))
    (test-consume--teardown)))

(ert-deftest test-music-config-toggle-consume-normal-enable-message ()
  "Validate toggling on produces enabled message."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode nil)
        (let ((msg (test-consume--capture-message #'cj/music-toggle-consume)))
          (should (string-match-p "enabled" msg))))
    (test-consume--teardown)))

(ert-deftest test-music-config-toggle-consume-normal-disable-message ()
  "Validate toggling off produces disabled message."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode t)
        (add-hook 'emms-player-finished-hook #'cj/music--consume-track)
        (let ((msg (test-consume--capture-message #'cj/music-toggle-consume)))
          (should (string-match-p "disabled" msg))))
    (test-consume--teardown)))

;;; Normal Cases - cj/music--consume-track

(ert-deftest test-music-config--consume-track-normal-calls-kill-at-selected ()
  "Validate consume-track calls kill-track at the selected marker position."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-consume--setup-playlist-buffer
                   '("/music/track1.mp3" "/music/track2.mp3" "/music/track3.mp3")))
             (cj/music-consume-mode t)
             (kill-called nil)
             (kill-position nil)
             (expected-pos (with-current-buffer buf
                             (marker-position emms-playlist-selected-marker))))
        (cl-letf (((symbol-function 'emms-playlist-mode-kill-track)
                   (lambda ()
                     (setq kill-called t
                           kill-position (point)))))
          (cj/music--consume-track)
          (should kill-called)
          (should (= kill-position expected-pos))))
    (test-consume--teardown)))

(ert-deftest test-music-config--consume-track-normal-calls-kill-at-middle-track ()
  "Validate consume-track navigates to middle track before killing."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-consume--setup-playlist-buffer
                   '("/music/track1.mp3" "/music/track2.mp3" "/music/track3.mp3")))
             (cj/music-consume-mode t)
             (kill-position nil)
             (expected-pos nil))
        ;; Move selected marker to second track
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line 1)
          (set-marker emms-playlist-selected-marker (point))
          (setq expected-pos (marker-position emms-playlist-selected-marker)))
        (cl-letf (((symbol-function 'emms-playlist-mode-kill-track)
                   (lambda () (setq kill-position (point)))))
          (cj/music--consume-track)
          (should (= kill-position expected-pos))))
    (test-consume--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config-toggle-consume-boundary-double-toggle-restores-state ()
  "Validate toggling twice returns to original off state."
  (unwind-protect
      (progn
        (setq cj/music-consume-mode nil)
        (cj/music-toggle-consume)
        (cj/music-toggle-consume)
        (should (eq cj/music-consume-mode nil))
        (should-not (memq #'cj/music--consume-track emms-player-finished-hook)))
    (test-consume--teardown)))

(ert-deftest test-music-config--consume-track-boundary-noop-when-disabled ()
  "Validate consume-track does nothing when consume mode is off."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-consume--setup-playlist-buffer
                   '("/music/track1.mp3" "/music/track2.mp3")))
             (cj/music-consume-mode nil))
        (cj/music--consume-track)
        (should (= 2 (test-consume--track-count buf))))
    (test-consume--teardown)))

(ert-deftest test-music-config--consume-track-boundary-single-track-calls-kill ()
  "Validate consuming the only track in playlist still calls kill-track."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-consume--setup-playlist-buffer
                   '("/music/only-track.mp3")))
             (cj/music-consume-mode t)
             (kill-called nil))
        (cl-letf (((symbol-function 'emms-playlist-mode-kill-track)
                   (lambda () (setq kill-called t))))
          (cj/music--consume-track)
          (should kill-called)))
    (test-consume--teardown)))

;;; Error Cases

(ert-deftest test-music-config--consume-track-error-no-marker-no-crash ()
  "Validate consume-track handles nil selected marker without error."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-consume--setup-playlist-buffer
                   '("/music/track1.mp3")))
             (cj/music-consume-mode t))
        ;; Clear the selected marker
        (with-current-buffer buf
          (setq emms-playlist-selected-marker nil))
        ;; Should not error
        (should-not (condition-case err
                        (progn (cj/music--consume-track) nil)
                      (error err))))
    (test-consume--teardown)))

(ert-deftest test-music-config--consume-track-error-empty-playlist-no-crash ()
  "Validate consume-track handles empty playlist buffer without error."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (get-buffer-create "*EMMS-Test-Playlist*"))
             (cj/music-consume-mode t))
        (with-current-buffer buf
          (emms-playlist-mode)
          (setq emms-playlist-buffer-p t)
          (setq emms-playlist-selected-marker nil))
        ;; Should not error
        (should-not (condition-case err
                        (progn (cj/music--consume-track) nil)
                      (error err))))
    (test-consume--teardown)))

(provide 'test-music-config-toggle-consume)
;;; test-music-config-toggle-consume.el ends here
