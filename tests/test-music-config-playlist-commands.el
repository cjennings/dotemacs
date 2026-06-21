;;; test-music-config-playlist-commands.el --- Tests for the music-config playlist commands -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Sibling tests cover the pure helpers and a first batch of small
;; commands.  This file covers the remaining interactive playlist
;; commands plus the random-aware navigation pair:
;;
;;   cj/music-playlist-load
;;   cj/music-playlist-reload
;;   cj/music-playlist-edit
;;   cj/music-next
;;   cj/music-previous
;;   cj/music--consume-track
;;
;; EMMS primitives (`emms-playlist-clear', `emms-play-playlist',
;; `emms-stop', `emms-random', `emms-next', `emms-previous',
;; `emms-start', `emms-playlist-select') are stubbed throughout.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'testutil-general)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir
    (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'emms-playlist-mode)
(require 'music-config)

(defun test-mc-pc--setup ()
  "Per-test setup."
  (cj/create-test-base-dir)
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t)
      (setq cj/music-playlist-file nil))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-mc-pc--teardown ()
  "Per-test cleanup."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf))
  (cj/delete-test-base-dir))

;;; cj/music-playlist-load

(ert-deftest test-mc-playlist-load-normal-loads-selected-file ()
  "Normal: user picks an existing playlist; load wires it via emms-play-playlist."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((tmp (make-temp-file "test-mc-pl-" nil ".m3u"))
            (played nil))
        (with-temp-file tmp (insert "track.mp3\n"))
        (cl-letf (((symbol-function 'cj/music--get-m3u-files)
                   (lambda () (list (cons "test.m3u" tmp))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "test.m3u"))
                  ((symbol-function 'emms-playlist-clear) #'ignore)
                  ((symbol-function 'emms-play-playlist)
                   (lambda (f) (setq played f)))
                  ((symbol-function 'message) #'ignore))
          (cj/music-playlist-load))
        (should (equal played tmp))
        (delete-file tmp))
    (test-mc-pc--teardown)))

(ert-deftest test-mc-playlist-load-error-on-missing-file ()
  "Error: user picks a name whose file isn't on disk -> user-error."
  (test-mc-pc--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/music--get-m3u-files)
                 (lambda () '(("ghost.m3u" . "/nope/ghost.m3u"))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "ghost.m3u"))
                ((symbol-function 'emms-playlist-clear) #'ignore)
                ((symbol-function 'emms-play-playlist) #'ignore))
        (should-error (cj/music-playlist-load) :type 'user-error))
    (test-mc-pc--teardown)))

;;; cj/music-playlist-reload

(ert-deftest test-mc-playlist-reload-replays-current-file ()
  "Normal: with a valid playlist file the function plays it again."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((tmp (make-temp-file "test-mc-reload-" nil ".m3u"))
            (played nil))
        (with-temp-file tmp (insert "track.mp3\n"))
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file tmp))
        (cl-letf (((symbol-function 'emms-playlist-clear) #'ignore)
                  ((symbol-function 'emms-play-playlist)
                   (lambda (f) (setq played f)))
                  ((symbol-function 'message) #'ignore))
          (cj/music-playlist-reload))
        (should (equal played tmp))
        (delete-file tmp))
    (test-mc-pc--teardown)))

(ert-deftest test-mc-playlist-reload-error-no-associated-file ()
  "Error: no associated playlist file -> assert step errors with user-error."
  (test-mc-pc--setup)
  (unwind-protect
      (with-current-buffer cj/music-playlist-buffer-name
        (setq cj/music-playlist-file nil)
        (should-error (cj/music-playlist-reload) :type 'user-error))
    (test-mc-pc--teardown)))

;;; cj/music-playlist-edit

(ert-deftest test-mc-playlist-edit-opens-other-window-when-clean ()
  "Normal: with a clean playlist, edit opens the file in another window."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((tmp (make-temp-file "test-mc-edit-" nil ".m3u"))
            (opened nil))
        (with-temp-file tmp (insert "track.mp3\n"))
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file tmp))
        (cl-letf (((symbol-function 'cj/music--playlist-modified-p)
                   (lambda () nil))
                  ((symbol-function 'find-file-other-window)
                   (lambda (p &rest _) (setq opened p))))
          (cj/music-playlist-edit))
        (should (equal opened tmp))
        (delete-file tmp))
    (test-mc-pc--teardown)))

;;; cj/music-next / cj/music-previous

(ert-deftest test-mc-next-uses-emms-next-when-not-random ()
  "Normal: with random off, next delegates to emms-next."
  (let ((emms-random-playlist nil)
        (called nil))
    (cl-letf (((symbol-function 'emms-next)
               (lambda () (setq called 'next)))
              ((symbol-function 'emms-random)
               (lambda () (setq called 'random))))
      (cj/music-next))
    (should (eq called 'next))))

(ert-deftest test-mc-next-uses-emms-random-when-random-on ()
  "Normal: with random on, next picks a random track."
  (let ((emms-random-playlist t)
        (called nil))
    (cl-letf (((symbol-function 'emms-next)
               (lambda () (setq called 'next)))
              ((symbol-function 'emms-random)
               (lambda () (setq called 'random))))
      (cj/music-next))
    (should (eq called 'random))))

(ert-deftest test-mc-previous-uses-emms-previous-when-not-random ()
  "Normal: with random off, previous delegates to emms-previous."
  (let ((emms-random-playlist nil)
        (called nil))
    (cl-letf (((symbol-function 'emms-previous)
               (lambda () (setq called 'prev))))
      (cj/music-previous))
    (should (eq called 'prev))))

(ert-deftest test-mc-previous-empty-random-history-messages-and-delegates ()
  "Boundary: random on but no history -> message + delegate to emms-previous."
  (let ((emms-random-playlist t)
        (cj/music--random-history nil)
        (called nil)
        (msg nil))
    (cl-letf (((symbol-function 'emms-previous)
               (lambda () (setq called 'prev)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/music-previous))
    (should (eq called 'prev))
    (should (string-match-p "No random history" msg))))

(ert-deftest test-mc-previous-random-history-jumps-to-popped-track ()
  "Normal: random with history -> pop name, find pos, emms-playlist-select + start."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history '("/a/track-2.mp3" "/a/track-1.mp3"))
            (selected nil)
            (started nil))
        (cl-letf (((symbol-function 'cj/music--find-track-in-playlist)
                   (lambda (name) (when (equal name "/a/track-2.mp3") 42)))
                  ((symbol-function 'emms-playlist-select)
                   (lambda (pos) (setq selected pos)))
                  ((symbol-function 'emms-start)
                   (lambda () (setq started t))))
          (cj/music-previous))
        (should (= selected 42))
        (should started)
        ;; Top of history was consumed.
        (should (equal cj/music--random-history '("/a/track-1.mp3"))))
    (test-mc-pc--teardown)))

(ert-deftest test-mc-previous-random-popped-track-missing-messages ()
  "Boundary: history pop returns a track no longer in the playlist -> message."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history '("/a/gone.mp3"))
            (started nil)
            (msg nil))
        (cl-letf (((symbol-function 'cj/music--find-track-in-playlist)
                   (lambda (_) nil))
                  ((symbol-function 'emms-start)
                   (lambda () (setq started t)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (cj/music-previous))
        (should-not started)
        (should (string-match-p "no longer in playlist" msg)))
    (test-mc-pc--teardown)))

;;; cj/music--consume-track

(ert-deftest test-mc-consume-track-no-op-when-mode-off ()
  "Boundary: with consume-mode off, the function doesn't touch the playlist."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((cj/music-consume-mode nil)
            (killed nil))
        (cl-letf (((symbol-function 'emms-playlist-mode-kill-track)
                   (lambda () (setq killed t))))
          (cj/music--consume-track))
        (should-not killed))
    (test-mc-pc--teardown)))

(ert-deftest test-mc-consume-track-kills-selected-when-mode-on ()
  "Normal: with consume on + a live selected marker, kill-track is called."
  (test-mc-pc--setup)
  (unwind-protect
      (let ((cj/music-consume-mode t)
            (killed nil))
        (with-current-buffer cj/music-playlist-buffer-name
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "track\n"))
          (setq emms-playlist-selected-marker (point-min-marker)))
        (cl-letf (((symbol-function 'emms-playlist-mode-kill-track)
                   (lambda () (setq killed t))))
          (cj/music--consume-track))
        (should killed))
    (test-mc-pc--teardown)))

(provide 'test-music-config-playlist-commands)
;;; test-music-config-playlist-commands.el ends here
