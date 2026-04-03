;;; test-music-config-random-navigation.el --- Tests for random-aware next/previous -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music-next, cj/music-previous, and the random
;; history tracking mechanism.
;;
;; When emms-random-playlist is active, next should pick a random track
;; (not sequential), and previous should navigate back through the
;; history of recently played tracks.
;;
;; Test organization:
;; - Normal Cases: next/previous in sequential mode, next/previous in random mode
;; - Boundary Cases: empty history, single history entry, history at max capacity
;; - Error Cases: previous with no history, no tracks in playlist
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

(defun test-randnav--setup-playlist-buffer (track-names)
  "Create an EMMS playlist buffer with TRACK-NAMES and return it.
Each entry in TRACK-NAMES becomes a file track in the playlist."
  (let ((buf (get-buffer-create "*EMMS-Test-Playlist*")))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (name track-names)
          (emms-playlist-insert-track (emms-track 'file name)))))
    buf))

(defun test-randnav--teardown ()
  "Clean up random navigation state and test buffers."
  (setq cj/music--random-history nil)
  (setq emms-random-playlist nil)
  (when-let ((buf (get-buffer "*EMMS-Test-Playlist*")))
    (kill-buffer buf)))

(defun test-randnav--capture-message (fn &rest args)
  "Call FN with ARGS and return the message it produces."
  (let ((captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest msg-args)
                 (setq captured (apply #'format fmt msg-args)))))
      (apply fn args))
    captured))

;;; Normal Cases - cj/music-next

(ert-deftest test-music-config-next-normal-sequential-calls-emms-next ()
  "Validate cj/music-next calls emms-next when random mode is off."
  (unwind-protect
      (let ((emms-random-playlist nil)
            (called nil))
        (cl-letf (((symbol-function 'emms-next)
                   (lambda () (setq called t))))
          (cj/music-next)
          (should called)))
    (test-randnav--teardown)))

(ert-deftest test-music-config-next-normal-random-calls-emms-random ()
  "Validate cj/music-next calls emms-random when random mode is on."
  (unwind-protect
      (let ((emms-random-playlist t)
            (called nil))
        (cl-letf (((symbol-function 'emms-random)
                   (lambda () (setq called t))))
          (cj/music-next)
          (should called)))
    (test-randnav--teardown)))

(ert-deftest test-music-config-next-normal-sequential-does-not-call-random ()
  "Validate cj/music-next does not call emms-random when random mode is off."
  (unwind-protect
      (let ((emms-random-playlist nil)
            (random-called nil))
        (cl-letf (((symbol-function 'emms-next)
                   (lambda () nil))
                  ((symbol-function 'emms-random)
                   (lambda () (setq random-called t))))
          (cj/music-next)
          (should-not random-called)))
    (test-randnav--teardown)))

;;; Normal Cases - cj/music-previous

(ert-deftest test-music-config-previous-normal-sequential-calls-emms-previous ()
  "Validate cj/music-previous calls emms-previous when random mode is off."
  (unwind-protect
      (let ((emms-random-playlist nil)
            (called nil))
        (cl-letf (((symbol-function 'emms-previous)
                   (lambda () (setq called t))))
          (cj/music-previous)
          (should called)))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-normal-random-selects-history-track ()
  "Validate cj/music-previous selects the history track within the existing playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-randnav--setup-playlist-buffer
                   '("/music/track1.mp3" "/music/track2.mp3" "/music/track3.mp3")))
             (emms-random-playlist t)
             (cj/music--random-history (list "/music/track2.mp3" "/music/track1.mp3"))
             (selected-pos nil))
        (cl-letf (((symbol-function 'emms-playlist-select)
                   (lambda (pos) (setq selected-pos pos)))
                  ((symbol-function 'emms-start)
                   (lambda () nil)))
          (cj/music-previous)
          ;; Should have selected a position in the buffer
          (should selected-pos)
          ;; The track at that position should be track2
          (with-current-buffer buf
            (goto-char selected-pos)
            (let ((track (emms-playlist-track-at (point))))
              (should (equal (emms-track-name track) "/music/track2.mp3"))))))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-normal-random-pops-history ()
  "Validate cj/music-previous removes the played track from history."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track2.mp3")))
             (emms-random-playlist t)
             (cj/music--random-history (list "/music/track2.mp3" "/music/track1.mp3")))
        (cl-letf (((symbol-function 'emms-playlist-select)
                   (lambda (_pos) nil))
                  ((symbol-function 'emms-start)
                   (lambda () nil)))
          (cj/music-previous)
          (should (equal cj/music--random-history '("/music/track1.mp3")))))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-normal-sequential-does-not-touch-history ()
  "Validate cj/music-previous in sequential mode ignores history."
  (unwind-protect
      (let ((emms-random-playlist nil)
            (cj/music--random-history (list "/music/track2.mp3" "/music/track1.mp3")))
        (cl-letf (((symbol-function 'emms-previous)
                   (lambda () nil)))
          (cj/music-previous)
          (should (equal cj/music--random-history
                         '("/music/track2.mp3" "/music/track1.mp3")))))
    (test-randnav--teardown)))

;;; Normal Cases - cj/music--record-random-history

(ert-deftest test-music-config--record-random-history-normal-pushes-track ()
  "Validate recording history pushes current track name onto the list."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history nil))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/track1.mp3"))))
          (cj/music--record-random-history)
          (should (equal cj/music--random-history '("/music/track1.mp3")))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-normal-preserves-order ()
  "Validate history maintains most-recent-first order."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history '("/music/track1.mp3")))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/track2.mp3"))))
          (cj/music--record-random-history)
          (should (equal (car cj/music--random-history) "/music/track2.mp3"))
          (should (equal (cadr cj/music--random-history) "/music/track1.mp3"))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-normal-skips-when-sequential ()
  "Validate recording is skipped when random mode is off."
  (unwind-protect
      (let ((emms-random-playlist nil)
            (cj/music--random-history nil))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/track1.mp3"))))
          (cj/music--record-random-history)
          (should (null cj/music--random-history))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-normal-skips-duplicate ()
  "Validate recording skips if current track is already at head of history."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history '("/music/track1.mp3")))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/track1.mp3"))))
          (cj/music--record-random-history)
          (should (= 1 (length cj/music--random-history)))))
    (test-randnav--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config-previous-boundary-empty-history-falls-back ()
  "Validate cj/music-previous falls back to emms-previous when history is empty."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history nil)
            (fallback-called nil))
        (cl-letf (((symbol-function 'emms-previous)
                   (lambda () (setq fallback-called t))))
          (cj/music-previous)
          (should fallback-called)))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-boundary-empty-history-messages ()
  "Validate cj/music-previous shows message when history is empty in random mode."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history nil))
        (cl-letf (((symbol-function 'emms-previous)
                   (lambda () nil)))
          (let ((msg (test-randnav--capture-message #'cj/music-previous)))
            (should (stringp msg))
            (should (string-match-p "history" msg)))))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-boundary-single-entry-empties-history ()
  "Validate popping the last history entry leaves history empty."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track2.mp3")))
             (emms-random-playlist t)
             (cj/music--random-history (list "/music/track1.mp3")))
        (cl-letf (((symbol-function 'emms-playlist-select)
                   (lambda (_pos) nil))
                  ((symbol-function 'emms-start)
                   (lambda () nil)))
          (cj/music-previous)
          (should (null cj/music--random-history))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-boundary-caps-at-max ()
  "Validate history never exceeds cj/music--random-history-max entries."
  (unwind-protect
      (let* ((emms-random-playlist t)
             (cj/music--random-history-max 5)
             (cj/music--random-history (number-sequence 1 5)))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/new.mp3"))))
          (cj/music--record-random-history)
          (should (= (length cj/music--random-history) 5))
          (should (equal (car cj/music--random-history) "/music/new.mp3"))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-boundary-drops-oldest ()
  "Validate that when history is full, the oldest entry is dropped."
  (unwind-protect
      (let* ((emms-random-playlist t)
             (cj/music--random-history-max 3)
             (cj/music--random-history '("/music/c.mp3" "/music/b.mp3" "/music/a.mp3")))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () (emms-track 'file "/music/d.mp3"))))
          (cj/music--record-random-history)
          (should (= (length cj/music--random-history) 3))
          (should (equal (car cj/music--random-history) "/music/d.mp3"))
          (should-not (member "/music/a.mp3" cj/music--random-history))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--record-random-history-boundary-no-track-no-crash ()
  "Validate recording handles nil current track without error."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history nil))
        (cl-letf (((symbol-function 'emms-playlist-current-selected-track)
                   (lambda () nil)))
          (cj/music--record-random-history)
          (should (null cj/music--random-history))))
    (test-randnav--teardown)))

;;; Error Cases

(ert-deftest test-music-config-previous-error-random-no-history-no-crash ()
  "Validate cj/music-previous does not crash when random is on but history is nil."
  (unwind-protect
      (let ((emms-random-playlist t)
            (cj/music--random-history nil))
        (cl-letf (((symbol-function 'emms-previous)
                   (lambda () nil)))
          (should-not (condition-case err
                          (progn (cj/music-previous) nil)
                        (error err)))))
    (test-randnav--teardown)))

(ert-deftest test-music-config-previous-error-track-not-in-playlist-messages ()
  "Validate cj/music-previous shows message when history track is no longer in playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track3.mp3")))
             (emms-random-playlist t)
             (cj/music--random-history (list "/music/gone.mp3")))
        (let ((msg (test-randnav--capture-message #'cj/music-previous)))
          (should (stringp msg))
          (should (string-match-p "no longer" msg))))
    (test-randnav--teardown)))

;;; Normal Cases - cj/music--find-track-in-playlist

(ert-deftest test-music-config--find-track-in-playlist-normal-returns-position ()
  "Validate finding a track returns its buffer position."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (buf (test-randnav--setup-playlist-buffer
                   '("/music/track1.mp3" "/music/track2.mp3" "/music/track3.mp3"))))
        (let ((pos (cj/music--find-track-in-playlist "/music/track2.mp3")))
          (should pos)
          (with-current-buffer buf
            (goto-char pos)
            (should (equal (emms-track-name (emms-playlist-track-at (point)))
                           "/music/track2.mp3")))))
    (test-randnav--teardown)))

(ert-deftest test-music-config--find-track-in-playlist-normal-finds-first-track ()
  "Validate finding the first track in the playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track2.mp3"))))
        (let ((pos (cj/music--find-track-in-playlist "/music/track1.mp3")))
          (should pos)))
    (test-randnav--teardown)))

(ert-deftest test-music-config--find-track-in-playlist-normal-finds-last-track ()
  "Validate finding the last track in the playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track2.mp3" "/music/track3.mp3"))))
        (let ((pos (cj/music--find-track-in-playlist "/music/track3.mp3")))
          (should pos)))
    (test-randnav--teardown)))

;;; Boundary Cases - cj/music--find-track-in-playlist

(ert-deftest test-music-config--find-track-in-playlist-boundary-not-found-returns-nil ()
  "Validate returning nil when track is not in playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer
                    '("/music/track1.mp3" "/music/track2.mp3"))))
        (should-not (cj/music--find-track-in-playlist "/music/gone.mp3")))
    (test-randnav--teardown)))

(ert-deftest test-music-config--find-track-in-playlist-boundary-empty-playlist ()
  "Validate returning nil for an empty playlist."
  (unwind-protect
      (let* ((cj/music-playlist-buffer-name "*EMMS-Test-Playlist*")
             (_buf (test-randnav--setup-playlist-buffer '())))
        (should-not (cj/music--find-track-in-playlist "/music/track1.mp3")))
    (test-randnav--teardown)))

(provide 'test-music-config-random-navigation)
;;; test-music-config-random-navigation.el ends here
