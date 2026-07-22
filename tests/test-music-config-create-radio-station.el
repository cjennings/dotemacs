;;; test-music-config-create-radio-station.el --- Tests for manual radio-station entry -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music-create-radio-station under the queue-first model:
;; a hand-entered name + URL becomes a url track in the playlist queue (with
;; the name as its title property) and playback starts.  Nothing is written
;; to disk — saving is the normal playlist-save flow.
;;
;; Test organization:
;; - Normal Cases: track queued with title, playback started, no file written
;; - Boundary Cases: unicode name preserved verbatim
;; - Error Cases: empty name, empty URL

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(defmacro test-music-create-radio--with-env (&rest body)
  "Run BODY with a fresh playlist buffer, playback mocked, messages captured."
  `(let* ((cj/music-playlist-buffer-name
           (generate-new-buffer-name "*test-create-radio*"))
          (emms-player-playing-p nil)
          (started 0) (msg nil))
     (ignore started msg)
     (unwind-protect
         (cl-letf (((symbol-function 'emms-start)
                    (lambda () (setq started (1+ started))))
                   ((symbol-function 'emms-stop) #'ignore)
                   ((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (when fmt (setq msg (apply #'format fmt args))))))
           ,@body)
       (when (get-buffer cj/music-playlist-buffer-name)
         (kill-buffer cj/music-playlist-buffer-name)))))

(defun test-music-create-radio--queued-tracks ()
  "Track objects currently in the test playlist buffer."
  (let ((tracks '()))
    (with-current-buffer cj/music-playlist-buffer-name
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((tr (emms-playlist-track-at (point))))
            (push tr tracks))
          (forward-line 1))))
    (nreverse tracks)))

;;; Normal Cases

(ert-deftest test-music-config-create-radio-station-normal-queues-track ()
  "Normal: name+url queues a url track carrying the name, and playback starts."
  (test-music-create-radio--with-env
   (cj/music-create-radio-station "Jazz FM" "http://stream.jazzfm.com/radio")
   (let ((tracks (test-music-create-radio--queued-tracks)))
     (should (= (length tracks) 1))
     (should (eq (emms-track-type (car tracks)) 'url))
     (should (equal (emms-track-name (car tracks)) "http://stream.jazzfm.com/radio"))
     (should (equal (emms-track-get (car tracks) 'info-title) "Jazz FM")))
   (should (= started 1))
   (should (string-match-p "Jazz FM" msg))))

(ert-deftest test-music-config-create-radio-station-normal-writes-no-file ()
  "Normal: nothing lands on disk — saving is the playlist-save flow's job."
  (let ((tmp (file-name-as-directory (make-temp-file "cj-radio-nofile-" t))))
    (unwind-protect
        (test-music-create-radio--with-env
         (let ((cj/music-m3u-root tmp)
               (cj/music-radio-save-dir tmp))
           (cj/music-create-radio-station "NPR" "https://example.test/stream")
           (should-not (directory-files tmp nil "\\.m3u\\'"))))
      (delete-directory tmp t))))

;;; Boundary Cases

(ert-deftest test-music-config-create-radio-station-boundary-unicode-name ()
  "Boundary: a unicode name is kept verbatim on the track (no filename munging)."
  (test-music-create-radio--with-env
   (cj/music-create-radio-station "Café Del Mar ☕" "https://cafe.example/stream")
   (should (equal (emms-track-get (car (test-music-create-radio--queued-tracks))
                                  'info-title)
                  "Café Del Mar ☕"))))

;;; Keymap

(ert-deftest test-music-config-radio-map-prefix-mirrors-playlist-keys ()
  "Normal: C-; m r is a radio prefix whose n/t/m mirror the playlist buffer."
  (let ((map (lookup-key cj/music-map "r")))
    (should (keymapp map))
    (should (eq (lookup-key map "n") 'cj/music-radio-search-by-name))
    (should (eq (lookup-key map "t") 'cj/music-radio-search-by-tag))
    (should (eq (lookup-key map "m") 'cj/music-create-radio-station))))

(ert-deftest test-music-config-menu-map-lowercase-keys ()
  "Normal: the menu's former uppercase keys live on lowercase homes, and the
playlist buffer saves on s (single on 1, old save key v unbound)."
  (should (eq (lookup-key cj/music-map "v") 'cj/music-playlist-show))
  (should (eq (lookup-key cj/music-map "u") 'emms-shuffle))
  (should (eq (lookup-key cj/music-map "l") 'emms-toggle-repeat-playlist))
  (should-not (lookup-key cj/music-map "R"))
  (should-not (lookup-key cj/music-map "M"))
  (should-not (lookup-key cj/music-map "Z"))
  (should (eq (lookup-key emms-playlist-mode-map "s") 'cj/music-playlist-save))
  (should (eq (lookup-key emms-playlist-mode-map "1") 'emms-toggle-repeat-track))
  (should-not (lookup-key emms-playlist-mode-map "v")))

;;; Error Cases

(ert-deftest test-music-config-create-radio-station-error-empty-name-signals-user-error ()
  "Error: empty name signals user-error."
  (should-error (cj/music-create-radio-station "" "https://x") :type 'user-error))

(ert-deftest test-music-config-create-radio-station-error-empty-url-signals-user-error ()
  "Error: empty URL signals user-error."
  (should-error (cj/music-create-radio-station "NPR" "") :type 'user-error))

(provide 'test-music-config-create-radio-station)
;;; test-music-config-create-radio-station.el ends here
