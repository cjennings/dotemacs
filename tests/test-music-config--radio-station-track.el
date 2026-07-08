;;; test-music-config--radio-station-track.el --- station->track + enqueue tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The queue-first radio model: a picked station becomes an EMMS url track
;; carrying its metadata as track properties (info-title, radio-uuid,
;; radio-favicon) instead of being written to an .m3u.  Covers the pure
;; station->track builder and the enqueue-and-play buffer mechanics (with
;; playback mocked at the EMMS boundary).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(declare-function cj/music-radio--station-track "music-config" (st))
(declare-function cj/music-radio--enqueue-and-play "music-config" (tracks))

;;; --------------------------- station-track -----------------------------------

(ert-deftest test-music-radio-station-track-normal ()
  "Normal: a full station plist yields a url track with all three properties."
  (let ((track (cj/music-radio--station-track
                '(:name "Adroit Jazz Underground"
                  :url_resolved "https://icecast.walmradio.com:8443/jazz"
                  :stationuuid "ea8059be-d119"
                  :favicon "https://walmradio.com/icon.png"))))
    (should (eq (emms-track-type track) 'url))
    (should (equal (emms-track-name track) "https://icecast.walmradio.com:8443/jazz"))
    (should (equal (emms-track-get track 'info-title) "Adroit Jazz Underground"))
    (should (equal (emms-track-get track 'radio-uuid) "ea8059be-d119"))
    (should (equal (emms-track-get track 'radio-favicon) "https://walmradio.com/icon.png"))))

(ert-deftest test-music-radio-station-track-no-url-is-nil ()
  "Error: a station with no stream URL yields nil, not a broken track."
  (should-not (cj/music-radio--station-track '(:name "No URL" :url_resolved "" :url ""))))

(ert-deftest test-music-radio-station-track-boundary-no-name ()
  "Boundary: a nameless station falls back to \"Radio\" for its title."
  (let ((track (cj/music-radio--station-track '(:url "https://s.example/live"))))
    (should (equal (emms-track-get track 'info-title) "Radio"))))

(ert-deftest test-music-radio-station-track-boundary-newline-name-stripped ()
  "Boundary: newlines in an external station name are flattened to spaces."
  (let ((track (cj/music-radio--station-track
                '(:name "Line\nBreak" :url "https://s.example/live"))))
    (should (equal (emms-track-get track 'info-title) "Line Break"))))

(ert-deftest test-music-radio-station-track-boundary-empty-uuid-favicon-absent ()
  "Boundary: empty-string uuid/favicon are treated as absent, not stored."
  (let ((track (cj/music-radio--station-track
                '(:name "X" :url "https://s.example/live" :stationuuid "" :favicon ""))))
    (should-not (emms-track-get track 'radio-uuid))
    (should-not (emms-track-get track 'radio-favicon))))

;;; ------------------------- enqueue-and-play ----------------------------------

(defmacro test-music-radio--with-playlist (&rest body)
  "Run BODY with a fresh, uniquely named playlist buffer and playback mocked."
  `(let* ((cj/music-playlist-buffer-name
           (generate-new-buffer-name "*test-radio-enqueue*"))
          (emms-player-playing-p nil)
          (started 0) (stopped 0))
     (unwind-protect
         (cl-letf (((symbol-function 'emms-start)
                    (lambda () (setq started (1+ started))))
                   ((symbol-function 'emms-stop)
                    (lambda () (setq stopped (1+ stopped)))))
           ,@body)
       (when (get-buffer cj/music-playlist-buffer-name)
         (kill-buffer cj/music-playlist-buffer-name)))))

(ert-deftest test-music-radio-enqueue-and-play-normal-appends-and-plays ()
  "Normal: tracks land in the playlist buffer in order; the first is selected
and playback starts."
  (test-music-radio--with-playlist
   (let ((t1 (cj/music-radio--station-track '(:name "One" :url "https://one.example/a")))
         (t2 (cj/music-radio--station-track '(:name "Two" :url "https://two.example/b"))))
     (cj/music-radio--enqueue-and-play (list t1 t2))
     (with-current-buffer cj/music-playlist-buffer-name
       (let ((names '()))
         (save-excursion
           (goto-char (point-min))
           (while (not (eobp))
             (when-let ((tr (emms-playlist-track-at (point))))
               (push (emms-track-name tr) names))
             (forward-line 1)))
         (should (equal (nreverse names)
                        '("https://one.example/a" "https://two.example/b"))))
       (should (equal (emms-track-name (emms-playlist-selected-track))
                      "https://one.example/a")))
     (should (= started 1)))))

(ert-deftest test-music-radio-enqueue-and-play-boundary-appends-after-existing ()
  "Boundary: an existing queue is kept; new tracks append and the first NEW
track is the one selected."
  (test-music-radio--with-playlist
   (let ((old (cj/music-radio--station-track '(:name "Old" :url "https://old.example/x")))
         (new (cj/music-radio--station-track '(:name "New" :url "https://new.example/y"))))
     (cj/music-radio--enqueue-and-play (list old))
     (cj/music-radio--enqueue-and-play (list new))
     (with-current-buffer cj/music-playlist-buffer-name
       (should (equal (emms-track-name (emms-playlist-selected-track))
                      "https://new.example/y"))))))

(ert-deftest test-music-radio-enqueue-and-play-interrupts-current-playback ()
  "Normal: when something is playing, enqueue stops it before starting."
  (test-music-radio--with-playlist
   (let ((emms-player-playing-p t)
         (tr (cj/music-radio--station-track '(:name "Z" :url "https://z.example/s"))))
     (cj/music-radio--enqueue-and-play (list tr))
     (should (= stopped 1))
     (should (= started 1)))))

(ert-deftest test-music-radio-enqueue-and-play-boundary-nil-is-no-op ()
  "Boundary: an empty track list does nothing — no buffer churn, no playback."
  (test-music-radio--with-playlist
   (cj/music-radio--enqueue-and-play nil)
   (should (= started 0))
   (should (= stopped 0))))

(provide 'test-music-config--radio-station-track)
;;; test-music-config--radio-station-track.el ends here
