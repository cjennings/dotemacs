;;; test-transcription-video.el --- Tests for video transcription dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the video branch of the transcription pipeline.  Audio
;; files keep flowing through `cj/--start-transcription-process'
;; unchanged (covered by sibling test files).  Video files go through
;; ffmpeg audio extraction first, then into the same transcription
;; pipeline with the extracted file marked for cleanup once
;; transcription completes.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'transcription-config)

;;; cj/--video-file-p

(ert-deftest test-tx-video-file-p-recognizes-common-video-extensions ()
  "Normal: common video extensions are recognized."
  (dolist (path '("clip.mp4" "talk.mkv" "demo.mov" "ad.webm" "old.avi"
                  "screencast.m4v" "promo.mpg"))
    (should (cj/--video-file-p path))))

(ert-deftest test-tx-video-file-p-rejects-audio-and-non-media-extensions ()
  "Boundary: audio and unrelated extensions return nil."
  (dolist (path '("song.mp3" "notes.txt" "image.png" "archive.tar.gz"))
    (should-not (cj/--video-file-p path))))

(ert-deftest test-tx-video-file-p-case-insensitive ()
  "Boundary: uppercase extensions count too."
  (should (cj/--video-file-p "Clip.MP4"))
  (should (cj/--video-file-p "TALK.MKV")))

(ert-deftest test-tx-video-file-p-handles-no-extension ()
  "Boundary: extensionless and nil/empty input returns nil."
  (should-not (cj/--video-file-p "README"))
  (should-not (cj/--video-file-p ""))
  (should-not (cj/--video-file-p nil)))

;;; cj/--media-file-p

(ert-deftest test-tx-media-file-p-accepts-audio ()
  "Normal: audio passes."
  (should (cj/--media-file-p "song.mp3")))

(ert-deftest test-tx-media-file-p-accepts-video ()
  "Normal: video passes."
  (should (cj/--media-file-p "clip.mp4")))

(ert-deftest test-tx-media-file-p-rejects-non-media ()
  "Boundary: text, image, etc. fail."
  (should-not (cj/--media-file-p "notes.txt"))
  (should-not (cj/--media-file-p "image.png")))

;;; cj/--extract-audio-from-video

(ert-deftest test-tx-extract-audio-invokes-ffmpeg-with-expected-args ()
  "Normal: extraction shells ffmpeg with -vn and the chosen MP3 encoder."
  (let* ((video "/clips/demo.mp4")
         (out "/tmp/cj-tx-extract.mp3")
         make-process-kwargs)
    (cl-letf (((symbol-function 'cj/executable-find-or-warn)
               (lambda (&rest _) "/usr/bin/ffmpeg"))
              ((symbol-function 'make-process)
               (lambda (&rest kw) (setq make-process-kwargs kw) 'fake-process)))
      (cj/--extract-audio-from-video video out #'ignore))
    (should make-process-kwargs)
    (let ((cmd (plist-get make-process-kwargs :command)))
      (should (equal (car cmd) "/usr/bin/ffmpeg"))
      (should (member "-vn" cmd))
      (should (member video cmd))
      (should (member out cmd))
      (should (member "libmp3lame" cmd)))))

(ert-deftest test-tx-extract-audio-errors-when-ffmpeg-missing ()
  "Error: ffmpeg not on PATH signals user-error before make-process."
  (cl-letf (((symbol-function 'cj/executable-find-or-warn)
             (lambda (&rest _) nil))
            ((symbol-function 'make-process)
             (lambda (&rest _) (error "make-process must not be called"))))
    (should-error (cj/--extract-audio-from-video "/x.mp4" "/tmp/y.mp3" #'ignore)
                  :type 'user-error)))

;;; cj/transcribe-media dispatcher

(ert-deftest test-tx-transcribe-media-audio-routes-directly ()
  "Normal: audio paths go straight to the transcription worker, no ffmpeg."
  (let* ((tmp (make-temp-file "cj-tx-aud-" nil ".mp3"))
         worker-arg ffmpeg-called)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--start-transcription-process)
                   (lambda (file &rest _) (setq worker-arg file) 'fake-proc))
                  ((symbol-function 'cj/--extract-audio-from-video)
                   (lambda (&rest _) (setq ffmpeg-called t))))
          (cj/transcribe-media tmp))
      (delete-file tmp))
    (should (equal worker-arg tmp))
    (should-not ffmpeg-called)))

(ert-deftest test-tx-transcribe-media-video-extracts-then-transcribes ()
  "Normal: video paths invoke ffmpeg; on success the extracted audio
goes through `cj/--start-transcription-process' with a cleanup hint."
  (let* ((tmp (make-temp-file "cj-tx-vid-" nil ".mp4"))
         extract-args worker-call)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--extract-audio-from-video)
                   (lambda (vid out cb)
                     (setq extract-args (list vid out cb))
                     ;; Simulate immediate ffmpeg success.
                     (funcall cb)))
                  ((symbol-function 'cj/--start-transcription-process)
                   (lambda (file &rest rest)
                     (setq worker-call (cons file rest))
                     'fake-proc)))
          (cj/transcribe-media tmp))
      (delete-file tmp))
    ;; ffmpeg was asked to extract from tmp.
    (should extract-args)
    (should (equal (car extract-args) tmp))
    ;; The temp audio path passed to ffmpeg matches the path passed to
    ;; the worker -- in other words the extraction output IS what the
    ;; worker transcribes.
    (should (equal (nth 1 extract-args) (car worker-call)))
    ;; The worker got the temp-audio as cleanup-file (so it gets
    ;; deleted after transcription completes).
    (should (equal (nth 1 extract-args) (cadr worker-call)))))

(ert-deftest test-tx-transcribe-media-video-output-base-is-the-source ()
  "Regression: a video's transcript derives from the VIDEO path (alongside the
source), not the temp /tmp audio.  The worker gets the video as its output base
\(third arg), so cj/--transcription-output-files lands talk.mp4 -> talk.txt
beside the video instead of in /tmp."
  (let* ((tmp (make-temp-file "cj-tx-vid-" nil ".mp4"))
         worker-call)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--extract-audio-from-video)
                   (lambda (_vid _out cb) (funcall cb)))
                  ((symbol-function 'cj/--start-transcription-process)
                   (lambda (file &rest rest)
                     (setq worker-call (cons file rest))
                     'fake-proc)))
          (cj/transcribe-media tmp))
      (delete-file tmp))
    ;; the output base (third arg) is the source video, not the temp audio
    (should (equal (nth 2 worker-call) tmp))
    ;; so the derived transcript sits beside the video, not in /tmp
    (should (equal (car (cj/--transcription-output-files (nth 2 worker-call)))
                   (concat (file-name-sans-extension tmp) ".txt")))))

(ert-deftest test-tx-transcribe-media-rejects-non-media ()
  "Error: non-media paths get rejected up front."
  (should-error (cj/transcribe-media "/notes/readme.txt") :type 'user-error))

;;; Aliases

(ert-deftest test-tx-old-transcribe-audio-aliases-new-media-command ()
  "Backwards compat: `cj/transcribe-audio' still resolves to the new
media dispatcher via defalias."
  (should (eq (symbol-function 'cj/transcribe-audio) 'cj/transcribe-media)))

(ert-deftest test-tx-old-at-point-aliases-new-media-at-point ()
  "Backwards compat: `cj/transcribe-audio-at-point' still resolves."
  (should (eq (symbol-function 'cj/transcribe-audio-at-point)
              'cj/transcribe-media-at-point)))

;;; Keybinding

(ert-deftest test-tx-dired-T-binds-media-at-point ()
  "Normal: T in dired-mode-map invokes `cj/transcribe-media-at-point'."
  (require 'dired)
  (should (eq (lookup-key dired-mode-map (kbd "T"))
              #'cj/transcribe-media-at-point)))

(provide 'test-transcription-video)
;;; test-transcription-video.el ends here
