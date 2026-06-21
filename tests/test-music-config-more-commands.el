;;; test-music-config-more-commands.el --- More music-config command tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-music-config-commands.el' covers add/fuzzy/clear/
;; next/previous/consume.  This file covers more wrappers:
;;
;;   cj/music-playlist-load
;;   cj/music-playlist-save
;;   cj/music-playlist-edit
;;   cj/music-playlist-toggle
;;   cj/music-playlist-show
;;   cj/music-create-radio-station

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'music-config)

;; Top-level defvars so let-binds reach the dynamic var under lexical
;; scope.
(defvar cj/music-playlist-file nil)
(defvar emms-source-playlist-ask-before-overwrite t)

;;; cj/music-playlist-load

(ert-deftest test-music-playlist-load-plays-selected-playlist ()
  "Normal: load picks an entry, clears the playlist, and plays the file."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-music-m3u-" t)))
         (rock (expand-file-name "rock.m3u" tmp))
         played)
    (with-temp-file rock (insert "/song.mp3\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/music--assert-m3u-files-exist)
                   (lambda () (list (cons "rock" rock))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "rock"))
                  ((symbol-function 'emms-playlist-clear) #'ignore)
                  ((symbol-function 'emms-play-playlist)
                   (lambda (f) (setq played f)))
                  ((symbol-function 'cj/music--sync-playlist-file) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (cj/music-playlist-load))
      (delete-directory tmp t))
    (should (equal played rock))))

(ert-deftest test-music-playlist-load-errors-when-target-missing ()
  "Error: a missing target file signals user-error."
  (cl-letf (((symbol-function 'cj/music--assert-m3u-files-exist)
             (lambda () (list (cons "gone" "/no/such/file.m3u"))))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "gone")))
    (should-error (cj/music-playlist-load) :type 'user-error)))

;;; cj/music-playlist-save

(ert-deftest test-music-playlist-save-writes-fresh-name ()
  "Normal: save with a fresh name writes via emms-playlist-save."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-music-save-" t)))
         (cj/music-m3u-root tmp)
         saved-args msg)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/music--get-m3u-basenames)
                   (lambda () nil))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "fresh"))
                  ((symbol-function 'cj/music--ensure-playlist-buffer)
                   (lambda () (current-buffer)))
                  ((symbol-function 'emms-playlist-save)
                   (lambda (fmt path) (setq saved-args (list fmt path))))
                  ((symbol-function 'cj/music--sync-playlist-file) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (cj/music-playlist-save))
      (delete-directory tmp t))
    (should (equal (car saved-args) 'm3u))
    (should (string-match-p "fresh\\.m3u\\'" (cadr saved-args)))
    (should (string-match-p "Saved playlist" msg))))

;;; cj/music-playlist-edit

(ert-deftest test-music-playlist-edit-opens-file-other-window ()
  "Normal: with an unmodified playlist file present, edit opens it."
  (let* ((tmp (make-temp-file "cj-music-edit-" nil ".m3u"))
         (cj/music-playlist-file tmp)
         opened)
    (with-temp-file tmp (insert "/song.mp3\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/music--assert-valid-playlist-file) #'ignore)
                  ((symbol-function 'cj/music--ensure-playlist-buffer)
                   (lambda () (current-buffer)))
                  ((symbol-function 'cj/music--playlist-modified-p)
                   (lambda () nil))
                  ((symbol-function 'find-file-other-window)
                   (lambda (f &rest _) (setq opened f))))
          (cj/music-playlist-edit))
      (delete-file tmp))
    (should (equal opened tmp))))

;;; cj/music-playlist-toggle

(ert-deftest test-music-playlist-toggle-hides-when-visible ()
  "Normal: with the playlist buffer showing, toggle deletes the window."
  (let* ((cj/music-playlist-buffer-name "*test-cj-music*")
         (buf (get-buffer-create cj/music-playlist-buffer-name))
         deleted msg)
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'delete-window)
                   (lambda (w) (setq deleted w)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (cj/music-playlist-toggle))
      (kill-buffer buf))
    (should (eq deleted 'fake-win))
    (should (string-match-p "closed" msg))))

;;; cj/music-playlist-show

(ert-deftest test-music-playlist-show-switches-to-playlist-buffer ()
  "Normal: show calls switch-to-buffer with the playlist buffer."
  (let* ((cj/music-playlist-buffer-name "*test-cj-music-show*")
         (buf (get-buffer-create cj/music-playlist-buffer-name))
         switched msg)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/emms--setup) #'ignore)
                  ((symbol-function 'cj/music--ensure-playlist-buffer)
                   (lambda () buf))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (b &rest _) (setq switched b)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (cj/music-playlist-show))
      (kill-buffer buf))
    (should (eq switched buf))
    (should msg)))

;;; cj/music-create-radio-station

(ert-deftest test-music-create-radio-station-writes-m3u ()
  "Normal: with name+url, an EXTM3U-style file is written into music-m3u-root."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-music-radio-" t)))
         (cj/music-m3u-root tmp)
         msg)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
            (cj/music-create-radio-station "NPR" "https://example.test/stream"))
          (let ((file (expand-file-name "NPR_Radio.m3u" tmp)))
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (let ((text (buffer-string)))
                (should (string-match-p "#EXTM3U" text))
                (should (string-match-p "NPR" text))
                (should (string-match-p "https://example.test/stream" text))))))
      (delete-directory tmp t))
    (should (string-match-p "Created radio station" msg))))

(ert-deftest test-music-create-radio-station-rejects-empty-name ()
  "Error: an empty name is rejected with user-error."
  (should-error (cj/music-create-radio-station "" "https://x") :type 'user-error))

(ert-deftest test-music-create-radio-station-rejects-empty-url ()
  "Error: an empty URL is rejected with user-error."
  (should-error (cj/music-create-radio-station "NPR" "") :type 'user-error))

(provide 'test-music-config-more-commands)
;;; test-music-config-more-commands.el ends here
