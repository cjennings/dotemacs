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
;;
;; cj/music-create-radio-station lives in
;; test-music-config-create-radio-station.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

;; Top-level defvars so let-binds reach the dynamic var under lexical
;; scope.
(defvar cj/music-playlist-file nil)

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

(defmacro test-music-save--with-env (&rest body)
  "Run BODY with a fresh playlist buffer and temp save directories.
Binds TMP-M3U and TMP-RADIO (both cleaned up) and captures completing-read's
INITIAL argument in CR-INITIAL."
  `(let* ((cj/music-playlist-buffer-name
           (generate-new-buffer-name "*test-save*"))
          (tmp-m3u (file-name-as-directory (make-temp-file "cj-save-m3u-" t)))
          (tmp-radio (file-name-as-directory (make-temp-file "cj-save-radio-" t)))
          (cj/music-m3u-root tmp-m3u)
          (cj/music-radio-save-dir tmp-radio)
          (cr-initial 'unset))
     (ignore cr-initial)
     (unwind-protect
         (cl-letf (((symbol-function 'message) #'ignore)
                   ((symbol-function 'completing-read)
                    (lambda (_prompt _coll &optional _pred _req initial _hist def &rest _)
                      (setq cr-initial initial)
                      (or initial def "fallback"))))
           ,@body)
       (when (get-buffer cj/music-playlist-buffer-name)
         (kill-buffer cj/music-playlist-buffer-name))
       (delete-directory tmp-m3u t)
       (delete-directory tmp-radio t))))

(defun test-music-save--queue (tracks)
  "Put TRACKS into the (fresh) test playlist buffer."
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (save-excursion
      (goto-char (point-max))
      (dolist (tr tracks)
        (emms-playlist-insert-track tr)))))

(ert-deftest test-music-playlist-save-station-queue-prefills-and-targets-radio-dir ()
  "Normal: an all-stream queue pre-fills the station name and saves into the
radio dir with the station metadata written."
  (test-music-save--with-env
   (let ((tr (emms-track 'url "https://gs.example/stream")))
     (emms-track-set tr 'info-title "Groove Salad")
     (emms-track-set tr 'radio-uuid "uuid-gs")
     (test-music-save--queue (list tr)))
   (cj/music-playlist-save)
   (should (equal cr-initial "Groove Salad"))
   (let ((file (expand-file-name "Groove Salad.m3u" tmp-radio)))
     (should (file-exists-p file))
     (with-temp-buffer
       (insert-file-contents file)
       (let ((text (buffer-string)))
         (should (string-match-p "^#EXTINF:-1,Groove Salad$" text))
         (should (string-match-p "^#RADIOBROWSERUUID:uuid-gs$" text))
         (should (string-match-p "^https://gs\\.example/stream$" text)))))
   (should-not (directory-files tmp-m3u nil "\\.m3u\\'"))))

(ert-deftest test-music-playlist-save-file-queue-targets-m3u-root-no-prefill ()
  "Normal: a file queue saves into the music root with no station pre-fill."
  (test-music-save--with-env
   (test-music-save--queue (list (emms-track 'file "/music/a.flac")))
   (cj/music-playlist-save)
   (should-not cr-initial)
   (should (= (length (directory-files tmp-m3u nil "\\.m3u\\'")) 1))
   (should-not (directory-files tmp-radio nil "\\.m3u\\'"))))

(ert-deftest test-music-playlist-save-associated-file-name-wins-over-station ()
  "Normal: a queue with an associated playlist file defaults to that name,
even when it contains stations."
  (test-music-save--with-env
   (let ((tr (emms-track 'url "https://gs.example/stream")))
     (emms-track-set tr 'info-title "Groove Salad")
     (test-music-save--queue (list tr)))
   (with-current-buffer (cj/music--ensure-playlist-buffer)
     (setq cj/music-playlist-file (expand-file-name "morning.m3u" tmp-radio)))
   (cj/music-playlist-save)
   (should-not cr-initial)
   (should (file-exists-p (expand-file-name "morning.m3u" tmp-radio)))))

(ert-deftest test-music-playlist-save-error-empty-name ()
  "Error: an empty name at the prompt signals user-error, not a hidden .m3u."
  (test-music-save--with-env
   (test-music-save--queue (list (emms-track 'url "https://x.example/s")))
   (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
     (should-error (cj/music-playlist-save) :type 'user-error))
   (should-not (directory-files tmp-radio nil "m3u"))))

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

(provide 'test-music-config-more-commands)
;;; test-music-config-more-commands.el ends here
