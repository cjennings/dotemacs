;;; test-dirvish-config-wrappers.el --- Tests for dirvish interactive wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the pure helpers (`cj/--ediff-pair-from-files',
;; `cj/--playlist-filter-audio', `cj/--playlist-sanitize-name', and
;; `cj/--wallpaper-program-for').  This file covers the user-facing
;; wrappers that route through them:
;;
;;   cj/dired-ediff-files
;;   cj/dired-create-playlist-from-marked
;;   cj/set-wallpaper

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dirvish-config)

;; The playlist writer reads `music-dir' from user-constants.
(defvar music-dir (file-name-as-directory temporary-file-directory))

;;; cj/dired-ediff-files

(ert-deftest test-dirvish-ediff-files-with-two-marked-calls-ediff ()
  "Normal: with two marked files, `ediff-files' is called on them in
oldest -> newest order (the function does newer-than-p comparison and
puts the older one first)."
  (let* ((older (make-temp-file "cj-ediff-older-"))
         (newer (make-temp-file "cj-ediff-newer-"))
         ediff-args)
    ;; Make sure mtimes differ.
    (set-file-times older '(0 0))
    (unwind-protect
        (cl-letf (((symbol-function 'dired-get-marked-files)
                   (lambda (&rest _) (list older newer)))
                  ((symbol-function 'dired-dwim-target-directory)
                   (lambda () temporary-file-directory))
                  ((symbol-function 'ediff-files)
                   (lambda (a b) (setq ediff-args (list a b))))
                  ((symbol-function 'current-window-configuration)
                   (lambda () nil))
                  ((symbol-function 'add-hook) #'ignore))
          (cj/dired-ediff-files)
          ;; Pair returns (older . newer) so ediff-files sees (older newer).
          (should (equal ediff-args (list older newer))))
      (delete-file older)
      (delete-file newer))))

;;; cj/dired-create-playlist-from-marked

(ert-deftest test-dirvish-create-playlist-writes-m3u-with-tracks ()
  "Normal: with marked audio files and a fresh playlist name, the m3u is
written into `music-dir' with one track per line."
  (let* ((music-dir (file-name-as-directory (make-temp-file "cj-playlist-" t)))
         (tracks '("/songs/a.mp3" "/songs/b.flac" "/songs/cover.png"))
         (playlist-name "myset")
         (expected (expand-file-name (concat playlist-name ".m3u") music-dir)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dired-get-marked-files)
                     (lambda (&rest _) tracks))
                    ((symbol-function 'read-string)
                     (lambda (&rest _) playlist-name))
                    ((symbol-function 'message) #'ignore))
            (cj/dired-create-playlist-from-marked))
          (should (file-exists-p expected))
          (with-temp-buffer
            (insert-file-contents expected)
            (let ((text (buffer-string)))
              (should (string-match-p "/songs/a\\.mp3" text))
              (should (string-match-p "/songs/b\\.flac" text))
              (should-not (string-match-p "cover\\.png" text)))))
      (delete-directory music-dir t))))

(ert-deftest test-dirvish-create-playlist-no-audio-marked-errors ()
  "Error: when no marked files are audio, signal a user-error."
  (cl-letf (((symbol-function 'dired-get-marked-files)
             (lambda (&rest _) '("/notes/readme.txt" "/img/cover.png"))))
    (should-error (cj/dired-create-playlist-from-marked) :type 'user-error)))

(ert-deftest test-dirvish-create-playlist-cancel-on-overwrite ()
  "Boundary: when the chosen name exists and the user picks [c]ancel, the
function signals user-error without writing."
  (let* ((music-dir (file-name-as-directory (make-temp-file "cj-playlist-cancel-" t)))
         (existing-name "duplicate")
         (existing (expand-file-name (concat existing-name ".m3u") music-dir)))
    (with-temp-file existing (insert "old contents\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'dired-get-marked-files)
                   (lambda (&rest _) '("/songs/a.mp3")))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) existing-name))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest _) ?c)))
          (should-error (cj/dired-create-playlist-from-marked) :type 'user-error)
          (with-temp-buffer
            (insert-file-contents existing)
            ;; The file content should be unchanged since we cancelled.
            (should (equal (buffer-string) "old contents\n"))))
      (delete-directory music-dir t))))

;;; cj/set-wallpaper

(ert-deftest test-dirvish-set-wallpaper-unknown-display-server-messages ()
  "Boundary: outside X11 / Wayland, set-wallpaper messages and never
calls the wallpaper-setter binary."
  (let (msg called-process)
    (cl-letf (((symbol-function 'dired-file-name-at-point)
               (lambda () "/some/picture.jpg"))
              ((symbol-function 'env-x11-p) (lambda () nil))
              ((symbol-function 'env-wayland-p) (lambda () nil))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq called-process t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/set-wallpaper))
    (should-not called-process)
    (should (string-match-p "Unknown display server" msg))))

(ert-deftest test-dirvish-set-wallpaper-x11-calls-feh ()
  "Normal: on X11, set-wallpaper hands the file to feh --bg-fill."
  (let (call-process-args msg)
    (cl-letf (((symbol-function 'dired-file-name-at-point)
               (lambda () "/some/picture.jpg"))
              ((symbol-function 'env-x11-p) (lambda () t))
              ((symbol-function 'env-wayland-p) (lambda () nil))
              ((symbol-function 'cj/executable-find-or-warn)
               (lambda (&rest _) "/usr/bin/feh"))
              ((symbol-function 'call-process)
               (lambda (&rest args) (setq call-process-args args)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/set-wallpaper))
    ;; (apply #'call-process path nil 0 nil ("--bg-fill" "/some/picture.jpg"))
    (should call-process-args)
    (should (equal (car call-process-args) "/usr/bin/feh"))
    (should (member "--bg-fill" call-process-args))
    (should (member "/some/picture.jpg" call-process-args))
    (should (string-match-p "Wallpaper set" msg))))

(ert-deftest test-dirvish-set-wallpaper-no-file-errors ()
  "Error: with no file at point, set-wallpaper signals user-error rather
than passing nil to expand-file-name."
  (cl-letf (((symbol-function 'dired-file-name-at-point) (lambda () nil)))
    (should-error (cj/set-wallpaper) :type 'user-error)))

(provide 'test-dirvish-config-wrappers)
;;; test-dirvish-config-wrappers.el ends here
