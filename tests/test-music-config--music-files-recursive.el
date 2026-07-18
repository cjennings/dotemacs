;;; test-music-config--music-files-recursive.el --- Tests for filtered directory collection -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Directory adds used to hand the whole tree to emms-add-directory-tree,
;; which adds every file it finds -- cover.jpg and friends ended up as
;; playlist rows.  The filtered walk returns only files passing
;; cj/music--valid-file-p, skipping hidden dirs/files, sorted.  Real temp-dir
;; fixtures; the EMMS boundary is mocked only in the command-level test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'music-config)

(defmacro test-music-files--with-fixture (var &rest body)
  "Run BODY with VAR bound to a temp music-directory fixture."
  (declare (indent 1))
  `(let ((,var (make-temp-file "music-test-" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "album" ,var))
           (make-directory (expand-file-name ".hidden" ,var))
           (dolist (f '("song.mp3" "cover.jpg" "album/track.flac"
                        "album/folder.png" "album/notes.txt"
                        ".hidden/secret.mp3" ".stray.ogg"))
             (write-region "" nil (expand-file-name f ,var)))
           ,@body)
       (delete-directory ,var t))))

;;; Normal Cases

(ert-deftest test-music-files-recursive-music-only ()
  "Normal: only files with accepted music extensions come back, sorted;
cover art, text files, and hidden entries stay out."
  (test-music-files--with-fixture root
    (should (equal (mapcar (lambda (f) (file-relative-name f root))
                           (cj/music--music-files-recursive root))
                   '("album/track.flac" "song.mp3")))))

(ert-deftest test-music-add-directory-recursive-adds-only-music ()
  "Normal: the directory-add command feeds only music files to EMMS."
  (test-music-files--with-fixture root
    (let (added)
      (cl-letf (((symbol-function 'cj/music--ensure-playlist-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'emms-add-file)
                 (lambda (f) (push f added))))
        (cj/music-add-directory-recursive root))
      (should (equal (mapcar (lambda (f) (file-relative-name f root))
                             (nreverse added))
                     '("album/track.flac" "song.mp3"))))))

;;; Boundary Cases

(ert-deftest test-music-files-recursive-case-insensitive-extensions ()
  "Boundary: extensions match case-insensitively (Song.MP3 counts)."
  (let ((root (make-temp-file "music-test-case-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "Song.MP3" root))
          (should (= 1 (length (cj/music--music-files-recursive root)))))
      (delete-directory root t))))

(ert-deftest test-music-files-recursive-empty-directory ()
  "Boundary: a directory with no music files returns nil."
  (let ((root (make-temp-file "music-test-empty-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "readme.txt" root))
          (should-not (cj/music--music-files-recursive root)))
      (delete-directory root t))))

;;; Error Cases

(ert-deftest test-music-add-directory-recursive-not-a-directory-errors ()
  "Error: a non-directory argument signals user-error."
  (should-error (cj/music-add-directory-recursive "/nonexistent/nowhere")
                :type 'user-error))

(provide 'test-music-config--music-files-recursive)
;;; test-music-config--music-files-recursive.el ends here
