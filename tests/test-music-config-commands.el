;;; test-music-config-commands.el --- Tests for music-config interactive commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the pure helpers (valid-file-p, valid-directory-p,
;; collect-entries-recursive, completion-table, safe-filename, etc).
;; This file covers the user-facing wrappers that route through them:
;;
;;   cj/music-add-directory-recursive
;;   cj/music-fuzzy-select-and-add
;;   cj/music-playlist-clear
;;   cj/music-next
;;   cj/music-previous
;;   cj/music-toggle-consume

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'music-config)

;; Top-level defvars so let-binds reach the dynamic binding under
;; lexical scope.
(defvar emms-random-playlist nil)
(defvar cj/music--random-history nil)
(defvar cj/music-consume-mode nil)
(defvar cj/music-playlist-file nil)

;;; cj/music-add-directory-recursive

(ert-deftest test-music-add-directory-recursive-passes-dir-to-emms ()
  "Normal: add-directory-recursive routes through emms-add-directory-tree."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-music-add-" t)))
         called)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/music--ensure-playlist-buffer) #'ignore)
                  ((symbol-function 'emms-add-directory-tree)
                   (lambda (dir) (setq called dir)))
                  ((symbol-function 'message) #'ignore))
          (cj/music-add-directory-recursive tmp))
      (delete-directory tmp t))
    (should (equal called tmp))))

(ert-deftest test-music-add-directory-recursive-errors-on-non-directory ()
  "Error: passing a regular file (not a directory) signals user-error."
  (let ((tmp (make-temp-file "cj-music-not-dir-")))
    (unwind-protect
        (should-error (cj/music-add-directory-recursive tmp)
                      :type 'user-error)
      (delete-file tmp))))

;;; cj/music-fuzzy-select-and-add

(ert-deftest test-music-fuzzy-select-adds-file-when-not-trailing-slash ()
  "Normal: a non-directory pick routes through emms-add-file."
  (let ((cj/music-root (file-name-as-directory (make-temp-file "cj-music-root-" t)))
        added)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "song.mp3" cj/music-root))
          (cl-letf (((symbol-function 'cj/music--collect-entries-recursive)
                     (lambda (_) '("song.mp3")))
                    ((symbol-function 'cj/music--completion-table)
                     (lambda (xs) xs))
                    ((symbol-function 'cj/music--ensure-playlist-buffer) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "song.mp3"))
                    ((symbol-function 'emms-add-file)
                     (lambda (f) (setq added f)))
                    ((symbol-function 'message) #'ignore))
            (cj/music-fuzzy-select-and-add)))
      (delete-directory cj/music-root t))
    (should (equal added (expand-file-name "song.mp3" cj/music-root)))))

(ert-deftest test-music-fuzzy-select-adds-directory-when-trailing-slash ()
  "Normal: a pick ending in / routes through add-directory-recursive."
  (let ((cj/music-root (file-name-as-directory (make-temp-file "cj-music-root-" t)))
        added-dir)
    (unwind-protect
        (progn
          (make-directory (expand-file-name "albums/jazz" cj/music-root) t)
          (cl-letf (((symbol-function 'cj/music--collect-entries-recursive)
                     (lambda (_) '("albums/jazz/")))
                    ((symbol-function 'cj/music--completion-table)
                     (lambda (xs) xs))
                    ((symbol-function 'cj/music--ensure-playlist-buffer) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "albums/jazz/"))
                    ((symbol-function 'cj/music-add-directory-recursive)
                     (lambda (d) (setq added-dir d)))
                    ((symbol-function 'message) #'ignore))
            (cj/music-fuzzy-select-and-add)))
      (delete-directory cj/music-root t))
    (should (equal (file-name-as-directory added-dir)
                   (file-name-as-directory
                    (expand-file-name "albums/jazz" cj/music-root))))))

;;; cj/music-playlist-clear

(ert-deftest test-music-playlist-clear-calls-emms-stop-and-clear ()
  "Normal: playlist-clear calls emms-stop, emms-playlist-clear, and
nils the active-playlist-file."
  (let ((cj/music-playlist-file "/tmp/old.m3u")
        stopped cleared msg)
    (cl-letf (((symbol-function 'emms-stop)
               (lambda () (setq stopped t)))
              ((symbol-function 'emms-playlist-clear)
               (lambda () (setq cleared t)))
              ((symbol-function 'cj/music--ensure-playlist-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/music-playlist-clear))
    (should stopped)
    (should cleared)
    (should (string-match-p "Playlist cleared" msg))))

;;; cj/music-next

(ert-deftest test-music-next-uses-random-when-active ()
  "Normal: with random mode active, next calls emms-random."
  (let (called)
    (cl-letf (((symbol-function 'emms-random)
               (lambda () (setq called 'random)))
              ((symbol-function 'emms-next)
               (lambda () (setq called 'next))))
      (let ((emms-random-playlist t))
        (cj/music-next)))
    (should (eq called 'random))))

(ert-deftest test-music-next-uses-emms-next-when-sequential ()
  "Normal: without random mode, next calls emms-next."
  (let (called)
    (cl-letf (((symbol-function 'emms-random)
               (lambda () (setq called 'random)))
              ((symbol-function 'emms-next)
               (lambda () (setq called 'next))))
      (let ((emms-random-playlist nil))
        (cj/music-next)))
    (should (eq called 'next))))

;;; cj/music-previous

(ert-deftest test-music-previous-replays-from-random-history ()
  "Normal: with history present in random mode, previous reselects the
last-played track and starts it."
  (let ((emms-random-playlist t)
        (cj/music--random-history '("/songs/last.mp3" "/songs/older.mp3"))
        started)
    (cl-letf (((symbol-function 'cj/music--find-track-in-playlist)
               (lambda (_) 42))
              ((symbol-function 'emms-playlist-select) #'ignore)
              ((symbol-function 'emms-start)
               (lambda () (setq started t)))
              ((symbol-function 'emms-previous)
               (lambda () (setq started 'previous))))
      (cj/music-previous))
    (should (eq started t))))

(ert-deftest test-music-previous-falls-through-to-emms-previous ()
  "Normal: with no history (random off), previous calls emms-previous."
  (let ((emms-random-playlist nil)
        (cj/music--random-history nil)
        called)
    (cl-letf (((symbol-function 'emms-previous)
               (lambda () (setq called t))))
      (cj/music-previous))
    (should called)))

;;; cj/music-toggle-consume

(ert-deftest test-music-toggle-consume-flips-flag-and-hook ()
  "Normal: toggle-consume flips the flag, then toggles back."
  (let ((cj/music-consume-mode nil)
        (added-hooks nil)
        (removed-hooks nil))
    (cl-letf (((symbol-function 'add-hook)
               (lambda (hook _fn &rest _) (push hook added-hooks)))
              ((symbol-function 'remove-hook)
               (lambda (hook _fn &rest _) (push hook removed-hooks)))
              ((symbol-function 'message) #'ignore))
      (cj/music-toggle-consume)
      (should cj/music-consume-mode)
      (should (member 'emms-player-finished-hook added-hooks))
      (cj/music-toggle-consume)
      (should-not cj/music-consume-mode)
      (should (member 'emms-player-finished-hook removed-hooks)))))

(provide 'test-music-config-commands)
;;; test-music-config-commands.el ends here
