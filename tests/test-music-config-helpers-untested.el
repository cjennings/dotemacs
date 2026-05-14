;;; test-music-config-helpers-untested.el --- Tests for previously-uncovered music-config helpers -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The sibling test files cover the bulk of music-config's pure
;; helpers.  This file fills the remaining gap: assertion guards,
;; the M3U-file picker, the playlist-file setter, the EMMS lazy
;; setup, and the simpler interactive commands (`playlist-clear',
;; `add-directory-recursive', `find-track-in-playlist').  EMMS
;; primitives are stubbed so the tests don't touch the player.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'testutil-general)

;; Stub the project-local keymap before music-config loads.
(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

;; Pull EMMS in from elpa so music-config's `require' calls succeed.
(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir
    (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'emms-playlist-mode)
(require 'music-config)

(defun test-mc-untested--setup ()
  "Per-test setup: clean state + temp dir for any FS fixtures."
  (cj/create-test-base-dir)
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t)
      (setq cj/music-playlist-file nil))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-mc-untested--teardown ()
  "Per-test cleanup."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf))
  (cj/delete-test-base-dir))

;;; ---------- cj/music--assert-m3u-files-exist ----------

(ert-deftest test-mc-assert-m3u-files-normal-returns-files ()
  "Normal: when get-m3u-files returns a non-empty list, return it."
  (cl-letf (((symbol-function 'cj/music--get-m3u-files)
             (lambda () '(("rock.m3u" . "/tmp/rock.m3u")
                          ("jazz.m3u" . "/tmp/jazz.m3u")))))
    (should (equal (cj/music--assert-m3u-files-exist)
                   '(("rock.m3u" . "/tmp/rock.m3u")
                     ("jazz.m3u" . "/tmp/jazz.m3u"))))))

(ert-deftest test-mc-assert-m3u-files-error-empty-signals-user-error ()
  "Error: an empty M3U file list signals user-error."
  (cl-letf (((symbol-function 'cj/music--get-m3u-files) (lambda () nil)))
    (should-error (cj/music--assert-m3u-files-exist) :type 'user-error)))

;;; ---------- cj/music--sync-playlist-file ----------

(ert-deftest test-mc-sync-playlist-file-sets-buffer-local ()
  "Normal: sync sets the buffer-local playlist file and resets point to bob.

The EMMS playlist buffer is read-only via `emms-playlist-mode', so the
test prelude inserts filler with `inhibit-read-only' bound."
  (test-mc-untested--setup)
  (unwind-protect
      (with-current-buffer cj/music-playlist-buffer-name
        (let ((inhibit-read-only t))
          (insert "filler\n"))
        (goto-char (point-max))
        (cj/music--sync-playlist-file "/tmp/whatever.m3u")
        (should (equal cj/music-playlist-file "/tmp/whatever.m3u"))
        (should (= (point) (point-min))))
    (test-mc-untested--teardown)))

;;; ---------- cj/music--select-m3u-file ----------

(ert-deftest test-mc-select-m3u-file-normal-returns-chosen-path ()
  "Normal: pick a file name -> returns its full path."
  (cl-letf (((symbol-function 'cj/music--get-m3u-files)
             (lambda () '(("rock.m3u" . "/tmp/rock.m3u")
                          ("jazz.m3u" . "/tmp/jazz.m3u"))))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "jazz.m3u")))
    (should (equal (cj/music--select-m3u-file "Pick: ")
                   "/tmp/jazz.m3u"))))

(ert-deftest test-mc-select-m3u-file-boundary-cancel-returns-nil ()
  "Boundary: picking \"(Cancel)\" returns nil."
  (cl-letf (((symbol-function 'cj/music--get-m3u-files)
             (lambda () '(("rock.m3u" . "/tmp/rock.m3u"))))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "(Cancel)")))
    (should (null (cj/music--select-m3u-file "Pick: ")))))

(ert-deftest test-mc-select-m3u-file-error-no-files-signals-user-error ()
  "Error: no M3U files available -> the underlying assertion errors."
  (cl-letf (((symbol-function 'cj/music--get-m3u-files) (lambda () nil)))
    (should-error (cj/music--select-m3u-file "Pick: ") :type 'user-error)))

;;; ---------- cj/emms--setup ----------

(ert-deftest test-mc-emms-setup-noop-when-already-loaded ()
  "Normal: when emms is already a feature, setup does not re-require."
  (let ((called nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym) (eq sym 'emms)))
              ((symbol-function 'require)
               (lambda (&rest _) (setq called t) t)))
      (cj/emms--setup))
    (should-not called)))

(ert-deftest test-mc-emms-setup-requires-emms-when-absent ()
  "Boundary: when emms isn't yet loaded, setup requires it."
  (let ((required nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym) (not (eq sym 'emms))))
              ((symbol-function 'require)
               (lambda (feat &rest _) (setq required feat) t)))
      (cj/emms--setup))
    (should (eq required 'emms))))

;;; ---------- cj/music-playlist-clear ----------

(ert-deftest test-mc-playlist-clear-resets-state-and-stops-playback ()
  "Normal: clear stops emms, empties the playlist, nils the file reference."
  (test-mc-untested--setup)
  (unwind-protect
      (let ((stop-called nil)
            (clear-called nil))
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file "/tmp/in-flight.m3u"))
        (cl-letf (((symbol-function 'emms-stop)
                   (lambda () (setq stop-called t)))
                  ((symbol-function 'emms-playlist-clear)
                   (lambda () (setq clear-called t)))
                  ((symbol-function 'message) #'ignore))
          (cj/music-playlist-clear))
        (should stop-called)
        (should clear-called)
        (with-current-buffer cj/music-playlist-buffer-name
          (should (null cj/music-playlist-file))))
    (test-mc-untested--teardown)))

;;; ---------- cj/music-add-directory-recursive ----------

(ert-deftest test-mc-add-directory-recursive-normal-calls-emms ()
  "Normal: with an existing directory, the recursive add reaches emms."
  (test-mc-untested--setup)
  (unwind-protect
      (let* ((dir cj/test-base-dir)
             (called-with nil))
        (cl-letf (((symbol-function 'emms-add-directory-tree)
                   (lambda (d) (setq called-with d)))
                  ((symbol-function 'message) #'ignore))
          (cj/music-add-directory-recursive dir))
        (should (equal (file-name-as-directory called-with)
                       (file-name-as-directory dir))))
    (test-mc-untested--teardown)))

(ert-deftest test-mc-add-directory-recursive-error-not-a-directory ()
  "Error: a non-directory path signals user-error."
  (let ((tmp (make-temp-file "test-mc-")))
    (unwind-protect
        (should-error (cj/music-add-directory-recursive tmp) :type 'user-error)
      (delete-file tmp))))

;;; ---------- cj/music--find-track-in-playlist ----------

(ert-deftest test-mc-find-track-in-playlist-finds-match ()
  "Normal: a track present in the playlist returns its buffer position."
  (test-mc-untested--setup)
  (unwind-protect
      (with-current-buffer cj/music-playlist-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "line-1\n")
          (let ((p1 (point)))
            (insert "line-2\n")
            (let ((p2 (point)))
              (insert "line-3\n")
              (let ((calls 0))
                (cl-letf (((symbol-function 'emms-playlist-track-at)
                           (lambda (_)
                             (cl-incf calls)
                             ;; Return synthetic tracks per line.
                             (pcase calls
                               (1 '((type . file) (name . "/a/one.mp3")))
                               (2 '((type . file) (name . "/a/two.mp3")))
                               (3 '((type . file) (name . "/a/three.mp3"))))))
                          ((symbol-function 'emms-track-name)
                           (lambda (track) (alist-get 'name track))))
                  ;; "/a/two.mp3" sits on line 2 (between p1 and p2).
                  (let ((pos (cj/music--find-track-in-playlist "/a/two.mp3")))
                    (should (integerp pos))
                    (should (and (>= pos p1) (< pos p2))))))))))
    (test-mc-untested--teardown)))

(ert-deftest test-mc-find-track-in-playlist-not-found-returns-nil ()
  "Boundary: a track absent from the playlist returns nil."
  (test-mc-untested--setup)
  (unwind-protect
      (with-current-buffer cj/music-playlist-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "line-1\n"))
        (cl-letf (((symbol-function 'emms-playlist-track-at)
                   (lambda (_) '((type . file) (name . "/x/other.mp3"))))
                  ((symbol-function 'emms-track-name)
                   (lambda (track) (alist-get 'name track))))
          (should-not (cj/music--find-track-in-playlist "/never/exists.mp3"))))
    (test-mc-untested--teardown)))

(provide 'test-music-config-helpers-untested)
;;; test-music-config-helpers-untested.el ends here
