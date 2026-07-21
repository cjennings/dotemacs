;;; test-external-open-commands.el --- Tests for external-open commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests in `test-external-open-lib-*.el' cover the pure
;; `cj/external-open-command' and launcher-p helpers.  This file
;; covers the user-facing wrappers:
;;
;;   cj/xdg-open
;;   cj/open-this-file-with
;;   cj/find-file-auto
;;
;; Process primitives and host-environment predicates are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'external-open)

;;; cj/xdg-open

(ert-deftest test-external-open-xdg-open-runs-call-process-on-posix ()
  "Normal: a usable command and file yields a `call-process' invocation."
  (let ((tmp (make-temp-file "test-ext-open-" nil ".tmp"))
        (call-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'env-windows-p) (lambda () nil))
                  ((symbol-function 'cj/external-open-command)
                   (lambda () "/usr/bin/xdg-open"))
                  ((symbol-function 'call-process)
                   (lambda (prog _infile _buf _disp &rest args)
                     (setq call-args (cons prog args))
                     0)))
          (cj/xdg-open tmp))
      (delete-file tmp))
    (should (equal (car call-args) "/usr/bin/xdg-open"))
    ;; The file's basename lands somewhere in the args.
    (let ((basename (file-name-nondirectory tmp)))
      (should (cl-find-if (lambda (a)
                            (and (stringp a) (string-match-p basename a)))
                          call-args)))))

(ert-deftest test-external-open-xdg-open-errors-when-no-file ()
  "Error: a buffer with no associated file signals user-error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/file-from-context) (lambda (_) nil)))
      (should-error (cj/xdg-open) :type 'user-error))))

(ert-deftest test-external-open-xdg-open-errors-when-no-command ()
  "Error: an unsupported host environment signals user-error."
  (let ((tmp (make-temp-file "test-ext-open-no-cmd-" nil ".tmp")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/external-open-command) (lambda () nil)))
          (should-error (cj/xdg-open tmp) :type 'user-error))
      (delete-file tmp))))

;;; cj/open-this-file-with

(ert-deftest test-external-open-open-this-file-with-errors-no-file ()
  "Error: outside a file-visiting buffer signals user-error."
  (with-temp-buffer
    (should-error (cj/open-this-file-with "vlc") :type 'user-error)))

(ert-deftest test-external-open-open-this-file-with-spawns-detached-process ()
  "Normal: posix path launches an argv `call-process' with DESTINATION 0."
  (let ((captured nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/my file.mp4")
      (cl-letf (((symbol-function 'env-windows-p) (lambda () nil))
                ((symbol-function 'executable-find)
                 (lambda (&rest _) "/usr/bin/vlc"))
                ((symbol-function 'call-process)
                 (lambda (&rest args) (setq captured args) 0)))
        (cj/open-this-file-with "vlc --fs"))
      (setq buffer-file-name nil))
    (should (equal captured '("vlc" nil 0 nil "--fs" "/tmp/my file.mp4")))))

(ert-deftest test-external-open-open-this-file-with-errors-missing-program ()
  "Error: a program not on PATH signals user-error before launching."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/foo.mp4")
    (cl-letf (((symbol-function 'env-windows-p) (lambda () nil))
              ((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should-error (cj/open-this-file-with "no-such-program")
                    :type 'user-error))
    (setq buffer-file-name nil)))

;;; cj/find-file-auto

(ert-deftest test-external-open-video-looping-errors-missing-program ()
  "Error: a missing video player gives a clear user-error, not an opaque crash.
The command fires via the find-file advice, so visiting a video on a
machine without mpv must fail with a message naming the program."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
            ((symbol-function 'call-process)
             (lambda (&rest _) (error "call-process should not run"))))
    (should-error (cj/open-video-looping "/tmp/some-video.mp4")
                  :type 'user-error)))

(ert-deftest test-external-open-find-file-auto-routes-media-externally ()
  "Normal: a non-video external extension (`.docx', in
`default-open-extensions') triggers `cj/xdg-open' instead of the original
`find-file'."
  (let ((opened nil)
        (orig-called nil))
    (cl-letf (((symbol-function 'cj/xdg-open)
               (lambda (file) (setq opened file)))
              ;; orig-fun replacement -- shouldn't run for a routed extension.
              ((symbol-function 'cj/find-file-auto--orig-stub)
               (lambda (&rest _) (setq orig-called t))))
      (cj/find-file-auto #'cj/find-file-auto--orig-stub "/tmp/report.docx"))
    (should (equal opened "/tmp/report.docx"))
    (should-not orig-called)))

(ert-deftest test-external-open-find-file-auto-routes-video-to-looping-player ()
  "Normal: a video filename triggers `cj/open-video-looping', not `cj/xdg-open'
or the original `find-file'."
  (let ((looped nil) (xdg nil) (orig-called nil))
    (cl-letf (((symbol-function 'cj/open-video-looping)
               (lambda (file) (setq looped file)))
              ((symbol-function 'cj/xdg-open)
               (lambda (_) (setq xdg t)))
              ((symbol-function 'cj/find-file-auto--orig-stub)
               (lambda (&rest _) (setq orig-called t))))
      (cj/find-file-auto #'cj/find-file-auto--orig-stub "/tmp/clip.mp4"))
    (should (equal looped "/tmp/clip.mp4"))
    (should-not xdg)
    (should-not orig-called)))

(ert-deftest test-external-open-find-file-auto-passes-through-text-files ()
  "Boundary: a `.txt' filename falls through to the original `find-file'."
  (let ((opened nil)
        (orig-args nil))
    (cl-letf (((symbol-function 'cj/xdg-open)
               (lambda (file) (setq opened file)))
              ((symbol-function 'cj/find-file-auto--orig-stub)
               (lambda (&rest args) (setq orig-args args))))
      (cj/find-file-auto #'cj/find-file-auto--orig-stub "/tmp/notes.txt"))
    (should-not opened)
    (should (equal (car orig-args) "/tmp/notes.txt"))))

(ert-deftest test-external-open-find-file-auto-non-string-passes-through ()
  "Boundary: a nil filename falls through (the routing only matches strings)."
  (let ((orig-called nil))
    (cl-letf (((symbol-function 'cj/xdg-open)
               (lambda (_) (error "should not be called")))
              ((symbol-function 'cj/find-file-auto--orig-stub)
               (lambda (&rest _) (setq orig-called t))))
      (cj/find-file-auto #'cj/find-file-auto--orig-stub nil))
    (should orig-called)))

;;; cj/--video-file-p

(ert-deftest test-external-open-video-file-p-matches-video ()
  "Normal: common video extensions match, case-insensitively."
  (should (cj/--video-file-p "/tmp/a.mp4"))
  (should (cj/--video-file-p "/tmp/a.mkv"))
  (should (cj/--video-file-p "/tmp/a.webm"))
  (should (cj/--video-file-p "/tmp/A.MP4")))

(ert-deftest test-external-open-video-file-p-rejects-non-video ()
  "Boundary: audio, docs, and nil do not match."
  (should-not (cj/--video-file-p "/tmp/a.mp3"))
  (should-not (cj/--video-file-p "/tmp/a.txt"))
  (should-not (cj/--video-file-p "/tmp/a.docx"))
  (should-not (cj/--video-file-p nil)))

;;; cj/--video-open-arglist

(ert-deftest test-external-open-video-arglist-appends-file-after-args ()
  "Normal: the player args precede the file in the argument list."
  (let ((cj/video-open-args '("--loop-file=inf")))
    (should (equal (cj/--video-open-arglist "/tmp/a.mp4")
                   '("--loop-file=inf" "/tmp/a.mp4")))))

(ert-deftest test-external-open-video-arglist-respects-custom-args ()
  "Boundary: custom args are honored; empty args yields just the file."
  (let ((cj/video-open-args '("--loop=inf" "--mute=yes")))
    (should (equal (cj/--video-open-arglist "/tmp/a.mkv")
                   '("--loop=inf" "--mute=yes" "/tmp/a.mkv"))))
  (let ((cj/video-open-args nil))
    (should (equal (cj/--video-open-arglist "/tmp/a.mkv") '("/tmp/a.mkv")))))

;;; cj/open-video-looping

(ert-deftest test-external-open-video-looping-calls-player-with-loop-args ()
  "Normal: posix path calls the player with loop args + file, async (no wait)."
  (let ((tmp (make-temp-file "test-ext-video-" nil ".mp4"))
        (call nil))
    (unwind-protect
        (cl-letf (((symbol-function 'env-windows-p) (lambda () nil))
                  ((symbol-function 'call-process)
                   (lambda (prog _infile dest _disp &rest args)
                     (setq call (list prog dest args))
                     0)))
          (let ((cj/video-open-command "mpv")
                (cj/video-open-args '("--loop-file=inf")))
            (cj/open-video-looping tmp)))
      (delete-file tmp))
    (should (equal (nth 0 call) "mpv"))
    (should (equal (nth 1 call) 0))               ; async destination: don't wait
    (should (member "--loop-file=inf" (nth 2 call)))
    (should (cl-find-if (lambda (a) (and (stringp a)
                                         (string-match-p "\\.mp4\\'" a)))
                        (nth 2 call)))))

(ert-deftest test-external-open-video-looping-errors-when-no-file ()
  "Error: a buffer with no associated file signals user-error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/file-from-context) (lambda (_) nil)))
      (should-error (cj/open-video-looping) :type 'user-error))))

(provide 'test-external-open-commands)
;;; test-external-open-commands.el ends here
