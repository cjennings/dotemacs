;;; test-chrono-tools-tmr-sound.el --- Tests for tmr sound selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/tmr-select-sound-file' and its helpers in chrono-tools.el.
;; The prefix-arg branch delegates to `cj/tmr-reset-sound-to-default' so
;; there's no duplicated reset logic to test twice.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub vars that chrono-tools.el assumes are bound at runtime.
(defvar sounds-dir nil)
(defvar tmr-sound-file nil)
(defvar notification-sound "/tmp/notify.mp3")

(require 'chrono-tools)

;; -------------------------------- helpers

(defun test-chrono-tools--with-sounds-dir (files fn)
  "Create a temp `sounds-dir' containing FILES, call FN, clean up."
  (let* ((dir (make-temp-file "test-chrono-tools-tmr-" t))
         (sounds-dir dir))
    (unwind-protect
        (progn
          (dolist (f files)
            (with-temp-file (expand-file-name f dir) (insert "x")))
          (funcall fn dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; -------------------------------- available-sound-files

(ert-deftest test-chrono-tools-available-sounds-normal ()
  "Normal: returns audio files in `sounds-dir' matching the extension list."
  (test-chrono-tools--with-sounds-dir
   '("a.mp3" "b.opus" "readme.txt")
   (lambda (_dir)
     (let ((files (cj/tmr--available-sound-files)))
       (should (member "a.mp3" files))
       (should (member "b.opus" files))
       (should-not (member "readme.txt" files))))))

(ert-deftest test-chrono-tools-available-sounds-empty-dir ()
  "Boundary: empty `sounds-dir' returns nil."
  (test-chrono-tools--with-sounds-dir
   '()
   (lambda (_dir)
     (should-not (cj/tmr--available-sound-files)))))

(ert-deftest test-chrono-tools-available-sounds-no-dir ()
  "Boundary: missing `sounds-dir' returns nil."
  (let ((sounds-dir (expand-file-name (format "missing-%s" (random)) "/tmp")))
    (should-not (cj/tmr--available-sound-files))))

;; -------------------------------- reset-sound-to-default

(ert-deftest test-chrono-tools-reset-sets-default ()
  "Reset assigns `notification-sound' to `tmr-sound-file'."
  (let ((tmr-sound-file "/tmp/other.mp3")
        (notification-sound "/tmp/the-default.mp3"))
    (cj/tmr-reset-sound-to-default)
    (should (equal tmr-sound-file "/tmp/the-default.mp3"))))

;; -------------------------------- select-sound-file

(ert-deftest test-chrono-tools-select-with-prefix-arg-resets ()
  "Prefix arg delegates to reset, no completing-read prompt."
  (let ((current-prefix-arg '(4))
        (tmr-sound-file "/tmp/other.mp3")
        (notification-sound "/tmp/the-default.mp3")
        (prompted nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompted t) "")))
      (cj/tmr-select-sound-file))
    (should-not prompted)
    (should (equal tmr-sound-file "/tmp/the-default.mp3"))))

(ert-deftest test-chrono-tools-select-normal-sets-selected-file ()
  "Normal: pick a file, `tmr-sound-file' updates to its full path."
  (test-chrono-tools--with-sounds-dir
   '("alpha.mp3" "beta.opus")
   (lambda (dir)
     (let ((current-prefix-arg nil)
           (tmr-sound-file nil)
           (notification-sound "/tmp/the-default.mp3"))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "alpha.mp3")))
         (cj/tmr-select-sound-file))
       (should (equal tmr-sound-file
                      (expand-file-name "alpha.mp3" dir)))))))

(ert-deftest test-chrono-tools-select-boundary-empty-dir ()
  "Boundary: empty sounds dir leaves `tmr-sound-file' unchanged."
  (test-chrono-tools--with-sounds-dir
   '()
   (lambda (_dir)
     (let ((current-prefix-arg nil)
           (tmr-sound-file "/tmp/keep-me.mp3")
           (notification-sound "/tmp/the-default.mp3"))
       (cj/tmr-select-sound-file)
       (should (equal tmr-sound-file "/tmp/keep-me.mp3"))))))

(ert-deftest test-chrono-tools-select-boundary-missing-dir ()
  "Boundary: missing sounds dir leaves `tmr-sound-file' unchanged."
  (let ((current-prefix-arg nil)
        (sounds-dir (expand-file-name (format "missing-%s" (random)) "/tmp"))
        (tmr-sound-file "/tmp/keep-me.mp3")
        (notification-sound "/tmp/the-default.mp3"))
    (cj/tmr-select-sound-file)
    (should (equal tmr-sound-file "/tmp/keep-me.mp3"))))

(ert-deftest test-chrono-tools-select-boundary-cancel-returns-no-change ()
  "Boundary: empty completion result leaves `tmr-sound-file' unchanged."
  (test-chrono-tools--with-sounds-dir
   '("alpha.mp3")
   (lambda (_dir)
     (let ((current-prefix-arg nil)
           (tmr-sound-file "/tmp/keep-me.mp3")
           (notification-sound "/tmp/the-default.mp3"))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "")))
         (cj/tmr-select-sound-file))
       (should (equal tmr-sound-file "/tmp/keep-me.mp3"))))))

(provide 'test-chrono-tools-tmr-sound)
;;; test-chrono-tools-tmr-sound.el ends here
