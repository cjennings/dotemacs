;;; test-dirvish-config-playlist.el --- Tests for the playlist helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--playlist-filter-audio' and `cj/--playlist-sanitize-name' are
;; the two pure pieces under `cj/dired-create-playlist-from-marked'.
;; The interactive command does the dired marking, prompting,
;; overwrite-confirmation loop, and the file write -- the helpers stay
;; testable in isolation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

;;; --------------------------- filter-audio --------------------------

(ert-deftest test-cj--playlist-filter-audio-keeps-only-audio ()
  "Normal: a mixed list returns only the entries with audio extensions."
  (should (equal (cj/--playlist-filter-audio
                  '("/m/song.mp3" "/d/notes.org" "/m/track.flac" "/d/run.sh")
                  '("mp3" "flac"))
                 '("/m/song.mp3" "/m/track.flac"))))

(ert-deftest test-cj--playlist-filter-audio-case-insensitive-extension ()
  "Boundary: uppercase / mixed-case extensions are matched against the
lowercase extension list."
  (should (equal (cj/--playlist-filter-audio
                  '("/m/A.MP3" "/m/B.Flac" "/m/c.OGG")
                  '("mp3" "flac" "ogg"))
                 '("/m/A.MP3" "/m/B.Flac" "/m/c.OGG"))))

(ert-deftest test-cj--playlist-filter-audio-files-without-extension-excluded ()
  "Boundary: a file with no extension can't be an audio file."
  (should (equal (cj/--playlist-filter-audio
                  '("/m/README" "/m/song.mp3" "/m/Makefile")
                  '("mp3"))
                 '("/m/song.mp3"))))

(ert-deftest test-cj--playlist-filter-audio-empty-input-empty-output ()
  "Boundary: no input files -> no output."
  (should-not (cj/--playlist-filter-audio '() '("mp3" "flac"))))

(ert-deftest test-cj--playlist-filter-audio-empty-extensions-empty-output ()
  "Boundary: no allowed extensions -> nothing matches."
  (should-not (cj/--playlist-filter-audio
               '("/m/song.mp3" "/m/track.flac")
               '())))

;;; ---------------------- sanitize-playlist-name ---------------------

(ert-deftest test-cj--playlist-sanitize-name-strips-trailing-m3u ()
  "Normal: a `.m3u' suffix is stripped."
  (should (equal (cj/--playlist-sanitize-name "summer.m3u") "summer")))

(ert-deftest test-cj--playlist-sanitize-name-bare-name-unchanged ()
  "Normal: a name without `.m3u' is returned unchanged."
  (should (equal (cj/--playlist-sanitize-name "summer") "summer")))

(ert-deftest test-cj--playlist-sanitize-name-embedded-m3u-untouched ()
  "Boundary: `.m3u' that isn't at the end is kept (it's part of the name)."
  (should (equal (cj/--playlist-sanitize-name "my.m3u.draft")
                 "my.m3u.draft")))

(ert-deftest test-cj--playlist-sanitize-name-empty-string-unchanged ()
  "Boundary: empty string returns empty string."
  (should (equal (cj/--playlist-sanitize-name "") "")))

(ert-deftest test-cj--playlist-sanitize-name-only-suffix ()
  "Boundary: a name that's just `.m3u' becomes empty after stripping."
  (should (equal (cj/--playlist-sanitize-name ".m3u") "")))

;;; cj/--playlist-name-safe-p

(ert-deftest test-cj--playlist-name-safe-p-bare-name ()
  "Normal: a bare filename is safe."
  (should (cj/--playlist-name-safe-p "roadtrip")))

(ert-deftest test-cj--playlist-name-safe-p-empty ()
  "Boundary: an empty name is not safe."
  (should-not (cj/--playlist-name-safe-p "")))

(ert-deftest test-cj--playlist-name-safe-p-rejects-separators ()
  "Error: any directory separator (relative, absolute, or nested) is rejected."
  (dolist (bad '("../evil" "../../etc/cron" "/etc/passwd" "sub/dir/name"))
    (should-not (cj/--playlist-name-safe-p bad))))

;;; cj/--playlist-resolve-target
;;
;; Drives the real `file-exists-p' against a temp `music-dir' (mocking a C
;; primitive triggers a native-comp trampoline rebuild that fails under
;; --batch); only the ordinary `read-string' / `read-char-choice' prompts are
;; stubbed.

(ert-deftest test-cj--playlist-resolve-target-returns-path-for-new-name ()
  "Normal: a safe name with no existing file returns its .m3u path under music-dir."
  (let* ((music-dir (make-temp-file "cj-playlist-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "roadtrip")))
          (should (equal (expand-file-name "roadtrip.m3u" music-dir)
                         (cj/--playlist-resolve-target))))
      (delete-directory music-dir t))))

(ert-deftest test-cj--playlist-resolve-target-reprompts-on-unsafe-name ()
  "Boundary: an unsafe name (with `/') re-prompts until a safe name is given."
  (let* ((music-dir (make-temp-file "cj-playlist-" t))
         (answers '("../escape" "safe"))
         (asked 0))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (prog1 (nth asked answers) (cl-incf asked))))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (should (equal (expand-file-name "safe.m3u" music-dir)
                         (cj/--playlist-resolve-target)))
          (should (= 2 asked)))
      (delete-directory music-dir t))))

(ert-deftest test-cj--playlist-resolve-target-overwrite-returns-existing-path ()
  "Normal: when the target exists, choosing overwrite returns the same path."
  (let* ((music-dir (make-temp-file "cj-playlist-" t))
         (existing (expand-file-name "mix.m3u" music-dir)))
    (unwind-protect
        (progn
          (with-temp-file existing (insert "old\n"))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "mix"))
                    ((symbol-function 'read-char-choice) (lambda (&rest _) ?o)))
            (should (equal existing (cj/--playlist-resolve-target)))))
      (delete-directory music-dir t))))

(ert-deftest test-cj--playlist-resolve-target-cancel-signals-user-error ()
  "Error: when the target exists, choosing cancel aborts with a `user-error'."
  (let* ((music-dir (make-temp-file "cj-playlist-" t))
         (existing (expand-file-name "mix.m3u" music-dir)))
    (unwind-protect
        (progn
          (with-temp-file existing (insert "old\n"))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "mix"))
                    ((symbol-function 'read-char-choice) (lambda (&rest _) ?c)))
            (should-error (cj/--playlist-resolve-target) :type 'user-error)))
      (delete-directory music-dir t))))

(provide 'test-dirvish-config-playlist)
;;; test-dirvish-config-playlist.el ends here
