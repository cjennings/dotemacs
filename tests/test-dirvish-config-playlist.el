;;; test-dirvish-config-playlist.el --- Tests for the playlist helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--playlist-filter-audio' and `cj/--playlist-sanitize-name' are
;; the two pure pieces under `cj/dired-create-playlist-from-marked'.
;; The interactive command does the dired marking, prompting,
;; overwrite-confirmation loop, and the file write -- the helpers stay
;; testable in isolation.

;;; Code:

(require 'ert)
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

(provide 'test-dirvish-config-playlist)
;;; test-dirvish-config-playlist.el ends here
