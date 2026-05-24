;;; test-dwim-shell-config-input-safety.el --- Tests for input validators -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the pure input validators that guard user-controlled strings before
;; they reach a shell command: git clone URLs, ffmpeg timestamps, and rename
;; prefixes.  These reject shell metacharacters and malformed input so the
;; command-construction sites stay injection-safe.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dwim-shell-config)

;; ---------------------------------------------------------------------------
;;; cj/dwim-shell--valid-git-url-p
;; ---------------------------------------------------------------------------

(ert-deftest test-dwim-valid-git-url-accepts-common-forms ()
  "Normal: https, scp-style, ssh, and git URLs are accepted."
  (dolist (url '("https://github.com/user/repo.git"
                 "http://example.com/x"
                 "git@github.com:user/repo.git"
                 "ssh://git@host/path/repo.git"
                 "git://host/path"))
    (should (cj/dwim-shell--valid-git-url-p url))))

(ert-deftest test-dwim-valid-git-url-rejects-empty-and-junk ()
  "Boundary: empty string and non-URL text are rejected."
  (should-not (cj/dwim-shell--valid-git-url-p ""))
  (should-not (cj/dwim-shell--valid-git-url-p "not a url"))
  (should-not (cj/dwim-shell--valid-git-url-p "   ")))

(ert-deftest test-dwim-valid-git-url-rejects-shell-metacharacters ()
  "Error: URLs carrying shell metacharacters are rejected."
  (should-not (cj/dwim-shell--valid-git-url-p "https://x.com;reboot"))
  (should-not (cj/dwim-shell--valid-git-url-p "https://x.com; rm -rf /"))
  (should-not (cj/dwim-shell--valid-git-url-p "https://x.com$(whoami)")))

;; ---------------------------------------------------------------------------
;;; cj/dwim-shell--valid-ffmpeg-timestamp-p
;; ---------------------------------------------------------------------------

(ert-deftest test-dwim-valid-timestamp-accepts-seconds-and-clock ()
  "Normal: plain seconds and HH:MM:SS forms are accepted."
  (dolist (ts '("5" "0" "90.5" "00:05" "00:00:05" "1:02:03.5"))
    (should (cj/dwim-shell--valid-ffmpeg-timestamp-p ts))))

(ert-deftest test-dwim-valid-timestamp-rejects-bad-input ()
  "Error: non-numeric, negative, or metacharacter-bearing timestamps rejected."
  (dolist (ts '("" "abc" "-5" "5; rm" "00:00:05; reboot"))
    (should-not (cj/dwim-shell--valid-ffmpeg-timestamp-p ts))))

;; ---------------------------------------------------------------------------
;;; cj/dwim-shell--safe-rename-prefix-p
;; ---------------------------------------------------------------------------

(ert-deftest test-dwim-safe-rename-prefix-accepts-filename-safe ()
  "Normal/Boundary: alphanumerics, spaces, dot, dash, underscore, and empty."
  (dolist (p '("" "img_" "vacation 2026" "shot-01."))
    (should (cj/dwim-shell--safe-rename-prefix-p p))))

(ert-deftest test-dwim-safe-rename-prefix-rejects-unsafe ()
  "Error: quotes, slashes, semicolons, and newlines are rejected."
  (dolist (p '("a'b" "a/b" "a;b" "a\nb"))
    (should-not (cj/dwim-shell--safe-rename-prefix-p p))))

(provide 'test-dwim-shell-config-input-safety)
;;; test-dwim-shell-config-input-safety.el ends here
