;;; test-media-utils--argv.el --- Tests for media-utils argv builders -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the pure helpers behind cj/media-play-it's shell-free
;; launch: cj/media--yt-dlp-argv (stream-URL resolution command),
;; cj/media--stream-urls (yt-dlp -g output parsing), and
;; cj/media--play-argv (player launch argv).  Argv lists are the
;; hardening: URLs and args never meet a shell.
;;
;; Test organization:
;; - Normal Cases: formats present/absent, args split, multi-line output
;; - Boundary Cases: metacharacter URLs verbatim, empty output, nil args
;; - Error Cases: none (builders are total; error paths live in the caller)
;;
;;; Code:

(require 'ert)
(require 'media-utils)

;;; cj/media--yt-dlp-argv

(ert-deftest test-media-utils--yt-dlp-argv-normal-with-formats ()
  "Normal: formats join with / behind -f, URL last."
  (should (equal (cj/media--yt-dlp-argv "https://example.com/v" '("22" "18" "best"))
                 '("yt-dlp" "-f" "22/18/best" "-g" "https://example.com/v"))))

(ert-deftest test-media-utils--yt-dlp-argv-normal-without-formats ()
  "Normal: nil formats drops the -f pair."
  (should (equal (cj/media--yt-dlp-argv "https://example.com/v" nil)
                 '("yt-dlp" "-g" "https://example.com/v"))))

(ert-deftest test-media-utils--yt-dlp-argv-boundary-metacharacter-url ()
  "Boundary: a URL with shell metacharacters stays one verbatim element."
  (let ((url "https://example.com/v?a=1&b=$(x);c='d'"))
    (should (equal (car (last (cj/media--yt-dlp-argv url nil))) url))))

;;; cj/media--stream-urls

(ert-deftest test-media-utils--stream-urls-normal-two-lines ()
  "Normal: each non-empty output line is one stream URL."
  (should (equal (cj/media--stream-urls "https://a/video\nhttps://a/audio\n")
                 '("https://a/video" "https://a/audio"))))

(ert-deftest test-media-utils--stream-urls-boundary-blank-and-crlf ()
  "Boundary: blank lines and CR line endings are stripped."
  (should (equal (cj/media--stream-urls "https://a/v\r\n\n \nhttps://a/u\r\n")
                 '("https://a/v" "https://a/u"))))

(ert-deftest test-media-utils--stream-urls-boundary-empty-output ()
  "Boundary: empty output yields nil."
  (should-not (cj/media--stream-urls "")))

;;; cj/media--play-argv

(ert-deftest test-media-utils--play-argv-normal-args-split ()
  "Normal: the player's raw args string splits into argv words."
  (should (equal (cj/media--play-argv "vlc" "--no-video --intf dummy"
                                      '("https://a/v"))
                 '("vlc" "--no-video" "--intf" "dummy" "https://a/v"))))

(ert-deftest test-media-utils--play-argv-boundary-nil-args ()
  "Boundary: nil args yields program + URLs only."
  (should (equal (cj/media--play-argv "mpv" nil '("https://a/v"))
                 '("mpv" "https://a/v"))))

(ert-deftest test-media-utils--play-argv-boundary-multiple-urls ()
  "Boundary: every resolved stream URL is appended in order."
  (should (equal (cj/media--play-argv "mpv" nil '("https://a/v" "https://a/u"))
                 '("mpv" "https://a/v" "https://a/u"))))

(provide 'test-media-utils--argv)
;;; test-media-utils--argv.el ends here
