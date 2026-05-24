;;; test-elfeed-config-youtube-feed-format.el --- Tests for YouTube feed conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/youtube-to-elfeed-feed-format fetches a YouTube page synchronously to
;; derive the channel/playlist feed URL and title.  These tests mock the
;; network boundary (url-retrieve-synchronously) and verify it passes a
;; timeout and always kills the temporary URL buffer — including when the
;; page doesn't contain the expected markers, the path that previously leaked
;; the buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'url)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'elfeed-config)

(defun test-elfeed--url-buffer (html)
  "Return a fresh buffer containing HTML, as url-retrieve-synchronously would."
  (let ((b (generate-new-buffer " *test-url-retrieve*")))
    (with-current-buffer b (insert html))
    b))

;;; Normal Cases

(ert-deftest test-elfeed-youtube-channel-parses-and-cleans-up ()
  "Normal: a channel page yields the feed line and the temp buffer is killed."
  (let ((url-buf nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (setq url-buf
                       (test-elfeed--url-buffer
                        (concat "<link href=\"https://www.youtube.com/feeds/videos.xml"
                                "?channel_id=UCabc123\">"
                                "<meta property=\"og:title\" content=\"Test Channel\">"))))))
      (let ((result (cj/youtube-to-elfeed-feed-format "https://youtube.com/@test" 'channel)))
        (should (string-match-p "channel_id=UCabc123" result))
        (should (string-match-p "Test Channel" result)))
      (should-not (buffer-live-p url-buf)))))

;;; Boundary Cases

(ert-deftest test-elfeed-youtube-passes-a-timeout ()
  "Boundary: the synchronous fetch is given a timeout, not left unbounded."
  (let ((captured nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest args)
                 (setq captured args)
                 (test-elfeed--url-buffer ""))))
      (ignore-errors (cj/youtube-to-elfeed-feed-format "https://youtube.com/@t" 'channel))
      ;; (url &optional silent inhibit-cookies timeout) — a 4th arg is present.
      (should (>= (length captured) 4))
      (should (numberp (nth 3 captured))))))

;;; Error Cases

(ert-deftest test-elfeed-youtube-cleans-up-buffer-on-parse-failure ()
  "Error: a page without the markers errors, and the temp buffer is not leaked."
  (let ((url-buf nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (setq url-buf (test-elfeed--url-buffer "<html>nothing useful</html>")))))
      (should-error (cj/youtube-to-elfeed-feed-format "https://youtube.com/@t" 'channel))
      (should-not (buffer-live-p url-buf)))))

(provide 'test-elfeed-config-youtube-feed-format)
;;; test-elfeed-config-youtube-feed-format.el ends here
