;;; test-elfeed-config--decode-html-entities.el --- Tests for cj/--decode-html-entities -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--decode-html-entities replaces the six inline replace-regexp-in-string
;; calls that cj/youtube-to-elfeed-feed-format used to hand-decode an og:title.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'elfeed-config)

(ert-deftest test-elfeed-decode-html-entities-all ()
  "Normal: every supported entity is decoded."
  (should (equal (cj/--decode-html-entities
                  "a &amp; b &lt;c&gt; &quot;d&quot; &#39;e&#x27;")
                 "a & b <c> \"d\" 'e'")))

(ert-deftest test-elfeed-decode-html-entities-no-entities ()
  "Boundary: text without entities is unchanged."
  (should (equal (cj/--decode-html-entities "plain title") "plain title"))
  (should (equal (cj/--decode-html-entities "") "")))

(ert-deftest test-elfeed-decode-html-entities-amp-first ()
  "Boundary: &amp; is decoded before the others (no double-decoding chains)."
  (should (equal (cj/--decode-html-entities "Tom &amp; Jerry &lt;3")
                 "Tom & Jerry <3")))

(provide 'test-elfeed-config--decode-html-entities)
;;; test-elfeed-config--decode-html-entities.el ends here
