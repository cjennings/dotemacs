;;; test-dwim-shell-config-concat.el --- Tests for concat filelist builder -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers cj/dwim-shell--build-concat-filelist, which renders an ffmpeg
;; concat-demuxer filelist from a list of paths.  Building it in Elisp (rather
;; than echo/tr/sed over <<*>>) keeps paths with spaces and quotes intact.

;;; Code:

(require 'ert)
(require 'dwim-shell-config)

(ert-deftest test-dwim-concat-filelist-plain-paths ()
  "Normal: plain paths become single-quoted file lines, newline-separated."
  (should (equal (cj/dwim-shell--build-concat-filelist '("/v/a.mp4" "/v/b.mp4"))
                 "file '/v/a.mp4'\nfile '/v/b.mp4'")))

(ert-deftest test-dwim-concat-filelist-space-in-path ()
  "Boundary: a path with spaces stays inside the quotes, unsplit."
  (should (equal (cj/dwim-shell--build-concat-filelist '("/v/my movie.mp4"))
                 "file '/v/my movie.mp4'")))

(ert-deftest test-dwim-concat-filelist-quote-in-path ()
  "Error/edge: a single quote in a path is escaped for the concat list."
  (should (equal (cj/dwim-shell--build-concat-filelist '("/v/it's here.mp4"))
                 "file '/v/it'\\''s here.mp4'")))

(provide 'test-dwim-shell-config-concat)
;;; test-dwim-shell-config-concat.el ends here
