;;; test-elfeed-config-helpers.el --- Tests for elfeed stream/process helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Coverage for two elfeed-config helpers that were untested:
;;   - cj/extract-stream-url: runs yt-dlp -g to resolve a direct stream URL,
;;     returning the URL, nil on non-URL / nonzero exit, or signalling when
;;     yt-dlp is absent.
;;   - cj/elfeed-process-entries: applies an action to each selected entry,
;;     marking them read; errors when nothing is selected, skips entries with
;;     no link, and (by default) catches per-entry action errors.
;; All external boundaries (yt-dlp via call-process, the elfeed-search API) are
;; stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'elfeed-config)

;;; cj/extract-stream-url

(ert-deftest test-elfeed-extract-stream-url-normal-returns-url ()
  "Normal: a successful yt-dlp run returns the trimmed https stream URL."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (p) (and (equal p "yt-dlp") "/usr/bin/yt-dlp")))
            ((symbol-function 'cj/log-silently) #'ignore)
            ((symbol-function 'call-process)
             (lambda (_prog _infile _dest _disp &rest _args)
               (insert "https://stream.example/abc\n") 0)))
    (should (equal "https://stream.example/abc"
                   (cj/extract-stream-url "https://youtube.com/watch?v=x" "best")))))

(ert-deftest test-elfeed-extract-stream-url-boundary-non-url-output-is-nil ()
  "Boundary: output that is not an http(s) URL yields nil, not the raw text."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/yt-dlp"))
            ((symbol-function 'cj/log-silently) #'ignore)
            ((symbol-function 'call-process)
             (lambda (_p _i _d _disp &rest _) (insert "ERROR: unavailable\n") 0)))
    (should (null (cj/extract-stream-url "u" nil)))))

(ert-deftest test-elfeed-extract-stream-url-boundary-nonzero-exit-is-nil ()
  "Boundary: a nonzero yt-dlp exit code yields nil."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/yt-dlp"))
            ((symbol-function 'cj/log-silently) #'ignore)
            ((symbol-function 'call-process)
             (lambda (_p _i _d _disp &rest _) (insert "boom") 1)))
    (should (null (cj/extract-stream-url "u" nil)))))

(ert-deftest test-elfeed-extract-stream-url-error-without-yt-dlp ()
  "Error: a missing yt-dlp signals before attempting the call."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (cj/extract-stream-url "u" "best") :type 'error)))

;;; cj/elfeed-process-entries

(ert-deftest test-elfeed-process-entries-normal-applies-action-and-marks-read ()
  "Normal: each selected entry's link is passed to the action and untagged."
  (let ((acted nil) (untagged nil))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) '(e1 e2)))
              ((symbol-function 'elfeed-untag) (lambda (e &rest _) (push e untagged)))
              ((symbol-function 'elfeed-entry-link) (lambda (e) (format "http://%s" e)))
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'forward-line) #'ignore))
      (cj/elfeed-process-entries (lambda (link) (push link acted)) "open")
      (should (equal '("http://e1" "http://e2") (nreverse acted)))
      (should (equal '(e1 e2) (nreverse untagged))))))

(ert-deftest test-elfeed-process-entries-error-no-selection ()
  "Error: no selected entries signals rather than silently doing nothing."
  (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) nil)))
    (should-error (cj/elfeed-process-entries #'ignore "open") :type 'error)))

(ert-deftest test-elfeed-process-entries-boundary-skips-entry-with-no-link ()
  "Boundary: an entry with no link is untagged but not passed to the action."
  (let ((acted 0))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) '(e1)))
              ((symbol-function 'elfeed-untag) #'ignore)
              ((symbol-function 'elfeed-entry-link) (lambda (_) nil))
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t)))
      (cj/elfeed-process-entries (lambda (_) (cl-incf acted)) "open")
      (should (= 0 acted)))))

(ert-deftest test-elfeed-process-entries-error-default-catches-action-error ()
  "Error: by default a per-entry action error is caught (messaged), not raised."
  (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) '(e1)))
            ((symbol-function 'elfeed-untag) #'ignore)
            ((symbol-function 'elfeed-entry-link) (lambda (_) "http://e1"))
            ((symbol-function 'elfeed-search-update-entry) #'ignore)
            ((symbol-function 'use-region-p) (lambda () t))
            ((symbol-function 'message) #'ignore))
    ;; Returns normally despite the action erroring.
    (cj/elfeed-process-entries (lambda (_) (error "boom")) "open")
    (should t)))

(ert-deftest test-elfeed-process-entries-boundary-skip-error-handling-propagates ()
  "Boundary: with SKIP-ERROR-HANDLING, a per-entry action error propagates."
  (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) '(e1)))
            ((symbol-function 'elfeed-untag) #'ignore)
            ((symbol-function 'elfeed-entry-link) (lambda (_) "http://e1"))
            ((symbol-function 'elfeed-search-update-entry) #'ignore)
            ((symbol-function 'use-region-p) (lambda () t)))
    (should-error (cj/elfeed-process-entries (lambda (_) (error "boom")) "open" t))))

(provide 'test-elfeed-config-helpers)
;;; test-elfeed-config-helpers.el ends here
