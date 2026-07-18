;;; test-music-config--radio-tags.el --- Tests for radio-browser tag pre-population -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The tag-search prompt completes over the popular tags fetched from
;; radio-browser's /json/tags endpoint (cached per session).  These tests cover
;; the pure pieces: the endpoint URL, the parse (trim + drop-empty + dedupe,
;; since the source data is user-generated and dirty), and the session cache.
;; The network GET is mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'music-config)

(defconst test-music-radio-tags--fixture
  (concat "[{\"name\":\"jazz \",\"stationcount\":300},"
          "{\"name\":\" jazz\",\"stationcount\":5},"
          "{\"name\":\"\",\"stationcount\":2},"
          "{\"name\":\"rock\",\"stationcount\":100}]")
  "A recorded /json/tags response with whitespace, empty, and duplicate names.")

;;; Normal Cases

(ert-deftest test-music-radio-tags-url-shape ()
  "Normal: the tags URL targets /json/tags ordered by station count with the limit."
  (let* ((cj/music-radio-tag-limit 500)
         (u (cj/music-radio--tags-url "de1.api.radio-browser.info")))
    (should (string-match-p "/json/tags" u))
    (should (string-match-p "order=stationcount" u))
    (should (string-match-p "limit=500" u))))

(ert-deftest test-music-radio-parse-tags-trims-dedupes-drops-empty ()
  "Normal: tag names come back trimmed, deduped, and without empties."
  (should (equal (cj/music-radio--parse-tags test-music-radio-tags--fixture)
                 '("jazz" "rock"))))

(ert-deftest test-music-radio-available-tags-caches-per-session ()
  "Normal: the fetch runs once; later calls serve the cache."
  (let ((cj/music-radio--tags-cache nil)
        (calls 0))
    (cl-letf (((symbol-function 'cj/music-radio--http-get)
               (lambda (_url) (cl-incf calls) test-music-radio-tags--fixture)))
      (should (equal (cj/music-radio--available-tags) '("jazz" "rock")))
      (should (equal (cj/music-radio--available-tags) '("jazz" "rock")))
      (should (= calls 1)))))

;;; Boundary Cases

(ert-deftest test-music-radio-parse-tags-empty-array ()
  "Boundary: an empty tag array parses to nil."
  (should-not (cj/music-radio--parse-tags "[]")))

;;; Error Cases

(ert-deftest test-music-radio-available-tags-fetch-failure-returns-nil-and-retries ()
  "Error: a failed fetch yields nil, leaves the cache empty, and retries next call."
  (let ((cj/music-radio--tags-cache nil)
        (calls 0))
    (cl-letf (((symbol-function 'cj/music-radio--http-get)
               (lambda (_url) (cl-incf calls) nil)))
      (should-not (cj/music-radio--available-tags))
      (should-not cj/music-radio--tags-cache)
      (should-not (cj/music-radio--available-tags))
      (should (= calls 2)))))

(ert-deftest test-music-radio-parse-tags-malformed-user-errors ()
  "Error: a non-JSON body signals user-error, not a raw parse error."
  (should-error (cj/music-radio--parse-tags "<html>502</html>")
                :type 'user-error))

(provide 'test-music-config--radio-tags)
;;; test-music-config--radio-tags.el ends here
