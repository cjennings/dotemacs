;;; test-music-config--radio.el --- radio-browser lookup pure-logic tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Phase 1 of the radio-browser lookup (spec docs/specs/2026-07-06-radio-browser-lookup-spec.org):
;; the pure pieces behind the search command.  Parsing a recorded JSON response,
;; picking a station's stream URL, emitting the .m3u, formatting the marginalia
;; annotation (Variant B: codec/bitrate/country/votes/tags), disambiguating a
;; colliding filename, and building the search URL.  The network GET and the
;; interactive command are exercised in the daemon, not here.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'music-config)

(declare-function cj/music-radio--parse-search "music-config" (json-text))
(declare-function cj/music-radio--station-url "music-config" (st))
(declare-function cj/music-radio--station-m3u "music-config" (st))
(declare-function cj/music-radio--tags-snippet "music-config" (tags n))
(declare-function cj/music-radio--format-candidate "music-config" (st))
(declare-function cj/music-radio--disambiguate-name "music-config" (name uuid taken))
(declare-function cj/music-radio--search-url "music-config" (server query))

(defconst test-music-radio--fixture
  (concat "[{\"stationuuid\":\"ea8059be-d119-4de3-b27b-0d9bd6aedb17\","
          "\"name\":\"Adroit Jazz Underground\",\"url_resolved\":\"https://icecast.walmradio.com:8443/jazz\","
          "\"codec\":\"MP3\",\"bitrate\":320,\"countrycode\":\"US\",\"votes\":174208,\"tags\":\"bebop,hard bop,cool\"},"
          "{\"stationuuid\":\"00000000-no-url\",\"name\":\"No URL Station\",\"url_resolved\":\"\",\"url\":\"\","
          "\"codec\":\"AAC\",\"bitrate\":0,\"countrycode\":\"FR\",\"votes\":5,\"tags\":\"\"}]")
  "A two-station recorded radio-browser search response.")

(defun test-music-radio--first ()
  "First station plist from the fixture."
  (car (cj/music-radio--parse-search test-music-radio--fixture)))

;;; --------------------------- parse-search -----------------------------------

(ert-deftest test-music-radio-parse-search-normal ()
  "Normal: a recorded response parses to station plists with the expected fields."
  (let ((stations (cj/music-radio--parse-search test-music-radio--fixture)))
    (should (= (length stations) 2))
    (should (equal (plist-get (car stations) :name) "Adroit Jazz Underground"))
    (should (equal (plist-get (car stations) :stationuuid)
                   "ea8059be-d119-4de3-b27b-0d9bd6aedb17"))))

(ert-deftest test-music-radio-parse-search-empty ()
  "Boundary: an empty result array parses to nil."
  (should-not (cj/music-radio--parse-search "[]")))

(ert-deftest test-music-radio-parse-search-malformed-user-errors ()
  "Error: a non-JSON body (a gateway page) signals user-error, not a raw parse error."
  (should-error (cj/music-radio--parse-search "<html>502 Bad Gateway</html>")
                :type 'user-error))

;;; --------------------------- station-url ------------------------------------

(ert-deftest test-music-radio-station-url-resolved ()
  "Normal: url_resolved is preferred."
  (should (equal (cj/music-radio--station-url (test-music-radio--first))
                 "https://icecast.walmradio.com:8443/jazz")))

(ert-deftest test-music-radio-station-url-fallback-to-url ()
  "Boundary: empty url_resolved falls back to url."
  (should (equal (cj/music-radio--station-url
                  '(:url_resolved "" :url "http://example.test/stream"))
                 "http://example.test/stream")))

(ert-deftest test-music-radio-station-url-none ()
  "Error: neither url_resolved nor url yields nil."
  (should-not (cj/music-radio--station-url '(:url_resolved "" :url ""))))

;;; --------------------------- station-m3u ------------------------------------

(ert-deftest test-music-radio-station-m3u-shape ()
  "Normal: the .m3u carries #EXTM3U, the UUID, #EXTINF, and the stream URL."
  (let ((m3u (cj/music-radio--station-m3u (test-music-radio--first))))
    (should (string-prefix-p "#EXTM3U\n" m3u))
    (should (string-match-p "#RADIOBROWSERUUID:ea8059be-d119-4de3-b27b-0d9bd6aedb17" m3u))
    (should (string-match-p "#EXTINF:1,Adroit Jazz Underground" m3u))
    (should (string-match-p "https://icecast.walmradio.com:8443/jazz" m3u))))

(ert-deftest test-music-radio-station-m3u-no-url-is-nil ()
  "Error: a station with no usable stream URL emits nil (not a broken file)."
  (should-not (cj/music-radio--station-m3u '(:name "Broken" :url_resolved "" :url ""))))

(ert-deftest test-music-radio-station-m3u-captures-favicon ()
  "Normal: a station with a favicon writes a #RADIOBROWSERFAVICON line so the
cover-art layer needs no lookup later."
  (let ((m3u (cj/music-radio--station-m3u
              '(:name "Art Radio" :url_resolved "https://art.example/live"
                :stationuuid "u-art" :favicon "https://cdn.example/art.png"))))
    (should (string-match-p "#RADIOBROWSERFAVICON:https://cdn.example/art.png" m3u))))

(ert-deftest test-music-radio-station-m3u-no-favicon-omits-line ()
  "Boundary: a station with an empty favicon writes no #RADIOBROWSERFAVICON line."
  (let ((m3u (cj/music-radio--station-m3u
              '(:name "Plain" :url_resolved "https://plain.example/live"
                :stationuuid "u-plain" :favicon ""))))
    (should-not (string-match-p "#RADIOBROWSERFAVICON" m3u))))

;;; --------------------------- tags-snippet -----------------------------------

(ert-deftest test-music-radio-tags-snippet-takes-first-n ()
  "Normal: the first N comma-separated tags render trimmed."
  (should (equal (cj/music-radio--tags-snippet "bebop, hard bop, cool, free jazz" 3)
                 "bebop, hard bop, cool")))

(ert-deftest test-music-radio-tags-snippet-empty ()
  "Boundary: empty or nil tags render as the empty string."
  (should (equal (cj/music-radio--tags-snippet "" 3) ""))
  (should (equal (cj/music-radio--tags-snippet nil 3) "")))

;;; --------------------------- format-candidate (Variant B) -------------------

(ert-deftest test-music-radio-format-candidate-variant-b ()
  "Normal: the annotation carries codec, votes, and tags (Variant B)."
  (let ((ann (cj/music-radio--format-candidate (test-music-radio--first))))
    (should (string-match-p "MP3" ann))
    (should (string-match-p "174208" ann))
    (should (string-match-p "bebop" ann))))

;;; --------------------------- disambiguate-name ------------------------------

(ert-deftest test-music-radio-disambiguate-name-unique ()
  "Normal: a name not already taken keeps its safe basename."
  (should (equal (cj/music-radio--disambiguate-name "Jazz Radio" "uuid1234" '())
                 "Jazz_Radio")))

(ert-deftest test-music-radio-disambiguate-name-collision ()
  "Boundary: a colliding name gets a UUID fragment appended, making it unique."
  (let ((first (cj/music-radio--disambiguate-name "Jazz Radio" "aaaabbbbcccc" '())))
    (should (equal first "Jazz_Radio"))
    (should-not (equal (cj/music-radio--disambiguate-name "Jazz Radio" "aaaabbbbcccc"
                                                          (list first))
                       first))))

;;; --------------------------- search-url -------------------------------------

(ert-deftest test-music-radio-search-url-encodes-query-and-limit ()
  "Normal: the search URL hex-encodes the query and carries the limit; name is the default field."
  (let* ((cj/music-radio-search-limit 30)
         (u (cj/music-radio--search-url "de1.api.radio-browser.info" "smooth jazz")))
    (should (string-match-p "name=smooth%20jazz" u))
    (should (string-match-p "limit=30" u))
    (should (string-match-p "/json/stations/search" u))))

(ert-deftest test-music-radio-search-url-tag-field ()
  "Normal: field \"tag\" searches the tag= parameter instead of name=."
  (let ((u (cj/music-radio--search-url "de1.api.radio-browser.info" "ambient" "tag")))
    (should (string-match-p "tag=ambient" u))
    (should-not (string-match-p "name=ambient" u))))

(declare-function cj/music-radio--candidates "music-config" (stations))
(declare-function cj/music-radio--write-stations "music-config" (stations dir))

;;; --------------------------- candidates (dedup) -----------------------------

(ert-deftest test-music-radio-candidates-distinct ()
  "Normal: distinct station names produce distinct display keys mapping to their stations."
  (let* ((stations '((:name "Jazz Radio" :codec "MP3" :bitrate 128)
                     (:name "Blues FM" :codec "AAC" :bitrate 64)))
         (cands (cj/music-radio--candidates stations)))
    (should (= (length cands) 2))
    (should (assoc "Jazz Radio" cands))
    (should (assoc "Blues FM" cands))))

(ert-deftest test-music-radio-candidates-same-name-disambiguated ()
  "Boundary: two stations with the same name get distinct display keys."
  (let* ((stations '((:name "Jazz Radio" :codec "MP3" :bitrate 128 :stationuuid "a")
                     (:name "Jazz Radio" :codec "OGG" :bitrate 192 :stationuuid "b")))
         (cands (cj/music-radio--candidates stations))
         (keys (mapcar #'car cands)))
    (should (= (length cands) 2))
    (should (= (length (delete-dups (copy-sequence keys))) 2))))

;;; --------------------------- write-stations ---------------------------------

(ert-deftest test-music-radio-write-stations-writes-and-skips ()
  "Normal + Error: a station with a URL is written; one with no URL is skipped and named."
  (let ((dir (make-temp-file "radio-write-" t)))
    (unwind-protect
        (let* ((stations (list (test-music-radio--first)
                               '(:name "No URL Here" :url_resolved "" :url "")))
               (result (cj/music-radio--write-stations stations dir)))
          (should (= (length (plist-get result :written)) 1))
          (should (member "No URL Here" (plist-get result :skipped)))
          (should (file-exists-p (car (plist-get result :written)))))
      (delete-directory dir t))))

(ert-deftest test-music-radio-write-stations-radio-suffix ()
  "Normal: a written filename carries the -Radio suffix before .m3u."
  (let ((dir (make-temp-file "radio-write-" t)))
    (unwind-protect
        (let* ((result (cj/music-radio--write-stations (list (test-music-radio--first)) dir))
               (path (car (plist-get result :written))))
          (should (string-suffix-p "-Radio.m3u" path))
          (should (string-match-p "Adroit_Jazz_Underground-Radio\\.m3u\\'" path)))
      (delete-directory dir t))))

(ert-deftest test-music-radio-write-stations-collision-writes-two-files ()
  "Boundary: two same-named stations in one run write two distinct files, no overwrite."
  (let ((dir (make-temp-file "radio-write-" t)))
    (unwind-protect
        (let* ((stations '((:name "Same Name" :url_resolved "http://a.test/s" :stationuuid "aaaa1111")
                           (:name "Same Name" :url_resolved "http://b.test/s" :stationuuid "bbbb2222")))
               (result (cj/music-radio--write-stations stations dir))
               (written (plist-get result :written)))
          (should (= (length written) 2))
          (should-not (equal (nth 0 written) (nth 1 written))))
      (delete-directory dir t))))

(provide 'test-music-config--radio)
;;; test-music-config--radio.el ends here
