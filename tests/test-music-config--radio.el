;;; test-music-config--radio.el --- radio-browser lookup pure-logic tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The pure pieces behind the radio-browser search command (spec
;; docs/specs/2026-07-06-radio-browser-lookup-spec.org): parsing a recorded
;; JSON response, picking a station's stream URL, formatting the marginalia
;; annotation (Variant B: codec/bitrate/country/votes/tags), and building the
;; search URL.  The station->track builder and the queue mechanics live in
;; test-music-config--radio-station-track.el; the network GET and the
;; interactive command are exercised in the daemon, not here.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Declare special here too (the module's bare defvar is file-local) so the
;; registration test's `let' binds dynamically.
(defvar marginalia-annotator-registry)

(require 'music-config)

(declare-function cj/music-radio--parse-search "music-config" (json-text))
(declare-function cj/music-radio--station-url "music-config" (st))
(declare-function cj/music-radio--tags-snippet "music-config" (tags n))
(declare-function cj/music-radio--format-candidate "music-config" (st))
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
  "Normal: url_resolved wins when present."
  (should (equal (cj/music-radio--station-url (test-music-radio--first))
                 "https://icecast.walmradio.com:8443/jazz")))

(ert-deftest test-music-radio-station-url-fallback-to-url ()
  "Boundary: an empty url_resolved falls back to url."
  (should (equal (cj/music-radio--station-url
                  '(:url_resolved "" :url "http://fallback.test/stream"))
                 "http://fallback.test/stream")))

(ert-deftest test-music-radio-station-url-none ()
  "Error: neither url_resolved nor url yields nil."
  (should-not (cj/music-radio--station-url '(:url_resolved "" :url ""))))

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

;;; --------------------------- query whitespace --------------------------------

(ert-deftest test-music-radio-search-and-play-trims-query ()
  "Normal: surrounding whitespace on the query is stripped before the search.
A trailing space in the minibuffer otherwise reaches the API as %20 and
matches nothing."
  (let (captured)
    (cl-letf (((symbol-function 'cj/emms--setup) #'ignore)
              ((symbol-function 'cj/music-radio--search)
               (lambda (query _field) (setq captured query) nil)))
      (should-error (cj/music-radio--search-and-play "  jazz  " "tag")
                    :type 'user-error))
    (should (equal captured "jazz"))))

(ert-deftest test-music-radio-search-and-play-whitespace-only-no-search ()
  "Error: a whitespace-only query errors out before any network search."
  (let (searched)
    (cl-letf (((symbol-function 'cj/emms--setup) #'ignore)
              ((symbol-function 'cj/music-radio--search)
               (lambda (&rest _) (setq searched t) nil)))
      (should-error (cj/music-radio--search-and-play "   " "tag")
                    :type 'user-error))
    (should-not searched)))

;;; --------------------------- column alignment --------------------------------

(ert-deftest test-music-radio-format-candidate-votes-column-fixed-width ()
  "Normal: the votes field pads to a fixed width so the tags column aligns
across stations with different vote counts."
  (let* ((low (cj/music-radio--format-candidate
               '(:codec "MP3" :bitrate 128 :countrycode "US" :votes 7 :tags "jazz")))
         (high (cj/music-radio--format-candidate
                '(:codec "MP3" :bitrate 128 :countrycode "US" :votes 174208 :tags "jazz"))))
    (should (= (string-match "jazz" low) (string-match "jazz" high)))))

(ert-deftest test-music-radio-completion-table-annotates-station ()
  "Normal: the table's annotation function returns the Variant-B string for
a station candidate (marginalia handles the right-alignment)."
  (let* ((candidates '(("Jazz FM" . (:codec "MP3" :bitrate 128 :countrycode "US"
                                     :votes 5 :tags "jazz"))))
         (table (cj/music-radio--completion-table candidates))
         (meta (funcall table "" nil 'metadata))
         (annotate (alist-get 'annotation-function (cdr meta))))
    (should (functionp annotate))
    (should-not (alist-get 'affixation-function (cdr meta)))
    (should (string-match-p "MP3" (funcall annotate "Jazz FM")))
    (should (string-match-p "jazz" (funcall annotate "Jazz FM")))))

(ert-deftest test-music-radio-completion-table-done-sentinel-no-annotation ()
  "Boundary: the [done] sentinel has no station and annotates as nil."
  (let* ((candidates '(("[done]") ("Station" . (:codec "MP3" :bitrate 128
                                                :countrycode "US" :votes 1 :tags "x"))))
         (table (cj/music-radio--completion-table candidates))
         (annotate (alist-get 'annotation-function
                              (cdr (funcall table "" nil 'metadata)))))
    (should-not (funcall annotate "[done]"))))

(ert-deftest test-music-radio-completion-table-registers-with-marginalia ()
  "Normal: building the table registers cj-radio-station so marginalia
right-aligns the table's own annotations."
  (let ((marginalia-annotator-registry '()))
    (cj/music-radio--completion-table '(("X" . (:codec "MP3"))))
    (should (equal (assq 'cj-radio-station marginalia-annotator-registry)
                   '(cj-radio-station builtin none)))))

(provide 'test-music-config--radio)
;;; test-music-config--radio.el ends here
