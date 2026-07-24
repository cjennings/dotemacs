;;; test-media-utils--yt-dl-message.el --- Tests for the yt-dl sentinel message -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/media--yt-dl-message, the pure helper behind
;; cj/yt-dl-it's process sentinel.
;;
;; The behavior under test is a correctness fix, not cosmetics.  cj/yt-dl-it
;; launches "tsp yt-dlp ...", and tsp enqueues the job and exits immediately.
;; The sentinel therefore fires on tsp's exit, not on yt-dlp's, so the old
;; "Finished downloading" text claimed a completed download at the moment the
;; download was merely queued -- and a yt-dlp failure minutes later was silent.
;; The helper reports queueing, which is the only thing tsp's exit actually
;; proves.
;;
;; Test organization:
;; - Normal Cases: clean tsp exit reports queued; abnormal exit reports failure
;; - Boundary Cases: unrelated events return nil; URL text passes through verbatim
;; - Error Cases: empty event string returns nil
;;
;;; Code:

(require 'ert)
(require 'media-utils)

;;; Normal Cases

(ert-deftest test-media-utils--yt-dl-message-normal-finished-says-queued ()
  "Normal: a clean tsp exit reports the job queued, never downloaded."
  (let ((msg (cj/media--yt-dl-message "finished\n" "https://example.com/v")))
    (should (string-match-p "[Qq]ueued" msg))
    (should-not (string-match-p "[Ff]inished downloading" msg))))

(ert-deftest test-media-utils--yt-dl-message-normal-abnormal-reports-failure ()
  "Normal: an abnormal tsp exit reports that queueing failed."
  (let ((msg (cj/media--yt-dl-message "exited abnormally with code 1\n"
                                      "https://example.com/v")))
    (should msg)
    (should-not (string-match-p "[Qq]ueued for" msg))))

;;; Boundary Cases

(ert-deftest test-media-utils--yt-dl-message-boundary-unrelated-event-is-nil ()
  "Boundary: an event that reports neither outcome produces no message."
  (should (null (cj/media--yt-dl-message "run\n" "https://example.com/v")))
  (should (null (cj/media--yt-dl-message "stopped\n" "https://example.com/v"))))

(ert-deftest test-media-utils--yt-dl-message-boundary-url-passes-through ()
  "Boundary: the URL text is carried into the message verbatim."
  (let ((url "https://example.com/watch?v=a&b=c%20d"))
    (should (string-match-p (regexp-quote url)
                            (cj/media--yt-dl-message "finished\n" url)))))

(ert-deftest test-media-utils--yt-dl-message-boundary-empty-url ()
  "Boundary: an empty URL still yields a message rather than signaling."
  (should (stringp (cj/media--yt-dl-message "finished\n" ""))))

;;; Error Cases

(ert-deftest test-media-utils--yt-dl-message-error-empty-event-is-nil ()
  "Error: an empty event string matches no outcome and returns nil."
  (should (null (cj/media--yt-dl-message "" "https://example.com/v"))))

(provide 'test-media-utils--yt-dl-message)
;;; test-media-utils--yt-dl-message.el ends here
