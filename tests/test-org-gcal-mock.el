;;; test-org-gcal-mock.el --- Mock test for org-gcal sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Mock test to capture what org-gcal sends to Google Calendar API
;; This helps debug bidirectional sync issues without hitting the real API

;;; Code:

(require 'ert)
(require 'org)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load org-gcal (this will require auth, but we'll mock the requests)
(require 'org-gcal-config nil t)

;; Variables to capture requests
(defvar test-org-gcal-captured-requests nil
  "List of captured HTTP requests.")

(defvar test-org-gcal-captured-url nil
  "Last captured URL.")

(defvar test-org-gcal-captured-type nil
  "Last captured HTTP method (GET/POST/PUT/PATCH).")

(defvar test-org-gcal-captured-data nil
  "Last captured request body/data.")

(defvar test-org-gcal-captured-headers nil
  "Last captured request headers.")

;;; Mock request-deferred to capture what org-gcal sends

(defun test-org-gcal-mock-request-deferred (url &rest args)
  "Mock request-deferred to capture requests instead of sending them.
URL is the API endpoint. ARGS contains :type, :data, :headers, etc."
  (let* ((type (plist-get args :type))
         (data (plist-get args :data))
         (headers (plist-get args :headers)))
    ;; Capture the request
    (setq test-org-gcal-captured-url url)
    (setq test-org-gcal-captured-type type)
    (setq test-org-gcal-captured-data data)
    (setq test-org-gcal-captured-headers headers)
    (push (list :url url
                :type type
                :data data
                :headers headers)
          test-org-gcal-captured-requests)

    ;; Print for debugging
    (message "MOCK REQUEST: %s %s" type url)
    (when data
      (message "MOCK DATA: %S" data))

    ;; Return a mock deferred object that succeeds immediately
    (require 'deferred)
    (deferred:succeed
     ;; Mock response with a fake event
     (list :data '(:id "test-event-id-123"
                   :etag "test-etag-456"
                   :summary "Test Event"
                   :start (:dateTime "2025-10-28T14:00:00-05:00")
                   :end (:dateTime "2025-10-28T15:00:00-05:00"))
           :status-code 200))))

(ert-deftest test-org-gcal-capture-post-request ()
  "Test capturing what org-gcal sends when posting an event."
  ;; Reset captured requests
  (setq test-org-gcal-captured-requests nil)
  (setq test-org-gcal-captured-url nil)
  (setq test-org-gcal-captured-type nil)
  (setq test-org-gcal-captured-data nil)

  ;; Mock request-deferred
  (cl-letf (((symbol-function 'request-deferred) #'test-org-gcal-mock-request-deferred))

    ;; Create a test org buffer with an event
    (with-temp-buffer
      (org-mode)
      (insert "* TEST: Mock Sync Test Event\n")
      (insert "<2025-10-28 Tue 14:00-15:00>\n")
      (insert "\n")
      (insert "Test event for mocking.\n")

      ;; Go to the headline
      (goto-char (point-min))
      (org-back-to-heading)

      ;; Try to post (this should be captured by our mock)
      (condition-case err
          (org-gcal-post-at-point)
        (error
         (message "Error during post: %S" err)))))

  ;; Check what was captured
  (should test-org-gcal-captured-requests)
  (let ((request (car test-org-gcal-captured-requests)))
    (message "Captured URL: %s" (plist-get request :url))
    (message "Captured Type: %s" (plist-get request :type))
    (message "Captured Data: %S" (plist-get request :data))

    ;; Verify it's trying to POST/PATCH
    (should (member (plist-get request :type) '("POST" "PATCH" "PUT")))

    ;; Verify URL contains calendar API
    (should (string-match-p "googleapis.com/calendar" (plist-get request :url)))))

(provide 'test-org-gcal-mock)
;;; test-org-gcal-mock.el ends here
