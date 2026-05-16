;;; web_fetch.el --- Web fetch tool for gptel -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools, web

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool that fetches an HTTP/HTTPS URL and returns its body.
;; HTML is piped through `pandoc -f html -t plain' (falling back to
;; `w3m -dump -T text/html') so the model gets a reading shape that
;; isn't full of markup; pass RAW=t to skip stripping and get the
;; verbatim response.  Output is capped at 200KB by default (hard cap
;; 1MB) and the cap is reported inline when triggered.
;;
;; This tool is `:confirm t' because it makes outbound network
;; requests -- the user sees every URL before the fetch happens.  The
;; URL goes wherever the user-agent points it, including internal
;; networks if the URL names one; consider the network posture before
;; approving sensitive endpoints.

;;; Code:

(require 'gptel)
(require 'url)

(defconst cj/gptel-web-fetch--default-max-bytes (* 200 1024)
  "Default cap on returned body size.  ~200KB.")

(defconst cj/gptel-web-fetch--hard-max-bytes (* 1024 1024)
  "Hard upper bound on the user-controllable byte cap.  1MB.")

(defun cj/gptel-web-fetch--validate-url (url)
  "Validate URL as an http or https request target.  Return URL on success.
Signals `user-error' for non-string, empty, or non-http/https URLs."
  (unless (and (stringp url) (not (string-empty-p url)))
    (user-error "web_fetch: expected non-empty URL string, got %S" url))
  (unless (string-match-p "\\`https?://[^[:space:]]+\\'" url)
    (user-error "web_fetch: URL must be http:// or https://, got %S" url))
  url)

(defun cj/gptel-web-fetch--effective-max-bytes (n)
  "Return the byte cap to use given caller-supplied N.
Nil / non-integer / out-of-range → default.  Above hard cap → hard cap."
  (cond
   ((not (integerp n)) cj/gptel-web-fetch--default-max-bytes)
   ((< n 1) cj/gptel-web-fetch--default-max-bytes)
   ((> n cj/gptel-web-fetch--hard-max-bytes) cj/gptel-web-fetch--hard-max-bytes)
   (t n)))

(defun cj/gptel-web-fetch--retrieve (url)
  "Synchronously GET URL.  Return a cons (STATUS-CODE . BODY).
Signals on network failure.  STATUS-CODE is an integer when parseable
from the response status line, or nil when the line is unrecognized."
  (let ((buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "web_fetch: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let* ((status (when (re-search-forward
                                "^HTTP/[0-9.]+ \\([0-9]+\\)" (point-max) t)
                           (string-to-number (match-string 1))))
                 (body-start (when (re-search-forward "\n\n" nil t)
                               (point))))
            (cons status
                  (if body-start
                      (buffer-substring-no-properties body-start (point-max))
                    (buffer-substring-no-properties (point-min) (point-max))))))
      (kill-buffer buf))))

(defun cj/gptel-web-fetch--html-to-text (html)
  "Strip HTML to plain text.  Returns the stripped string.
Tries `pandoc -f html -t plain' first, falls back to
`w3m -dump -T text/html'.  Signals `user-error' if neither is
on PATH."
  (let* ((coding-system-for-write 'utf-8)
         (coding-system-for-read 'utf-8)
         (tool (cond
                ((executable-find "pandoc")
                 (list "pandoc" "-f" "html" "-t" "plain"))
                ((executable-find "w3m")
                 (list "w3m" "-dump" "-T" "text/html"))
                (t nil))))
    (unless tool
      (user-error
       "web_fetch: HTML stripping needs pandoc or w3m on PATH; pass raw=t to bypass"))
    ;; `call-process-region' with DELETE=t and OUTPUT=t replaces the
    ;; input range with the tool's output, so `buffer-string' returns
    ;; the stripped text.
    (with-temp-buffer
      (insert html)
      (let ((exit (apply #'call-process-region
                         (point-min) (point-max) (car tool)
                         t t nil (cdr tool))))
        (if (zerop exit)
            (buffer-string)
          (error "web_fetch: %s exited with %d" (car tool) exit))))))

(defun cj/gptel-web-fetch--truncate (text max-bytes)
  "Truncate TEXT to MAX-BYTES.  Returns TEXT unchanged when under the cap."
  (if (<= (length text) max-bytes)
      text
    (concat (substring text 0 max-bytes)
            (format
             "\n\n[truncated: response exceeded %d bytes; %d bytes total]"
             max-bytes (length text)))))

(defun cj/gptel-web-fetch--run (url &optional raw max-bytes)
  "Fetch URL and return its body.
When RAW is nil (the default) HTML responses are stripped to plain
text via pandoc or w3m.  MAX-BYTES caps the returned size; nil /
out-of-range falls back to the default 200KB cap."
  (let* ((validated (cj/gptel-web-fetch--validate-url url))
         (cap (cj/gptel-web-fetch--effective-max-bytes max-bytes))
         (response (cj/gptel-web-fetch--retrieve validated))
         (status (car response))
         (body (cdr response)))
    (when (and status (>= status 400))
      (error "web_fetch: HTTP %d from %s" status validated))
    (let ((text (if raw body
                  (cj/gptel-web-fetch--html-to-text body))))
      (cj/gptel-web-fetch--truncate text cap))))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "web_fetch"
   :function (lambda (url &optional raw max_bytes)
               (cj/gptel-web-fetch--run url raw max_bytes))
   :description "Fetch an http:// or https:// URL and return its body.  HTML responses are stripped to plain text via pandoc (or w3m as a fallback); pass raw=true to skip stripping.  Output is capped at 200KB by default (max 1MB); the cap is reported inline when triggered.  Network call: the URL goes wherever the user-agent points, including internal networks if specified."
   :args (list '(:name "url"
                       :type string
                       :description "HTTP or HTTPS URL to fetch.  Non-http schemes are rejected.")
               '(:name "raw"
                       :type boolean
                       :description "When true, return the response body verbatim without HTML stripping.  Default false."
                       :optional t)
               '(:name "max_bytes"
                       :type integer
                       :description "Output size cap in bytes.  Defaults to 200000; hard-capped at 1048576."
                       :optional t))
   :category "web"
   :confirm t
   :include t)

  (add-to-list 'gptel-tools (gptel-get-tool '("web" "web_fetch"))))

(provide 'web_fetch)
;;; web_fetch.el ends here
