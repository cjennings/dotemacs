;;; test-org-webclipper-commands.el --- Tests for org-webclipper commands + protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-org-webclipper-process.el' covers
;; `cj/--process-webclip-content'.  This file covers:
;;
;;   cj/webclipper-ensure-initialized
;;   cj/org-protocol-webclip
;;   cj/org-protocol-webclip-handler
;;   cj/org-webclipper-EWW
;;
;; All org-protocol / org-capture / org-web-tools / w3m / eww
;; primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-webclipper)

(defvar org-protocol-protocol-alist nil
  "Stub for `org-protocol-protocol-alist'.")
(defvar org-capture-templates nil
  "Stub for `org-capture-templates'.")
(defvar webclipped-file "/tmp/test-webclipped.org"
  "Stub for the user-constants `webclipped-file' destination.")

;;; cj/webclipper-ensure-initialized

(ert-deftest test-webclipper-ensure-initialized-registers-protocol-and-templates ()
  "Normal: first call sets up the protocol entry + W and w capture templates,
and flips the initialized flag."
  (let ((cj/webclipper-initialized nil)
        (org-protocol-protocol-alist nil)
        (org-capture-templates nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t)))
      (cj/webclipper-ensure-initialized))
    (should cj/webclipper-initialized)
    (should (assoc "webclip" org-protocol-protocol-alist))
    (should (assoc "W" org-capture-templates))
    (should (assoc "w" org-capture-templates))))

(ert-deftest test-webclipper-ensure-initialized-is-idempotent ()
  "Boundary: second call doesn't re-register or duplicate templates."
  (let ((cj/webclipper-initialized nil)
        (org-protocol-protocol-alist nil)
        (org-capture-templates nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t)))
      (cj/webclipper-ensure-initialized)
      (let ((proto-len (length org-protocol-protocol-alist))
            (tmpl-len (length org-capture-templates)))
        (cj/webclipper-ensure-initialized)
        (should (= (length org-protocol-protocol-alist) proto-len))
        (should (= (length org-capture-templates) tmpl-len))))))

;;; cj/org-protocol-webclip

(ert-deftest test-webclipper-protocol-stores-url-title-and-captures ()
  "Normal: the protocol handler stores url+title and triggers `org-capture'."
  (let ((cj/webclipper-initialized t)
        (cj/webclip-current-url nil)
        (cj/webclip-current-title nil)
        (capture-key nil))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (_arg k) (setq capture-key k))))
      (cj/org-protocol-webclip
       '(:url "https://example.com" :title "Hello")))
    (should (equal cj/webclip-current-url "https://example.com"))
    (should (equal cj/webclip-current-title "Hello"))
    (should (equal capture-key "W"))))

(ert-deftest test-webclipper-protocol-defaults-title-when-missing ()
  "Boundary: a missing title in INFO becomes \"Untitled\"."
  (let ((cj/webclipper-initialized t)
        (cj/webclip-current-url nil)
        (cj/webclip-current-title nil))
    (cl-letf (((symbol-function 'org-capture) #'ignore))
      (cj/org-protocol-webclip '(:url "https://x.test")))
    (should (equal cj/webclip-current-title "Untitled"))))

;;; cj/org-protocol-webclip-handler

(ert-deftest test-webclipper-protocol-handler-errors-when-no-url ()
  "Error: handler with no stashed url signals an error."
  (let ((cj/webclip-current-url nil)
        (cj/webclip-current-title "Whatever"))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'setopt) (lambda (&rest _) nil)))
      (should-error (cj/org-protocol-webclip-handler) :type 'error))))

(ert-deftest test-webclipper-protocol-handler-returns-processed-content ()
  "Normal: handler converts the stashed URL into processed org content."
  (let ((cj/webclip-current-url "https://example.com")
        (cj/webclip-current-title "Title"))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'setopt) (lambda (&rest _) nil))
              ((symbol-function 'org-web-tools--url-as-readable-org)
               (lambda (_) "* Page Title\n** Sub heading\nBody.\n"))
              ((symbol-function 'message) #'ignore))
      (let ((out (cj/org-protocol-webclip-handler)))
        ;; The first H1 is stripped, sub-heading is demoted.
        (should (string-match-p "^\\*\\*\\* Sub heading" out))
        (should (string-match-p "Body" out)))
      ;; Stash is cleared.
      (should (null cj/webclip-current-url))
      (should (null cj/webclip-current-title)))))

(ert-deftest test-webclipper-protocol-handler-wraps-fetch-error ()
  "Error: a fetch failure is wrapped in a clear error message."
  (let ((cj/webclip-current-url "https://example.com")
        (cj/webclip-current-title "Title"))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'setopt) (lambda (&rest _) nil))
              ((symbol-function 'org-web-tools--url-as-readable-org)
               (lambda (_) (error "network down"))))
      (let ((err (should-error (cj/org-protocol-webclip-handler) :type 'error)))
        (should (string-match-p "Failed to clip" (cadr err)))))))

;;; cj/org-webclipper-EWW

(ert-deftest test-webclipper-eww-copies-from-eww-buffer ()
  "Normal: an eww-mode source buffer routes through `org-eww-copy-for-org-mode'."
  (let ((source (generate-new-buffer "*test-webclip-eww*"))
        (called nil)
        (kill-ring '("captured-org-content")))
    (with-current-buffer source (setq major-mode 'eww-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/webclipper-ensure-initialized) #'ignore)
                  ((symbol-function 'org-capture-get)
                   (lambda (k) (when (eq k :original-buffer) source)))
                  ((symbol-function 'org-eww-copy-for-org-mode)
                   (lambda () (setq called 'eww))))
          (should (equal (cj/org-webclipper-EWW) "captured-org-content")))
      (when (buffer-live-p source) (kill-buffer source)))
    (should (eq called 'eww))))

(ert-deftest test-webclipper-eww-copies-from-w3m-buffer ()
  "Normal: a w3m-mode source buffer routes through `org-w3m-copy-for-org-mode'."
  (let ((source (generate-new-buffer "*test-webclip-w3m*"))
        (called nil)
        (kill-ring '("captured-w3m")))
    (with-current-buffer source (setq major-mode 'w3m-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/webclipper-ensure-initialized) #'ignore)
                  ((symbol-function 'org-capture-get)
                   (lambda (k) (when (eq k :original-buffer) source)))
                  ((symbol-function 'org-w3m-copy-for-org-mode)
                   (lambda () (setq called 'w3m))))
          (should (equal (cj/org-webclipper-EWW) "captured-w3m")))
      (when (buffer-live-p source) (kill-buffer source)))
    (should (eq called 'w3m))))

(ert-deftest test-webclipper-eww-errors-on-unsupported-mode ()
  "Error: a non-eww/w3m source buffer signals."
  (let ((source (generate-new-buffer "*test-webclip-other*")))
    (with-current-buffer source (setq major-mode 'fundamental-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/webclipper-ensure-initialized) #'ignore)
                  ((symbol-function 'org-capture-get)
                   (lambda (k) (when (eq k :original-buffer) source))))
          (should-error (cj/org-webclipper-EWW) :type 'error))
      (when (buffer-live-p source) (kill-buffer source)))))

(provide 'test-org-webclipper-commands)
;;; test-org-webclipper-commands.el ends here
