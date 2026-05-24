;;; test-quick-video-capture--url-state.el --- Tests for video-capture URL state -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the URL handoff between the org-protocol entry point and the
;; capture handler in quick-video-capture.el.  The URL is passed via a
;; dynamically-bound variable scoped to the org-capture call, so an aborted or
;; erroring capture cannot leave a stale URL behind for the next manual capture.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'quick-video-capture)

;;; cj/video-download-capture-handler

(ert-deftest test-qvc-handler-downloads-bound-url ()
  "Normal: the handler downloads the dynamically-bound URL and saves nothing."
  (let (downloaded)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) nil))
              ((symbol-function 'cj/yt-dl-it) (lambda (u) (setq downloaded u))))
      (let ((cj/--video-download-url "https://example.com/v"))
        (should (equal "" (cj/video-download-capture-handler))))
      (should (equal "https://example.com/v" downloaded)))))

(ert-deftest test-qvc-handler-prompts-when-no-bound-url ()
  "Boundary: with no bound URL the handler prompts the user."
  (let (downloaded)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) nil))
              ((symbol-function 'cj/yt-dl-it) (lambda (u) (setq downloaded u)))
              ((symbol-function 'read-string) (lambda (&rest _) "https://typed/v")))
      (let ((cj/--video-download-url nil))
        (cj/video-download-capture-handler))
      (should (equal "https://typed/v" downloaded)))))

(ert-deftest test-qvc-handler-empty-url-errors ()
  "Error: an empty URL signals rather than downloading nothing."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil))
            ((symbol-function 'read-string) (lambda (&rest _) "")))
    (let ((cj/--video-download-url ""))
      (should-error (cj/video-download-capture-handler)))))

;;; cj/org-protocol-video-download — no global leak

(ert-deftest test-qvc-protocol-binds-url-during-capture ()
  "Normal: the protocol handler makes the URL visible to the capture call."
  (let (seen-during)
    (cl-letf (((symbol-function 'cj/ensure-video-download-initialized) #'ignore)
              ((symbol-function 'org-capture)
               (lambda (&rest _) (setq seen-during cj/--video-download-url))))
      (cj/org-protocol-video-download '(:url "https://example.com/p")))
    (should (equal "https://example.com/p" seen-during))))

(ert-deftest test-qvc-protocol-leaves-no-stale-url ()
  "Boundary: after the protocol capture returns, no URL remains bound."
  (cl-letf (((symbol-function 'cj/ensure-video-download-initialized) #'ignore)
            ((symbol-function 'org-capture) #'ignore))
    (cj/org-protocol-video-download '(:url "https://example.com/p")))
  (should (null cj/--video-download-url)))

(ert-deftest test-qvc-protocol-aborted-capture-clears-url ()
  "Error: a capture that errors/aborts mid-flow still leaves no stale URL.
This is the regression the dynamic binding prevents: the old global
setq survived an interrupted capture into the next manual one."
  (cl-letf (((symbol-function 'cj/ensure-video-download-initialized) #'ignore)
            ((symbol-function 'org-capture)
             (lambda (&rest _) (error "simulated capture abort"))))
    (ignore-errors
      (cj/org-protocol-video-download '(:url "https://example.com/p"))))
  (should (null cj/--video-download-url)))

(provide 'test-quick-video-capture--url-state)
;;; test-quick-video-capture--url-state.el ends here
