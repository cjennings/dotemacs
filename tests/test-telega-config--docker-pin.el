;;; test-telega-config--docker-pin.el --- Tests for the telega docker image pin -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for pinning the telega-server container image.
;;
;; telega infers its image from `telega-tdlib-min-version' and only pins to a
;; version tag when min and max versions are equal and the version ends in
;; ".0".  This config has min "1.8.64" and max nil, so the inference always
;; falls through to "zevlg/telega-server:latest" -- a moving tag that can
;; swap the server out from under a fixed elisp version without notice.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'telega-config)

;; -- cj/--telega-docker-pinned-image -----------------------------------------

(ert-deftest test-telega-config-pin-returns-configured-reference ()
  "Normal: a configured pin is returned verbatim."
  (let ((cj/telega-docker-image "zevlg/telega-server@sha256:abc123"))
    (should (equal (cj/--telega-docker-pinned-image)
                   "zevlg/telega-server@sha256:abc123"))))

(ert-deftest test-telega-config-pin-nil-when-unset ()
  "Boundary: no pin returns nil so telega's own inference stays in charge."
  (let ((cj/telega-docker-image nil))
    (should-not (cj/--telega-docker-pinned-image))))

(ert-deftest test-telega-config-pin-nil-for-empty-string ()
  "Boundary: an empty or whitespace pin is not a reference.
An empty string would otherwise reach the docker command line as a blank
image argument, which fails in a way that looks unrelated to this setting."
  (let ((cj/telega-docker-image ""))
    (should-not (cj/--telega-docker-pinned-image)))
  (let ((cj/telega-docker-image "   "))
    (should-not (cj/--telega-docker-pinned-image))))

(ert-deftest test-telega-config-pin-nil-for-non-string ()
  "Error: a non-string pin is ignored rather than passed to the shell."
  (let ((cj/telega-docker-image 'latest))
    (should-not (cj/--telega-docker-pinned-image)))
  (let ((cj/telega-docker-image 42))
    (should-not (cj/--telega-docker-pinned-image))))

(ert-deftest test-telega-config-pin-trims-surrounding-whitespace ()
  "Boundary: a pin with stray whitespace is trimmed, not rejected.
A trailing newline is easy to introduce when pasting a digest from docker."
  (let ((cj/telega-docker-image "  zevlg/telega-server@sha256:abc123\n"))
    (should (equal (cj/--telega-docker-pinned-image)
                   "zevlg/telega-server@sha256:abc123"))))

;; -- cj/--telega-docker-image-name (the advice) ------------------------------

(ert-deftest test-telega-config-image-advice-prefers-the-pin ()
  "Normal: with a pin set, the advice returns it instead of calling telega."
  (let ((cj/telega-docker-image "zevlg/telega-server@sha256:abc123")
        (called nil))
    (should (equal (cj/--telega-docker-image-name
                    (lambda () (setq called t) "zevlg/telega-server:latest"))
                   "zevlg/telega-server@sha256:abc123"))
    (should-not called)))

(ert-deftest test-telega-config-image-advice-delegates-without-a-pin ()
  "Boundary: with no pin, telega's own inference is used unchanged.
Removing the pin must restore stock behavior rather than break the image
name, so this stays a reversible setting."
  (let ((cj/telega-docker-image nil))
    (should (equal (cj/--telega-docker-image-name
                    (lambda () "zevlg/telega-server:latest"))
                   "zevlg/telega-server:latest"))))

(ert-deftest test-telega-config-image-advice-is-named-and-removable ()
  "Normal: the advice is a named function so it can be removed by reference."
  (should (fboundp 'cj/--telega-docker-image-name)))

(ert-deftest test-telega-config-pin-default-is-a-digest-reference ()
  "Normal: the shipped default pins by digest, not by a floating tag.
A tag pin (:latest, or even a version tag upstream can re-push) still
moves; a digest names one immutable image.

Reads the defcustom's standard value rather than the live variable, so
customizing the pin (including to nil, handing the choice back to telega)
is not a test failure -- only changing the shipped default is."
  (let ((default (eval (car (get 'cj/telega-docker-image 'standard-value)) t)))
    (should (stringp default))
    (should (string-match-p "@sha256:[0-9a-f]\\{64\\}\\'" default))))

(provide 'test-telega-config--docker-pin)
;;; test-telega-config--docker-pin.el ends here
