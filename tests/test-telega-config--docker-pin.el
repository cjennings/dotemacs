;;; test-telega-config--docker-pin.el --- Tests for the telega docker image pin -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for pinning the telega-server container image.
;;
;; telega infers its image from `telega-tdlib-min-version' and only pins to a
;; version tag when min and max versions are equal and the version ends in
;; ".0".  This config has min "1.8.66" and max nil, so the inference always
;; falls through to "zevlg/telega-server:latest" -- a moving tag that can
;; swap the server out from under a fixed elisp version without notice.
;;
;; Since 2026-08-25 the pin names an image built locally from
;; docker/telega-server/Dockerfile (upstream's image is missing a shared
;; library, zevlg/telega.el#596).  Three files have to agree on that image:
;; the defcustom default, the Makefile's build tag, and the Dockerfile's
;; digest-pinned base.  The tests below hold them together.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'telega-config)

(defun test-telega-config--file-string (relative)
  "Return the contents of RELATIVE under `user-emacs-directory'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative user-emacs-directory))
    (buffer-string)))

(defun test-telega-config--pin-default ()
  "Return the defcustom's shipped default, not the live value.
Customizing the pin (including to nil) is not a test failure; only
changing the shipped default is."
  (eval (car (get 'cj/telega-docker-image 'standard-value)) t))

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

;; -- the shipped default and the files it depends on -------------------------

(ert-deftest test-telega-config-pin-default-matches-makefile-build-tag ()
  "Normal: the default pin is exactly the tag `make telega-image' builds.
The image is built locally, so the pin is a tag rather than a registry
digest.  The Makefile owns the tag; the defcustom must name the same one
or a fresh machine builds an image telega never looks for."
  (let ((makefile (test-telega-config--file-string "Makefile")))
    (should (string-match "^TELEGA_IMAGE[ \t]*[?:]?=[ \t]*\\([^ \t\n]+\\)" makefile))
    (should (equal (test-telega-config--pin-default)
                   (match-string 1 makefile)))))

(ert-deftest test-telega-config-pin-default-is-a-local-tag-not-a-digest ()
  "Boundary: the default is a plain tag, with no registry digest suffix.
A locally built image has no RepoDigest, so a digest reference here could
never resolve."
  (let ((default (test-telega-config--pin-default)))
    (should (stringp default))
    (should (string-match-p "\\`[a-z0-9./-]+:[A-Za-z0-9._-]+\\'" default))
    (should-not (string-match-p "@sha256:" default))))

(ert-deftest test-telega-config-dockerfile-pins-base-image-by-digest ()
  "Normal: the Dockerfile's base is an immutable upstream digest.
This is where the digest guarantee the old pin gave now lives.  A tag in
the FROM line would let upstream swap the base under a rebuild."
  (let ((dockerfile (test-telega-config--file-string "docker/telega-server/Dockerfile")))
    (should (string-match-p
             "^FROM zevlg/telega-server@sha256:[0-9a-f]\\{64\\}[ \t]*$"
             dockerfile))))

(ert-deftest test-telega-config-dockerfile-adds-the-missing-library ()
  "Normal: the Dockerfile installs libglycin, the whole reason it exists.
Upstream's image fails to start without it (zevlg/telega.el#596)."
  (let ((dockerfile (test-telega-config--file-string "docker/telega-server/Dockerfile")))
    (should (string-match-p "^RUN apk add .*libglycin" dockerfile))))

;; -- cj/--telega-docker-image-present-p (the docker boundary) ----------------
;; Exercised against a fake `docker' executable on a private exec-path rather
;; than by mocking `call-process' (a subr; see the native-comp mocking gotcha).

(defun test-telega-config--with-fake-docker (exit-code thunk)
  "Call THUNK with a fake `docker' on `exec-path' that exits EXIT-CODE."
  (let* ((dir (make-temp-file "fake-docker-" t))
         (script (expand-file-name "docker" dir)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert (format "#!/bin/sh\nexit %d\n" exit-code)))
          (set-file-modes script #o700)
          (let ((exec-path (list dir)))
            (funcall thunk)))
      (delete-directory dir t))))

(ert-deftest test-telega-config-image-present-p-true-when-inspect-succeeds ()
  "Normal: `docker image inspect' exiting 0 means the image is present."
  (test-telega-config--with-fake-docker 0
    (lambda () (should (cj/--telega-docker-image-present-p "cj/telega-server:x")))))

(ert-deftest test-telega-config-image-present-p-nil-when-inspect-fails ()
  "Boundary: a non-zero exit (no such image) reads as not present."
  (test-telega-config--with-fake-docker 1
    (lambda () (should-not (cj/--telega-docker-image-present-p "cj/telega-server:x")))))

(ert-deftest test-telega-config-image-present-p-nil-without-docker ()
  "Error: with no docker on `exec-path', the helper returns nil instead of
signalling, so the launcher can still route the user to the make target."
  (let ((exec-path nil))
    (should-not (cj/--telega-docker-image-present-p "cj/telega-server:x"))))

;; -- cj/telega refuses to launch against a missing local image ---------------

(ert-deftest test-telega-config-missing-image-message-names-image-and-target ()
  "Normal: the message names the missing image and the make target that builds it."
  (let ((msg (cj/--telega-missing-image-message "cj/telega-server:x")))
    (should (string-match-p "cj/telega-server:x" msg))
    (should (string-match-p "make telega-image" msg))))

(ert-deftest test-telega-config-launcher-errors-when-pinned-image-is-absent ()
  "Error: with a pin set and no such image, `cj/telega' stops with the make hint.
Without this, docker fails to pull a local-only tag and the error names a
registry the image was never meant to come from."
  (let ((cj/telega-docker-image "cj/telega-server:x")
        (launched nil))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "telega.el"))
              ((symbol-function 'cj/--telega-docker-image-present-p) (lambda (_) nil))
              ((symbol-function 'telega) (lambda (&rest _) (setq launched t))))
      (let ((err (should-error (cj/telega) :type 'user-error)))
        (should (string-match-p "make telega-image" (cadr err))))
      (should-not launched))))

(ert-deftest test-telega-config-launcher-runs-when-pinned-image-is-present ()
  "Normal: with the pinned image present, `cj/telega' launches telega."
  (let ((cj/telega-docker-image "cj/telega-server:x")
        (launched nil))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "telega.el"))
              ((symbol-function 'cj/--telega-docker-image-present-p) (lambda (_) t))
              ((symbol-function 'telega) (lambda (&rest _) (setq launched t))))
      (cj/telega)
      (should launched))))

(ert-deftest test-telega-config-launcher-skips-image-check-without-a-pin ()
  "Boundary: with no pin, telega infers and pulls its own image; no check runs."
  (let ((cj/telega-docker-image nil)
        (checked nil)
        (launched nil))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "telega.el"))
              ((symbol-function 'cj/--telega-docker-image-present-p)
               (lambda (_) (setq checked t) nil))
              ((symbol-function 'telega) (lambda (&rest _) (setq launched t))))
      (cj/telega)
      (should-not checked)
      (should launched))))

(provide 'test-telega-config--docker-pin)
;;; test-telega-config--docker-pin.el ends here
