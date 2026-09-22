;;; test-transcription-config--ratio-backend.el --- the ratio self-hosted backend -*- lexical-binding: t; -*-

;;; Commentary:
;; The `ratio' backend runs whisper plus speaker diarization on my own host
;; through the scripts/ratio-transcribe client.  It needs no API key, so its
;; descriptor carries nil for both :auth-host and :env-var, and the process
;; environment passes through unchanged.
;;
;; Two seams have to agree for a backend to be usable: the descriptor alist
;; that resolves the script, and the completing-read list the interactive
;; switcher offers.  The switcher used to carry its own copy of that list, so
;; a descriptor added without a switcher entry was reachable only by setq.  It
;; now derives its choices from the alist; the boundary test below guards
;; against a return to a hardcoded list.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args)
    "Stub notification function for testing."
    nil))

(require 'transcription-config)

(defun test-transcription-ratio--switcher-choices ()
  "Return the backend names `cj/transcription-switch-backend' offers.
Captures the collection handed to `completing-read' and answers with the
current backend so the switcher is a no-op."
  (let (offered)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq offered (mapcar #'car collection))
                 (symbol-name cj/transcribe-backend))))
      (let ((cj/transcribe-backend cj/transcribe-backend))
        (cj/transcription-switch-backend)))
    offered))

;;; Normal

(ert-deftest test-transcription-config-ratio-descriptor-resolves ()
  "Normal: the ratio backend resolves to the ratio-transcribe client with no
API-key requirement."
  (let ((desc (cj/--backend-plist 'ratio)))
    (should (equal (plist-get desc :script) "ratio-transcribe"))
    (should (null (plist-get desc :auth-host)))
    (should (null (plist-get desc :env-var)))))

(ert-deftest test-transcription-config-ratio-script-path-and-executable ()
  "Normal: the script path lands on scripts/ratio-transcribe and the file is
there and executable, so a transcription can actually start."
  (let ((cj/transcribe-backend 'ratio))
    (let ((path (cj/--transcription-script-path)))
      (should (string-suffix-p "scripts/ratio-transcribe" path))
      (should (file-executable-p path)))))

(ert-deftest test-transcription-config-ratio-environment-passes-through ()
  "Normal: no API key means the process environment is returned unchanged,
and nothing consults authinfo."
  (cl-letf (((symbol-function 'cj/--auth-source-password)
             (lambda (&rest _) (ert-fail "auth-source consulted for a keyless backend"))))
    (should (eq (cj/--build-process-environment 'ratio) process-environment))))

(ert-deftest test-transcription-config-switcher-offers-ratio ()
  "Normal: the interactive switcher lists ratio, so the backend is reachable
without a setq."
  (should (member "ratio" (test-transcription-ratio--switcher-choices))))

;;; Boundary

(ert-deftest test-transcription-config-switcher-matches-descriptors ()
  "Boundary: the switcher offers exactly the descriptor set.  It derives the
list from the alist now; this is what stops a hardcoded copy coming back."
  (should (equal (sort (test-transcription-ratio--switcher-choices) #'string<)
                 (sort (mapcar (lambda (entry) (symbol-name (car entry)))
                               cj/--transcription-backends)
                       #'string<))))

(ert-deftest test-transcription-config-every-descriptor-script-exists ()
  "Boundary: every descriptor names a script that exists under scripts/.
A descriptor for a script that never landed (a hosted alternative referenced
from another repo, say) would fail at transcription time instead."
  (dolist (entry cj/--transcription-backends)
    (let ((cj/transcribe-backend (car entry)))
      (should (file-exists-p (cj/--transcription-script-path))))))

;;; Error

(ert-deftest test-transcription-config-unknown-backend-still-signals ()
  "Error: adding ratio did not loosen the descriptor lookup; an unknown
backend still signals `user-error'."
  (should-error (cj/--backend-plist 'no-such-backend) :type 'user-error))

(provide 'test-transcription-config--ratio-backend)
;;; test-transcription-config--ratio-backend.el ends here
