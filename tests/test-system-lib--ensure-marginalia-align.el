;;; test-system-lib--ensure-marginalia-align.el --- Tests for marginalia category registration -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Custom completion categories (cj-music-file, cj-radio-station, and every
;; category passed to the system-lib table helpers) bypass marginalia, so
;; their annotations never get its right-alignment even with marginalia-align
;; set.  The registration helper adds a builtin entry per category so the
;; table's own annotation function renders through marginalia's aligned field.

;;; Code:

(require 'ert)

;; The module's bare defvar marks this special only file-locally; declare it
;; here too so `let' binds dynamically (the scope-shadowing trap).
(defvar marginalia-annotator-registry)

(require 'system-lib)

;;; Normal Cases

(ert-deftest test-system-lib-ensure-marginalia-align-registers-category ()
  "Normal: an unregistered category gains a builtin registry entry."
  (let ((marginalia-annotator-registry '((file some-annotator builtin none))))
    (cj/completion-ensure-marginalia-align 'cj-test-category)
    (should (equal (assq 'cj-test-category marginalia-annotator-registry)
                   '(cj-test-category builtin none)))))

(ert-deftest test-system-lib-ensure-marginalia-align-idempotent ()
  "Normal: registering the same category twice leaves one entry."
  (let ((marginalia-annotator-registry '()))
    (cj/completion-ensure-marginalia-align 'cj-test-category)
    (cj/completion-ensure-marginalia-align 'cj-test-category)
    (should (= 1 (length marginalia-annotator-registry)))))

;;; Boundary Cases

(ert-deftest test-system-lib-ensure-marginalia-align-preserves-existing-entry ()
  "Boundary: a category with an existing (possibly custom) entry is untouched."
  (let ((marginalia-annotator-registry '((cj-test-category my-custom-annotator))))
    (cj/completion-ensure-marginalia-align 'cj-test-category)
    (should (equal (assq 'cj-test-category marginalia-annotator-registry)
                   '(cj-test-category my-custom-annotator)))))

;;; Error Cases

(ert-deftest test-system-lib-ensure-marginalia-align-marginalia-absent-noop ()
  "Error: without marginalia loaded (registry void) the helper is a silent
no-op -- annotations just stay unaligned, nothing breaks."
  ;; marginalia is not loadable in the batch environment, so the global
  ;; registry is genuinely void outside the `let's above.
  (should-not (cj/completion-ensure-marginalia-align 'cj-test-category)))

(provide 'test-system-lib--ensure-marginalia-align)
;;; test-system-lib--ensure-marginalia-align.el ends here
