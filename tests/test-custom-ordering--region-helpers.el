;;; test-custom-ordering--region-helpers.el --- Tests for the shared ordering region helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--ordering-validate-region and cj/--ordering-replace-region were extracted
;; from the seven pure ordering helpers (the copy-pasted start>end guard) and the
;; interactive ordering commands (the copy-pasted delete-region + insert tail).
;; The per-command behavior stays covered by the existing wrapper/transform
;; tests; these cover the extracted helpers directly.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-ordering)

;;; cj/--ordering-validate-region

(ert-deftest test-custom-ordering-validate-region-accepts-ordered ()
  "Normal: start < end returns nil without signalling."
  (should (null (cj/--ordering-validate-region 1 10))))

(ert-deftest test-custom-ordering-validate-region-accepts-equal ()
  "Boundary: start = end (empty region) is allowed."
  (should (null (cj/--ordering-validate-region 5 5))))

(ert-deftest test-custom-ordering-validate-region-rejects-inverted ()
  "Error: start > end signals with both positions in the message."
  (let ((err (should-error (cj/--ordering-validate-region 10 3) :type 'error)))
    (should (string-match-p "10" (error-message-string err)))
    (should (string-match-p "3" (error-message-string err)))))

;;; cj/--ordering-replace-region

(ert-deftest test-custom-ordering-replace-region-swaps-text ()
  "Normal: the region between START and END is replaced with INSERTION and
point is left at START."
  (with-temp-buffer
    (insert "AAAABBBB")
    (cj/--ordering-replace-region 1 5 "xx")   ; replace the first AAAA
    (should (equal "xxBBBB" (buffer-string)))
    (should (= (point) 3))))                  ; START (1) + len("xx")

(ert-deftest test-custom-ordering-replace-region-empty-insertion ()
  "Boundary: an empty INSERTION just deletes the region."
  (with-temp-buffer
    (insert "keepDROP")
    (cj/--ordering-replace-region 5 9 "")     ; drop "DROP" (positions 5-8)
    (should (equal "keep" (buffer-string)))))

(provide 'test-custom-ordering--region-helpers)
;;; test-custom-ordering--region-helpers.el ends here
