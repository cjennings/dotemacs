;;; test-music-config--completion-table.el --- Tests for completion table generation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--completion-table function.
;; Tests the completion table generator that creates custom completion tables.
;;
;; Test organization:
;; - Normal Cases: Metadata, completions, case-insensitive matching
;; - Boundary Cases: Empty candidates, partial matching, exact matches
;; - Error Cases: Nil candidates
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Load production code
(require 'music-config)

;;; Normal Cases

(ert-deftest test-music-config--completion-table-normal-metadata-action-returns-metadata ()
  "Completion table returns metadata when action is 'metadata."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "" nil 'metadata)))
    (should (eq (car result) 'metadata))
    ;; Check metadata contains expected properties
    (should (equal (alist-get 'display-sort-function (cdr result)) 'identity))
    (should (equal (alist-get 'cycle-sort-function (cdr result)) 'identity))
    (should (eq (alist-get 'completion-ignore-case (cdr result)) t))))

(ert-deftest test-music-config--completion-table-normal-t-action-returns-all-completions ()
  "Completion table returns all matching completions when action is t."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "" nil t)))
    ;; Empty string should match all candidates
    (should (equal (sort result #'string<) '("Classical" "Jazz" "Rock")))))

(ert-deftest test-music-config--completion-table-normal-nil-action-tries-completion ()
  "Completion table tries completion when action is nil."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "Roc" nil nil)))
    ;; Should return completion attempt for "Roc" -> "Rock"
    (should (stringp result))
    (should (string-prefix-p "Roc" result))))

(ert-deftest test-music-config--completion-table-normal-case-insensitive-metadata ()
  "Completion table metadata indicates case-insensitive completion."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (metadata (funcall table "" nil 'metadata)))
    ;; Metadata should indicate case-insensitive
    (should (eq (alist-get 'completion-ignore-case (cdr metadata)) t))))

;;; Boundary Cases

(ert-deftest test-music-config--completion-table-boundary-empty-candidates ()
  "Completion table with empty candidate list returns no completions."
  (let* ((candidates '())
         (table (cj/music--completion-table candidates))
         (result (funcall table "anything" nil t)))
    (should (null result))))

(ert-deftest test-music-config--completion-table-boundary-single-candidate ()
  "Completion table with single candidate returns it on match."
  (let* ((candidates '("OnlyOne"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "Only" nil t)))
    (should (equal result '("OnlyOne")))))

(ert-deftest test-music-config--completion-table-boundary-partial-matching ()
  "Completion table matches multiple candidates with common prefix."
  (let* ((candidates '("playlist1" "playlist2" "jazz"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "play" nil t)))
    (should (= (length result) 2))
    (should (member "playlist1" result))
    (should (member "playlist2" result))
    (should-not (member "jazz" result))))

(ert-deftest test-music-config--completion-table-boundary-no-matches ()
  "Completion table returns empty when no candidates match."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "Metal" nil t)))
    (should (null result))))

(ert-deftest test-music-config--completion-table-boundary-exact-match ()
  "Completion table returns t for exact match with nil action."
  (let* ((candidates '("Rock" "Jazz" "Classical"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "Jazz" nil nil)))
    ;; Exact match with nil action returns t
    (should (eq result t))))

(ert-deftest test-music-config--completion-table-boundary-mixed-case-candidates ()
  "Completion table with mixed-case duplicate candidates."
  (let* ((candidates '("Rock" "ROCK" "rock"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "R" nil t)))
    ;; All start with "R", but exact case matters for complete-with-action
    ;; Only exact case match "R" prefix
    (should (member "Rock" result))
    (should (member "ROCK" result))
    ;; "rock" doesn't match "R" prefix (lowercase)
    (should-not (member "rock" result))))

(ert-deftest test-music-config--completion-table-boundary-unicode-candidates ()
  "Completion table handles unicode characters in candidates."
  (let* ((candidates '("中文" "日本語" "한국어"))
         (table (cj/music--completion-table candidates))
         (result (funcall table "中" nil t)))
    (should (member "中文" result))))

;;; Error Cases

(ert-deftest test-music-config--completion-table-error-nil-candidates-handles-gracefully ()
  "Completion table with nil candidates handles gracefully."
  (let* ((candidates nil)
         (table (cj/music--completion-table candidates))
         (result (funcall table "anything" nil t)))
    ;; Should not crash, returns empty
    (should (null result))))

(provide 'test-music-config--completion-table)
;;; test-music-config--completion-table.el ends here
