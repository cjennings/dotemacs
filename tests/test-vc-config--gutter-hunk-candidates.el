;;; test-vc-config--gutter-hunk-candidates.el --- Tests for cj/--git-gutter-hunk-candidates -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for cj/--git-gutter-hunk-candidates, the pure helper that
;; builds completion candidates (label . line) from git-gutter hunk start
;; lines against the current buffer's text.  The interactive wrapper
;; cj/goto-git-gutter-diff-hunks maps git-gutter:diffinfos onto start
;; lines and delegates here.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vc-config)

;;; Normal Cases

(ert-deftest test-vc-gutter-hunk-candidates-labels-carry-line-text ()
  "Normal: each candidate label contains the hunk line's text."
  (with-temp-buffer
    (insert "alpha\nbravo\ncharlie\n")
    (let ((candidates (cj/--git-gutter-hunk-candidates '(2 3))))
      (should (= 2 (length candidates)))
      (should (string-match-p "bravo" (car (nth 0 candidates))))
      (should (string-match-p "charlie" (car (nth 1 candidates)))))))

(ert-deftest test-vc-gutter-hunk-candidates-cdr-is-start-line ()
  "Normal: each candidate's cdr is the hunk's start line number."
  (with-temp-buffer
    (insert "alpha\nbravo\ncharlie\n")
    (let ((candidates (cj/--git-gutter-hunk-candidates '(1 3))))
      (should (equal '(1 3) (mapcar #'cdr candidates))))))

;;; Boundary Cases

(ert-deftest test-vc-gutter-hunk-candidates-empty-input-returns-nil ()
  "Boundary: no hunks produce no candidates."
  (with-temp-buffer
    (insert "alpha\n")
    (should (null (cj/--git-gutter-hunk-candidates '())))))

(ert-deftest test-vc-gutter-hunk-candidates-first-line ()
  "Boundary: a hunk on line 1 resolves to the first line's text."
  (with-temp-buffer
    (insert "alpha\nbravo\n")
    (let ((candidates (cj/--git-gutter-hunk-candidates '(1))))
      (should (string-match-p "alpha" (caar candidates)))
      (should (= 1 (cdar candidates))))))

(ert-deftest test-vc-gutter-hunk-candidates-unicode-line-text ()
  "Boundary: line text with unicode survives into the label."
  (with-temp-buffer
    (insert "naïve — 日本語\n")
    (let ((candidates (cj/--git-gutter-hunk-candidates '(1))))
      (should (string-match-p "日本語" (caar candidates))))))

;;; Error Cases

(ert-deftest test-vc-goto-git-gutter-diff-hunks-no-hunks-user-error ()
  "Error: the command signals `user-error' when the buffer has no hunks."
  (with-temp-buffer
    (setq-local git-gutter:diffinfos nil)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (cj/goto-git-gutter-diff-hunks) :type 'user-error))))

(provide 'test-vc-config--gutter-hunk-candidates)
;;; test-vc-config--gutter-hunk-candidates.el ends here
