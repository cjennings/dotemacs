;;; test-show-kill-ring--insert-item.el --- Tests for show-kill-insert-item -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for `show-kill-insert-item' in show-kill-ring.el — inserts a
;; kill-ring entry into the current buffer, truncating to
;; `show-kill-max-item-size' with an ellipsis when too long.  The ellipsis
;; sits inline for short items and on its own line for items wider than the
;; frame.  Frame width is read at runtime so the test is environment-stable.

;;; Code:

(require 'ert)
(require 'show-kill-ring)

;;; Normal Cases

(ert-deftest test-show-kill-ring-insert-item-short-verbatim ()
  "Normal: an item shorter than the max is inserted unchanged."
  (let ((show-kill-max-item-size 1000))
    (with-temp-buffer
      (show-kill-insert-item "hello")
      (should (string= (buffer-string) "hello")))))

(ert-deftest test-show-kill-ring-insert-item-inline-ellipsis ()
  "Normal: an over-max item narrower than the frame gets an inline ellipsis."
  (let* ((show-kill-max-item-size 5)
         (len (/ (frame-width) 2))        ; > max, < (frame-width - 5)
         (item (make-string len ?b)))
    (with-temp-buffer
      (show-kill-insert-item item)
      (should (string= (buffer-string) "bbbbb...")))))

;;; Boundary Cases

(ert-deftest test-show-kill-ring-insert-item-length-equals-max-truncates ()
  "Boundary: length exactly equal to max truncates — the guard is (< len max)."
  (let ((show-kill-max-item-size 5))
    (with-temp-buffer
      (show-kill-insert-item "hello")     ; length 5, equals max
      (should (string= (buffer-string) "hello...")))))

(ert-deftest test-show-kill-ring-insert-item-wide-newline-ellipsis ()
  "Boundary: an item wider than the frame puts the ellipsis on its own line."
  (let* ((show-kill-max-item-size 5)
         (item (make-string (+ (frame-width) 10) ?a)))
    (with-temp-buffer
      (show-kill-insert-item item)
      (should (string= (buffer-string) "aaaaa\n...")))))

(ert-deftest test-show-kill-ring-insert-item-max-nil-verbatim ()
  "Boundary: a non-numeric max disables truncation."
  (let ((show-kill-max-item-size nil))
    (with-temp-buffer
      (show-kill-insert-item "anything long enough to exceed nothing")
      (should (string= (buffer-string)
                       "anything long enough to exceed nothing")))))

(ert-deftest test-show-kill-ring-insert-item-max-negative-verbatim ()
  "Boundary: a negative max disables truncation."
  (let ((show-kill-max-item-size -1))
    (with-temp-buffer
      (show-kill-insert-item "abc")
      (should (string= (buffer-string) "abc")))))

(ert-deftest test-show-kill-ring-insert-item-empty-string ()
  "Boundary: an empty item inserts nothing and does not error."
  (let ((show-kill-max-item-size 1000))
    (with-temp-buffer
      (show-kill-insert-item "")
      (should (string= (buffer-string) "")))))

(provide 'test-show-kill-ring--insert-item)
;;; test-show-kill-ring--insert-item.el ends here
