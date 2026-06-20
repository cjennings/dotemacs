;;; test-custom-text-enclose--enclose-region-or-word.el --- Tests for the shared enclose dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--enclose-region-or-word is the dispatch+edit skeleton extracted from
;; cj/surround/wrap/unwrap-word-or-region (region target, else word at point,
;; else a no-target message).  The three commands stay covered by
;; test-custom-text-enclose-public-wrappers.el; these cover the helper directly,
;; including the custom and default no-target messages.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-text-enclose)

(ert-deftest test-cte-enclose-region-target ()
  "Normal: an active region is the target; TRANSFORM is applied to it."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "abc")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (cj/--enclose-region-or-word #'upcase))
    (should (equal (buffer-string) "ABC"))
    (should (= (point) 4))))            ; after the inserted "ABC" (start 1 + 3)

(ert-deftest test-cte-enclose-word-at-point-target ()
  "Normal: with no region, the word at point is the target."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))            ; point on "foo"
    (cj/--enclose-region-or-word (lambda (s) (concat "<" s ">")))
    (should (equal (buffer-string) "<foo> bar"))))

(ert-deftest test-cte-enclose-no-target-default-message ()
  "Boundary: no region and no word => default message, buffer untouched."
  (with-temp-buffer
    (insert "   ")                     ; whitespace, no word
    (goto-char (point-min))
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/--enclose-region-or-word #'upcase))
      (should (string-match-p "No word at point" msg))
      (should (equal (buffer-string) "   ")))))

(ert-deftest test-cte-enclose-no-target-custom-message ()
  "Boundary: a supplied NO-TARGET-MESSAGE overrides the default."
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/--enclose-region-or-word #'upcase "custom no-target text"))
      (should (equal msg "custom no-target text")))))

(provide 'test-custom-text-enclose--enclose-region-or-word)
;;; test-custom-text-enclose--enclose-region-or-word.el ends here
