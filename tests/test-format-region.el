;;; test-format-region.el --- tests for cj/format-region-or-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;;  Some basic tests for the custom function cj/format-region-or-buffer in custom-functions.el

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'custom-functions)

;; ----------------------------- Utility Functions -----------------------------

(defun buffer-string-no-properties ()
  "Return the contents of the current buffer without any text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

;; ----------------------------------- Tests -----------------------------------

(ert-deftest cj/format-region-or-buffer-test/region ()
  "Test cj/format-region-or-buffer on a selected region."
  (with-temp-buffer
    (insert "     line with leading spaces and a    \n     tab\there")
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (point-max))
    (cj/format-region-or-buffer)
    ;; expected: trailing whitespace and leading spaces are removed, tabs are replaced by spaces
    (should (string= (buffer-string-no-properties) "line with leading spaces and a\ntab here"))))

(ert-deftest cj/format-region-or-buffer-test/whole-buffer ()
  "Test cj/format-region-or-buffer on an entire buffer."
  (with-temp-buffer
    (insert "    \n\t\n     line with leading spaces and a    \n     tab\there")
    ;; expected: trailing and leading whitespace of buffer and lines are removed, tabs are replaced by spaces
    (cj/format-region-or-buffer)
    (should (string= (buffer-string-no-properties) "\nline with leading spaces and a\ntab here"))))

(ert-deftest cj/format-region-or-buffer-test/extreme-buffer-size ()
  "Tests cj/format-region-or-buffer on an very large buffer."
  (with-temp-buffer
    (insert (make-string most-positive-fixnum ?\s))
    (cj/format-region-or-buffer)
    ;; expected: even large buffers should not cause an error
    (should (string= (buffer-string-no-properties) ""))))

(provide 'test-format-region)
;;; test-format-region.el ends here.
