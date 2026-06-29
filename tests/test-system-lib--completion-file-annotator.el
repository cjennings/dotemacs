;;; test-system-lib--completion-file-annotator.el --- Tests for cj/completion-file-annotator -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/completion-file-annotator', the annotation-function
;; factory used to annotate file-basename completion pickers with size and
;; modification date.

;;; Code:

(require 'ert)
(require 'system-lib)

(ert-deftest test-system-lib-completion-file-annotator-normal-file-shows-size-and-date ()
  "Normal: a regular file is annotated with a size and an ISO date."
  (let ((file (make-temp-file "cfa-test-" nil ".txt" "hello world")))
    (unwind-protect
        (let* ((annotate (cj/completion-file-annotator
                          (lambda (_cand) file)))
               (result (funcall annotate "anything")))
          (should (stringp result))
          ;; file-size-human-readable of 11 bytes is "11"
          (should (string-match-p "11" result))
          ;; ISO date for the file's mtime
          (should (string-match-p
                   (format-time-string "%Y-%m-%d"
                                       (file-attribute-modification-time
                                        (file-attributes file)))
                   result)))
      (delete-file file))))

(ert-deftest test-system-lib-completion-file-annotator-boundary-directory-marked-dir ()
  "Boundary: a directory candidate is annotated with the `dir' marker."
  (let ((dir (make-temp-file "cfa-dir-" t)))
    (unwind-protect
        (let* ((annotate (cj/completion-file-annotator (lambda (_c) dir)))
               (result (funcall annotate "d")))
          (should (stringp result))
          (should (string-match-p "dir" result)))
      (delete-directory dir t))))

(ert-deftest test-system-lib-completion-file-annotator-error-nil-path-returns-nil ()
  "Error: a candidate whose path-resolver returns nil yields no annotation."
  (let ((annotate (cj/completion-file-annotator (lambda (_c) nil))))
    (should (null (funcall annotate "missing")))))

(ert-deftest test-system-lib-completion-file-annotator-error-missing-file-returns-nil ()
  "Error: a path that does not exist yields no annotation."
  (let* ((path (expand-file-name "definitely-not-here-12345.txt"
                                 temporary-file-directory))
         (annotate (cj/completion-file-annotator (lambda (_c) path))))
    (should (null (funcall annotate "gone")))))

(provide 'test-system-lib--completion-file-annotator)
;;; test-system-lib--completion-file-annotator.el ends here
