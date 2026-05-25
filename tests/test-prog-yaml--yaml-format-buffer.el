;;; test-prog-yaml--yaml-format-buffer.el --- Tests for cj/yaml-format-buffer -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the cj/yaml-format-buffer function in prog-yaml.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'prog-yaml)

;;; argv path — process invocation (stubbed, no shell)

(ert-deftest test-prog-yaml--yaml-format-buffer-invokes-prettier-argv ()
  "Normal: with prettier present, the formatter calls it via argv, no shell."
  (let (program args)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end prog &rest rest)
                 (setq program prog
                       args (nthcdr 3 rest))
                 (with-current-buffer (nth 1 rest) (insert "key: value\n"))
                 0)))
      (with-temp-buffer
        (insert "key: value\n")
        (cj/yaml-format-buffer)))
    (should (equal "prettier" program))
    (should (equal '("--parser" "yaml") args))))

(ert-deftest test-prog-yaml--yaml-format-buffer-no-clobber-on-failure ()
  "Error: a non-zero prettier exit leaves the buffer untouched and errors."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
            ((symbol-function 'call-process-region)
             (lambda (_start _end _prog _delete buffer &rest _)
               (with-current-buffer buffer (insert "[error] bad yaml"))
               1)))
    (with-temp-buffer
      (insert "key: : bad\n")
      (should-error (cj/yaml-format-buffer) :type 'user-error)
      (should (string= (buffer-string) "key: : bad\n")))))

;;; Normal Cases

(ert-deftest test-prog-yaml--yaml-format-buffer-normal-fixes-indentation ()
  "Badly indented YAML is normalized to 2-space indent."
  (with-temp-buffer
    (insert "items:\n      - one\n      - two\n")
    (cj/yaml-format-buffer)
    (should (string= (buffer-string) "items:\n  - one\n  - two\n"))))

(ert-deftest test-prog-yaml--yaml-format-buffer-normal-nested-map ()
  "Nested map with inconsistent indentation is normalized."
  (with-temp-buffer
    (insert "server:\n    host: localhost\n    port: 8080\n")
    (cj/yaml-format-buffer)
    (should (string= (buffer-string) "server:\n  host: localhost\n  port: 8080\n"))))

(ert-deftest test-prog-yaml--yaml-format-buffer-normal-already-formatted ()
  "Already well-formatted YAML is unchanged."
  (let ((formatted "name: test\nversion: 1.0\n"))
    (with-temp-buffer
      (insert formatted)
      (cj/yaml-format-buffer)
      (should (string= (buffer-string) formatted)))))

(ert-deftest test-prog-yaml--yaml-format-buffer-normal-preserves-key-order ()
  "Key order is preserved (not sorted)."
  (with-temp-buffer
    (insert "zebra: 1\nalpha: 2\n")
    (cj/yaml-format-buffer)
    (should (string-match-p "zebra.*\nalpha" (buffer-string)))))

;;; Boundary Cases

(ert-deftest test-prog-yaml--yaml-format-buffer-boundary-empty-document ()
  "Empty YAML document formats without error."
  (with-temp-buffer
    (insert "")
    (cj/yaml-format-buffer)
    (should (string= (string-trim (buffer-string)) ""))))

(ert-deftest test-prog-yaml--yaml-format-buffer-boundary-single-key ()
  "Single key-value pair formats cleanly."
  (with-temp-buffer
    (insert "key: value\n")
    (cj/yaml-format-buffer)
    (should (string= (buffer-string) "key: value\n"))))

(ert-deftest test-prog-yaml--yaml-format-buffer-boundary-unicode ()
  "YAML with unicode values is preserved."
  (with-temp-buffer
    (insert "name: café\ncity: Zürich\n")
    (cj/yaml-format-buffer)
    (should (string-match-p "café" (buffer-string)))
    (should (string-match-p "Zürich" (buffer-string)))))

;;; Error Cases

(ert-deftest test-prog-yaml--yaml-format-buffer-error-no-prettier ()
  "Signals user-error when prettier is not found."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (with-temp-buffer
      (insert "key: value\n")
      (should-error (cj/yaml-format-buffer) :type 'user-error))))

(provide 'test-prog-yaml--yaml-format-buffer)
;;; test-prog-yaml--yaml-format-buffer.el ends here
