;;; test-system-lib--format-region-with-program.el --- Tests for cj/format-region-with-program -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/format-region-with-program' runs an external formatter over the whole
;; buffer via `call-process-region' (argv, no shell) and replaces the buffer
;; only when the program exits zero.  Extracted from the byte-identical
;; per-language helpers in prog-json.el / prog-yaml.el, so this is the first
;; direct unit coverage of the logic.  call-process-region is mocked at the
;; boundary (the established pattern in test-prog-json--json-format-buffer.el).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-lib)

(ert-deftest test-system-lib-format-region-with-program-replaces-on-success ()
  "Normal: on exit 0 the buffer is replaced with the program's output, returns t."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (_start _end _prog &rest rest)
               (with-current-buffer (nth 1 rest) (insert "FORMATTED"))
               0)))
    (with-temp-buffer
      (insert "raw")
      (should (eq t (cj/format-region-with-program "fmt")))
      (should (equal "FORMATTED" (buffer-string))))))

(ert-deftest test-system-lib-format-region-with-program-forwards-argv ()
  "Normal: PROGRAM and ARGS reach call-process-region as argv (no shell)."
  (let (got-prog got-args)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (_start _end prog &rest rest)
                 (setq got-prog prog
                       got-args (nthcdr 3 rest))
                 (with-current-buffer (nth 1 rest) (insert "x"))
                 0)))
      (with-temp-buffer
        (cj/format-region-with-program "jq" "--sort-keys" ".")))
    (should (equal "jq" got-prog))
    (should (equal '("--sort-keys" ".") got-args))))

(ert-deftest test-system-lib-format-region-with-program-empty-output ()
  "Boundary: empty program output empties the buffer and still returns t."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (_start _end _prog &rest _rest) 0)))   ; writes nothing
    (with-temp-buffer
      (insert "raw")
      (should (eq t (cj/format-region-with-program "fmt")))
      (should (equal "" (buffer-string))))))

(ert-deftest test-system-lib-format-region-with-program-nonzero-untouched ()
  "Error: a non-zero exit leaves the buffer untouched and signals user-error
carrying the program's stderr text."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (_start _end _prog &rest rest)
               (with-current-buffer (nth 1 rest) (insert "boom: bad input"))
               1)))
    (with-temp-buffer
      (insert "raw")
      (let ((err (should-error (cj/format-region-with-program "fmt")
                               :type 'user-error)))
        (should (string-match-p "boom: bad input" (error-message-string err))))
      (should (equal "raw" (buffer-string))))))

(provide 'test-system-lib--format-region-with-program)
;;; test-system-lib--format-region-with-program.el ends here
