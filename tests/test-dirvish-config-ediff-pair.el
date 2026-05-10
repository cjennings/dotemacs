;;; test-dirvish-config-ediff-pair.el --- Tests for the ediff-pair helper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--ediff-pair-from-files' picks the (FILE1 . FILE2) pair that
;; `cj/dired-ediff-files' should hand to `ediff-files'.  The pair is
;; (older . newer) so ediff renders the older revision on the left.
;; The helper takes a prompt callback (used when only one file is
;; marked) and a "newer than" comparator, so tests stay independent of
;; real mtimes and the dired prompt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(ert-deftest test-cj--ediff-pair-two-files-first-is-newer ()
  "Normal: file1 newer than file2 -> pair is (file2 . file1) so older runs first."
  (let ((newer-fn (lambda (a _b) (string= a "/a"))))
    (should (equal (cj/--ediff-pair-from-files
                    '("/a" "/b") (lambda () "/unused") newer-fn)
                   '("/b" . "/a")))))

(ert-deftest test-cj--ediff-pair-two-files-second-is-newer ()
  "Normal: file1 older than file2 -> pair preserves the input order."
  (let ((newer-fn (lambda (_a _b) nil)))
    (should (equal (cj/--ediff-pair-from-files
                    '("/a" "/b") (lambda () "/unused") newer-fn)
                   '("/a" . "/b")))))

(ert-deftest test-cj--ediff-pair-one-file-calls-prompt ()
  "Boundary: a single marked file triggers the prompt callback for the second."
  (let ((prompt-calls 0)
        (newer-fn (lambda (_a _b) nil)))
    (let ((pair (cj/--ediff-pair-from-files
                 '("/only")
                 (lambda () (cl-incf prompt-calls) "/picked")
                 newer-fn)))
      (should (= prompt-calls 1))
      (should (equal pair '("/only" . "/picked"))))))

(ert-deftest test-cj--ediff-pair-three-files-signals-user-error ()
  "Error: more than two files signals user-error without prompting."
  (let ((prompt-calls 0))
    (should-error
     (cj/--ediff-pair-from-files
      '("/a" "/b" "/c")
      (lambda () (cl-incf prompt-calls) "/x")
      (lambda (_a _b) nil))
     :type 'user-error)
    (should (zerop prompt-calls))))

(ert-deftest test-cj--ediff-pair-zero-files-signals-user-error ()
  "Error: zero files signals user-error -- regression for the latent crash
where `cj/dired-ediff-files' fell through to `(file-newer-than-file-p nil ...)'."
  (should-error
   (cj/--ediff-pair-from-files
    '() (lambda () "/x") (lambda (_a _b) nil))
   :type 'user-error))

(provide 'test-dirvish-config-ediff-pair)
;;; test-dirvish-config-ediff-pair.el ends here
