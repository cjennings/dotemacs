;;; test-gptel-tools-read-text-file.el --- Tests for read_text_file gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helpers in read_text_file.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'read_text_file)

;; -------------------------- helpers

(defun test-gptel-tools-read-text-file--in-home (suffix content fn)
  "Run FN with a temp file (containing CONTENT) under HOME using SUFFIX."
  (let* ((name (format ".test-gptel-tools-read-text-file-%s-%s.tmp"
                       suffix (format-time-string "%s%N")))
         (path (expand-file-name name "~")))
    (unwind-protect
        (progn
          (with-temp-file path (insert content))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path)))))

;; -------------------------- validate-file-path

(ert-deftest test-gptel-tools-read-text-file-validate-path-normal ()
  "Normal: an existing readable file under HOME passes."
  (test-gptel-tools-read-text-file--in-home
   "normal" "hi"
   (lambda (path)
     (should (equal (cj/validate-file-path path) (file-truename path))))))

(ert-deftest test-gptel-tools-read-text-file-validate-path-error-outside-home ()
  "Error: path outside HOME signals."
  (should-error (cj/validate-file-path "/etc/hostname")))

(ert-deftest test-gptel-tools-read-text-file-validate-path-error-missing ()
  "Error: missing file signals."
  (let ((path (expand-file-name
               (format ".test-gptel-tools-read-text-file-missing-%s.tmp"
                       (format-time-string "%s%N"))
               "~")))
    (when (file-exists-p path) (delete-file path))
    (should-error (cj/validate-file-path path))))

(ert-deftest test-gptel-tools-read-text-file-validate-path-error-directory ()
  "Error: a directory signals."
  (should-error (cj/validate-file-path "~")))

;; -------------------------- get-file-metadata

(ert-deftest test-gptel-tools-read-text-file-get-metadata-shape ()
  "Returns a plist with :size and :string keys."
  (test-gptel-tools-read-text-file--in-home
   "meta" "abc"
   (lambda (path)
     (let ((meta (cj/get-file-metadata path)))
       (should (plist-get meta :size))
       (should (= 3 (plist-get meta :size)))
       (should (stringp (plist-get meta :string)))
       (should (string-match-p "modified" (plist-get meta :string)))))))

;; -------------------------- check-file-size-limits

(ert-deftest test-gptel-tools-read-text-file-size-limits-normal ()
  "Small size below warning limit is a no-op."
  (should-not (cj/check-file-size-limits 1024 nil)))

(ert-deftest test-gptel-tools-read-text-file-size-limits-error-hard-cap ()
  "Sizes above 100MB always signal."
  (should-error (cj/check-file-size-limits (* 101 1024 1024) t))
  (should-error (cj/check-file-size-limits (* 101 1024 1024) nil)))

(ert-deftest test-gptel-tools-read-text-file-size-limits-warning-with-no-confirm ()
  "Above 10MB but below 100MB with no-confirm passes through silently."
  (should-not (cj/check-file-size-limits (* 11 1024 1024) t)))

;; -------------------------- detect-binary-file

(ert-deftest test-gptel-tools-read-text-file-detect-binary-text-file ()
  "Text file: detect-binary returns nil."
  (test-gptel-tools-read-text-file--in-home
   "text" "plain ascii content"
   (lambda (path)
     (should-not (cj/detect-binary-file path)))))

(ert-deftest test-gptel-tools-read-text-file-detect-binary-with-null-byte ()
  "File with NUL in first 1024 bytes returns truthy."
  (test-gptel-tools-read-text-file--in-home
   "bin" (concat "head\0tail")
   (lambda (path)
     (should (cj/detect-binary-file path)))))

;; -------------------------- handle-special-file-types

(ert-deftest test-gptel-tools-read-text-file-handle-special-epub-error ()
  "EPUB special-type handler signals \"not yet implemented\"."
  (should-error (cj/handle-special-file-types "/tmp/foo.epub" t)))

(ert-deftest test-gptel-tools-read-text-file-handle-special-binary-returns-nil ()
  "Generic binary file with no-confirm returns nil to indicate normal read."
  (should-not (cj/handle-special-file-types "/tmp/foo.bin" t)))

(provide 'test-gptel-tools-read-text-file)
;;; test-gptel-tools-read-text-file.el ends here
