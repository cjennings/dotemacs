;;; test-gptel-tools-list-directory-files.el --- Tests for list_directory_files -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helpers in list_directory_files.el.

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

(require 'list_directory_files)

;; -------------------------- helpers

(defun test-gptel-tools-list--with-tree (fn)
  "Create a small directory tree, call FN with its root, clean up."
  (let ((root (make-temp-file "test-gptel-tools-list-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" root) (insert "a"))
          (with-temp-file (expand-file-name "b.org" root) (insert "b"))
          (make-directory (expand-file-name "sub" root))
          (with-temp-file (expand-file-name "sub/c.txt" root) (insert "c"))
          (funcall fn root))
      (delete-directory root t))))

;; -------------------------- mode-to-permissions

(ert-deftest test-gptel-tools-list-mode-to-permissions-regular-file ()
  "Mode 0644 on a regular file: -rw-r--r--."
  (should (equal (list-directory-files--mode-to-permissions #o0644)
                 "-rw-r--r--")))

(ert-deftest test-gptel-tools-list-mode-to-permissions-directory ()
  "Mode 0755 + dir bit: drwxr-xr-x."
  (should (equal (list-directory-files--mode-to-permissions
                  (logior #o40000 #o0755))
                 "drwxr-xr-x")))

(ert-deftest test-gptel-tools-list-mode-to-permissions-executable ()
  "Mode 0700: -rwx------."
  (should (equal (list-directory-files--mode-to-permissions #o0700)
                 "-rwx------")))

;; -------------------------- get-file-info

(ert-deftest test-gptel-tools-list-get-file-info-success ()
  "Success: returns a plist with :success t and metadata."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let ((info (list-directory-files--get-file-info
                  (expand-file-name "a.txt" root))))
       (should (plist-get info :success))
       (should (numberp (plist-get info :size)))
       (should (stringp (plist-get info :permissions)))))))

(ert-deftest test-gptel-tools-list-get-file-info-directory ()
  "Directory info: :is-directory is t."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let ((info (list-directory-files--get-file-info
                  (expand-file-name "sub" root))))
       (should (plist-get info :is-directory))))))

(ert-deftest test-gptel-tools-list-get-file-info-error ()
  "Error: metadata failures are returned as failed info plists."
  (cl-letf (((symbol-function 'file-attributes)
             (lambda (&rest _args) (error "stat failed"))))
    (let ((info (list-directory-files--get-file-info "/tmp/nope")))
      (should-not (plist-get info :success))
      (should (string-match-p "stat failed" (plist-get info :error))))))

;; -------------------------- filter-by-extension

(ert-deftest test-gptel-tools-list-filter-by-extension-keeps-match ()
  "Filter for txt keeps txt files."
  (let* ((filter (list-directory-files--filter-by-extension "txt"))
         (info '(:success t :path "/x/foo.txt" :is-directory nil)))
    (should (funcall filter info))))

(ert-deftest test-gptel-tools-list-filter-by-extension-drops-non-match ()
  "Filter for txt drops non-txt files."
  (let* ((filter (list-directory-files--filter-by-extension "txt"))
         (info '(:success t :path "/x/foo.org" :is-directory nil)))
    (should-not (funcall filter info))))

(ert-deftest test-gptel-tools-list-filter-by-extension-always-keeps-directories ()
  "Filter keeps directories regardless of extension."
  (let* ((filter (list-directory-files--filter-by-extension "txt"))
         (info '(:success t :path "/x/sub" :is-directory t)))
    (should (funcall filter info))))

(ert-deftest test-gptel-tools-list-filter-by-extension-no-extension-is-nil ()
  "No extension produces a nil filter (i.e. no filtering)."
  (should-not (list-directory-files--filter-by-extension nil)))

(ert-deftest test-gptel-tools-list-filter-by-extension-case-insensitive ()
  "Boundary: extension filtering is case-insensitive."
  (let* ((filter (list-directory-files--filter-by-extension "txt"))
         (info '(:success t :path "/x/FOO.TXT" :is-directory nil)))
    (should (funcall filter info))))

(ert-deftest test-gptel-tools-list-filter-by-extension-drops-failed-file-info ()
  "Boundary: failed file info entries do not pass file extension filters."
  (let* ((filter (list-directory-files--filter-by-extension "txt"))
         (info '(:success nil :path "/x/foo.txt" :is-directory nil)))
    (should-not (funcall filter info))))

;; -------------------------- format-file-entry

(ert-deftest test-gptel-tools-list-format-file-entry-shape ()
  "Formatted entry contains permissions, size, mtime, and relative path."
  (let* ((info (list (cons :path "/home/u/foo.txt")
                     (cons :permissions "-rw-r--r--")
                     (cons :executable nil)
                     (cons :size 42)
                     (cons :last-modified (current-time))))
         ;; Build as plist by flattening the cons list.
         (info-plist (cl-loop for (k . v) in info append (list k v)))
         (out (list-directory-files--format-file-entry info-plist "/home/u")))
    (should (string-match-p "-rw-r--r--" out))
    (should (string-match-p "foo.txt" out))))

;; -------------------------- list-directory

(ert-deftest test-gptel-tools-list-list-directory-flat ()
  "Non-recursive listing returns only entries in the top level."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((result (list-directory-files--list-directory root nil nil))
            (files (plist-get result :files)))
       (should files)
       (let ((paths (mapcar (lambda (i) (plist-get i :path)) files)))
         (should (cl-some (lambda (p) (string-match-p "/a\\.txt\\'" p)) paths))
         (should-not (cl-some (lambda (p) (string-match-p "/c\\.txt\\'" p)) paths)))))))

(ert-deftest test-gptel-tools-list-list-directory-recursive ()
  "Recursive listing also returns sub-directory contents."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((result (list-directory-files--list-directory root t nil))
            (files (plist-get result :files))
            (paths (mapcar (lambda (i) (plist-get i :path)) files)))
       (should (cl-some (lambda (p) (string-match-p "/c\\.txt\\'" p)) paths))))))

(ert-deftest test-gptel-tools-list-list-directory-max-depth ()
  "Boundary: max-depth limits recursive traversal."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((result (list-directory-files--list-directory root t nil 0))
            (files (plist-get result :files))
            (paths (mapcar (lambda (i) (plist-get i :path)) files)))
       (should-not (cl-some (lambda (p) (string-match-p "/c\\.txt\\'" p)) paths))))))

(ert-deftest test-gptel-tools-list-list-directory-filtered-recursive-keeps-matching-files ()
  "Normal: recursive extension filter returns matching nested files."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((filter (list-directory-files--filter-by-extension "txt"))
            (result (list-directory-files--list-directory root t filter))
            (files (plist-get result :files))
            (paths (mapcar (lambda (i) (plist-get i :path)) files)))
       (should (cl-some (lambda (p) (string-match-p "/a\\.txt\\'" p)) paths))
       (should (cl-some (lambda (p) (string-match-p "/c\\.txt\\'" p)) paths))
       (should-not (cl-some (lambda (p) (string-match-p "/b\\.org\\'" p)) paths))))))

(ert-deftest test-gptel-tools-list-list-directory-records-entry-errors ()
  "Error: per-entry metadata failures are collected."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (cl-letf (((symbol-function 'list-directory-files--get-file-info)
                (lambda (path)
                  (if (string-match-p "/a\\.txt\\'" path)
                      (list :success nil :path path :error "denied")
                    (let* ((attrs (file-attributes path 'string))
                           (dirp (eq t (file-attribute-type attrs))))
                      (list :success t
                            :path path
                            :size 0
                            :last-modified (current-time)
                            :is-directory dirp
                            :permissions "-rw-r--r--"
                            :executable nil))))))
       (let ((errors (plist-get (list-directory-files--list-directory root nil nil)
                                :errors)))
         (should errors)
         (should (string-match-p "denied" (car errors))))))))

(ert-deftest test-gptel-tools-list-list-directory-error-not-a-directory ()
  "Non-directory path returns errors entry."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((result (list-directory-files--list-directory
                     (expand-file-name "a.txt" root) nil nil))
            (errors (plist-get result :errors)))
       (should errors)))))

(ert-deftest test-gptel-tools-list-list-directory-error-accessing-directory ()
  "Error: directory access failures are collected."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (cl-letf (((symbol-function 'directory-files)
                (lambda (&rest _args) (error "cannot list"))))
       (let ((errors (plist-get (list-directory-files--list-directory root nil nil)
                                :errors)))
         (should errors)
         (should (string-match-p "cannot list" (car errors))))))))

;; -------------------------- format-output

(ert-deftest test-gptel-tools-list-format-output-has-files-section ()
  "Format-output includes a \"Found N file(s)\" line when files present."
  (test-gptel-tools-list--with-tree
   (lambda (root)
     (let* ((result (list-directory-files--list-directory root nil nil))
            (out (list-directory-files--format-output root result)))
       (should (string-match-p "Found [0-9]+ file" out))))))

(ert-deftest test-gptel-tools-list-format-output-empty ()
  "Empty result: \"No files found\"."
  (let ((out (list-directory-files--format-output
              "/nowhere" '(:files nil :errors nil))))
    (should (string-match-p "No files found" out))))

(ert-deftest test-gptel-tools-list-format-output-errors-only ()
  "Format-output includes errors when no files are present."
  (let ((out (list-directory-files--format-output
              "/nowhere" '(:files nil :errors ("boom")))))
    (should (string-match-p "Errors encountered" out))
    (should (string-match-p "boom" out))))

(ert-deftest test-gptel-tools-list-format-output-files-and-errors ()
  "Format-output separates file listings and errors."
  (let* ((info (list :success t
                     :path (expand-file-name "foo.txt" "~")
                     :size 1
                     :last-modified (current-time)
                     :is-directory nil
                     :permissions "-rw-r--r--"
                     :executable nil))
         (out (list-directory-files--format-output
               "~" (list :files (list info) :errors (list "boom")))))
    (should (string-match-p "Found 1 file" out))
    (should (string-match-p "Errors encountered" out))))

(provide 'test-gptel-tools-list-directory-files)
;;; test-gptel-tools-list-directory-files.el ends here
