;;; test-gptel-tools-move-to-trash.el --- Tests for move_to_trash gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helpers in move_to_trash.el.

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

(require 'move_to_trash)

;; -------------------------- helpers

(defun test-gptel-tools-trash--with-tmp-tree (fn)
  "Create a temp source dir and trash dir; run FN with both; clean up."
  (let* ((src (make-temp-file "test-gptel-tools-trash-src-" t))
         (trash (make-temp-file "test-gptel-tools-trash-dst-" t)))
    (unwind-protect
        (funcall fn src trash)
      (when (file-exists-p src) (delete-directory src t))
      (when (file-exists-p trash) (delete-directory trash t)))))

;; -------------------------- generate-unique-name

(ert-deftest test-gptel-tools-trash-generate-unique-name-no-conflict ()
  "No conflict: returns the plain base name in trash."
  (test-gptel-tools-trash--with-tmp-tree
   (lambda (_src trash)
     (let ((out (gptel--move-to-trash-generate-unique-name
                 "/anywhere/foo.txt" trash)))
       (should (equal (file-name-nondirectory out) "foo.txt"))))))

(ert-deftest test-gptel-tools-trash-generate-unique-name-conflict-timestamps ()
  "Name conflict: returns a name with a timestamp suffix."
  (test-gptel-tools-trash--with-tmp-tree
   (lambda (_src trash)
     (with-temp-file (expand-file-name "foo.txt" trash) (insert ""))
     (let* ((out (gptel--move-to-trash-generate-unique-name
                  "/anywhere/foo.txt" trash))
            (name (file-name-nondirectory out)))
       (should-not (equal name "foo.txt"))
       (should (string-match-p "\\`foo-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.txt\\'"
                               name))))))

(ert-deftest test-gptel-tools-trash-generate-unique-name-no-extension ()
  "Conflict on a name without extension: timestamp appended to the bare name."
  (test-gptel-tools-trash--with-tmp-tree
   (lambda (_src trash)
     (with-temp-file (expand-file-name "noext" trash) (insert ""))
     (let* ((out (gptel--move-to-trash-generate-unique-name
                  "/anywhere/noext" trash))
            (name (file-name-nondirectory out)))
       (should-not (equal name "noext"))
       (should (string-match-p "\\`noext-[0-9]" name))))))

;; -------------------------- validate-path

(ert-deftest test-gptel-tools-trash-validate-path-normal-home ()
  "Normal: an existing path under HOME validates."
  (let ((path (expand-file-name
               (format ".test-gptel-tools-trash-home-%s.tmp"
                       (format-time-string "%s%N"))
               "~")))
    (unwind-protect
        (progn
          (with-temp-file path (insert ""))
          (should (equal (gptel--move-to-trash-validate-path path)
                         (expand-file-name path))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest test-gptel-tools-trash-validate-path-normal-tmp ()
  "Normal: an existing path under /tmp validates."
  (let ((path (make-temp-file "test-gptel-tools-trash-tmpvalidate-")))
    (unwind-protect
        (should (equal (gptel--move-to-trash-validate-path path)
                       (expand-file-name path)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest test-gptel-tools-trash-validate-path-error-outside-allowed ()
  "Error: a path outside HOME or /tmp signals."
  (should-error (gptel--move-to-trash-validate-path "/etc/hostname")))

(ert-deftest test-gptel-tools-trash-validate-path-error-critical-dir ()
  "Error: critical directories (home root, .emacs.d, .config, /tmp) signal."
  (should-error (gptel--move-to-trash-validate-path "~"))
  (should-error (gptel--move-to-trash-validate-path "~/.emacs.d"))
  (should-error (gptel--move-to-trash-validate-path "~/.config"))
  (should-error (gptel--move-to-trash-validate-path "/tmp")))

(ert-deftest test-gptel-tools-trash-validate-path-error-missing ()
  "Error: missing path signals."
  (let ((path (expand-file-name
               (format ".test-gptel-tools-trash-missing-%s.tmp"
                       (format-time-string "%s%N"))
               "~")))
    (when (file-exists-p path) (delete-file path))
    (should-error (gptel--move-to-trash-validate-path path))))

;; -------------------------- perform

(ert-deftest test-gptel-tools-trash-perform-moves-file ()
  "Perform: moves the file out of the source dir into the trash dir."
  (test-gptel-tools-trash--with-tmp-tree
   (lambda (src trash)
     (let ((file (expand-file-name "doomed.txt" src)))
       (with-temp-file file (insert "trash me"))
       (let ((status (gptel--move-to-trash-perform file trash)))
         (should (string-match-p "moved to trash" status))
         (should-not (file-exists-p file))
         (should (file-exists-p (expand-file-name "doomed.txt" trash))))))))

(ert-deftest test-gptel-tools-trash-perform-handles-directory ()
  "Perform: moves a directory as a unit."
  (test-gptel-tools-trash--with-tmp-tree
   (lambda (src trash)
     (let ((dir (expand-file-name "subdir" src)))
       (make-directory dir)
       (with-temp-file (expand-file-name "inside.txt" dir) (insert "x"))
       (let ((status (gptel--move-to-trash-perform dir trash)))
         (should (string-match-p "Directory moved to trash" status))
         (should-not (file-exists-p dir))
         (should (file-exists-p (expand-file-name "subdir/inside.txt" trash))))))))

(provide 'test-gptel-tools-move-to-trash)
;;; test-gptel-tools-move-to-trash.el ends here
