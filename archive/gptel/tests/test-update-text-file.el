;;; test-update-text-file.el --- Tests for update_text_file gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Normal / Boundary / Error tests for each operation in
;; gptel-tools/update_text_file.el, plus file-level wrapper tests.
;; The pure-string helpers carry most of the coverage; the wrapper
;; only adds the I/O surface (backup, write, validation).

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  ;; Stub gptel so the tool file can be loaded without the real package.
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'update_text_file)

;; ----------------------------------------------------- helpers

(defun test-update-text-file--with-temp (content fn)
  "Write CONTENT to a temp file, call FN with its path, then delete."
  (let ((path (make-temp-file "test-update-text-file-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert content))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path)))))

;; ----------------------------------------------------- replace

(ert-deftest test-update-text-file-replace-normal ()
  "Normal: replace all occurrences of the literal pattern."
  (should (equal (cj/update-text-file--replace "foo bar foo" "foo" "BAZ")
                 "BAZ bar BAZ")))

(ert-deftest test-update-text-file-replace-boundary-no-match ()
  "Boundary: pattern absent returns content unchanged."
  (should (equal (cj/update-text-file--replace "abc" "xyz" "QQ") "abc")))

(ert-deftest test-update-text-file-replace-boundary-special-chars ()
  "Boundary: regex metacharacters in pattern are treated as literals."
  (should (equal (cj/update-text-file--replace "a.b.c" "." "-") "a-b-c"))
  (should (equal (cj/update-text-file--replace "(x)(y)" "(x)" "_") "_(y)"))
  (should (equal (cj/update-text-file--replace "a$b" "$" "S") "aSb")))

(ert-deftest test-update-text-file-replace-boundary-unicode ()
  "Boundary: unicode in both pattern and replacement."
  (should (equal (cj/update-text-file--replace "café résumé" "café" "thé")
                 "thé résumé")))

(ert-deftest test-update-text-file-replace-boundary-replacement-with-backref-like ()
  "Boundary: replacement strings with \\1 etc. are literal, not back-refs."
  (should (equal (cj/update-text-file--replace "foo" "foo" "\\1bar")
                 "\\1bar")))

(ert-deftest test-update-text-file-replace-error-empty-pattern ()
  "Error: empty pattern signals."
  (should-error (cj/update-text-file--replace "abc" "" "x")))

(ert-deftest test-update-text-file-replace-error-nil-pattern ()
  "Error: nil pattern signals."
  (should-error (cj/update-text-file--replace "abc" nil "x")))

(ert-deftest test-update-text-file-replace-error-nil-replacement ()
  "Error: nil replacement signals."
  (should-error (cj/update-text-file--replace "abc" "a" nil)))

;; ----------------------------------------------------- append

(ert-deftest test-update-text-file-append-normal ()
  "Normal: append adds text plus a trailing newline."
  (should (equal (cj/update-text-file--append "line1\n" "line2")
                 "line1\nline2\n")))

(ert-deftest test-update-text-file-append-boundary-no-trailing-newline ()
  "Boundary: appends still produce a newline when content has none."
  (should (equal (cj/update-text-file--append "abc" "def")
                 "abc\ndef\n")))

(ert-deftest test-update-text-file-append-boundary-empty-content ()
  "Boundary: appending to empty content yields just the new text + newline."
  (should (equal (cj/update-text-file--append "" "hello") "hello\n")))

(ert-deftest test-update-text-file-append-boundary-text-with-trailing-newline ()
  "Boundary: text that already ends in newline isn't duplicated."
  (should (equal (cj/update-text-file--append "a\n" "b\n") "a\nb\n")))

(ert-deftest test-update-text-file-append-error-empty-text ()
  "Error: empty text signals."
  (should-error (cj/update-text-file--append "foo" "")))

(ert-deftest test-update-text-file-append-error-nil-text ()
  "Error: nil text signals."
  (should-error (cj/update-text-file--append "foo" nil)))

;; ----------------------------------------------------- prepend

(ert-deftest test-update-text-file-prepend-normal ()
  "Normal: prepend adds text plus a separator newline."
  (should (equal (cj/update-text-file--prepend "line1\n" "line0")
                 "line0\nline1\n")))

(ert-deftest test-update-text-file-prepend-boundary-empty-content ()
  "Boundary: prepending to empty content keeps just the new text + sep."
  (should (equal (cj/update-text-file--prepend "" "hello") "hello\n")))

(ert-deftest test-update-text-file-prepend-boundary-text-with-trailing-newline ()
  "Boundary: text already terminated by newline is not double-broken."
  (should (equal (cj/update-text-file--prepend "rest" "first\n")
                 "first\nrest")))

(ert-deftest test-update-text-file-prepend-error-empty-text ()
  "Error: empty text signals."
  (should-error (cj/update-text-file--prepend "foo" "")))

(ert-deftest test-update-text-file-prepend-error-nil-text ()
  "Error: nil text signals."
  (should-error (cj/update-text-file--prepend "foo" nil)))

;; ----------------------------------------------------- insert-at-line

(ert-deftest test-update-text-file-insert-at-line-normal ()
  "Normal: insert before line 2 of a 3-line file."
  (should (equal (cj/update-text-file--insert-at-line "a\nb\nc\n" 2 "X")
                 "a\nX\nb\nc\n")))

(ert-deftest test-update-text-file-insert-at-line-boundary-first-line ()
  "Boundary: inserting at line 1 prepends."
  (should (equal (cj/update-text-file--insert-at-line "a\nb\n" 1 "X")
                 "X\na\nb\n")))

(ert-deftest test-update-text-file-insert-at-line-boundary-one-past-end ()
  "Boundary: inserting one past the last line appends."
  (should (equal (cj/update-text-file--insert-at-line "a\nb\n" 3 "X")
                 "a\nb\nX\n")))

(ert-deftest test-update-text-file-insert-at-line-boundary-no-trailing-newline ()
  "Boundary: works on content without a trailing newline."
  (should (equal (cj/update-text-file--insert-at-line "a\nb" 2 "X")
                 "a\nX\nb")))

(ert-deftest test-update-text-file-insert-at-line-boundary-text-with-trailing-newline ()
  "Boundary: inserted text that ends in newline is not double-terminated."
  (should (equal (cj/update-text-file--insert-at-line "a\nb\n" 2 "X\n")
                 "a\nX\nb\n")))

(ert-deftest test-update-text-file-insert-at-line-boundary-multiline-text ()
  "Boundary: multi-line inserted text is inserted as a block."
  (should (equal (cj/update-text-file--insert-at-line "a\nb\n" 2 "X\nY")
                 "a\nX\nY\nb\n")))

(ert-deftest test-update-text-file-insert-at-line-boundary-empty-file-line-1 ()
  "Boundary: inserting at line 1 in an empty file works."
  (should (equal (cj/update-text-file--insert-at-line "" 1 "X")
                 "X\n")))

(ert-deftest test-update-text-file-insert-at-line-error-empty-file-line-2 ()
  "Error: line 2 is out of range for an empty file."
  (should-error (cj/update-text-file--insert-at-line "" 2 "X")))

(ert-deftest test-update-text-file-insert-at-line-error-out-of-range ()
  "Error: line number beyond file length signals."
  (should-error (cj/update-text-file--insert-at-line "a\nb\n" 5 "X")))

(ert-deftest test-update-text-file-insert-at-line-error-zero ()
  "Error: line number 0 signals."
  (should-error (cj/update-text-file--insert-at-line "a\n" 0 "X")))

(ert-deftest test-update-text-file-insert-at-line-error-negative ()
  "Error: negative line number signals."
  (should-error (cj/update-text-file--insert-at-line "a\n" -1 "X")))

(ert-deftest test-update-text-file-insert-at-line-error-empty-text ()
  "Error: empty text signals."
  (should-error (cj/update-text-file--insert-at-line "a\n" 1 "")))

;; ----------------------------------------------------- delete-lines

(ert-deftest test-update-text-file-delete-lines-normal ()
  "Normal: removes lines containing the literal pattern."
  (should (equal (cj/update-text-file--delete-lines "keep\nkill me\nkeep\n" "kill")
                 "keep\nkeep\n")))

(ert-deftest test-update-text-file-delete-lines-boundary-no-match ()
  "Boundary: pattern matches nothing returns content unchanged."
  (should (equal (cj/update-text-file--delete-lines "a\nb\nc\n" "z")
                 "a\nb\nc\n")))

(ert-deftest test-update-text-file-delete-lines-boundary-all-lines-match ()
  "Boundary: every line removed yields the empty string."
  (should (equal (cj/update-text-file--delete-lines "x\nx\nx\n" "x") "")))

(ert-deftest test-update-text-file-delete-lines-boundary-special-chars-literal ()
  "Boundary: regex metacharacters in pattern are treated as literals."
  (should (equal (cj/update-text-file--delete-lines "a.b\naxb\n" ".")
                 "axb\n")))

(ert-deftest test-update-text-file-delete-lines-boundary-no-trailing-newline ()
  "Boundary: content without trailing newline keeps that shape."
  (should (equal (cj/update-text-file--delete-lines "keep\ndrop" "drop")
                 "keep")))

(ert-deftest test-update-text-file-delete-lines-boundary-empty-file ()
  "Boundary: deleting from an empty file returns the empty string."
  (should (equal (cj/update-text-file--delete-lines "" "anything") "")))

(ert-deftest test-update-text-file-delete-lines-boundary-backslash-literal ()
  "Boundary: backslashes in the pattern are literal."
  (should (equal (cj/update-text-file--delete-lines "keep\npath\\name\n" "\\")
                 "keep\n")))

(ert-deftest test-update-text-file-delete-lines-error-empty-pattern ()
  "Error: empty pattern signals."
  (should-error (cj/update-text-file--delete-lines "a\nb\n" "")))

(ert-deftest test-update-text-file-delete-lines-error-nil-pattern ()
  "Error: nil pattern signals."
  (should-error (cj/update-text-file--delete-lines "a\nb\n" nil)))

;; ----------------------------------------------------- apply-operation

(ert-deftest test-update-text-file-apply-operation-dispatch ()
  "Each operation name dispatches to its transform."
  (should (equal (cj/update-text-file--apply-operation "abc" "replace" "b" "B" nil)
                 "aBc"))
  (should (equal (cj/update-text-file--apply-operation "a" "append" "b" nil nil)
                 "a\nb\n"))
  (should (equal (cj/update-text-file--apply-operation "a" "prepend" "b" nil nil)
                 "b\na"))
  (should (equal (cj/update-text-file--apply-operation "a\nb\n" "insert-at-line" "X" nil 2)
                 "a\nX\nb\n"))
  (should (equal (cj/update-text-file--apply-operation "a\nb\n" "delete-lines" "a" nil nil)
                 "b\n")))

(ert-deftest test-update-text-file-apply-operation-error-unknown ()
  "Unknown operation signals."
  (should-error (cj/update-text-file--apply-operation "x" "frobnicate" nil nil nil)))

;; ----------------------------------------------------- validate-path

(ert-deftest test-update-text-file-validate-path-normal ()
  "Normal: an existing readable+writable file under HOME passes."
  (let* ((file (make-temp-file "test-update-text-file-")))
    (unwind-protect
        (progn
          ;; make-temp-file may land in /tmp; rebase to HOME for the test.
          (let* ((home-file (expand-file-name
                             (concat ".test-update-text-file-" (format-time-string "%s") ".tmp")
                             "~")))
            (unwind-protect
                (progn
                  (copy-file file home-file t)
                  (should (equal (cj/update-text-file--validate-path home-file)
                                 (file-truename home-file))))
              (when (file-exists-p home-file) (delete-file home-file)))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest test-update-text-file-validate-path-error-missing ()
  "Error: a missing file under HOME signals."
  (let ((path (expand-file-name
               (concat ".test-update-text-file-missing-"
                       (format-time-string "%s") ".tmp")
               "~")))
    (when (file-exists-p path) (delete-file path))
    (should-error (cj/update-text-file--validate-path path))))

(ert-deftest test-update-text-file-validate-path-error-outside-home ()
  "Error: a path outside HOME signals."
  (should-error (cj/update-text-file--validate-path "/etc/hostname")))

(ert-deftest test-update-text-file-validate-path-error-directory ()
  "Error: a directory signals."
  (should-error (cj/update-text-file--validate-path "~")))

(ert-deftest test-update-text-file-validate-path-error-unreadable ()
  "Error: an unreadable file signals."
  (test-update-text-file--in-home
   "unreadable" "secret\n"
   (lambda (path)
     (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
       (should-error (cj/update-text-file--validate-path path))))))

(ert-deftest test-update-text-file-validate-path-error-unwritable ()
  "Error: an unwritable file signals."
  (test-update-text-file--in-home
   "unwritable" "locked\n"
   (lambda (path)
     (cl-letf (((symbol-function 'file-writable-p) (lambda (_) nil)))
       (should-error (cj/update-text-file--validate-path path))))))

(ert-deftest test-update-text-file-validate-path-boundary-relative-home-path ()
  "Boundary: a relative path resolves under HOME."
  (test-update-text-file--in-home
   "relative" "ok\n"
   (lambda (path)
     (let ((relative (file-relative-name path (expand-file-name "~"))))
       (should (equal (cj/update-text-file--validate-path relative)
                      (file-truename path)))))))

(ert-deftest test-update-text-file-validate-path-boundary-symlink-inside-home ()
  "Boundary: a symlink inside HOME resolving inside HOME is accepted."
  (test-update-text-file--in-home
   "symlink-target" "ok\n"
   (lambda (target)
     (let ((link (expand-file-name
                  (format ".test-update-text-file-link-%s.tmp"
                          (format-time-string "%s%N"))
                  "~")))
       (unwind-protect
           (progn
             (make-symbolic-link target link t)
             (should (equal (cj/update-text-file--validate-path link)
                            (file-truename target))))
         (when (file-symlink-p link) (delete-file link)))))))

(ert-deftest test-update-text-file-validate-path-error-symlink-outside-home ()
  "Error: a symlink inside HOME pointing outside HOME is rejected."
  (let ((outside (make-temp-file "test-update-text-file-outside-"))
        (link (expand-file-name
               (format ".test-update-text-file-outside-link-%s.tmp"
                       (format-time-string "%s%N"))
               "~")))
    (unwind-protect
        (progn
          (make-symbolic-link outside link t)
          (should-error (cj/update-text-file--validate-path link)))
      (when (file-exists-p outside) (delete-file outside))
      (when (file-symlink-p link) (delete-file link)))))

;; ----------------------------------------------------- backup-name

(ert-deftest test-update-text-file-backup-name-shape ()
  "Backup names append a timestamped .bak suffix."
  (let ((name (cj/update-text-file--backup-name "/home/user/foo.txt")))
    (should (string-prefix-p "/home/user/foo.txt-" name))
    (should (string-suffix-p ".bak" name))
    ;; Format is YYYY-MM-DD-HHMMSS.
    (should (string-match-p "-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{6\\}\\.bak\\'"
                            name))))

;; ----------------------------------------------------- file-level wrapper

(defun test-update-text-file--in-home (suffix content fn)
  "Write CONTENT to a temp file under HOME with SUFFIX, call FN, then delete.
Backups (path-TS.bak) are cleaned up after FN returns."
  (let* ((name (format ".test-update-text-file-%s-%s.tmp"
                       suffix (format-time-string "%s%N")))
         (path (expand-file-name name "~")))
    (unwind-protect
        (progn
          (with-temp-file path (insert content))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path))
      (dolist (b (file-expand-wildcards (concat path "-*.bak")))
        (when (file-exists-p b) (delete-file b))))))

(ert-deftest test-update-text-file-run-replace-normal ()
  "Wrapper: replace operation rewrites the file and creates a backup."
  (test-update-text-file--in-home
   "replace" "alpha bravo alpha\n"
   (lambda (path)
     (let ((result (cj/update-text-file--run path "replace" "alpha" "GAMMA" nil)))
       (should (string-match-p "Updated" result))
       (should (string-match-p "backup:" result))
       (with-temp-buffer
         (insert-file-contents path)
         (should (equal (buffer-string) "GAMMA bravo GAMMA\n")))
       (let ((backup (car (file-expand-wildcards (concat path "-*.bak")))))
         (should backup)
         (with-temp-buffer
           (insert-file-contents backup)
           (should (equal (buffer-string) "alpha bravo alpha\n"))))))))

(ert-deftest test-update-text-file-run-no-change-no-backup ()
  "Wrapper: no-op operation leaves the file untouched and creates no backup."
  (test-update-text-file--in-home
   "noop" "abc\n"
   (lambda (path)
     (let ((result (cj/update-text-file--run path "replace" "zzz" "QQ" nil)))
       (should (string-match-p "No changes" result))
       (with-temp-buffer
         (insert-file-contents path)
         (should (equal (buffer-string) "abc\n")))
       (should-not (file-expand-wildcards (concat path "-*.bak")))))))

(ert-deftest test-update-text-file-run-append-normal ()
  "Wrapper: append operation adds a line to the file."
  (test-update-text-file--in-home
   "append" "first\n"
   (lambda (path)
     (cj/update-text-file--run path "append" "second" nil nil)
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "first\nsecond\n"))))))

(ert-deftest test-update-text-file-run-insert-at-line-normal ()
  "Wrapper: insert-at-line inserts and rewrites the file."
  (test-update-text-file--in-home
   "insert" "a\nb\nc\n"
   (lambda (path)
     (cj/update-text-file--run path "insert-at-line" "X" nil 2)
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "a\nX\nb\nc\n"))))))

(ert-deftest test-update-text-file-run-delete-lines-normal ()
  "Wrapper: delete-lines removes matching lines."
  (test-update-text-file--in-home
   "delete" "keep1\nkill\nkeep2\nkill\n"
   (lambda (path)
     (cj/update-text-file--run path "delete-lines" "kill" nil nil)
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "keep1\nkeep2\n"))))))

(ert-deftest test-update-text-file-run-error-transform-leaves-file-unchanged ()
  "Wrapper: transform errors create no backup and leave the file unchanged."
  (test-update-text-file--in-home
   "transform-error" "abc\n"
   (lambda (path)
     (should-error (cj/update-text-file--run path "replace" "" "x" nil))
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "abc\n")))
     (should-not (file-expand-wildcards (concat path "-*.bak"))))))

(ert-deftest test-update-text-file-run-error-unknown-operation-leaves-file-unchanged ()
  "Wrapper: unknown operations create no backup and leave the file unchanged."
  (test-update-text-file--in-home
   "unknown-operation" "abc\n"
   (lambda (path)
     (should-error (cj/update-text-file--run path "frobnicate" "x" nil nil))
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "abc\n")))
     (should-not (file-expand-wildcards (concat path "-*.bak"))))))

(ert-deftest test-update-text-file-run-error-too-large-leaves-file-unchanged ()
  "Wrapper: the size guard errors before backup/write."
  (test-update-text-file--in-home
   "too-large" "abcdef\n"
   (lambda (path)
     (let ((cj/update-text-file--size-limit 3))
       (should-error (cj/update-text-file--run path "append" "x" nil nil)))
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "abcdef\n")))
     (should-not (file-expand-wildcards (concat path "-*.bak"))))))

(ert-deftest test-update-text-file-run-error-missing-file ()
  "Wrapper: missing file signals."
  (let ((path (expand-file-name
               (concat ".test-update-text-file-absent-"
                       (format-time-string "%s") ".tmp")
               "~")))
    (when (file-exists-p path) (delete-file path))
    (should-error (cj/update-text-file--run path "append" "x" nil nil))))

(ert-deftest test-update-text-file-run-error-outside-home ()
  "Wrapper: path outside home signals."
  (should-error (cj/update-text-file--run "/etc/hostname" "append" "x" nil nil)))

(provide 'test-update-text-file)
;;; test-update-text-file.el ends here
