;;; update_text_file.el --- Update text files for gptel  -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool for updating an existing text file with one of five
;; operations:
;;
;;   replace        Replace all occurrences of PATTERN with REPLACEMENT.
;;   append         Add TEXT at the end of the file.
;;   prepend        Add TEXT at the beginning of the file.
;;   insert-at-line Insert TEXT at LINE-NUM (1-indexed).
;;   delete-lines   Delete every line containing PATTERN.
;;
;; The operations are pure-string transforms — file I/O happens only at
;; the outer wrapper, which validates the path, takes a timestamped
;; backup, and writes the new content atomically.  The tool uses gptel's
;; `:confirm t' meta-flag for the user-facing prompt, mirroring how
;; `write_text_file' handles confirmation.
;;
;; PATTERN is a literal substring for `replace' and `delete-lines'.  No
;; regex.  The model can build literal multi-line patterns and we don't
;; want it to discover regex metacharacter gotchas through trial and
;; error.

;;; Code:

(require 'gptel)
(require 'subr-x)
(require 'cl-lib)

;; ---------------------------------------------------------------- helpers

(defun cj/update-text-file--validate-path (path)
  "Validate PATH for update.  Return the truename on success.

PATH must resolve inside the user's home directory, must exist, must
be a regular file, and must be readable and writable."
  (let ((full (expand-file-name path "~")))
    (unless (string-prefix-p (expand-file-name "~") full)
      (error "Path must be within home directory: %s" path))
    (unless (file-exists-p full)
      (error "File not found: %s" full))
    (when (file-directory-p full)
      (error "Path is a directory, not a file: %s" full))
    (unless (file-readable-p full)
      (error "No read permission for file: %s" full))
    (unless (file-writable-p full)
      (error "No write permission for file: %s" full))
    (if (file-symlink-p full)
        (file-truename full)
      full)))

(defun cj/update-text-file--backup-name (path)
  "Return a backup filename for PATH timestamped to the current second."
  (format "%s-%s.bak" path (format-time-string "%Y-%m-%d-%H%M%S")))

(defconst cj/update-text-file--size-limit (* 10 1024 1024)
  "Reject files larger than 10MB so a runaway operation can't churn the disk.")

;; ----------------------------------------------------- string transforms
;;
;; Each transform takes the file contents as a string plus operation
;; parameters and returns the new contents.  Pure functions — no I/O.

(defun cj/update-text-file--replace (content pattern replacement)
  "Return CONTENT with every occurrence of PATTERN replaced by REPLACEMENT.
PATTERN is treated as a literal substring.  Signal an error if PATTERN is
empty or nil."
  (unless (and (stringp pattern) (> (length pattern) 0))
    (error "Replace operation requires a non-empty pattern"))
  (unless (stringp replacement)
    (error "Replace operation requires a replacement string"))
  (replace-regexp-in-string (regexp-quote pattern) replacement content t t))

(defun cj/update-text-file--append (content text)
  "Return CONTENT with TEXT added at the end, separated by a newline.
A trailing newline is guaranteed.  Signal if TEXT is nil or empty."
  (unless (and (stringp text) (> (length text) 0))
    (error "Append operation requires non-empty text"))
  (let ((base (if (or (string-empty-p content)
                      (string-suffix-p "\n" content))
                  content
                (concat content "\n"))))
    (if (string-suffix-p "\n" text)
        (concat base text)
      (concat base text "\n"))))

(defun cj/update-text-file--prepend (content text)
  "Return CONTENT with TEXT added at the beginning.
TEXT is separated from CONTENT by a newline.  Signal if TEXT is nil
or empty."
  (unless (and (stringp text) (> (length text) 0))
    (error "Prepend operation requires non-empty text"))
  (if (string-suffix-p "\n" text)
      (concat text content)
    (concat text "\n" content)))

(defun cj/update-text-file--insert-at-line (content line-num text)
  "Return CONTENT with TEXT inserted before LINE-NUM (1-indexed).
LINE-NUM 1 prepends.  LINE-NUM one past the last line appends.  Signal
on out-of-range LINE-NUM or empty TEXT."
  (unless (and (integerp line-num) (> line-num 0))
    (error "Insert-at-line requires a positive integer line number"))
  (unless (and (stringp text) (> (length text) 0))
    (error "Insert-at-line requires non-empty text"))
  (let* ((lines (split-string content "\n"))
         ;; `split-string' on a newline-terminated string returns an
         ;; extra empty element at the end.  Trim it so the line count
         ;; matches what a human would say.
         (trailing-newline (string-suffix-p "\n" content))
         (line-count (if trailing-newline
                         (1- (length lines))
                       (length lines))))
    (when (> line-num (1+ line-count))
      (error "Line %d out of range (file has %d lines)" line-num line-count))
    (let* ((to-insert (if (string-suffix-p "\n" text)
                          (substring text 0 (1- (length text)))
                        text))
           (idx (1- line-num))
           (head (cl-subseq lines 0 idx))
           (tail (cl-subseq lines idx)))
      (mapconcat #'identity
                 (append head (list to-insert) tail)
                 "\n"))))

(defun cj/update-text-file--delete-lines (content pattern)
  "Return CONTENT with every line containing PATTERN removed.
PATTERN is a literal substring.  Trailing-newline state is preserved
when at least one line survives; an empty result is returned as the
empty string."
  (unless (and (stringp pattern) (> (length pattern) 0))
    (error "Delete-lines requires a non-empty pattern"))
  (let* ((trailing-newline (string-suffix-p "\n" content))
         (raw-lines (split-string content "\n"))
         ;; Drop the trailing empty element split-string produces when
         ;; the input ends in a newline.
         (lines (if trailing-newline
                    (butlast raw-lines)
                  raw-lines))
         (kept (cl-remove-if (lambda (line)
                               (string-match-p (regexp-quote pattern) line))
                             lines)))
    (cond
     ((null kept) "")
     (trailing-newline (concat (mapconcat #'identity kept "\n") "\n"))
     (t (mapconcat #'identity kept "\n")))))

(defun cj/update-text-file--apply-operation
    (content operation pattern replacement line-num)
  "Dispatch OPERATION on CONTENT.  Return the transformed string.

OPERATION is one of \"replace\", \"append\", \"prepend\",
\"insert-at-line\", or \"delete-lines\".  PATTERN, REPLACEMENT, and
LINE-NUM are used per operation; unused arguments are ignored."
  (pcase operation
    ("replace"        (cj/update-text-file--replace content pattern replacement))
    ("append"         (cj/update-text-file--append content pattern))
    ("prepend"        (cj/update-text-file--prepend content pattern))
    ("insert-at-line" (cj/update-text-file--insert-at-line content line-num pattern))
    ("delete-lines"   (cj/update-text-file--delete-lines content pattern))
    (_ (error "Unknown operation: %s" operation))))

;; ----------------------------------------------------- file-level wrapper

(defun cj/update-text-file--run (path operation pattern replacement line-num)
  "Update PATH with OPERATION and return a status string.

PATTERN, REPLACEMENT, and LINE-NUM are passed through per operation.
A timestamped backup is created next to the file before writing.  If
the operation produces no change the backup is removed and the file
is left untouched."
  (let* ((full (cj/update-text-file--validate-path path))
         (size (file-attribute-size (file-attributes full))))
    (when (> size cj/update-text-file--size-limit)
      (error "File too large (%s): exceeds 10MB limit"
             (file-size-human-readable size)))
    (let* ((before (with-temp-buffer
                     (insert-file-contents full)
                     (buffer-string)))
           (after (cj/update-text-file--apply-operation
                   before operation pattern replacement line-num)))
      (cond
       ((string= before after)
        (format "No changes made to %s" full))
       (t
        (let ((backup (cj/update-text-file--backup-name full)))
          (copy-file full backup t)
          (with-temp-file full (insert after))
          (format "Updated %s (backup: %s)"
                  full (file-name-nondirectory backup))))))))

;; ----------------------------------------------------- tool registration

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "update_text_file"
   :function (lambda (path operation &optional pattern replacement line-num)
               (cj/update-text-file--run path operation pattern replacement line-num))
   :description "Update an existing text file with one of: replace, append, prepend, insert-at-line, delete-lines. Creates a timestamped backup before writing. Patterns are literal substrings, not regex."
   :args (list '(:name "path"
                       :type string
                       :description "File path relative to home directory, e.g. 'documents/foo.txt' or '~/documents/foo.txt'")
               '(:name "operation"
                       :type string
                       :enum ["replace" "append" "prepend" "insert-at-line" "delete-lines"]
                       :description "Which update operation to perform")
               '(:name "pattern"
                       :type string
                       :description "For replace/delete-lines: the literal substring to match. For append/prepend/insert-at-line: the text to add. Required for every operation."
                       :optional t)
               '(:name "replacement"
                       :type string
                       :description "For replace: the literal replacement text. Ignored by other operations."
                       :optional t)
               '(:name "line_num"
                       :type integer
                       :description "For insert-at-line: 1-indexed line number to insert before. Ignored by other operations."
                       :optional t))
   :category "filesystem"
   :confirm t
   :include t)

  (add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "update_text_file"))))

(provide 'update_text_file)
;;; update_text_file.el ends here
