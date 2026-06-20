;;; face-docs-dump.el --- Dump face docstrings for theme-studio hovers -*- lexical-binding: t -*-

;;; Commentary:
;; Emits face-docs.json, the checked-in asset generate.py inlines so the
;; theme-studio element hovers can show each face's Emacs docstring on top of
;; the existing tooltip text.  Two maps:
;;
;;   "faces"  -- face-name -> first docstring line, for every face in
;;               `face-list' that carries documentation.  Keys the UI and
;;               package tables (both keyed by real Emacs face name).
;;   "syntax" -- theme-studio syntax-category key (kw, doc, str, ...) ->
;;               first docstring line of the font-lock face it colors.  Keys
;;               the syntax table.  The category->face mapping is read from
;;               `build-theme/--syntax-face-map' (build-theme.el) so it stays
;;               single-sourced; bg and p map to the `default' face.
;;
;; Run against a live daemon so lazily-loaded package faces are present:
;;   emacsclient -e '(progn (load ".../face-docs-dump.el")
;;                          (face-docs-dump "/path/to/face-docs.json"))'

;;; Code:

(require 'json)

(defun face-docs--first-line (doc)
  "Return the first non-empty line of DOC, whitespace-collapsed, or nil.
Returns nil when DOC is not a non-empty string."
  (when (and (stringp doc) (not (string-empty-p doc)))
    (let ((line (seq-find (lambda (l) (not (string-blank-p l)))
                          (split-string doc "\n"))))
      (when line
        (string-trim (replace-regexp-in-string "[ \t]+" " " line))))))

(defun face-docs--faces-map ()
  "Hash of face-name -> first docstring line for documented faces."
  (let ((faces (make-hash-table :test 'equal)))
    (dolist (f (face-list))
      (let ((doc (face-docs--first-line (face-documentation f))))
        (when doc (puthash (symbol-name f) doc faces))))
    faces))

(defun face-docs--syntax-map ()
  "Hash of syntax-category key -> first docstring line of its primary face.
Reads `build-theme/--syntax-face-map' for the category->faces mapping;
adds bg and p as the `default' face."
  (let ((syntax (make-hash-table :test 'equal))
        (pairs (append '((bg . (default)) (p . (default)))
                       (and (boundp 'build-theme/--syntax-face-map)
                            build-theme/--syntax-face-map))))
    (dolist (entry pairs)
      (let* ((kind (car entry))
             (face (car (cdr entry)))
             (doc (and (facep face)
                       (face-docs--first-line (face-documentation face)))))
        (when doc (puthash (symbol-name kind) doc syntax))))
    syntax))

(defun face-docs-dump (outfile)
  "Write the face and syntax docstring maps as JSON to OUTFILE.
Loads build-theme.el (sibling file) for the syntax-category face map."
  (let ((bt (expand-file-name "build-theme.el"
                              (file-name-directory
                               (or load-file-name buffer-file-name default-directory)))))
    (when (file-exists-p bt) (load bt nil t)))
  (let ((faces (face-docs--faces-map))
        (syntax (face-docs--syntax-map))
        ;; Docstrings carry curly quotes and other non-ASCII; pin the write
        ;; coding system so `with-temp-file' never opens the interactive
        ;; select-safe-coding-system prompt in the daemon frame.
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file outfile
      (insert (json-serialize (list :faces faces :syntax syntax))))
    (message "face-docs-dump: %d faces, %d syntax keys -> %s"
             (hash-table-count faces) (hash-table-count syntax) outfile)))

(provide 'face-docs-dump)
;;; face-docs-dump.el ends here
