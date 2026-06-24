;;; build-nerd-icons-legend.el --- emit nerd-icons filetype legend for theme-studio -*- lexical-binding: t -*-
;;; Commentary:
;; Loaded into a running Emacs (emacsclient -e '(load ".../build-nerd-icons-legend.el")')
;; to write nerd-icons-legend.json next to itself: the curated v1 filetype legend
;; for theme-studio's bespoke nerd-icons preview.  Each row resolves its glyph and
;; owner color face from the live nerd-icons alists at capture time, so the legend
;; tracks the installed nerd-icons version.  A curated key absent from the alist
;; is skipped and logged.  generate.py embeds the JSON; see
;; docs/specs/theme-studio-nerd-icons-colors-spec.org.
;;; Code:

(require 'json)
(require 'nerd-icons)

;; Curated v1 rows: (KEY LABEL CATEGORY LOOKUP).  CATEGORY selects the source
;; alist and its face shape; LOOKUP is the alist key (nil for the dir row, which
;; has a fixed owner face per the spec's dir-precedence decision).
(defconst cj/--nerd-icons-legend-spec
  '(("ext:el"   "init.el"     extension "el")
    ("ext:py"   "app.py"      extension "py")
    ("ext:org"  "notes.org"   extension "org")
    ("ext:md"   "README.md"   extension "md")
    ("ext:ts"   "main.ts"     extension "ts")
    ("ext:html" "index.html"  extension "html")
    ("ext:rs"   "lib.rs"      extension "rs")
    ("ext:js"   "app.js"      extension "js")
    ("ext:yml"  "ci.yml"      extension "yml")
    ("ext:c"    "main.c"      extension "c")
    ("dir"      "src/"        dir       nil)
    ("cmd"      "M-x command" command   command)
    ("buf"      "*scratch*"   buffer    emacs-lisp-mode))
  "The v1 legend rows: (KEY LABEL CATEGORY LOOKUP), spanning a representative
set of the nerd-icons color faces rather than all 34.")

(defun cj/--nerd-icons-legend-glyph (fn name)
  "Return the bare glyph string for icon NAME drawn by FN, or nil."
  (when (and (fboundp fn) (stringp name))
    (let ((s (ignore-errors (funcall fn name))))
      (and (stringp s)
           (> (length (string-trim s)) 0)
           (string-trim (substring-no-properties s))))))

(defun cj/--nerd-icons-legend-make (key label category glyph face)
  "Build the JSON alist for one legend row, or nil (logged) when GLYPH/FACE missing."
  (if (and glyph face)
      (list (cons "key" key)
            (cons "label" label)
            (cons "face" (symbol-name face))
            (cons "category" (symbol-name category))
            (cons "glyph" glyph))
    (message "nerd-icons-legend: skipping %s (glyph=%S face=%S)" key glyph face)
    nil))

(defun cj/--nerd-icons-legend-row (key label category lookup)
  "Resolve one curated row from the live nerd-icons alists, or nil if absent."
  (pcase category
    ('extension
     (let ((e (assoc lookup nerd-icons-extension-icon-alist)))
       (when e
         (cj/--nerd-icons-legend-make
          key label category
          (cj/--nerd-icons-legend-glyph (nth 1 e) (nth 2 e))
          (plist-get (nthcdr 3 e) :face)))))
    ('buffer
     (let ((e (assq lookup nerd-icons-mode-icon-alist)))
       (when e
         (cj/--nerd-icons-legend-make
          key label category
          (cj/--nerd-icons-legend-glyph (nth 1 e) (nth 2 e))
          (plist-get (nthcdr 3 e) :face)))))
    ('command
     (let ((e (assq lookup nerd-icons-completion-category-icons)))
       (when e
         (cj/--nerd-icons-legend-make
          key label category
          (cj/--nerd-icons-legend-glyph (nth 1 e) (nth 2 e))
          (nth 3 e)))))
    ('dir
     (cj/--nerd-icons-legend-make
      key label category
      (let ((s (ignore-errors (nerd-icons-icon-for-dir "src"))))
        (and (stringp s) (string-trim (substring-no-properties s))))
      'nerd-icons-yellow))))

(let ((rows (delq nil (mapcar (lambda (r) (apply #'cj/--nerd-icons-legend-row r))
                              cj/--nerd-icons-legend-spec))))
  (with-temp-file (expand-file-name
                   "nerd-icons-legend.json"
                   (file-name-directory (or load-file-name buffer-file-name
                                            "~/.emacs.d/scripts/theme-studio/")))
    (let ((json-encoding-pretty-print t))
      (insert (json-encode (apply #'vector rows)) "\n")))
  (message "nerd-icons-legend: wrote %d rows" (length rows)))

(provide 'build-nerd-icons-legend)
;;; build-nerd-icons-legend.el ends here
