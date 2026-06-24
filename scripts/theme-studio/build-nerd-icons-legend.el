;;; build-nerd-icons-legend.el --- emit nerd-icons legend + gallery for theme-studio -*- lexical-binding: t -*-
;;; Commentary:
;; A library of capture functions plus one entry point, cj/nerd-icons-write-legend,
;; that writes nerd-icons-legend.json next to this file.  Invoke it from a running
;; Emacs (where nerd-icons is loaded):
;;
;;   emacsclient -e '(progn (load ".../build-nerd-icons-legend.el") (cj/nerd-icons-write-legend))'
;;
;; The JSON is an object with two keys:
;;   "legend"  -- the curated v1 filetype legend (a representative row set drawn
;;                from a diverse subset of the nerd-icons color faces).
;;   "gallery" -- the full colored catalog (vNext): every distinct face-bearing
;;                nerd-icons icon, grouped by owner color face, one group per face,
;;                the groups ordered by hue so color families cluster.
;; Each legend row and gallery glyph resolves its glyph + owner face from the live
;; nerd-icons alists at capture time, so the artifact tracks the installed
;; nerd-icons version.  A curated legend key absent from the alist is skipped and
;; logged; a gallery entry whose glyph or face won't resolve is skipped.
;; generate.py embeds the JSON; see docs/specs/theme-studio-nerd-icons-colors-spec.org.
;;
;; nerd-icons is required only at write time (inside cj/nerd-icons-write-legend),
;; not at load, so the pure capture functions load and unit-test without it (the
;; alist vars are declared special below and injected by the test).
;;; Code:

(require 'json)
(require 'color)

;; Declared, not required: nerd-icons supplies these at write time; the declarations
;; keep the byte-compiler quiet and let tests bind synthetic values without nerd-icons.
(defvar nerd-icons-extension-icon-alist)
(defvar nerd-icons-regexp-icon-alist)
(defvar nerd-icons-mode-icon-alist)
(defvar nerd-icons-completion-category-icons)
(declare-function nerd-icons-icon-for-dir "nerd-icons")

;; ---- v1 legend (curated representative rows) ------------------------------

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
  "Build the JSON alist for one legend row, or nil (logged) if GLYPH/FACE absent."
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

(defun cj/--nerd-icons-legend-rows ()
  "Resolve the curated v1 legend rows as a list of JSON alists."
  (delq nil (mapcar (lambda (r) (apply #'cj/--nerd-icons-legend-row r))
                    cj/--nerd-icons-legend-spec)))

;; ---- gallery (full colored catalog, a grid of distinct icons by color) -----

(defconst cj/--nerd-icons-gallery-alists
  '(nerd-icons-extension-icon-alist
    nerd-icons-regexp-icon-alist
    nerd-icons-mode-icon-alist)
  "Source alists for the gallery.  Entries are shaped (KEY FN NAME :face FACE ...);
NAME is the nerd-font icon name (e.g. \"nf-dev-terminal\").  The dir alist carries
no :face (directory icons are colored by advice, not a per-entry face) and is
intentionally absent.")

(defun cj/--nerd-icons-spec-foreground (spec)
  "Return the :foreground of the default (t) display clause in SPEC, or nil.
The clause is (t . PLIST), so the foreground is plist-get of its cdr.  A
display-conditional spec (no t clause, as the real nerd-icons faces use) returns
nil here and falls back to the live, frame-resolved face foreground."
  (plist-get (cdr (assoc t spec)) :foreground))

(defun cj/--nerd-icons-face-hsl (face)
  "Return (HUE SAT LIGHT) for FACE's foreground: hue 0-360, sat and light 0-100.
Use the t-clause defface color when there is one (deterministic), else the live
frame-resolved foreground.  nil if no color resolves."
  (let* ((fg (or (cj/--nerd-icons-spec-foreground (face-default-spec face))
                 (face-foreground face nil 'default)))
         (rgb (and (stringp fg) (ignore-errors (color-name-to-rgb fg))))
         (hsl (and rgb (apply #'color-rgb-to-hsl rgb))))
    (when hsl
      (list (round (* 360 (nth 0 hsl)))
            (round (* 100 (nth 1 hsl)))
            (round (* 100 (nth 2 hsl)))))))

(defun cj/--nerd-icons-gallery-groups ()
  "Build the gallery grid: a list of JSON group alists, one per owner color face,
ordered by hue (ascending, ties by descending lightness) so families cluster.
Each group is ((\"face\" . NAME) (\"hue\" . DEG) (\"glyphs\" . VECTOR)) where each
glyph is ((\"glyph\" . G) (\"name\" . ICON-NAME)).  Within a face, icons are
deduplicated by name and sorted by name.  An entry without a :face, an
unresolvable glyph, or a face with no native color is skipped."
  (let ((table (make-hash-table :test 'eq))
        (seen (make-hash-table :test 'equal))
        (order nil))
    (dolist (sym cj/--nerd-icons-gallery-alists)
      (dolist (e (and (boundp sym) (symbol-value sym)))
        (let* ((face (plist-get (nthcdr 3 e) :face))
               (name (nth 2 e))
               (glyph (cj/--nerd-icons-legend-glyph (nth 1 e) name)))
          (when (and face glyph (stringp name))
            (let ((dk (concat (symbol-name face) "\0" name)))
              (unless (gethash dk seen)
                (puthash dk t seen)
                (unless (gethash face table) (push face order))
                (puthash face
                         (cons (list (cons "glyph" glyph) (cons "name" name))
                               (gethash face table))
                         table)))))))
    (let ((groups
           (delq nil
                 (mapcar (lambda (face)
                           (let ((hsl (cj/--nerd-icons-face-hsl face))
                                 (glyphs (sort (gethash face table)
                                               (lambda (a b) (string< (cdr (assoc "name" a))
                                                                      (cdr (assoc "name" b)))))))
                             (when hsl (list face (nth 0 hsl) (nth 2 hsl) glyphs))))
                         (nreverse order)))))
      (setq groups (sort groups (lambda (a b)
                                  (if (/= (nth 1 a) (nth 1 b))
                                      (< (nth 1 a) (nth 1 b))
                                    (> (nth 2 a) (nth 2 b))))))
      (mapcar (lambda (g)
                (list (cons "face" (symbol-name (nth 0 g)))
                      (cons "hue" (nth 1 g))
                      (cons "glyphs" (apply #'vector (nth 3 g)))))
              groups))))

;; ---- entry point ----------------------------------------------------------

(defun cj/nerd-icons-write-legend ()
  "Resolve the legend + gallery from the live nerd-icons alists and write
nerd-icons-legend.json next to this file.  Requires nerd-icons (loaded here, not
at file load, so the capture functions stay unit-testable without it)."
  (require 'nerd-icons)
  (let ((legend (cj/--nerd-icons-legend-rows))
        (gallery (cj/--nerd-icons-gallery-groups)))
    (with-temp-file (expand-file-name
                     "nerd-icons-legend.json"
                     (file-name-directory (or load-file-name buffer-file-name
                                              "~/.emacs.d/scripts/theme-studio/")))
      (let ((json-encoding-pretty-print t))
        (insert (json-encode (list (cons "legend" (apply #'vector legend))
                                   (cons "gallery" (apply #'vector gallery))))
                "\n")))
    (message "nerd-icons-legend: wrote %d legend rows, %d gallery groups (%d glyphs)"
             (length legend) (length gallery)
             (apply #'+ (mapcar (lambda (g) (length (cdr (assoc "glyphs" g)))) gallery)))))

(provide 'build-nerd-icons-legend)
;;; build-nerd-icons-legend.el ends here
