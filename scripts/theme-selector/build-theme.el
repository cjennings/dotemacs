;;; build-theme.el --- Convert a theme-selector theme.json into a deftheme -*- lexical-binding: t -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; The last link in the theme-selector pipeline: turn a theme.json exported by
;; the tool (see scripts/theme-selector/README.md and
;; docs/design/theme-selector-package-faces-spec.org) into a single,
;; self-contained, loadable Emacs deftheme written to themes/<name>-theme.el.
;;
;; Four tiers come out of the JSON:
;;   - default     -- background from assignments.bg, foreground from .p
;;   - syntax      -- assignments.<cat> -> font-lock / tree-sitter faces, with
;;                    the bold / italic category sets applied
;;   - ui          -- the ui keys are already real face names; fg/bg passthrough
;;   - packages    -- per-package face specs with :inherit / :height / weight /
;;                    slant
;;
;; Usage (from a shell or a running Emacs):
;;
;;   emacsclient -e '(progn (load ".../build-theme.el")
;;                          (build-theme/convert-file ".../dupre-revised.json"))'
;;
;; or in batch:
;;
;;   emacs --batch -l build-theme.el \
;;     --eval '(build-theme/convert-file "dupre-revised.json" "themes")'
;;
;; The output is a flat generated deftheme, not the hand-authored
;; palette/faces/theme trio that the original dupre theme ships -- a theme.json
;; carries resolved per-face hex, not dupre's semantic-mapping layer, so a flat
;; deftheme is the faithful output and never clobbers the curated dupre files.

;;; Code:

(require 'json)
(require 'subr-x)

(defconst build-theme/--syntax-face-map
  '((kw   . (font-lock-keyword-face))
    (bi   . (font-lock-builtin-face))
    (pp   . (font-lock-preprocessor-face))
    (fnd  . (font-lock-function-name-face))
    (fnc  . (font-lock-function-call-face))
    (ty   . (font-lock-type-face))
    (prop . (font-lock-property-name-face font-lock-property-use-face))
    (con  . (font-lock-constant-face))
    (num  . (font-lock-number-face))
    (str  . (font-lock-string-face))
    (esc  . (font-lock-escape-face))
    (re   . (font-lock-regexp-face))
    (doc  . (font-lock-doc-face))
    (cm   . (font-lock-comment-face))
    (cmd  . (font-lock-comment-delimiter-face))
    (var  . (font-lock-variable-name-face font-lock-variable-use-face))
    (op   . (font-lock-operator-face))
    (punc . (font-lock-punctuation-face font-lock-bracket-face
             font-lock-delimiter-face font-lock-misc-punctuation-face)))
  "Map each theme.json syntax-category key to the font-lock faces it colors.
A category may fan out to several faces (e.g. punc covers bracket and
delimiter too).  The dec (decorator) key is deliberately absent: Emacs has
no dedicated decorator face -- it renders decorators with
`font-lock-type-face', which the ty key already owns -- so coloring dec
independently is not possible without clobbering types.")

;;; ---------------------------------------------------------------------------
;;; Pure helpers

(defun build-theme/--hex-p (s)
  "Non-nil when S is a \"#rrggbb\" hex color string."
  (and (stringp s) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" s)))

(defun build-theme/--attrs (inherit fg bg bold italic height)
  "Build a face-attribute plist from the given fields, in canonical order.
INHERIT is a face symbol or nil.  FG and BG are hex strings or nil.  BOLD and
ITALIC are booleans.  HEIGHT is a float multiplier; 1.0 (or nil) is omitted as
the default.  Only set attributes are written, so a fully-nil face yields an
empty plist."
  (let (plist)
    (when (and height (numberp height) (/= height 1.0))
      (setq plist (list :height height)))
    (when italic (setq plist (append (list :slant 'italic) plist)))
    (when bold (setq plist (append (list :weight 'bold) plist)))
    (when bg (setq plist (append (list :background bg) plist)))
    (when fg (setq plist (append (list :foreground fg) plist)))
    (when inherit (setq plist (append (list :inherit inherit) plist)))
    plist))

(defun build-theme/--face-spec (face attrs)
  "Wrap FACE and its ATTRS plist as a `custom-theme-set-faces' spec.
Return nil when ATTRS is empty, so cleared faces emit nothing."
  (when attrs
    (list face (list (list t attrs)))))

(defun build-theme/--obj-get (obj key)
  "Value of KEY in alist OBJ, or nil."
  (cdr (assq key obj)))

(defun build-theme/--inherit-symbol (value)
  "Coerce an inherit VALUE (a face-name string, symbol, or nil) to a symbol."
  (cond ((null value) nil)
        ((symbolp value) value)
        ((stringp value) (intern value))
        (t nil)))

;;; ---------------------------------------------------------------------------
;;; Tiers

(defun build-theme/--default-spec (assignments)
  "Build the `default' face spec from ASSIGNMENTS bg / p."
  (let ((bg (build-theme/--obj-get assignments 'bg))
        (fg (build-theme/--obj-get assignments 'p)))
    (build-theme/--face-spec 'default (build-theme/--attrs nil fg bg nil nil nil))))

(defun build-theme/--syntax-face-specs (assignments bold italic)
  "Build syntax-tier face specs from ASSIGNMENTS plus the BOLD and ITALIC sets.
BOLD and ITALIC are lists of category-key symbols.  Each category fans out to
the font-lock faces in `build-theme/--syntax-face-map'."
  (let (specs)
    (dolist (pair build-theme/--syntax-face-map)
      (let* ((cat (car pair))
             (faces (cdr pair))
             (hex (build-theme/--obj-get assignments cat)))
        (when hex
          (let ((attrs (build-theme/--attrs nil hex nil
                                            (memq cat bold) (memq cat italic) nil)))
            (dolist (face faces)
              (when-let ((spec (build-theme/--face-spec face attrs)))
                (push spec specs)))))))
    (nreverse specs)))

(defun build-theme/--ui-face-specs (ui)
  "Build UI-tier face specs from the UI alist (face -> {fg,bg,bold,italic})."
  (let (specs)
    (dolist (entry ui)
      (let* ((face (car entry))
             (obj (cdr entry))
             (attrs (build-theme/--attrs nil
                                         (build-theme/--obj-get obj 'fg)
                                         (build-theme/--obj-get obj 'bg)
                                         (build-theme/--obj-get obj 'bold)
                                         (build-theme/--obj-get obj 'italic)
                                         nil)))
        (when-let ((spec (build-theme/--face-spec face attrs)))
          (push spec specs))))
    (nreverse specs)))

(defun build-theme/--package-face-specs (packages)
  "Build package-tier face specs from the PACKAGES alist (app -> face -> spec)."
  (let (specs)
    (dolist (app packages)
      (dolist (entry (cdr app))
        (let* ((face (car entry))
               (obj (cdr entry))
               (attrs (build-theme/--attrs
                       (build-theme/--inherit-symbol (build-theme/--obj-get obj 'inherit))
                       (build-theme/--obj-get obj 'fg)
                       (build-theme/--obj-get obj 'bg)
                       (build-theme/--obj-get obj 'bold)
                       (build-theme/--obj-get obj 'italic)
                       (build-theme/--obj-get obj 'height))))
          (when-let ((spec (build-theme/--face-spec face attrs)))
            (push spec specs)))))
    (nreverse specs)))

(defun build-theme/--all-specs (data)
  "Build the full ordered face-spec list from parsed theme.json DATA."
  (let ((assignments (build-theme/--obj-get data 'assignments))
        (bold (mapcar #'intern (build-theme/--obj-get data 'bold)))
        (italic (mapcar #'intern (build-theme/--obj-get data 'italic)))
        (ui (build-theme/--obj-get data 'ui))
        (packages (build-theme/--obj-get data 'packages)))
    (delq nil
          (append
           (list (build-theme/--default-spec assignments))
           (build-theme/--syntax-face-specs assignments bold italic)
           (build-theme/--ui-face-specs ui)
           (build-theme/--package-face-specs packages)))))

;;; ---------------------------------------------------------------------------
;;; Rendering

(defun build-theme/--render (name specs)
  "Render a deftheme file body for theme NAME from face SPECS, as a string."
  (concat
   (format ";;; %s-theme.el --- Generated by theme-selector -*- lexical-binding: t -*-\n" name)
   "\n;;; Commentary:\n"
   (format ";; Generated from %s.json by scripts/theme-selector/build-theme.el.\n" name)
   ";; Do not hand-edit; re-run the converter.\n"
   "\n;;; Code:\n\n"
   (format "(deftheme %s\n  \"Generated by theme-selector.\")\n\n" name)
   (format "(custom-theme-set-faces\n '%s\n" name)
   ;; Each spec is quoted: custom-theme-set-faces is a function, so an
   ;; unquoted (face ((t ...))) would be evaluated as a call.  Specs hold
   ;; only literal strings, symbols, and numbers, so a plain quote suffices.
   (mapconcat (lambda (spec) (concat " '" (prin1-to-string spec))) specs "\n")
   ")\n\n"
   (format "(provide-theme '%s)\n" name)
   (format ";;; %s-theme.el ends here\n" name)))

(defun build-theme/--parse (json-file)
  "Parse JSON-FILE into an alist, with null/false as nil and arrays as lists.
Signal a `file-missing' error when JSON-FILE does not exist."
  (unless (file-readable-p json-file)
    (signal 'file-missing (list "Cannot read theme.json" json-file)))
  (with-temp-buffer
    (insert-file-contents json-file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

;;; ---------------------------------------------------------------------------
;;; Entry point

(defun build-theme/convert-file (json-file &optional out-dir)
  "Convert JSON-FILE (a theme.json export) into a deftheme file.
Write themes/<name>-theme.el, where <name> is the JSON name field, into
OUT-DIR (default: the themes/ directory of this repo).  Return the written
path."
  (let* ((data (build-theme/--parse json-file))
         (name (build-theme/--obj-get data 'name))
         (specs (build-theme/--all-specs data))
         (dir (or out-dir
                  (expand-file-name
                   "../../themes"
                   (file-name-directory (or load-file-name buffer-file-name
                                            default-directory)))))
         (out (expand-file-name (format "%s-theme.el" name) dir)))
    (unless (and (stringp name) (string-match-p "\\`[a-zA-Z][a-zA-Z0-9-]*\\'" name))
      (error "Invalid theme name in %s: %S" json-file name))
    (make-directory dir t)
    (with-temp-file out
      (insert (build-theme/--render name specs)))
    out))

(provide 'build-theme)
;;; build-theme.el ends here
