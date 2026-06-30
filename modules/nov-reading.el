;;; nov-reading.el --- Reading-view theme layer for nov-mode EPUBs -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Added features).
;; Category: O (optional commands + faces).
;; Load shape: eager.
;; Eager reason: defines the reading faces and commands the nov launch hook and
;;   keymap reference; the faces must exist for theme-studio's inventory too.
;; Top-level side effects: defface x3, defcustoms, a defgroup.
;; Runtime requires: none (face-remap and text-scale are built in).
;; Direct test load: yes.
;;
;; A small theme layer on top of the stock `nov' package (no fork): how an EPUB
;; *reads*, kept buffer-local so it never disturbs the frame or other buffers.
;; Two knobs:
;;
;;   - Reading palette -- the background + foreground, as sepia / dark / light,
;;     each a face the dupre theme / theme-studio own (registered as the
;;     "nov-reading" bespoke app in theme-studio's face_data.py).
;;   - Typography -- a serif family and a base height, with +/-/= adjusting the
;;     page font size live via a buffer-local text-scale on top of the base.
;;
;; calibredb-epub-config.el owns the library/calibre side and the text-width /
;; centering layout; this module owns reading color and typography.  Its launch
;; entry point `cj/nov-reading-setup' is called from that module's nov-mode hook.

;;; Code:

(defgroup cj/nov-reading nil
  "Reading-view theming for nov-mode EPUBs."
  :group 'cj)

;; ----------------------------- Reading palettes ------------------------------
;; nov renders through shr and defines no faces, so a palette is a buffer-local
;; face-remap of `default'.  Each palette is one face carrying a :background and
;; :foreground, so the theme owns the real colors (the hex defaults here are a
;; starting point to tune in theme-studio).

(defface cj/nov-reading-sepia
  '((t :background "#1f1b16" :foreground "#c9b187"))
  "Sepia reading palette for nov-mode: warm dark background, tan text."
  :group 'cj/nov-reading)

(defface cj/nov-reading-dark
  '((t :background "#15140f" :foreground "#cfc8b8"))
  "Dark reading palette for nov-mode: near-black background, light-gray text."
  :group 'cj/nov-reading)

(defface cj/nov-reading-light
  '((t :background "#ece3cf" :foreground "#2a2622"))
  "Light reading palette for nov-mode: cream background, near-black text."
  :group 'cj/nov-reading)

(defcustom cj/nov-reading-palettes
  '(("sepia" . cj/nov-reading-sepia)
    ("dark"  . cj/nov-reading-dark)
    ("light" . cj/nov-reading-light))
  "Alist of reading-palette NAME -> face for nov-mode.
Each face supplies the reading view's :background and :foreground; the selector
and cycle commands choose among these names.  Add an entry to add a palette."
  :type '(alist :key-type string :value-type face)
  :group 'cj/nov-reading)

(defcustom cj/nov-reading-default-palette "sepia"
  "Reading palette applied to a fresh nov-mode buffer.
A key in `cj/nov-reading-palettes', or nil for the theme's normal rendering."
  :type '(choice (const :tag "None (theme default)" nil) string)
  :group 'cj/nov-reading)

(defvar-local cj/nov--reading-remap-cookie nil
  "The `face-remap-add-relative' cookie for the active reading palette, or nil.")

(defvar-local cj/nov--reading-palette nil
  "Name of the reading palette active in this buffer, or nil for none.")

(defun cj/nov--reading-palette-face (name)
  "Return the face for palette NAME, or nil when NAME is nil or unknown."
  (cdr (assoc name cj/nov-reading-palettes)))

(defun cj/nov--next-reading-palette (current names)
  "Return the palette after CURRENT in the cycle NAMES then nil, wrapping.
CURRENT nil is the no-palette state, and a returned nil means no palette.  An
unknown CURRENT falls back to the first palette."
  (let* ((cycle (append names (list nil)))
         (tail (cdr (member current cycle))))
    (car (or tail cycle))))

(defun cj/nov--apply-reading-palette (name)
  "Apply reading palette NAME buffer-local; NAME nil removes any palette.
Removes the previous palette remap first so switching never stacks remaps, and
leaves the typography remap (a separate `default' remap) untouched."
  (when cj/nov--reading-remap-cookie
    (face-remap-remove-relative cj/nov--reading-remap-cookie)
    (setq cj/nov--reading-remap-cookie nil))
  (let ((face (cj/nov--reading-palette-face name)))
    (when face
      (setq cj/nov--reading-remap-cookie
            (face-remap-add-relative 'default face)))
    (setq cj/nov--reading-palette (and face name))))

(defun cj/nov-set-reading-palette (name)
  "Choose reading palette NAME for this nov buffer; \"none\" clears it.
Interactively prompts among `cj/nov-reading-palettes' plus \"none\"."
  (interactive
   (list (completing-read "Reading palette: "
                          (cons "none" (mapcar #'car cj/nov-reading-palettes))
                          nil t)))
  (unless (derived-mode-p 'nov-mode)
    (user-error "Not in a nov-mode buffer"))
  (cj/nov--apply-reading-palette (unless (equal name "none") name))
  (message "Reading palette: %s" (or cj/nov--reading-palette "none")))

(defun cj/nov-cycle-reading-palette ()
  "Cycle to the next reading palette, then the no-palette state, wrapping."
  (interactive)
  (unless (derived-mode-p 'nov-mode)
    (user-error "Not in a nov-mode buffer"))
  (let ((next (cj/nov--next-reading-palette
               cj/nov--reading-palette
               (mapcar #'car cj/nov-reading-palettes))))
    (cj/nov--apply-reading-palette next)
    (message "Reading palette: %s" (or next "none"))))

;; ------------------------------- Typography ----------------------------------

(defcustom cj/nov-reading-font-family "Merriweather"
  "Variable-pitch serif family for the EPUB reading view."
  :type 'string
  :group 'cj/nov-reading)

(defcustom cj/nov-reading-text-height 180
  "Base `default'-face height (1/10 pt) a fresh nov buffer opens at.
The +/-/= keys adjust the page size from here with a buffer-local text-scale;
that adjustment resets to this base each time a book is opened."
  :type 'integer
  :group 'cj/nov-reading)

(defun cj/nov-reading-apply-typography ()
  "Apply the reading family and base height buffer-local.
Remaps `variable-pitch', `default', and `fixed-pitch' so nov's shr output reads
as a comfortably-sized serif page."
  (face-remap-add-relative 'variable-pitch
                           :family cj/nov-reading-font-family :height 1.0)
  (face-remap-add-relative 'default
                           :family cj/nov-reading-font-family
                           :height cj/nov-reading-text-height)
  (face-remap-add-relative 'fixed-pitch :height cj/nov-reading-text-height))

(defun cj/nov-reading-text-bigger ()
  "Increase the page font size (buffer-local), on top of the base height."
  (interactive)
  (text-scale-increase 1))

(defun cj/nov-reading-text-smaller ()
  "Decrease the page font size (buffer-local), on top of the base height."
  (interactive)
  (text-scale-decrease 1))

(defun cj/nov-reading-text-reset ()
  "Reset the page font size back to the base reading height (buffer-local)."
  (interactive)
  (text-scale-set 0))

;; ------------------------------- Launch hook ---------------------------------

(defun cj/nov-reading-setup ()
  "Apply the reading view (typography + default palette) to this nov buffer.
Called from the nov-mode launch hook in calibredb-epub-config.el."
  (cj/nov-reading-apply-typography)
  (when cj/nov-reading-default-palette
    (cj/nov--apply-reading-palette cj/nov-reading-default-palette)))

(provide 'nov-reading)
;;; nov-reading.el ends here
