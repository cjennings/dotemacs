;;; nov-reading.el --- Reading-view theme layer for nov-mode EPUBs -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Added features).
;; Category: O (optional commands + faces).
;; Load shape: eager.
;; Eager reason: defines the reading faces and commands the nov launch hook and
;;   keymap reference; the faces must exist for theme-studio's inventory too.
;; Top-level side effects: defface x9 (3 palettes + per-palette heading/link),
;;   defcustoms, a defgroup, a defvar.
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
;;     The live size is remembered globally, so every book opens where you left
;;     it; "=" returns to the base height.
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

;; Structural faces: recolor shr's heading (h1-h6) and link faces per palette,
;; remapped buffer-local so the EPUB's hierarchy reads in the palette's accent
;; while mail/eww (the other shr consumers) keep the theme's shr colors.  Heading
;; faces carry :foreground only -- shr's per-level height and weight survive the
;; relative remap; link faces add :underline so the cue reads as a link.

(defface cj/nov-reading-sepia-heading
  '((t :foreground "#e6c98a"))
  "Heading accent for the sepia reading palette (recolors shr-h1..h6)."
  :group 'cj/nov-reading)

(defface cj/nov-reading-sepia-link
  '((t :foreground "#c98f5a" :underline t))
  "Link accent for the sepia reading palette (recolors shr-link)."
  :group 'cj/nov-reading)

(defface cj/nov-reading-dark-heading
  '((t :foreground "#e8e0cc"))
  "Heading accent for the dark reading palette (recolors shr-h1..h6)."
  :group 'cj/nov-reading)

(defface cj/nov-reading-dark-link
  '((t :foreground "#8fb0c4" :underline t))
  "Link accent for the dark reading palette (recolors shr-link)."
  :group 'cj/nov-reading)

(defface cj/nov-reading-light-heading
  '((t :foreground "#5a3d28"))
  "Heading accent for the light reading palette (recolors shr-h1..h6)."
  :group 'cj/nov-reading)

(defface cj/nov-reading-light-link
  '((t :foreground "#8a5a2a" :underline t))
  "Link accent for the light reading palette (recolors shr-link)."
  :group 'cj/nov-reading)

(defcustom cj/nov-reading-palettes
  '(("sepia" :face cj/nov-reading-sepia
             :heading cj/nov-reading-sepia-heading
             :link cj/nov-reading-sepia-link)
    ("dark"  :face cj/nov-reading-dark
             :heading cj/nov-reading-dark-heading
             :link cj/nov-reading-dark-link)
    ("light" :face cj/nov-reading-light
             :heading cj/nov-reading-light-heading
             :link cj/nov-reading-light-link))
  "Alist of reading-palette NAME -> face property list for nov-mode.
Each entry's plist supplies the palette's colors, all theme-owned faces:
  :face     reading-view :background and :foreground, remapped onto `default'
  :heading  recolors shr's heading faces (h1-h6) for this palette
  :link     recolors shr's link face for this palette
The selector and cycle commands choose among these names.  Add an entry to add a
palette; omit :heading or :link to leave that element at the theme's default."
  :type '(alist :key-type string
                :value-type
                (plist :options ((:face face) (:heading face) (:link face))))
  :group 'cj/nov-reading)

(defcustom cj/nov-reading-default-palette "sepia"
  "Reading palette applied to a fresh nov-mode buffer.
A key in `cj/nov-reading-palettes', or nil for the theme's normal rendering."
  :type '(choice (const :tag "None (theme default)" nil) string)
  :group 'cj/nov-reading)

(defvar-local cj/nov--reading-remap-cookies nil
  "List of `face-remap-add-relative' cookies for the active reading palette.
Covers the `default' remap and any shr heading/link remaps, so switching
palettes can remove them all at once.")

(defvar-local cj/nov--reading-palette nil
  "Name of the reading palette active in this buffer, or nil for none.")

(defun cj/nov--reading-palette-plist (name)
  "Return the face property list for palette NAME, or nil when unknown.
NAME nil (the no-palette state) and unknown names both yield nil."
  (cdr (assoc name cj/nov-reading-palettes)))

(defun cj/nov--reading-palette-face (name)
  "Return the base (bg/fg) face for palette NAME, or nil when NAME is unknown."
  (plist-get (cj/nov--reading-palette-plist name) :face))

(defun cj/nov--next-reading-palette (current names)
  "Return the palette after CURRENT in the cycle NAMES then nil, wrapping.
CURRENT nil is the no-palette state, and a returned nil means no palette.  An
unknown CURRENT falls back to the first palette."
  (let* ((cycle (append names (list nil)))
         (tail (cdr (member current cycle))))
    (car (or tail cycle))))

(defun cj/nov--apply-reading-palette (name)
  "Apply reading palette NAME buffer-local; NAME nil removes any palette.
Remaps `default' to the palette's :face, and (when present) shr's heading faces
h1-h6 to its :heading face and shr-link to its :link face.  Removes the previous
palette's remaps first so switching never stacks, and leaves the typography
remap (a separate `default' remap) untouched."
  (mapc #'face-remap-remove-relative cj/nov--reading-remap-cookies)
  (setq cj/nov--reading-remap-cookies nil)
  (let* ((plist (cj/nov--reading-palette-plist name))
         (face (plist-get plist :face)))
    (when face
      (push (face-remap-add-relative 'default face)
            cj/nov--reading-remap-cookies)
      (let ((heading (plist-get plist :heading)))
        (when heading
          (dolist (h '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6))
            (push (face-remap-add-relative h heading)
                  cj/nov--reading-remap-cookies))))
      (let ((link (plist-get plist :link)))
        (when link
          (push (face-remap-add-relative 'shr-link link)
                cj/nov--reading-remap-cookies))))
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
  "Base `default'-face height (1/10 pt) the reading view renders at.
The +/-/= keys adjust the page size from here with a buffer-local text-scale.
That adjustment is remembered globally (see `cj/nov-reading-text-scale-file'):
every book and every session opens at the size you last left it, and `='
returns to this base."
  :type 'integer
  :group 'cj/nov-reading)

(defvar cj/nov-reading-text-scale-file
  (expand-file-name "data/nov-reading-text-scale" user-emacs-directory)
  "File persisting the global reading text-scale offset across sessions.
A single integer: the buffer-local `text-scale-mode-amount' the +/-/= keys
last set, applied on top of `cj/nov-reading-text-height' when a book opens.")

(defun cj/nov-reading--parse-text-scale (s)
  "Parse S (a string or nil) as an integer text-scale offset; 0 when invalid.
Surrounding whitespace is tolerated; non-integer content yields 0."
  (let ((trimmed (and (stringp s) (string-trim s))))
    (if (and trimmed (string-match-p "\\`[+-]?[0-9]+\\'" trimmed))
        (string-to-number trimmed)
      0)))

(defun cj/nov-reading--load-text-scale ()
  "Return the persisted reading text-scale offset, or 0 when none is saved."
  (if (file-readable-p cj/nov-reading-text-scale-file)
      (cj/nov-reading--parse-text-scale
       (with-temp-buffer
         (insert-file-contents cj/nov-reading-text-scale-file)
         (buffer-string)))
    0))

(defun cj/nov-reading--save-text-scale (amount)
  "Persist AMOUNT as the global reading text-scale offset.
Creates the data directory when absent."
  (make-directory (file-name-directory cj/nov-reading-text-scale-file) t)
  (with-temp-file cj/nov-reading-text-scale-file
    (insert (number-to-string amount))))

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
  "Increase the page font size and remember it across books and sessions."
  (interactive)
  (text-scale-increase 1)
  (cj/nov-reading--save-text-scale text-scale-mode-amount))

(defun cj/nov-reading-text-smaller ()
  "Decrease the page font size and remember it across books and sessions."
  (interactive)
  (text-scale-decrease 1)
  (cj/nov-reading--save-text-scale text-scale-mode-amount))

(defun cj/nov-reading-text-reset ()
  "Reset the page font size to the base reading height; clears the saved offset."
  (interactive)
  (text-scale-set 0)
  (cj/nov-reading--save-text-scale 0))

;; ------------------------------- Launch hook ---------------------------------

(defun cj/nov-reading-setup ()
  "Apply the reading view (typography + default palette) to this nov buffer.
Restores the remembered page font size on top of the base height.
Called from the nov-mode launch hook in calibredb-epub-config.el."
  (cj/nov-reading-apply-typography)
  (text-scale-set (cj/nov-reading--load-text-scale))
  (when cj/nov-reading-default-palette
    (cj/nov--apply-reading-palette cj/nov-reading-default-palette)))

(provide 'nov-reading)
;;; nov-reading.el ends here
