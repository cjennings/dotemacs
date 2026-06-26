;;; face-diagnostic.el --- Diagnose the face and font at point -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Added features).
;; Category: O (optional command).
;; Load shape: eager.
;; Eager reason: none; a diagnostic command, a command-loaded deferral candidate.
;; Top-level side effects: defines cj/face-diagnostic-mode and the
;;   cj/describe-face-at-point command; binds it to C-h F in help-map.
;; Runtime requires: seq.
;; Direct test load: yes (the pure core is tested by requiring this module).
;;
;; A read-only diagnostic for "why does the character at point paint this way?"
;; It separates the face stack by source (text properties, overlays, active
;; remaps, the default) and -- in later phases -- the merged attributes, the
;; real font versus the declared family, and per-face theme/config/inherit
;; provenance.  See docs/specs/face-font-diagnostic-popup-spec-implemented.org.
;;
;; This file is Phase 1: the pure read model.  `cj/--face-diagnosis-at' returns
;; a plist with the buffer classification, the character context, and the face
;; stack by source.  No prompts, no display -- the interactive command and the
;; rendering land in a later phase.

;;; Code:

(require 'seq)

;; ------------------------------ Buffer classify ------------------------------

(defun cj/--face-diag-classify-buffer (&optional buffer)
  "Classify BUFFER (default current) for face-diagnosis scope.
Return one of `theme-faced', `terminal-ansi', `document-shr', or
`image-no-text', from the major mode.  Out-of-scope buckets get a banner and a
best-effort dump rather than a full provenance trace."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((derived-mode-p 'term-mode 'comint-mode 'eshell-mode 'eat-mode)
      'terminal-ansi)
     ((derived-mode-p 'eww-mode 'nov-mode 'elfeed-show-mode 'mu4e-view-mode)
      'document-shr)
     ((derived-mode-p 'image-mode 'doc-view-mode 'pdf-view-mode)
      'image-no-text)
     (t 'theme-faced))))

;; ----------------------------- Character context -----------------------------

(defun cj/--face-diag-char-context (pos &optional buffer)
  "Return a plist for the character at POS in BUFFER, or nil when there is none.
Keys: :char (the character), :codepoint (its integer value), :name (the Unicode
name string or nil), :script (the script symbol or nil)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((ch (char-after pos)))
      (when ch
        (list :char ch
              :codepoint ch
              :name (get-char-code-property ch 'name)
              :script (aref char-script-table ch))))))

;; ------------------------------- Face stack ----------------------------------

(defun cj/--face-diag-normalize-faces (val)
  "Normalize a `face'-style property VAL into a list of faces or specs.
A face symbol or an anonymous (:attr val ...) plist becomes a one-element list;
a list of faces is returned as-is; nil becomes nil."
  (cond
   ((null val) nil)
   ((symbolp val) (list val))
   ((keywordp (car-safe val)) (list val))   ; anonymous spec, e.g. (:foreground "red")
   ((listp val) val)
   (t (list val))))

(defun cj/--face-diag-text-property-faces (pos &optional buffer)
  "Return the faces from the `face' and `font-lock-face' props at POS in BUFFER.
The two properties are concatenated in that order, each normalized to a list."
  (with-current-buffer (or buffer (current-buffer))
    (let ((result '()))
      (dolist (prop '(face font-lock-face))
        (setq result (append result
                             (cj/--face-diag-normalize-faces
                              (get-text-property pos prop)))))
      result)))

(defun cj/--face-diag-overlay-faces (pos &optional buffer)
  "Return overlay face entries covering POS in BUFFER, highest priority first.
Each entry is a plist with :face, :priority (number or nil), and :overlay.
Overlays without a `face' property are skipped."
  (with-current-buffer (or buffer (current-buffer))
    (let ((entries
           (delq nil
                 (mapcar
                  (lambda (ov)
                    (let ((face (overlay-get ov 'face)))
                      (when face
                        (list :face face
                              :priority (overlay-get ov 'priority)
                              :overlay ov))))
                  (overlays-at pos)))))
      (sort entries
            (lambda (a b)
              (> (or (plist-get a :priority) 0)
                 (or (plist-get b :priority) 0)))))))

(defun cj/--face-diag-active-remaps (faces &optional buffer)
  "Return the `face-remapping-alist' entries in BUFFER that remap any of FACES.
FACES is a list of face symbols from the stack.  Each result is the raw
\(FACE . SPEC) entry from the alist."
  (with-current-buffer (or buffer (current-buffer))
    (seq-filter (lambda (entry) (memq (car-safe entry) faces))
                face-remapping-alist)))

(defun cj/--face-diag-stack (pos &optional buffer)
  "Return the face stack at POS in BUFFER as a plist separated by source.
Keys: :text-property (list of faces/specs), :overlays (list of plists),
:remaps (matching `face-remapping-alist' entries), :default (the symbol
`default')."
  (let* ((tp (cj/--face-diag-text-property-faces pos buffer))
         (ov (cj/--face-diag-overlay-faces pos buffer))
         (stack-syms
          (append (seq-filter #'symbolp tp)
                  (delq nil (mapcar (lambda (e)
                                      (let ((f (plist-get e :face)))
                                        (and (symbolp f) f)))
                                    ov))
                  '(default))))
    (list :text-property tp
          :overlays ov
          :remaps (cj/--face-diag-active-remaps stack-syms buffer)
          :default 'default)))

;; -------------------------- Effective merged attributes ----------------------
;; Emacs exposes no single call for the final merged attribute plist at a
;; position (the C redisplay engine merges text-prop + overlay faces, applies
;; remaps, and picks a font).  The core folds the ordered, remap-expanded spec
;; list itself and labels the result "computed": exotic relative-height or deep
;; :inherit cases may diverge slightly from the engine.

(defconst cj/--face-diag-attributes
  '(:family :height :weight :slant :foreground :background
    :underline :overline :strike-through :box :inverse-video)
  "Face attributes reported in the effective-merge group, in display order.")

(defun cj/--face-diag-spec-attr (spec attr)
  "Return ATTR's value from a single face SPEC, or the symbol `unspecified'.
A face symbol resolves through `face-attribute' (following :inherit); an
attribute plist is read directly; anything else is `unspecified'."
  (cond
   ((and spec (symbolp spec)) (face-attribute spec attr nil t))
   ((and (consp spec) (keywordp (car spec)))
    (if (plist-member spec attr) (plist-get spec attr) 'unspecified))
   (t 'unspecified)))

(defun cj/--face-diag-remap-specs (face &optional buffer)
  "Return the remap specs for FACE from `face-remapping-alist' in BUFFER, or nil.
Only symbol faces are looked up.  The remapping is normalized to a list of
specs: a lone face symbol or an attribute plist becomes a one-element list."
  (with-current-buffer (or buffer (current-buffer))
    (when (symbolp face)
      (let ((entry (assq face face-remapping-alist)))
        (when entry
          (let ((remap (cdr entry)))
            (cond
             ((null remap) nil)
             ((keywordp (car-safe remap)) (list remap))   ; (:attr val ...)
             ((listp remap) remap)                         ; (spec spec ...)
             (t (list remap)))))))))                       ; a lone face symbol

(defun cj/--face-diag-ordered-specs (pos &optional buffer)
  "Return the ordered face specs at POS in BUFFER, highest priority first.
Overlay faces (priority descending), then text-property faces, then the
default.  Each contributing face's remap specs come ahead of the face itself,
mirroring how a remap overrides its base."
  (let ((bases (append (mapcar (lambda (e) (plist-get e :face))
                               (cj/--face-diag-overlay-faces pos buffer))
                       (cj/--face-diag-text-property-faces pos buffer)
                       '(default)))
        (specs '()))
    (dolist (face bases)
      (setq specs (append specs
                          (cj/--face-diag-remap-specs face buffer)
                          (list face))))
    specs))

(defun cj/--face-diag-merged-attributes (pos &optional buffer)
  "Return the computed effective attribute plist at POS in BUFFER.
For each attribute the first non-`unspecified' value down the ordered,
remap-expanded spec list wins; if none specifies it the value is `unspecified'."
  (let ((specs (cj/--face-diag-ordered-specs pos buffer))
        (result '()))
    (dolist (attr cj/--face-diag-attributes)
      (let ((found (seq-some (lambda (spec)
                               (let ((v (cj/--face-diag-spec-attr spec attr)))
                                 (unless (eq v 'unspecified) (list v))))
                             specs)))
        (setq result (append result (list attr (if found (car found) 'unspecified))))))
    result))

;; ------------------------------- Real font -----------------------------------

(defun cj/--face-diag-real-font (pos &optional buffer)
  "Return a plist for the font actually used at POS in BUFFER.
Keys: :font (the font's name, or \"unavailable\") and :family (its family or
nil).  `font-at' is nil in batch and on text terminals, reported as
\"unavailable\" rather than an error -- this exposes fontset substitution when
the real family differs from the merged :family."
  (with-current-buffer (or buffer (current-buffer))
    (let ((font (ignore-errors (font-at pos))))
      (if (null font)
          (list :font "unavailable" :family nil)
        (list :font (or (ignore-errors (font-get font :name))
                        (ignore-errors (aref (query-font font) 0))
                        "unknown")
              :family (ignore-errors (font-get font :family)))))))

;; ------------------------------ Provenance -----------------------------------
;; Where a named face's attributes come from: which themes set it, whether
;; config saved/customized it, its :inherit chain, and which attributes stay
;; unspecified so they fall through to the default.  The theme-face and
;; saved-face properties are version-sensitive internals, read behind small
;; accessors and treated as absent rather than erroring when missing.

(defun cj/--face-diag-face-themes (face)
  "Return the themes that set FACE, newest first, from its `theme-face' property."
  (when (symbolp face)
    (mapcar #'car (get face 'theme-face))))

(defun cj/--face-diag-config-source (face)
  "Return how config set FACE: `saved', `customized', or nil.
`saved' is a persisted customize (saved-face); `customized' is an unsaved
customize this session.  A plain `set-face-attribute' leaves no marker and so
reads as nil."
  (cond
   ((get face 'saved-face) 'saved)
   ((get face 'customized-face) 'customized)
   (t nil)))

(defun cj/--face-diag-inherit-chain (face)
  "Return FACE's :inherit chain as a list of faces, nearest first.
Follows single-symbol :inherit links, guarding against cycles; a list-valued
:inherit is recorded and the walk stops there."
  (let ((chain '()) (cur face) (seen '()))
    (while (and cur (symbolp cur) (facep cur) (not (memq cur seen)))
      (push cur seen)
      (let ((inh (face-attribute cur :inherit nil)))
        (cond
         ((or (null inh) (eq inh 'unspecified)) (setq cur nil))
         ((symbolp inh) (setq chain (append chain (list inh))) (setq cur inh))
         ((listp inh) (setq chain (append chain inh)) (setq cur nil))
         (t (setq cur nil)))))
    chain))

(defun cj/--face-diag-unspecified-attrs (face)
  "Return attributes still unspecified on FACE after inherit-following.
These fall through to the default face -- the direct read on an
\"attribute never set\" bug like the all-white elfeed case."
  (when (facep face)
    (seq-filter (lambda (attr)
                  (eq (face-attribute face attr nil t) 'unspecified))
                cj/--face-diag-attributes)))

(defun cj/--face-diag-face-provenance (face)
  "Return the provenance plist for the named FACE.
Keys: :face, :themes (list), :config (`saved'/`customized'/nil),
:inherit-chain (list of faces), :unspecified (attributes falling to default)."
  (list :face face
        :themes (cj/--face-diag-face-themes face)
        :config (cj/--face-diag-config-source face)
        :inherit-chain (cj/--face-diag-inherit-chain face)
        :unspecified (cj/--face-diag-unspecified-attrs face)))

(defun cj/--face-diag-provenance (pos &optional buffer)
  "Return per-face provenance for the named faces in the stack at POS in BUFFER.
A list of provenance plists for the distinct real faces contributing at POS:
text-property and overlay face symbols, then the default."
  (let* ((tp (seq-filter #'symbolp (cj/--face-diag-text-property-faces pos buffer)))
         (ov (delq nil (mapcar (lambda (e)
                                 (let ((f (plist-get e :face)))
                                   (and (symbolp f) f)))
                               (cj/--face-diag-overlay-faces pos buffer))))
         (faces (seq-filter #'facep (seq-uniq (append ov tp '(default))))))
    (mapcar #'cj/--face-diag-face-provenance faces)))

;; ------------------------------- Assembled core ------------------------------

(defun cj/--face-diagnosis-at (pos &optional buffer)
  "Return the face-diagnosis plist for POS in BUFFER (groups 0-5).
Keys: :classification (symbol), :char (plist or nil at end-of-buffer), :stack
\(plist), :attributes (computed merged plist), :font (real-font plist),
:provenance (per-face list).  Pure: no prompts, no display, no buffer or frame
mutation."
  (list :classification (cj/--face-diag-classify-buffer buffer)
        :char (cj/--face-diag-char-context pos buffer)
        :stack (cj/--face-diag-stack pos buffer)
        :attributes (cj/--face-diag-merged-attributes pos buffer)
        :font (cj/--face-diag-real-font pos buffer)
        :provenance (cj/--face-diag-provenance pos buffer)))

;; ------------------------------- Rendering -----------------------------------

(defun cj/--face-diag-face-button (face)
  "Render FACE as a button that runs `describe-face' on it.
A real, named face becomes a `buttonize'd string (RET or mouse opens its
`describe-face' help); anything else -- an anonymous (:attr val ...) spec or a
symbol that is not a face -- is returned as a plain string so the report still
reads cleanly."
  (let ((label (format "%s" face)))
    (if (and (symbolp face) (facep face))
        (buttonize label (lambda (f) (describe-face f)) face
                   (format "describe-face: %s" face))
      label)))

(defun cj/--face-diag-render-banner (classification)
  "Return a one-line banner for an out-of-scope CLASSIFICATION, or \"\"."
  (pcase classification
    ('terminal-ansi
     "NOTE: terminal buffer -- colors come from the ANSI palette, not the theme.\n\n")
    ('document-shr
     "NOTE: document buffer -- colors come from the rendered document, not the theme.\n\n")
    ('image-no-text
     "NOTE: image/no-text buffer -- little face information applies here.\n\n")
    (_ "")))

(defun cj/--face-diag-render-char (char)
  "Render the CHAR context plist as a line, or a no-character notice."
  (if (null char)
      "Character: none at point.\n\n"
    (format "Character: %S  (U+%04X %s, script: %s)\n\n"
            (plist-get char :char)
            (plist-get char :codepoint)
            (or (plist-get char :name) "no name")
            (or (plist-get char :script) "none"))))

(defun cj/--face-diag-render-faces (faces)
  "Render a list of FACES (symbols or specs) comma-separated, or \"(none)\".
Real faces render as `describe-face' buttons (see `cj/--face-diag-face-button')."
  (if faces (mapconcat #'cj/--face-diag-face-button faces ", ") "(none)"))

(defun cj/--face-diag-render-stack (stack)
  "Render the STACK plist (faces by source) as a block."
  (concat
   "Face stack (highest priority first):\n"
   (format "  text properties: %s\n"
           (cj/--face-diag-render-faces (plist-get stack :text-property)))
   "  overlays: "
   (let ((ov (plist-get stack :overlays)))
     (if ov
         (mapconcat (lambda (e)
                      (concat (cj/--face-diag-face-button (plist-get e :face))
                              (format " (priority %s)"
                                      (or (plist-get e :priority) "nil"))))
                    ov ", ")
       "(none)"))
   "\n"
   "  active remaps: "
   (let ((rm (plist-get stack :remaps)))
     (if rm (mapconcat (lambda (e) (cj/--face-diag-face-button (car e))) rm ", ")
       "(none)"))
   "\n"
   "  default: default\n\n"))

(defun cj/--face-diag-render-attributes (attrs)
  "Render the merged ATTRS plist as a block."
  (concat
   "Effective attributes (computed):\n"
   (mapconcat (lambda (attr) (format "  %s: %s" attr (plist-get attrs attr)))
              cj/--face-diag-attributes "\n")
   "\n\n"))

(defun cj/--face-diag-render-font (font attrs)
  "Render the real FONT plist beside the merged ATTRS declared :family."
  (let ((real (plist-get font :font))
        (declared (plist-get attrs :family))
        (real-family (plist-get font :family)))
    (concat
     (format "Real font: %s\n" real)
     (format "Declared family: %s\n" declared)
     (if (and (stringp real-family) (stringp declared)
              (not (string-equal-ignore-case real-family declared)))
         (format "  (substituted: real family %s differs from declared %s)\n\n"
                 real-family declared)
       "\n"))))

(defun cj/--face-diag-render-provenance (prov)
  "Render the per-face PROV list as a block."
  (concat
   "Provenance:\n"
   (if prov
       (mapconcat
        (lambda (p)
          (concat
           "  "
           (cj/--face-diag-face-button (plist-get p :face))
           (format (concat "\n    themes: %s\n    config: %s\n"
                           "    inherits: %s\n    unspecified (-> default): %s")
                   (or (plist-get p :themes) "(none)")
                   (or (plist-get p :config) "(none)")
                   (or (plist-get p :inherit-chain) "(none)")
                   (or (plist-get p :unspecified) "(none)"))))
        prov "\n")
     "  (no named faces)")
   "\n"))

(defun cj/--face-diag-render (diag)
  "Render the face-diagnosis DIAG plist into a report string."
  (concat
   (cj/--face-diag-render-banner (plist-get diag :classification))
   (cj/--face-diag-render-char (plist-get diag :char))
   (cj/--face-diag-render-stack (plist-get diag :stack))
   (cj/--face-diag-render-attributes (plist-get diag :attributes))
   (cj/--face-diag-render-font (plist-get diag :font) (plist-get diag :attributes))
   (cj/--face-diag-render-provenance (plist-get diag :provenance))))

;; ------------------------------- Region mode ---------------------------------

(defun cj/--face-diag-run-starts (beg end)
  "Return the positions in [BEG, END) where the `face' property run begins."
  (let ((pos beg) (starts (list beg)))
    (while (and (setq pos (next-single-property-change pos 'face nil end))
                (< pos end))
      (push pos starts))
    (nreverse starts)))

(defun cj/--face-diag-render-region (beg end)
  "Render a diagnosis for each distinct face-run in [BEG, END), capped at 20."
  (let* ((starts (cj/--face-diag-run-starts beg end))
         (cap 20)
         (shown (seq-take starts cap)))
    (concat
     (mapconcat (lambda (pos)
                  (concat (format "=== position %d ===\n" pos)
                          (cj/--face-diag-render (cj/--face-diagnosis-at pos))))
                shown "\n")
     (when (> (length starts) cap)
       (format "\n... %d more face-runs not shown (cap %d).\n"
               (- (length starts) cap) cap)))))

;; ------------------------------- Command -------------------------------------

(define-derived-mode cj/face-diagnostic-mode special-mode "Face-Diag"
  "Major mode for the read-only face/font diagnosis report.")

(defun cj/--face-diag-display (report)
  "Show REPORT in the read-only *Face Diagnosis* buffer; return the buffer."
  (let ((buf (get-buffer-create "*Face Diagnosis*")))
    (with-current-buffer buf
      (cj/face-diagnostic-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report)
        (goto-char (point-min))))
    (display-buffer buf)
    buf))

(defun cj/describe-face-at-point ()
  "Pop up a read-only diagnosis of the face and font at point.
With an active region, diagnose each distinct face-run in the region.  The
report separates the face stack by source, shows the computed merged
attributes, the real font versus the declared family, and per-face
theme/config/inherit provenance.  Read-only; never mutates buffer or frame.
See docs/specs/face-font-diagnostic-popup-spec-implemented.org."
  (interactive)
  (cj/--face-diag-display
   (if (use-region-p)
       (cj/--face-diag-render-region (region-beginning) (region-end))
     (cj/--face-diag-render (cj/--face-diagnosis-at (point))))))

;; Bound on C-h F (Face) in the help cluster.  This shadows helpful-function,
;; which also sits on C-h F here; face-diagnostic loads after help-config, so
;; this binding wins.
(keymap-set help-map "F" #'cj/describe-face-at-point)

(provide 'face-diagnostic)
;;; face-diagnostic.el ends here
