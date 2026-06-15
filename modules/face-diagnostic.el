;;; face-diagnostic.el --- Diagnose the face and font at point -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Added features).
;; Category: O (optional command).
;; Load shape: command-loaded (no startup side effects; pure defuns).
;;
;; A read-only diagnostic for "why does the character at point paint this way?"
;; It separates the face stack by source (text properties, overlays, active
;; remaps, the default) and -- in later phases -- the merged attributes, the
;; real font versus the declared family, and per-face theme/config/inherit
;; provenance.  See docs/specs/face-font-diagnostic-popup-spec.org.
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
     ((derived-mode-p 'term-mode 'comint-mode 'eshell-mode 'ghostel-mode)
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

;; ------------------------------- Assembled core ------------------------------

(defun cj/--face-diagnosis-at (pos &optional buffer)
  "Return the face-diagnosis plist for POS in BUFFER (Phase 1: groups 0-2).
Keys: :classification (symbol), :char (plist or nil at end-of-buffer), :stack
\(plist).  Pure: no prompts, no display, no buffer or frame mutation."
  (list :classification (cj/--face-diag-classify-buffer buffer)
        :char (cj/--face-diag-char-context pos buffer)
        :stack (cj/--face-diag-stack pos buffer)))

(provide 'face-diagnostic)
;;; face-diagnostic.el ends here
