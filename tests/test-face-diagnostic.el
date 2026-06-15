;;; test-face-diagnostic.el --- Tests for the Phase 1 face-diagnosis core -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pure read model of the face/font diagnostic (Phase 1):
;; buffer classification, character context, and the face stack separated by
;; source.  All against temp-buffer fixtures with planted text properties,
;; overlays, and face remaps -- no display, no prompts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'face-diagnostic)

;;; cj/--face-diag-classify-buffer

(ert-deftest test-face-diag-classify-theme-faced ()
  "Normal: an ordinary buffer classifies as theme-faced."
  (with-temp-buffer
    (fundamental-mode)
    (should (eq (cj/--face-diag-classify-buffer) 'theme-faced))))

(ert-deftest test-face-diag-classify-terminal ()
  "Boundary: a terminal-family mode classifies as terminal-ansi."
  (with-temp-buffer
    (setq major-mode 'term-mode)
    (should (eq (cj/--face-diag-classify-buffer) 'terminal-ansi))))

(ert-deftest test-face-diag-classify-document ()
  "Boundary: an shr-rendering mode classifies as document-shr."
  (with-temp-buffer
    (setq major-mode 'eww-mode)
    (should (eq (cj/--face-diag-classify-buffer) 'document-shr))))

(ert-deftest test-face-diag-classify-image ()
  "Boundary: an image/document-view mode classifies as image-no-text."
  (with-temp-buffer
    (setq major-mode 'image-mode)
    (should (eq (cj/--face-diag-classify-buffer) 'image-no-text))))

;;; cj/--face-diag-char-context

(ert-deftest test-face-diag-char-context-normal ()
  "Normal: an ASCII letter reports char, codepoint, name, and script."
  (with-temp-buffer
    (insert "A")
    (let ((ctx (cj/--face-diag-char-context (point-min))))
      (should (= (plist-get ctx :char) ?A))
      (should (= (plist-get ctx :codepoint) 65))
      (should (equal (plist-get ctx :name) "LATIN CAPITAL LETTER A"))
      (should (eq (plist-get ctx :script) 'latin)))))

(ert-deftest test-face-diag-char-context-eob-nil ()
  "Boundary/Error: end of an empty buffer has no character, so nil."
  (with-temp-buffer
    (should-not (cj/--face-diag-char-context (point-max)))))

;;; cj/--face-diag-normalize-faces

(ert-deftest test-face-diag-normalize-faces ()
  "Normal/Boundary: symbol, list, anonymous spec, and nil normalize correctly."
  (should (equal (cj/--face-diag-normalize-faces 'bold) '(bold)))
  (should (equal (cj/--face-diag-normalize-faces '(bold italic)) '(bold italic)))
  (should (equal (cj/--face-diag-normalize-faces '(:foreground "red"))
                 '((:foreground "red"))))
  (should-not (cj/--face-diag-normalize-faces nil)))

;;; cj/--face-diag-text-property-faces

(ert-deftest test-face-diag-text-property-faces-symbol ()
  "Normal: a `face' property symbol appears in the list."
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold))
    (should (equal (cj/--face-diag-text-property-faces (point-min)) '(bold)))))

(ert-deftest test-face-diag-text-property-faces-includes-font-lock ()
  "Normal: `face' and `font-lock-face' are both collected, face first."
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold 'font-lock-face 'italic))
    (should (equal (cj/--face-diag-text-property-faces (point-min)) '(bold italic)))))

(ert-deftest test-face-diag-text-property-faces-none ()
  "Boundary: unpropertized text yields no faces."
  (with-temp-buffer
    (insert "x")
    (should-not (cj/--face-diag-text-property-faces (point-min)))))

;;; cj/--face-diag-overlay-faces

(ert-deftest test-face-diag-overlay-faces-sorted-by-priority ()
  "Normal: overlay faces are returned highest priority first."
  (with-temp-buffer
    (insert "xyz")
    (let ((lo (make-overlay 1 3))
          (hi (make-overlay 1 3)))
      (overlay-put lo 'face 'region)
      (overlay-put lo 'priority 1)
      (overlay-put hi 'face 'highlight)
      (overlay-put hi 'priority 10)
      (let ((entries (cj/--face-diag-overlay-faces 1)))
        (should (= (length entries) 2))
        (should (eq (plist-get (car entries) :face) 'highlight))
        (should (eq (plist-get (cadr entries) :face) 'region))))))

(ert-deftest test-face-diag-overlay-faces-skips-faceless ()
  "Boundary: an overlay without a `face' property is excluded."
  (with-temp-buffer
    (insert "xyz")
    (let ((ov (make-overlay 1 3)))
      (overlay-put ov 'help-echo "no face here")
      (should-not (cj/--face-diag-overlay-faces 1)))))

;;; cj/--face-diag-active-remaps

(ert-deftest test-face-diag-active-remaps-matches-stack ()
  "Normal: a remap of a stack face is returned; an unrelated remap is not."
  (with-temp-buffer
    (setq face-remapping-alist '((default :background "#111111")
                                 (link :foreground "#222222")))
    (let ((remaps (cj/--face-diag-active-remaps '(default))))
      (should (assq 'default remaps))
      (should-not (assq 'link remaps)))))

(ert-deftest test-face-diag-active-remaps-empty ()
  "Boundary: no remapping alist yields no entries."
  (with-temp-buffer
    (setq face-remapping-alist nil)
    (should-not (cj/--face-diag-active-remaps '(default)))))

;;; cj/--face-diag-stack

(ert-deftest test-face-diag-stack-assembles-sources ()
  "Normal: the stack carries text-property, overlay, remap, and default sources."
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold))
    (setq face-remapping-alist '((default :background "#111111")))
    (let ((ov (make-overlay 1 2)))
      (overlay-put ov 'face 'region)
      (let ((stack (cj/--face-diag-stack 1)))
        (should (equal (plist-get stack :text-property) '(bold)))
        (should (eq (plist-get (car (plist-get stack :overlays)) :face) 'region))
        (should (assq 'default (plist-get stack :remaps)))
        (should (eq (plist-get stack :default) 'default))))))

;;; cj/--face-diagnosis-at

(ert-deftest test-face-diagnosis-at-shape ()
  "Normal: the assembled core returns classification, char, and stack."
  (with-temp-buffer
    (fundamental-mode)
    (insert (propertize "A" 'face 'bold))
    (let ((diag (cj/--face-diagnosis-at (point-min))))
      (should (eq (plist-get diag :classification) 'theme-faced))
      (should (= (plist-get (plist-get diag :char) :char) ?A))
      (should (equal (plist-get (plist-get diag :stack) :text-property) '(bold))))))

(ert-deftest test-face-diagnosis-at-eob-char-nil ()
  "Boundary: at end of an empty buffer the char group is nil, stack still present."
  (with-temp-buffer
    (fundamental-mode)
    (let ((diag (cj/--face-diagnosis-at (point-max))))
      (should-not (plist-get diag :char))
      (should (eq (plist-get (plist-get diag :stack) :default) 'default)))))

(provide 'test-face-diagnostic)
;;; test-face-diagnostic.el ends here
