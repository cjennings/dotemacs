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

;;; cj/--face-diag-merged-attributes

(ert-deftest test-face-diag-merged-explicit-text-prop ()
  "Normal: an explicit text-property attribute is the winning merged value."
  (with-temp-buffer
    (insert (propertize "x" 'face '(:foreground "#abcdef" :weight bold)))
    (let ((attrs (cj/--face-diag-merged-attributes (point-min))))
      (should (equal (plist-get attrs :foreground) "#abcdef"))
      (should (eq (plist-get attrs :weight) 'bold)))))

(ert-deftest test-face-diag-merged-overlay-wins-over-text-prop ()
  "Normal: a higher-priority overlay attribute beats the text-property face."
  (with-temp-buffer
    (insert (propertize "x" 'face '(:foreground "blue")))
    (let ((ov (make-overlay 1 2)))
      (overlay-put ov 'face '(:foreground "red"))
      (overlay-put ov 'priority 10)
      (should (equal (plist-get (cj/--face-diag-merged-attributes 1) :foreground)
                     "red")))))

(ert-deftest test-face-diag-merged-applies-default-remap ()
  "Normal: a remap of the default face shows up in the merged attributes."
  (with-temp-buffer
    (insert "x")
    (setq face-remapping-alist '((default :foreground "#123456")))
    (should (equal (plist-get (cj/--face-diag-merged-attributes 1) :foreground)
                   "#123456"))))

(ert-deftest test-face-diag-merged-bold-face-symbol ()
  "Boundary: a face symbol in the stack contributes its set attributes."
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold))
    (should (eq (plist-get (cj/--face-diag-merged-attributes 1) :weight) 'bold))))

;;; cj/--face-diag-real-font

(ert-deftest test-face-diag-real-font-unavailable-in-batch ()
  "Boundary: font-at is nil under batch, so the real font reads \"unavailable\"."
  (with-temp-buffer
    (insert "x")
    (let ((font (cj/--face-diag-real-font 1)))
      (should (equal (plist-get font :font) "unavailable"))
      (should-not (plist-get font :family)))))

;;; cj/--face-diagnosis-at  (groups 0-4)

(ert-deftest test-face-diagnosis-at-includes-attributes-and-font ()
  "Normal: the assembled core carries the merged attributes and font groups."
  (with-temp-buffer
    (fundamental-mode)
    (insert (propertize "x" 'face '(:foreground "#abcdef")))
    (let ((diag (cj/--face-diagnosis-at (point-min))))
      (should (equal (plist-get (plist-get diag :attributes) :foreground) "#abcdef"))
      (should (equal (plist-get (plist-get diag :font) :font) "unavailable")))))

;;; provenance accessors

(ert-deftest test-face-diag-face-themes ()
  "Normal: theme names come from the face's theme-face property, newest first."
  (make-face 'fd-test-themed)
  (put 'fd-test-themed 'theme-face '((user spec1) (dupre spec2)))
  (should (equal (cj/--face-diag-face-themes 'fd-test-themed) '(user dupre))))

(ert-deftest test-face-diag-config-source ()
  "Normal/Boundary: saved-face -> saved, customized-face -> customized, else nil."
  (make-face 'fd-test-saved)
  (put 'fd-test-saved 'saved-face '(spec))
  (make-face 'fd-test-cust)
  (put 'fd-test-cust 'customized-face '(spec))
  (make-face 'fd-test-plain)
  (should (eq (cj/--face-diag-config-source 'fd-test-saved) 'saved))
  (should (eq (cj/--face-diag-config-source 'fd-test-cust) 'customized))
  (should-not (cj/--face-diag-config-source 'fd-test-plain)))

(ert-deftest test-face-diag-inherit-chain ()
  "Normal: a single-symbol :inherit produces a nearest-first chain."
  (make-face 'fd-test-parent)
  (make-face 'fd-test-child)
  (set-face-attribute 'fd-test-child nil :inherit 'fd-test-parent)
  (should (equal (cj/--face-diag-inherit-chain 'fd-test-child) '(fd-test-parent))))

(ert-deftest test-face-diag-inherit-chain-none ()
  "Boundary: a face with no :inherit has an empty chain."
  (make-face 'fd-test-noinherit)
  (should-not (cj/--face-diag-inherit-chain 'fd-test-noinherit)))

(ert-deftest test-face-diag-unspecified-attrs ()
  "Normal: a bare face leaves attributes unspecified, so they fall to default."
  (make-face 'fd-test-bare)
  (should (memq :foreground (cj/--face-diag-unspecified-attrs 'fd-test-bare))))

(ert-deftest test-face-diag-provenance-covers-stack-and-default ()
  "Normal: provenance covers the stack's named faces and always the default."
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold))
    (let ((faces (mapcar (lambda (p) (plist-get p :face))
                         (cj/--face-diag-provenance (point-min)))))
      (should (memq 'bold faces))
      (should (memq 'default faces)))))

(ert-deftest test-face-diagnosis-at-includes-provenance ()
  "Normal: the assembled core carries the provenance group for stack faces."
  (with-temp-buffer
    (fundamental-mode)
    (insert (propertize "x" 'face 'bold))
    (let ((prov (plist-get (cj/--face-diagnosis-at (point-min)) :provenance)))
      (should (cl-some (lambda (p) (eq (plist-get p :face) 'bold)) prov)))))

;;; cj/--face-diag-render

(ert-deftest test-face-diag-render-has-all-groups ()
  "Normal: the rendered report names every group and the stack's face."
  (with-temp-buffer
    (fundamental-mode)
    (insert (propertize "A" 'face 'bold))
    (let ((report (cj/--face-diag-render (cj/--face-diagnosis-at (point-min)))))
      (should (string-match-p "Character:" report))
      (should (string-match-p "Face stack" report))
      (should (string-match-p "bold" report))
      (should (string-match-p "Effective attributes" report))
      (should (string-match-p "Real font" report))
      (should (string-match-p "Provenance" report)))))

(ert-deftest test-face-diag-render-banner-out-of-scope ()
  "Boundary: a terminal classification renders a banner naming the ANSI source."
  (should (string-match-p "terminal" (cj/--face-diag-render-banner 'terminal-ansi)))
  (should (equal (cj/--face-diag-render-banner 'theme-faced) "")))

(ert-deftest test-face-diag-render-no-char ()
  "Boundary: a nil char group renders the no-character notice."
  (should (string-match-p "none at point" (cj/--face-diag-render-char nil))))

(ert-deftest test-face-diag-render-region-covers-runs ()
  "Normal: region rendering emits a position header per distinct face-run."
  (with-temp-buffer
    (insert (propertize "aa" 'face 'bold))
    (insert (propertize "bb" 'face 'italic))
    (let ((report (cj/--face-diag-render-region (point-min) (point-max))))
      (should (string-match-p "=== position 1 ===" report))
      (should (string-match-p "=== position 3 ===" report)))))

;;; cj/describe-face-at-point  (smoke)

(ert-deftest test-face-diag-command-creates-buffer ()
  "Normal: the command renders into the read-only *Face Diagnosis* buffer."
  (with-temp-buffer
    (insert (propertize "A" 'face 'bold))
    (goto-char (point-min))
    (cj/describe-face-at-point)
    (let ((buf (get-buffer "*Face Diagnosis*")))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              (should (eq major-mode 'cj/face-diagnostic-mode))
              (should buffer-read-only)
              (should (string-match-p "Face stack" (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'test-face-diagnostic)
;;; test-face-diagnostic.el ends here
