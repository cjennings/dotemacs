;;; test-nerd-icons-legend-dump.el --- ERT for the nerd-icons capture lib -*- lexical-binding: t -*-
;;; Commentary:
;; Unit tests for the gallery + hsl capture logic in build-nerd-icons-legend.el,
;; driven with synthetic source alists and faces so they run under `make test` /
;; the theme-studio batch without nerd-icons installed.  See run-tests.sh.
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(load (expand-file-name "build-nerd-icons-legend.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;; Make the source-alist vars special *with a value* in this test process so the
;; macro's `let` binds them dynamically and the library's symbol-value reads see
;; the synthetic data. (The library's own bare `defvar` only marks them special
;; for byte-compilation; the daemon binds them by loading nerd-icons.)
(defvar nerd-icons-extension-icon-alist nil)
(defvar nerd-icons-regexp-icon-alist nil)
(defvar nerd-icons-mode-icon-alist nil)

;; Defined by the library loaded above (at runtime, so the byte-compiler can't see them).
(declare-function cj/--nerd-icons-gallery-groups "build-nerd-icons-legend")
(declare-function cj/--nerd-icons-face-hsl "build-nerd-icons-legend")

;; Synthetic icon function (a real symbol, so fboundp passes) and test faces with
;; t-clause foregrounds the capture reads deterministically.
(defun ts-ni-icon (name) (concat "G:" name))
(defface ts-ni-red     '((t :foreground "#cb6b4d")) "test")
(defface ts-ni-blue    '((t :foreground "#6090c0")) "test")
(defface ts-ni-gray    '((t :foreground "#888888")) "test") ; achromatic: hue 0, lighter
(defface ts-ni-dgray   '((t :foreground "#444444")) "test") ; achromatic: hue 0, darker
(defface ts-ni-nocolor '((t :weight bold)) "test")          ; no foreground

(defun ts-ni-glyph-names (group)
  "List the icon names in GROUP's glyph vector, in order."
  (mapcar (lambda (e) (cdr (assoc "name" e))) (append (cdr (assoc "glyphs" group)) nil)))

(defun ts-ni-group (groups face)
  "The group in GROUPS whose face name is FACE."
  (seq-find (lambda (g) (equal (cdr (assoc "face" g)) face)) groups))

(defmacro ts-ni-with-alists (&rest body)
  "Run BODY with the nerd-icons source alists bound to synthetic entries."
  `(let ((nerd-icons-extension-icon-alist
          '(("r1" ts-ni-icon "RICON" :face ts-ni-red)
            ("b3" ts-ni-icon "BC"    :face ts-ni-blue)
            ("b1" ts-ni-icon "BA"    :face ts-ni-blue)
            ("b2" ts-ni-icon "BB"    :face ts-ni-blue)
            ("bd" ts-ni-icon "BA"    :face ts-ni-blue)    ; duplicate of BA -> deduped
            ("g1" ts-ni-icon "GICON" :face ts-ni-gray)
            ("g2" ts-ni-icon "DGICON" :face ts-ni-dgray)
            ("n1" ts-ni-icon "NC"    :face ts-ni-nocolor) ; face has no color -> dropped
            ("nf" ts-ni-icon "NOFACE")                    ; no :face -> skipped
            ("ng" ts-ni-icon nil     :face ts-ni-blue)))  ; no name -> skipped
         (nerd-icons-regexp-icon-alist nil)
         (nerd-icons-mode-icon-alist
          '((some-mode ts-ni-icon "MICON" :face ts-ni-red)))) ; merges into the red group
     ,@body))

(ert-deftest test-nerd-icons-dump-face-hsl-known-color ()
  "Normal: face-hsl returns (hue sat light) for a t-clause defface color."
  (let ((h (cj/--nerd-icons-face-hsl 'ts-ni-blue)))
    (should (= (length h) 3))
    (should (integerp (nth 0 h)))
    (should (<= 0 (nth 0 h) 360))))

(ert-deftest test-nerd-icons-dump-face-hsl-no-color-nil ()
  "Error: a face with no foreground yields nil (its group is later dropped)."
  (should (null (cj/--nerd-icons-face-hsl 'ts-ni-nocolor))))

(ert-deftest test-nerd-icons-dump-gallery-drops-and-skips ()
  "Boundary: no-color face dropped; no-:face and no-name entries skipped."
  (ts-ni-with-alists
   (let* ((groups (cj/--nerd-icons-gallery-groups))
          (faces (mapcar (lambda (g) (cdr (assoc "face" g))) groups))
          (names (apply #'append (mapcar #'ts-ni-glyph-names groups))))
     (should-not (member "ts-ni-nocolor" faces)) ; no native color -> whole group dropped
     (should-not (member "NOFACE" names))        ; entry without :face never added
     (should (member "ts-ni-blue" faces)))))

(ert-deftest test-nerd-icons-dump-gallery-dedup-and-sort ()
  "Normal: icons deduped by name within a color and sorted by name."
  (ts-ni-with-alists
   (should (equal (ts-ni-glyph-names (ts-ni-group (cj/--nerd-icons-gallery-groups) "ts-ni-blue"))
                  '("BA" "BB" "BC")))))

(ert-deftest test-nerd-icons-dump-gallery-merges-source-alists ()
  "Normal: a face collects icons from several source alists (extension + mode)."
  (ts-ni-with-alists
   (should (equal (ts-ni-glyph-names (ts-ni-group (cj/--nerd-icons-gallery-groups) "ts-ni-red"))
                  '("MICON" "RICON")))))

(ert-deftest test-nerd-icons-dump-gallery-hue-order ()
  "Normal: groups are ordered by ascending hue so color families cluster."
  (ts-ni-with-alists
   (let ((hues (mapcar (lambda (g) (cdr (assoc "hue" g))) (cj/--nerd-icons-gallery-groups))))
     (should (equal hues (sort (copy-sequence hues) #'<))))))

(ert-deftest test-nerd-icons-dump-gallery-lightness-tiebreak ()
  "Boundary: equal-hue faces order lighter-first (descending lightness)."
  (ts-ni-with-alists
   (let* ((faces (mapcar (lambda (g) (cdr (assoc "face" g))) (cj/--nerd-icons-gallery-groups)))
          (hg (nth 0 (cj/--nerd-icons-face-hsl 'ts-ni-gray)))
          (hd (nth 0 (cj/--nerd-icons-face-hsl 'ts-ni-dgray))))
     (should (= hg hd))                                       ; both achromatic -> hue 0
     (should (< (cl-position "ts-ni-gray" faces :test #'equal)
                (cl-position "ts-ni-dgray" faces :test #'equal))))))

(provide 'test-nerd-icons-legend-dump)
;;; test-nerd-icons-legend-dump.el ends here
