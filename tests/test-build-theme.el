;;; test-build-theme.el --- Tests for the theme.json -> dupre-*.el converter -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for scripts/theme-studio/build-theme.el, the converter that
;; turns a theme.json exported by the theme-studio into a loadable Emacs
;; deftheme file.  This is the correctness-sensitive end of the pipeline, so
;; it is covered Normal / Boundary / Error per category.

;;; Code:

(require 'ert)
(require 'json)

;; The converter lives under scripts/, not on the normal load-path.  Add it at
;; compile time too (the validate hook byte-compiles this file in isolation and
;; only -L's the project, modules, tests, and themes dirs).
(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name
                "../scripts/theme-studio"
                (file-name-directory
                 (or load-file-name
                     (bound-and-true-p byte-compile-current-file)
                     buffer-file-name
                     default-directory)))))

(require 'build-theme)

;;; ---------------------------------------------------------------------------
;;; Fixtures

(defconst test-build-theme--fixture-json
  "{
 \"name\": \"dupre-fixture\",
 \"palette\": [[\"#000000\",\"ground\"],[\"#7a9abe\",\"blue\"],[\"#84b068\",\"green\"]],
 \"syntax\": {
  \"bg\": {\"fg\":\"#000000\",\"bg\":null,\"bold\":false,\"italic\":false},
  \"p\":  {\"fg\":\"#cdced1\",\"bg\":null,\"bold\":false,\"italic\":false},
  \"kw\": {\"fg\":\"#7a9abe\",\"bg\":null,\"bold\":true,\"italic\":false},
  \"str\":{\"fg\":\"#84b068\",\"bg\":null,\"bold\":false,\"italic\":false},
  \"cm\": {\"fg\":\"#838d97\",\"bg\":null,\"bold\":false,\"italic\":true},
  \"dec\":{\"fg\":\"#e8bd30\",\"bg\":null,\"bold\":false,\"italic\":false}
 },
 \"ui\": {
  \"region\": {\"fg\":null, \"bg\":\"#264364\"},
  \"mode-line\": {\"fg\":\"#cdced1\", \"bg\":\"#2f343a\"}
 },
 \"packages\": {
  \"org-mode\": {
   \"org-level-1\": {\"fg\":\"#67809c\",\"bg\":null,\"bold\":true,\"italic\":false,\"inherit\":null,\"source\":\"default\"},
   \"org-level-2\": {\"fg\":\"#e8bd30\",\"bg\":null,\"bold\":false,\"italic\":false,\"inherit\":\"org-level-1\",\"height\":1.2,\"source\":\"user\"},
   \"org-tag\": {\"fg\":null,\"bg\":null,\"bold\":false,\"italic\":false,\"inherit\":null,\"source\":\"cleared\"}
  }
 }
}"
  "A self-contained theme.json exercising every tier: default, syntax (bold +
italic + the unmappable dec key), UI, and packages (a plain face, an
inherit+height face, and a cleared face).  Uses the nested \"syntax\" format the
converter reads -- each category is an object with fg/bg/bold/italic, and bg/p
are themselves category objects carrying fg.  Owned by the test so it can't
drift the way Craig's downloaded exports under scripts/theme-studio/ can.")

(defun test-build-theme--write-fixture (dir)
  "Write the fixture JSON into DIR and return its path."
  (let ((path (expand-file-name "dupre-fixture.json" dir)))
    (with-temp-file path (insert test-build-theme--fixture-json))
    path))

(defmacro test-build-theme--with-sandbox (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "build-theme-test-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;; --- WCAG contrast helpers (mirror of the dupre-theme test helpers) ---

(defun test-build-theme--channel-luminance (c)
  "Linearize an 8-bit channel value C (0-255) per the WCAG formula."
  (let ((x (/ c 255.0)))
    (if (<= x 0.03928) (/ x 12.92) (expt (/ (+ x 0.055) 1.055) 2.4))))

(defun test-build-theme--relative-luminance (hex)
  "WCAG relative luminance of HEX color \"#rrggbb\"."
  (+ (* 0.2126 (test-build-theme--channel-luminance (string-to-number (substring hex 1 3) 16)))
     (* 0.7152 (test-build-theme--channel-luminance (string-to-number (substring hex 3 5) 16)))
     (* 0.0722 (test-build-theme--channel-luminance (string-to-number (substring hex 5 7) 16)))))

(defun test-build-theme--contrast (fg bg)
  "WCAG contrast ratio between hex colors FG and BG."
  (let ((l1 (test-build-theme--relative-luminance fg))
        (l2 (test-build-theme--relative-luminance bg)))
    (/ (+ (max l1 l2) 0.05) (+ (min l1 l2) 0.05))))

;;; ---------------------------------------------------------------------------
;;; build-theme/--attrs (the core attribute builder)

(ert-deftest test-build-theme-attrs-fg-and-bold ()
  "Normal: a foreground plus bold yields :foreground and :weight bold."
  (should (equal (build-theme/--attrs nil "#67809c" nil t nil nil nil nil)
                 '(:foreground "#67809c" :weight bold))))

(ert-deftest test-build-theme-attrs-full-ordering ()
  "Normal: every attribute present, in canonical order."
  (should (equal (build-theme/--attrs 'org-level-1 "#e8bd30" "#1a1714" t t t t 1.3)
                 '(:inherit org-level-1 :foreground "#e8bd30" :background "#1a1714"
                            :weight bold :slant italic :underline t :strike-through t :height 1.3))))

(ert-deftest test-build-theme-attrs-underline-and-strike ()
  "Normal: underline and strike yield :underline t and :strike-through t."
  (should (equal (build-theme/--attrs nil "#67809c" nil nil nil t t nil)
                 '(:foreground "#67809c" :underline t :strike-through t)))
  ;; either alone
  (should (equal (build-theme/--attrs nil nil nil nil nil t nil nil)
                 '(:underline t)))
  (should (equal (build-theme/--attrs nil nil nil nil nil nil t nil)
                 '(:strike-through t))))

(ert-deftest test-build-theme-attrs-empty-is-nil ()
  "Boundary: a fully-cleared face (all nil) yields an empty plist."
  (should (equal (build-theme/--attrs nil nil nil nil nil nil nil nil) '())))

(ert-deftest test-build-theme-attrs-bold-false-omits-weight ()
  "Boundary: bold false produces no :weight key (only overrides are written)."
  (should (equal (build-theme/--attrs nil "#cdced1" nil nil nil nil nil nil)
                 '(:foreground "#cdced1"))))

(ert-deftest test-build-theme-attrs-height-one-omitted ()
  "Boundary: a height of exactly 1.0 is omitted (the default multiplier)."
  (should (equal (build-theme/--attrs nil "#cdced1" nil nil nil nil nil 1.0)
                 '(:foreground "#cdced1")))
  (should (equal (build-theme/--attrs nil "#cdced1" nil nil nil nil nil 1)
                 '(:foreground "#cdced1"))))

;;; ---------------------------------------------------------------------------
;;; build-theme/--face-spec (skips empty faces)

(ert-deftest test-build-theme-face-spec-normal ()
  "Normal: a face with attrs becomes a custom-theme-set-faces spec."
  (should (equal (build-theme/--face-spec 'font-lock-string-face '(:foreground "#84b068"))
                 '(font-lock-string-face ((t (:foreground "#84b068")))))))

(ert-deftest test-build-theme-face-spec-empty-skipped ()
  "Boundary: a face with no attributes (cleared) yields nil, not an empty spec."
  (should (null (build-theme/--face-spec 'whatever '()))))

;;; ---------------------------------------------------------------------------
;;; Syntax tier

(ert-deftest test-build-theme-syntax-keyword-bold ()
  "Normal: kw maps to font-lock-keyword-face and carries its bold flag.
Each syntax category is a nested object with fg/bold/italic."
  (let* ((syntax '((kw . ((fg . "#7a9abe") (bold . t)))
                   (str . ((fg . "#84b068")))))
         (specs (build-theme/--syntax-face-specs syntax)))
    (should (member '(font-lock-keyword-face ((t (:foreground "#7a9abe" :weight bold))))
                    specs))
    (should (member '(font-lock-string-face ((t (:foreground "#84b068"))))
                    specs))))

(ert-deftest test-build-theme-syntax-one-to-many ()
  "Normal: punc fans out to every punctuation/bracket/delimiter face."
  (let ((specs (build-theme/--syntax-face-specs '((punc . ((fg . "#a9b2bb")))))))
    (dolist (face '(font-lock-punctuation-face font-lock-bracket-face
                    font-lock-delimiter-face font-lock-misc-punctuation-face))
      (should (member `(,face ((t (:foreground "#a9b2bb")))) specs)))))

(ert-deftest test-build-theme-syntax-decorator-omitted ()
  "Boundary: dec has no independent Emacs face, so it maps to nothing.
Emacs renders decorators with font-lock-type-face, which ty already owns;
mapping dec would clobber the type color."
  (let ((specs (build-theme/--syntax-face-specs '((dec . ((fg . "#e8bd30")))))))
    (should (null specs))))

(ert-deftest test-build-theme-syntax-comment-italic ()
  "Normal: cm with its italic flag yields :slant italic on the comment face."
  (let ((specs (build-theme/--syntax-face-specs '((cm . ((fg . "#a9b2bb") (italic . t)))))))
    (should (member '(font-lock-comment-face ((t (:foreground "#a9b2bb" :slant italic))))
                    specs))))

;;; ---------------------------------------------------------------------------
;;; Default face

(ert-deftest test-build-theme-default-face ()
  "Normal: default takes background from syntax.bg.fg and foreground from syntax.p.fg."
  (should (equal (build-theme/--default-spec '((bg . ((fg . "#000000")))
                                               (p . ((fg . "#cdced1")))))
                 '(default ((t (:foreground "#cdced1" :background "#000000")))))))

;;; ---------------------------------------------------------------------------
;;; UI tier

(ert-deftest test-build-theme-ui-passthrough ()
  "Normal: a ui face passes fg/bg straight through."
  (let ((specs (build-theme/--ui-face-specs
                '((region . ((fg . nil) (bg . "#264364")))
                  (mode-line . ((fg . "#cdced1") (bg . "#2f343a")))))))
    (should (member '(region ((t (:background "#264364")))) specs))
    (should (member '(mode-line ((t (:foreground "#cdced1" :background "#2f343a")))) specs))))

(ert-deftest test-build-theme-box-styles ()
  "Normal/Boundary: a face box spec converts to the right Emacs :box value."
  (should (equal (build-theme/--box '((style . "released") (width . 1)))
                 '(:line-width 1 :style released-button)))
  (should (equal (build-theme/--box '((style . "pressed") (width . 2)))
                 '(:line-width 2 :style pressed-button)))
  (should (equal (build-theme/--box '((style . "line") (color . "#67809c")))
                 '(:line-width 1 :color "#67809c")))
  (should (equal (build-theme/--box '((style . "line"))) '(:line-width 1)))
  (should (null (build-theme/--box nil)))
  (should (null (build-theme/--box '((style . "none"))))))

(ert-deftest test-build-theme-ui-face-emits-box ()
  "Normal: a ui face with a box exports a :box attribute."
  (let ((specs (build-theme/--ui-face-specs
                '((mode-line . ((fg . "#cdced1") (bg . "#2f343a")
                                (box . ((style . "released") (width . 1)))))))))
    (should (member '(mode-line ((t (:foreground "#cdced1" :background "#2f343a"
                                     :box (:line-width 1 :style released-button)))))
                    specs))))

;;; ---------------------------------------------------------------------------
;;; Package tier

(ert-deftest test-build-theme-package-inherit-and-height ()
  "Normal: a package face writes :inherit plus overrides plus :height."
  (let ((specs (build-theme/--package-face-specs
                '((org-mode . ((org-level-2 . ((fg . "#e8bd30") (bg . nil)
                                               (bold . nil) (italic . nil)
                                               (inherit . "org-level-1") (height . 1.2)
                                               (source . "user")))))))))
    (should (member '(org-level-2 ((t (:inherit org-level-1 :foreground "#e8bd30" :height 1.2))))
                    specs))))

(ert-deftest test-build-theme-package-underline-and-strike ()
  "Normal: a package face writes :underline and :strike-through from the flags."
  (let ((specs (build-theme/--package-face-specs
                '((shr . ((shr-link . ((fg . "#67809c") (bg . nil) (bold . nil) (italic . nil)
                                       (underline . t) (strike . nil) (inherit . nil) (source . "default")))
                          (shr-strike-through . ((fg . "#5e6770") (bg . nil) (bold . nil) (italic . nil)
                                                 (underline . nil) (strike . t) (inherit . nil) (source . "default")))))))))
    (should (member '(shr-link ((t (:foreground "#67809c" :underline t)))) specs))
    (should (member '(shr-strike-through ((t (:foreground "#5e6770" :strike-through t)))) specs))))

(ert-deftest test-build-theme-package-cleared-skipped ()
  "Boundary: a cleared package face (no renderable attrs) is not emitted."
  (let ((specs (build-theme/--package-face-specs
                '((org-mode . ((org-tag . ((fg . nil) (bg . nil) (bold . nil)
                                           (italic . nil) (inherit . nil)
                                           (height . nil) (source . "cleared")))))))))
    (should (null specs))))

;;; ---------------------------------------------------------------------------
;;; Hex validation

(ert-deftest test-build-theme-hex-p ()
  "Normal/Error: only #rrggbb strings validate."
  (should (build-theme/--hex-p "#0d0b0a"))
  (should (build-theme/--hex-p "#FFFFFF"))
  (should-not (build-theme/--hex-p "0d0b0a"))
  (should-not (build-theme/--hex-p "#fff"))
  (should-not (build-theme/--hex-p nil)))

;;; ---------------------------------------------------------------------------
;;; End-to-end: convert a file and load the result

(ert-deftest test-build-theme-convert-file-writes-loadable-theme ()
  "Integration: converting the fixture produces a theme Emacs can load.
Components integrated:
- build-theme/convert-file (entry point, real)
- json parsing of the inline fixture (real)
- custom-theme-set-faces / load-theme (real)
Validates the syntax, default, UI, and package tiers all reach real faces,
including an inherit+height package face."
  (require 'org)
  (test-build-theme--with-sandbox out
    (let* ((in (test-build-theme--write-fixture out))
           (path (build-theme/convert-file in out)))
      (should (file-exists-p path))
      (should (string-suffix-p "dupre-fixture-theme.el" path))
      (let ((custom-theme-load-path (cons out custom-theme-load-path))
            (load-path (cons out load-path)))
        (unwind-protect
            (progn
              (load-theme 'dupre-fixture t)
              ;; default tier
              (should (string= (face-attribute 'default :background nil t) "#000000"))
              (should (string= (face-attribute 'default :foreground nil t) "#cdced1"))
              ;; syntax tier (kw is blue + bold in the fixture)
              (should (string= (face-attribute 'font-lock-keyword-face :foreground nil t) "#7a9abe"))
              (should (eq (face-attribute 'font-lock-keyword-face :weight nil t) 'bold))
              ;; ui tier
              (should (string= (face-attribute 'region :background nil t) "#264364"))
              ;; package tier — plain face and an inherit+height face
              (should (string= (face-attribute 'org-level-1 :foreground nil t) "#67809c"))
              (should (eq (face-attribute 'org-level-2 :inherit nil t) 'org-level-1))
              (should (= (face-attribute 'org-level-2 :height nil t) 1.2)))
          (disable-theme 'dupre-fixture))))))

(ert-deftest test-build-theme-convert-file-old-json-without-packages ()
  "Boundary: a theme.json with no packages key still converts and loads."
  (test-build-theme--with-sandbox out
    (let* ((json "{\"name\":\"noformat\",\"palette\":[[\"#000000\",\"ground\"]],\"syntax\":{\"bg\":{\"fg\":\"#000000\"},\"p\":{\"fg\":\"#ffffff\"},\"kw\":{\"fg\":\"#67809c\",\"bold\":true}},\"ui\":{}}")
           (in (expand-file-name "noformat.json" out)))
      (with-temp-file in (insert json))
      (let ((path (build-theme/convert-file in out)))
        (should (file-exists-p path))
        (let ((custom-theme-load-path (cons out custom-theme-load-path))
              (load-path (cons out load-path)))
          (unwind-protect
              (progn
                (load-theme 'noformat t)
                (should (string= (face-attribute 'default :background nil t) "#000000"))
                (should (string= (face-attribute 'font-lock-keyword-face :foreground nil t) "#67809c")))
            (disable-theme 'noformat)))))))

(ert-deftest test-build-theme-convert-file-missing-input-errors ()
  "Error: a missing input file signals rather than writing garbage."
  (test-build-theme--with-sandbox out
    (should-error (build-theme/convert-file (expand-file-name "does-not-exist.json" out) out))))

(ert-deftest test-build-theme-name-from-filename-not-json-field ()
  "Normal/Regression: the output name comes from the JSON file's basename, not
its internal name field, so each draft exports under its own name (a WIP.json
becomes WIP-theme.el, never theme-theme.el)."
  (test-build-theme--with-sandbox out
    ;; The fixture's internal name field is \"dupre-fixture\"; the file is sterling.json.
    (let ((in (expand-file-name "sterling.json" out)))
      (with-temp-file in (insert test-build-theme--fixture-json))
      (let ((path (build-theme/convert-file in out)))
        (should (string-suffix-p "sterling-theme.el" path))
        (should-not (string-match-p "dupre-fixture" path))
        (let ((custom-theme-load-path (cons out custom-theme-load-path))
              (load-path (cons out load-path)))
          (unwind-protect
              (progn
                (load-theme 'sterling t)
                (should (string= (face-attribute 'default :background nil t) "#000000")))
            (disable-theme 'sterling)))))))

(ert-deftest test-build-theme-generated-default-meets-wcag-aa ()
  "Error/Regression: the generated default face stays legible.
A WCAG-AA (>= 4.5:1) assertion on the round-tripped result -- proves the whole
parse -> spec -> file -> face pipeline preserves the designed contrast."
  (test-build-theme--with-sandbox out
    (let ((path (build-theme/convert-file (test-build-theme--write-fixture out) out)))
      (let ((custom-theme-load-path (cons out custom-theme-load-path))
            (load-path (cons out load-path)))
        (unwind-protect
            (progn
              (load-theme 'dupre-fixture t)
              (let ((fg (face-attribute 'default :foreground nil t))
                    (bg (face-attribute 'default :background nil t)))
                (should (>= (test-build-theme--contrast fg bg) 4.5))))
          (disable-theme 'dupre-fixture))))))

(provide 'test-build-theme)
;;; test-build-theme.el ends here
