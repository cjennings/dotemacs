;;; test-build-theme.el --- Tests for the theme.json -> deftheme converter -*- lexical-binding: t -*-

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

;; --- WCAG contrast helpers ---

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
;;
;; `--attrs' takes one face-spec alist and emits a face-attribute plist.  It
;; reads the full attribute model and tolerates the legacy boolean
;; bold/italic/underline/strike fields that older theme.json exports carry.

;; --- Legacy boolean fields still work (back-compat with committed presets) ---

(ert-deftest test-build-theme-attrs-legacy-fg-and-bold ()
  "Normal: legacy bold flag yields :weight bold."
  (should (equal (build-theme/--attrs '((fg . "#67809c") (bold . t)))
                 '(:foreground "#67809c" :weight bold))))

(ert-deftest test-build-theme-attrs-legacy-italic-underline-strike ()
  "Normal: legacy italic/underline/strike booleans map to their attributes."
  (should (equal (build-theme/--attrs '((italic . t))) '(:slant italic)))
  (should (equal (build-theme/--attrs '((underline . t))) '(:underline t)))
  (should (equal (build-theme/--attrs '((strike . t))) '(:strike-through t))))

(ert-deftest test-build-theme-attrs-empty-is-nil ()
  "Boundary: a blank face (empty alist, or all-nil fields) yields an empty plist."
  (should (equal (build-theme/--attrs '()) '()))
  (should (equal (build-theme/--attrs '((fg) (bg) (bold) (italic) (underline) (strike))) '())))

(ert-deftest test-build-theme-attrs-bold-false-omits-weight ()
  "Boundary: bold false (or absent) writes no :weight -- only overrides appear."
  (should (equal (build-theme/--attrs '((fg . "#cdced1") (bold . nil)))
                 '(:foreground "#cdced1")))
  (should (equal (build-theme/--attrs '((fg . "#cdced1"))) '(:foreground "#cdced1"))))

(ert-deftest test-build-theme-attrs-height-one-omitted ()
  "Boundary: a height of exactly 1.0 (or integer 1) is omitted as the default."
  (should (equal (build-theme/--attrs '((fg . "#cdced1") (height . 1.0))) '(:foreground "#cdced1")))
  (should (equal (build-theme/--attrs '((fg . "#cdced1") (height . 1))) '(:foreground "#cdced1")))
  (should (equal (build-theme/--attrs '((height . 1.2))) '(:height 1.2))))

;; --- New attributes ---

(ert-deftest test-build-theme-attrs-family ()
  "Normal/Boundary: a non-empty family string emits :family; empty is omitted."
  (should (equal (build-theme/--attrs '((family . "Iosevka"))) '(:family "Iosevka")))
  (should (equal (build-theme/--attrs '((family . ""))) '()))
  (should (equal (build-theme/--attrs '((family . nil))) '())))

(ert-deftest test-build-theme-attrs-distant-foreground ()
  "Normal: distant-fg emits :distant-foreground."
  (should (equal (build-theme/--attrs '((distant-fg . "#ffffff")))
                 '(:distant-foreground "#ffffff"))))

(ert-deftest test-build-theme-attrs-weight-range ()
  "Normal: an explicit weight string emits that weight symbol."
  (should (equal (build-theme/--attrs '((weight . "light"))) '(:weight light)))
  (should (equal (build-theme/--attrs '((weight . "semibold"))) '(:weight semibold)))
  (should (equal (build-theme/--attrs '((weight . "heavy"))) '(:weight heavy))))

(ert-deftest test-build-theme-attrs-weight-overrides-legacy-bold ()
  "Boundary: an explicit weight wins over a legacy bold flag on the same face."
  (should (equal (build-theme/--attrs '((weight . "light") (bold . t)))
                 '(:weight light))))

(ert-deftest test-build-theme-attrs-slant-range ()
  "Normal: an explicit slant string emits that slant; it wins over legacy italic."
  (should (equal (build-theme/--attrs '((slant . "oblique"))) '(:slant oblique)))
  (should (equal (build-theme/--attrs '((slant . "normal"))) '(:slant normal)))
  (should (equal (build-theme/--attrs '((slant . "oblique") (italic . t))) '(:slant oblique))))

(ert-deftest test-build-theme-attrs-underline-object ()
  "Normal/Boundary: the structured underline form covers line/wave and color."
  ;; plain line in the face color collapses to t
  (should (equal (build-theme/--attrs '((underline . ((style . "line") (color . nil)))))
                 '(:underline t)))
  ;; wave alone -> a :style plist
  (should (equal (build-theme/--attrs '((underline . ((style . "wave") (color . nil)))))
                 '(:underline (:style wave))))
  ;; colored line -> a :color plist
  (should (equal (build-theme/--attrs '((underline . ((style . "line") (color . "#cb6b4d")))))
                 '(:underline (:color "#cb6b4d"))))
  ;; colored wave -> both
  (should (equal (build-theme/--attrs '((underline . ((style . "wave") (color . "#cb6b4d")))))
                 '(:underline (:color "#cb6b4d" :style wave)))))

(ert-deftest test-build-theme-attrs-strike-object ()
  "Normal: structured strike emits t for no color, or the color string."
  (should (equal (build-theme/--attrs '((strike . ((color . nil))))) '(:strike-through t)))
  (should (equal (build-theme/--attrs '((strike . ((color . "#cb6b4d")))))
                 '(:strike-through "#cb6b4d"))))

(ert-deftest test-build-theme-attrs-migrated-shapes-match-legacy ()
  "Boundary: the shapes the import migration produces emit identically to the
legacy booleans they replace, so the cutover keeps generated themes byte-identical.
Mirrors migrateLegacyFace (app-core.js) / migrate_legacy (face_specs.py)."
  (should (equal (build-theme/--attrs '((weight . "bold")))
                 (build-theme/--attrs '((bold . t)))))
  (should (equal (build-theme/--attrs '((slant . "italic")))
                 (build-theme/--attrs '((italic . t)))))
  (should (equal (build-theme/--attrs '((underline . ((style . "line") (color . nil)))))
                 (build-theme/--attrs '((underline . t)))))
  (should (equal (build-theme/--attrs '((strike . ((color . nil)))))
                 (build-theme/--attrs '((strike . t))))))

(ert-deftest test-build-theme-attrs-overline ()
  "Normal/Boundary: overline emits t for no color, the color otherwise, nil when unset."
  (should (equal (build-theme/--attrs '((overline . ((color . nil))))) '(:overline t)))
  (should (equal (build-theme/--attrs '((overline . ((color . "#a9b2bb")))))
                 '(:overline "#a9b2bb")))
  (should (equal (build-theme/--attrs '((overline . nil))) '())))

(ert-deftest test-build-theme-attrs-inverse-and-extend ()
  "Normal/Boundary: inverse and extend emit t when set, nothing when nil."
  (should (equal (build-theme/--attrs '((inverse . t))) '(:inverse-video t)))
  (should (equal (build-theme/--attrs '((extend . t))) '(:extend t)))
  (should (equal (build-theme/--attrs '((inverse . t) (extend . t)))
                 '(:inverse-video t :extend t)))
  (should (equal (build-theme/--attrs '((inverse . nil) (extend . nil))) '())))

(ert-deftest test-build-theme-attrs-inherit-any-tier ()
  "Normal: inherit coerces a face-name string to a symbol (now allowed on every tier)."
  (should (equal (build-theme/--attrs '((inherit . "shadow"))) '(:inherit shadow)))
  (should (equal (build-theme/--attrs '((inherit . shadow))) '(:inherit shadow)))
  (should (equal (build-theme/--attrs '((inherit . nil))) '())))

(ert-deftest test-build-theme-attrs-full-ordering ()
  "Normal: every attribute present, emitted in canonical order."
  (should (equal (build-theme/--attrs
                  '((inherit . "org-level-1") (family . "Iosevka")
                    (fg . "#e8bd30") (bg . "#1a1714") (distant-fg . "#ffffff")
                    (weight . "semibold") (slant . "italic") (height . 1.3)
                    (underline . ((style . "wave") (color . "#cb6b4d")))
                    (overline . ((color . "#a9b2bb")))
                    (strike . ((color . nil)))
                    (box . ((style . "line") (color . "#67809c")))
                    (inverse . t) (extend . t)))
                 '(:inherit org-level-1 :family "Iosevka"
                   :foreground "#e8bd30" :background "#1a1714" :distant-foreground "#ffffff"
                   :weight semibold :slant italic :height 1.3
                   :underline (:color "#cb6b4d" :style wave) :overline "#a9b2bb"
                   :strike-through t :box (:line-width 1 :color "#67809c")
                   :inverse-video t :extend t))))

;; --- Attribute-helper edge cases (the coercion functions in isolation) ---

(ert-deftest test-build-theme-weight-helper ()
  "Boundary: weight prefers explicit string, falls back to bold, else nil."
  (should (eq (build-theme/--weight '((weight . "bold"))) 'bold))
  (should (eq (build-theme/--weight '((weight . "light") (bold . t))) 'light))
  (should (eq (build-theme/--weight '((bold . t))) 'bold))
  (should (null (build-theme/--weight '((weight . "") (bold . nil)))))
  (should (null (build-theme/--weight '()))))

(ert-deftest test-build-theme-slant-helper ()
  "Boundary: slant prefers explicit string, falls back to italic, else nil."
  (should (eq (build-theme/--slant '((slant . "oblique"))) 'oblique))
  (should (eq (build-theme/--slant '((italic . t))) 'italic))
  (should (null (build-theme/--slant '((slant . "")))))
  (should (null (build-theme/--slant '()))))

(ert-deftest test-build-theme-underline-helper ()
  "Boundary: underline coercion across nil / legacy t / structured forms."
  (should (null (build-theme/--underline '((underline . nil)))))
  (should (eq (build-theme/--underline '((underline . t))) t))
  (should (eq (build-theme/--underline '((underline . ((style . "line") (color . nil))))) t))
  (should (equal (build-theme/--underline '((underline . ((style . "wave"))))) '(:style wave)))
  (should (equal (build-theme/--underline '((underline . ((color . "#aa0000"))))) '(:color "#aa0000"))))

(ert-deftest test-build-theme-line-attr-helper ()
  "Boundary: the overline/strike coercion: nil / t / {color} forms."
  (should (null (build-theme/--line-attr nil)))
  (should (eq (build-theme/--line-attr t) t))
  (should (eq (build-theme/--line-attr '((color . nil))) t))
  (should (equal (build-theme/--line-attr '((color . "#abcdef"))) "#abcdef")))

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

(ert-deftest test-build-theme-convert-file-new-attributes-round-trip ()
  "Integration: the new attribute model survives parse -> spec -> file -> face.
Components integrated:
- build-theme/convert-file (entry point, real)
- json parsing of the inline fixture (real)
- custom-theme-set-faces / load-theme / face-attribute (real)
Exercises extend, structured underline (wave + color), overline, inverse-video,
distant-foreground, family, and the weight/slant ranges across the UI and
package tiers."
  (test-build-theme--with-sandbox out
    (let* ((json "{\"name\":\"newattrs\",\"palette\":[[\"#000000\",\"ground\"]],
 \"syntax\":{\"bg\":{\"fg\":\"#000000\"},\"p\":{\"fg\":\"#ffffff\"}},
 \"ui\":{
   \"region\":{\"bg\":\"#264364\",\"extend\":true},
   \"highlight\":{\"fg\":\"#eddba7\",\"underline\":{\"style\":\"wave\",\"color\":\"#cb6b4d\"},\"overline\":{\"color\":\"#a9b2bb\"}},
   \"secondary-selection\":{\"bg\":\"#333333\",\"inverse\":true,\"distant-fg\":\"#ffffff\"}
 },
 \"packages\":{
   \"misc\":{
     \"shadow\":{\"fg\":\"#cdced1\",\"family\":\"Iosevka\",\"weight\":\"light\",\"slant\":\"oblique\",\"source\":\"user\"}
   }
 }}")
           (in (expand-file-name "newattrs.json" out)))
      (with-temp-file in (insert json))
      (build-theme/convert-file in out)
      (let ((custom-theme-load-path (cons out custom-theme-load-path))
            (load-path (cons out load-path)))
        (unwind-protect
            (progn
              (load-theme 'newattrs t)
              (should (eq (face-attribute 'region :extend nil t) t))
              (should (equal (face-attribute 'highlight :underline nil t)
                             '(:color "#cb6b4d" :style wave)))
              (should (string= (face-attribute 'highlight :overline nil t) "#a9b2bb"))
              (should (eq (face-attribute 'secondary-selection :inverse-video nil t) t))
              (should (string= (face-attribute 'secondary-selection :distant-foreground nil t) "#ffffff"))
              (should (string= (face-attribute 'shadow :family nil t) "Iosevka"))
              (should (eq (face-attribute 'shadow :weight nil t) 'light))
              (should (eq (face-attribute 'shadow :slant nil t) 'oblique)))
          (disable-theme 'newattrs))))))

(provide 'test-build-theme)
;;; test-build-theme.el ends here
