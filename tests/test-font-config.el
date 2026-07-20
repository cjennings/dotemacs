;;; test-font-config.el --- Smoke tests for font-config -*- lexical-binding: t; -*-

;;; Commentary:

;; font-config.el is mostly top-level font/package setup.  These smoke tests
;; cover the logic that should stay correct regardless of which fonts are
;; installed: the install check, task-oriented Fontaine picker, persistence,
;; and emoji setup. The module :demand's fontaine and references nerd-icons, so
;; the tests skip when those packages are absent rather than failing on a bare
;; checkout. GUI and font lookups are stubbed so the run stays headless.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; font-config :demand's external packages, so make the installed packages
;; reachable; the test runners do not package-initialize themselves.
(package-initialize)

(defconst test-font-config--available
  (and (locate-library "fontaine")
       (locate-library "nerd-icons"))
  "Non-nil when the packages font-config needs are loadable.")

;;; cj/font-installed-p

(ert-deftest test-font-config-font-installed-p-true-when-found ()
  "Normal: the install check returns t when the font is found."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'find-font) (lambda (&rest _) t)))
    (should (eq t (cj/font-installed-p "BerkeleyMono Nerd Font")))))

(ert-deftest test-font-config-font-installed-p-nil-when-absent ()
  "Error: the install check returns nil when the font is missing."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'find-font) (lambda (&rest _) nil)))
    (should (null (cj/font-installed-p "No Such Font 12345")))))

;;; cj/maybe-install-nerd-icons-fonts

(ert-deftest test-font-config-nerd-icons-missing-font-installs-on-gui ()
  "Normal: a missing Nerd Icons font is installed on a GUI frame."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((installed nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'cj/font-installed-p) (lambda (_name) nil))
              ((symbol-function 'nerd-icons-install-fonts)
               (lambda (&rest _) (setq installed t)))
              ((symbol-function 'remove-hook) #'ignore))
      (cj/maybe-install-nerd-icons-fonts))
    (should installed)))

(ert-deftest test-font-config-nerd-icons-installed-font-skips-install ()
  "Boundary: an installed Nerd Icons font needs no install attempt."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((installed nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'cj/font-installed-p) (lambda (_name) t))
              ((symbol-function 'nerd-icons-install-fonts)
               (lambda (&rest _) (setq installed t))))
      (cj/maybe-install-nerd-icons-fonts))
    (should-not installed)))

;;; cj/setup-emoji-fontset

(ert-deftest test-font-config-setup-emoji-fontset-noop-without-gui ()
  "Boundary: without a GUI the emoji setup does nothing and does not error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((called nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) nil))
              ((symbol-function 'set-fontset-font)
               (lambda (&rest _) (setq called t))))
      (cj/setup-emoji-fontset)
      (should-not called))))

(ert-deftest test-font-config-setup-emoji-fontset-runs-on-gui ()
  "Normal: on a GUI frame the emoji setup runs without error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) t))
            ((symbol-function 'font-family-list)
             (lambda (&rest _) '("Noto Color Emoji")))
            ((symbol-function 'set-fontset-font) (lambda (&rest _) t)))
    (should (progn (cj/setup-emoji-fontset) t))))

;;; cj/set-emojify-display-style

(defvar emojify-display-style)

(ert-deftest test-font-config-emojify-display-style-image-on-gui ()
  "Normal: on a GUI frame the emoji display style is `image'."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((emojify-display-style nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) t)))
      (cj/set-emojify-display-style)
      (should (eq emojify-display-style 'image)))))

(ert-deftest test-font-config-emojify-display-style-unicode-without-gui ()
  "Boundary: without a GUI the emoji display style is `unicode'."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((emojify-display-style nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) nil)))
      (cj/set-emojify-display-style)
      (should (eq emojify-display-style 'unicode)))))

;;; cj/display-available-fonts

(ert-deftest test-font-config-display-available-fonts-second-call-no-error ()
  "Error: a second invocation does not signal on the read-only buffer."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'font-family-list)
             (lambda (&rest _) '("Fixture Font A" "Fixture Font B"))))
    (unwind-protect
        (progn
          (cj/display-available-fonts)
          ;; The first call ends in `special-mode' (read-only); the second must
          ;; not signal when it erases and rewrites the buffer.
          (cj/display-available-fonts)
          (with-current-buffer "*Available Fonts*"
            (should (> (buffer-size) 0))
            (should buffer-read-only)))
      (when (get-buffer "*Available Fonts*")
        (kill-buffer "*Available Fonts*")))))

;;; Fontaine workflow profiles

(ert-deftest test-font-config-fontaine-presets-are-task-oriented ()
  "Normal: the picker exposes seven complete workflow destinations."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (should (equal (delq t (mapcar #'car fontaine-presets))
                 '(everyday writing reading coding-xs coding coding-xl
                   presentation))))

(ert-deftest test-font-config-fontaine-candidates-describe-end-state ()
  "Normal: every profile label names its purpose, fonts, and point size."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((candidates (cj/fontaine-profile-candidates)))
    (should (= (length candidates) 7))
    (should (member (nth 0 candidates)
                    '("Everyday — Berkeley Mono + Lexend · 13 pt"
                      "Everyday — Berkeley Mono + Lexend · 14 pt")))
    (should (equal (nth 1 candidates)
                   "Writing — Berkeley Mono + Merriweather · 14 pt"))
    (should (equal (nth 2 candidates)
                   "Reading — Merriweather · 14 pt"))
    (should (equal (nth 3 candidates)
                   "Coding XS — Berkeley Mono · 11 pt"))
    (should (equal (nth 4 candidates)
                   "Coding — Berkeley Mono · 13 pt"))
    (should (equal (nth 5 candidates)
                   "Coding XL — Berkeley Mono · 16 pt"))
    (should (equal (nth 6 candidates)
                   "Presentation — Berkeley Mono + Lexend · 20 pt"))))

(ert-deftest test-font-config-fontaine-candidate-round-trips-to-profile ()
  "Boundary: a displayed destination maps back to its Fontaine symbol."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (dolist (profile '(everyday writing reading coding-xs coding coding-xl
                     presentation))
    (let ((label (cj/fontaine-profile-label profile)))
      (should (eq (cj/fontaine-profile-from-label label) profile)))))

(ert-deftest test-font-config-fontaine-uses-one-monospace-family ()
  "Normal: every workflow profile uses Berkeley Mono for fixed pitch."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (dolist (profile '(everyday writing coding-xs coding coding-xl presentation))
    (let ((properties (fontaine--get-preset-properties profile)))
      (should (equal (plist-get properties :default-family)
                     "BerkeleyMono Nerd Font")))))

(ert-deftest test-font-config-fontaine-reading-is-merriweather-only ()
  "Normal: Reading uses Merriweather for every primary face family."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((properties (fontaine--get-preset-properties 'reading)))
    (dolist (property '(:default-family
                        :fixed-pitch-family
                        :fixed-pitch-serif-family
                        :variable-pitch-family))
      (should (equal (plist-get properties property) "Merriweather")))))

(ert-deftest test-font-config-fontaine-reading-properties-are-public ()
  "Normal: consumers can resolve Reading without Fontaine private functions."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((properties (cj/fontaine-profile-properties 'reading)))
    (should (equal (plist-get properties :default-family) "Merriweather"))
    (should (= (plist-get properties :default-height) 140))))

(ert-deftest test-font-config-fontaine-remaps-reading-buffer-locally ()
  "Normal: the local adapter applies all Reading families at an override height."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((calls nil))
    (cl-letf (((symbol-function 'face-remap-add-relative)
               (lambda (face &rest properties)
                 (push (cons face properties) calls)
                 face)))
      (should (equal (cj/fontaine-remap-buffer-to-profile 'reading 180)
                     '(default fixed-pitch fixed-pitch-serif variable-pitch))))
    (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
      (should (member (list face :family "Merriweather" :height 180)
                      calls)))))

(ert-deftest test-font-config-fontaine-ui-buffer-remaps-default-to-berkeley ()
  "Normal: minibuffer and echo buffers remap their default face to Berkeley."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((base nil))
    (cl-letf (((symbol-function 'face-remap-set-base)
               (lambda (face &rest specs) (setq base (cons face specs)))))
      (cj/fontaine-remap-ui-buffer))
    (should (equal base
                   '(default (:family "BerkeleyMono Nerd Font"))))))

(ert-deftest test-font-config-fontaine-ui-chrome-stays-berkeley ()
  "Normal: Fontaine reasserts Berkeley on chrome faces and echo buffers."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((faces nil)
        (remap-count 0))
    (cl-letf (((symbol-function 'facep) (lambda (_face) t))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest properties)
                 (push (cons face properties) faces)))
              ((symbol-function 'get-buffer) (lambda (_name) (current-buffer)))
              ((symbol-function 'cj/fontaine-remap-ui-buffer)
               (lambda () (setq remap-count (1+ remap-count)))))
      (cj/fontaine-keep-ui-chrome-monospace))
    (dolist (face '(mode-line mode-line-active mode-line-inactive
                    minibuffer-prompt))
      (should (member (list face :family "BerkeleyMono Nerd Font") faces)))
    (should (= remap-count 2))))

(ert-deftest test-font-config-fontaine-ui-chrome-hooks-are-installed ()
  "Boundary: profile, theme, and minibuffer changes restore UI typography."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (should (memq 'cj/fontaine-keep-ui-chrome-monospace
                fontaine-set-preset-hook))
  (should (memq 'cj/fontaine-keep-ui-chrome-monospace
                enable-theme-functions))
  (should (memq 'cj/fontaine-remap-ui-buffer minibuffer-setup-hook)))

(ert-deftest test-font-config-fontaine-unknown-label-has-no-profile ()
  "Error: an unknown destination label does not select a preset."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (should-not (cj/fontaine-profile-from-label "Missing profile")))

(ert-deftest test-font-config-fontaine-annotation-marks-current-profile ()
  "Normal: completion marks only the active workflow destination."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((fontaine-current-preset 'writing))
    (should (equal (cj/fontaine-profile-annotation
                    (cj/fontaine-profile-label 'writing))
                   "  current"))
    (should (equal (cj/fontaine-profile-annotation
                    (cj/fontaine-profile-label 'coding))
                   ""))))

(ert-deftest test-font-config-fontaine-selector-applies-picked-profile ()
  "Normal: the one-prompt picker maps its complete label before applying."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((picked (cj/fontaine-profile-label 'presentation))
        (applied nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _) picked))
              ((symbol-function 'cj/fontaine-apply-profile)
               (lambda (profile) (setq applied profile))))
      (cj/fontaine-select-profile)
      (should (eq applied 'presentation)))))

(ert-deftest test-font-config-fontaine-restores-valid-profile ()
  "Normal: startup restores a persisted workflow profile."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'fontaine-restore-latest-preset)
             (lambda () 'writing)))
    (should (eq (cj/fontaine-restored-or-default-profile) 'writing))))

(ert-deftest test-font-config-fontaine-rejects-obsolete-restored-profile ()
  "Boundary: an old brand or point-size preset falls back to everyday."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'fontaine-restore-latest-preset)
             (lambda () '13-point-font)))
    (should (eq (cj/fontaine-restored-or-default-profile) 'everyday))))

(ert-deftest test-font-config-fontaine-has-no-per-frame-reset-hook ()
  "Boundary: creating a daemon frame cannot overwrite the active profile."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (should-not (memq 'cj/apply-font-settings-to-frame
                    server-after-make-frame-hook))
  (should-not (memq 'cj/cleanup-frame-list delete-frame-functions)))

(ert-deftest test-font-config-fontaine-apply-records-profile-for-persistence ()
  "Normal: applying a profile updates Fontaine history before setting it."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((fontaine-preset-history nil)
        (applied nil))
    (cl-letf (((symbol-function 'fontaine-set-preset)
               (lambda (profile) (setq applied profile))))
      (cj/fontaine-apply-profile 'coding)
      (should (eq applied 'coding))
      (should (equal (car fontaine-preset-history) "coding")))))

(ert-deftest test-font-config-fontaine-apply-rejects-unknown-profile ()
  "Error: applying an unknown workflow profile signals a user error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'fontaine-set-preset)
             (lambda (_profile) (ert-fail "must not apply"))))
    (should-error (cj/fontaine-apply-profile 'missing) :type 'user-error)))

(provide 'test-font-config)
;;; test-font-config.el ends here
