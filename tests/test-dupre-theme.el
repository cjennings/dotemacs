;;; test-dupre-theme.el --- Tests for dupre-theme -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the dupre-theme.

;;; Code:

(require 'ert)

;; Add themes directory to load-path and custom-theme-load-path
(let ((themes-dir (expand-file-name "../themes" (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path themes-dir)
  (add-to-list 'custom-theme-load-path themes-dir))

(require 'dupre-palette)

;;; Palette tests

(ert-deftest dupre-palette-exists ()
  "Palette constant should be defined."
  (should (boundp 'dupre-palette))
  (should (listp dupre-palette)))

(ert-deftest dupre-palette-has-base-colors ()
  "Palette should contain essential base colors."
  (should (assq 'bg dupre-palette))
  (should (assq 'fg dupre-palette))
  (should (assq 'bg+1 dupre-palette))
  (should (assq 'bg+2 dupre-palette)))

(ert-deftest dupre-palette-has-accent-colors ()
  "Palette should contain accent colors."
  (should (assq 'yellow dupre-palette))
  (should (assq 'blue dupre-palette))
  (should (assq 'green dupre-palette))
  (should (assq 'red dupre-palette)))

(ert-deftest dupre-palette-colors-are-hex ()
  "All palette colors should be valid hex strings."
  (dolist (entry dupre-palette)
    (let ((color (cadr entry)))
      (should (stringp color))
      (should (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)))))

(ert-deftest dupre-get-color-base ()
  "dupre-get-color should retrieve base colors."
  (should (string= (dupre-get-color 'bg) "#151311"))
  (should (string= (dupre-get-color 'fg) "#f0fef0"))
  (should (string= (dupre-get-color 'yellow) "#d7af5f")))

(ert-deftest dupre-get-color-semantic ()
  "dupre-get-color should resolve semantic mappings."
  (should (string= (dupre-get-color 'accent) (dupre-get-color 'yellow)))
  (should (string= (dupre-get-color 'err) (dupre-get-color 'intense-red)))
  (should (string= (dupre-get-color 'success) (dupre-get-color 'green))))

(ert-deftest dupre-get-color-unknown-errors ()
  "dupre-get-color should error on unknown colors."
  (should-error (dupre-get-color 'nonexistent-color)))

(ert-deftest dupre-with-colors-binds-values ()
  "dupre-with-colors should bind palette colors as variables."
  (dupre-with-colors
    (should (string= bg "#151311"))
    (should (string= fg "#f0fef0"))
    (should (string= yellow "#d7af5f"))
    (should (string= blue "#67809c"))))

(ert-deftest dupre-with-colors-binds-semantic ()
  "dupre-with-colors should bind semantic colors resolved to values."
  (dupre-with-colors
    (should (string= accent "#d7af5f"))
    (should (string= err "#ff2a00"))
    (should (string= success "#a4ac64"))))

;;; Theme loading tests

(ert-deftest dupre-theme-loads ()
  "Theme should load without errors."
  (load-theme 'dupre t)
  (should (memq 'dupre custom-enabled-themes)))

(ert-deftest dupre-theme-default-face ()
  "dupre-theme should set the default face correctly."
  (load-theme 'dupre t)
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (should (string= bg "#151311"))
    (should (string= fg "#f0fef0"))))

(ert-deftest dupre-theme-comment-face-italic ()
  "Comments should be rendered in italic slant."
  (load-theme 'dupre t)
  (should (eq (face-attribute 'font-lock-comment-face :slant) 'italic)))

(ert-deftest dupre-theme-keyword-face ()
  "Keywords should use blue color."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'font-lock-keyword-face :foreground) "#67809c")))

(ert-deftest dupre-theme-string-face ()
  "Strings should use green color."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'font-lock-string-face :foreground) "#a4ac64")))

(ert-deftest dupre-theme-function-face ()
  "Functions should use terracotta color."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'font-lock-function-name-face :foreground) "#a7502d")))

;;; Org-mode face tests (require org to be loaded)
;; Note: org-level-N faces use :inherit dupre-heading-N
;; We verify inheritance is set up correctly by checking the inherit attribute

(ert-deftest dupre-theme-org-level-1 ()
  "Org level 1 should inherit from dupre-heading-1."
  (require 'org)
  (load-theme 'dupre t)
  ;; Verify the inheritance relationship is set
  (should (eq (face-attribute 'org-level-1 :inherit) 'dupre-heading-1)))

(ert-deftest dupre-theme-org-level-2 ()
  "Org level 2 should inherit from dupre-heading-2."
  (require 'org)
  (load-theme 'dupre t)
  ;; Verify the inheritance relationship is set
  (should (eq (face-attribute 'org-level-2 :inherit) 'dupre-heading-2)))

(ert-deftest dupre-theme-org-todo ()
  "Org TODO should use intense-red."
  (require 'org)
  (load-theme 'dupre t)
  (should (string= (face-attribute 'org-todo :foreground) "#ff2a00")))

(ert-deftest dupre-theme-org-done ()
  "Org DONE should use green."
  (require 'org)
  (load-theme 'dupre t)
  (should (string= (face-attribute 'org-done :foreground) "#a4ac64")))

;;; Diff face tests (require diff-mode to be loaded)

(ert-deftest dupre-theme-diff-added ()
  "Diff added should use green foreground."
  (require 'diff-mode)
  (load-theme 'dupre t)
  (should (string= (face-attribute 'diff-added :foreground) "#a4ac64")))

(ert-deftest dupre-theme-diff-removed ()
  "Diff removed should use red foreground."
  (require 'diff-mode)
  (load-theme 'dupre t)
  (should (string= (face-attribute 'diff-removed :foreground) "#d47c59")))

;;; UI face tests

(ert-deftest dupre-theme-mode-line ()
  "Mode line should have correct background."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'mode-line :background) "#474544")))

(ert-deftest dupre-theme-region ()
  "Region should use bg+2 as background."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'region :background) "#474544")))

;;; Vertico face tests (skip if vertico not available)

(ert-deftest dupre-theme-vertico-current ()
  "Vertico current should use bg+2 background."
  (skip-unless (require 'vertico nil t))
  (load-theme 'dupre t)
  (should (string= (face-attribute 'vertico-current :background) "#474544")))

;;; Rainbow-delimiters tests (skip if package not available)

(ert-deftest dupre-theme-rainbow-depth-1 ()
  "Rainbow depth 1 should use yellow."
  (skip-unless (require 'rainbow-delimiters nil t))
  (load-theme 'dupre t)
  (should (string= (face-attribute 'rainbow-delimiters-depth-1-face :foreground) "#d7af5f")))

(ert-deftest dupre-theme-rainbow-depth-2 ()
  "Rainbow depth 2 should use blue."
  (skip-unless (require 'rainbow-delimiters nil t))
  (load-theme 'dupre t)
  (should (string= (face-attribute 'rainbow-delimiters-depth-2-face :foreground) "#67809c")))

;;; Error/warning face tests

(ert-deftest dupre-theme-error-face ()
  "Error face should use intense-red."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'error :foreground) "#ff2a00")))

(ert-deftest dupre-theme-warning-face ()
  "Warning face should use yellow+1."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'warning :foreground) "#ffd75f")))

(ert-deftest dupre-theme-success-face ()
  "Success face should use green."
  (load-theme 'dupre t)
  (should (string= (face-attribute 'success :foreground) "#a4ac64")))

(provide 'test-dupre-theme)
;;; test-dupre-theme.el ends here
