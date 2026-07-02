;;; test-modeline-config-flycheck-render.el --- flycheck counts rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--modeline-flycheck-render', the pure formatter that
;; turns a flycheck counts alist ((error . N) (warning . M) ...) into a
;; propertized modeline string.  Errors carry the error face, warnings
;; the warning face; zero-count severities are omitted; an all-clean
;; alist (or nil) renders nothing.  Glyphs come from nerd-icons when
;; available (private-use codepoints, safe from emojify) with plain-text
;; fallbacks in batch mode.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(ert-deftest test-modeline-config-flycheck-render-errors-and-warnings ()
  "Normal: both counts render, error face on errors, warning on warnings."
  (let ((s (cj/--modeline-flycheck-render '((error . 2) (warning . 5)))))
    (should (stringp s))
    (should (string-match-p "2" s))
    (should (string-match-p "5" s))
    (let ((epos (string-match "2" s))
          (wpos (string-match "5" s)))
      (should (eq (get-text-property epos 'face s) 'error))
      (should (eq (get-text-property wpos 'face s) 'warning)))))

(ert-deftest test-modeline-config-flycheck-render-errors-only ()
  "Normal: warnings absent when their count is zero or missing."
  (let ((s (cj/--modeline-flycheck-render '((error . 3)))))
    (should (stringp s))
    (should (string-match-p "3" s))
    (should-not (text-property-any 0 (length s) 'face 'warning s))))

(ert-deftest test-modeline-config-flycheck-render-clean-nil ()
  "Boundary: zero counts render nothing."
  (should-not (cj/--modeline-flycheck-render '((error . 0) (warning . 0)))))

(ert-deftest test-modeline-config-flycheck-render-nil-input ()
  "Boundary: nil counts alist renders nothing."
  (should-not (cj/--modeline-flycheck-render nil)))

(ert-deftest test-modeline-config-flycheck-render-info-ignored ()
  "Boundary: info-level counts alone render nothing (errors/warnings only)."
  (should-not (cj/--modeline-flycheck-render '((info . 4)))))

(provide 'test-modeline-config-flycheck-render)
;;; test-modeline-config-flycheck-render.el ends here
