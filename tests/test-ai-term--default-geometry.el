;;; test-ai-term--default-geometry.el --- Tests for host-aware display defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; ai-term's default display geometry is host-aware: a laptop opens the
;; agent from the bottom (75% height), a desktop opens it from the right
;; (50% width).  `cj/--ai-term-default-direction' and
;; `cj/--ai-term-default-size' encapsulate the `env-laptop-p' branch;
;; they feed the default fallbacks in `cj/--ai-term-capture-state' and
;; `cj/--ai-term-display-saved'.
;;
;; `env-laptop-p' is stubbed per-test so the assertions are deterministic
;; regardless of the host the suite runs on.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--default-direction-laptop ()
  "Normal: on a laptop the default direction is `below'."
  (cl-letf (((symbol-function 'env-laptop-p) (lambda () t)))
    (should (eq (cj/--ai-term-default-direction) 'below))))

(ert-deftest test-ai-term--default-direction-desktop ()
  "Normal: on a desktop the default direction is `right'."
  (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
    (should (eq (cj/--ai-term-default-direction) 'right))))

(ert-deftest test-ai-term--default-size-laptop ()
  "Normal: on a laptop the default size is `cj/ai-term-laptop-height'."
  (let ((cj/ai-term-laptop-height 0.75)
        (cj/ai-term-desktop-width 0.5))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () t)))
      (should (= (cj/--ai-term-default-size) 0.75)))))

(ert-deftest test-ai-term--default-size-desktop ()
  "Normal: on a desktop the default size is `cj/ai-term-desktop-width'."
  (let ((cj/ai-term-laptop-height 0.75)
        (cj/ai-term-desktop-width 0.5))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
      (should (= (cj/--ai-term-default-size) 0.5)))))

(ert-deftest test-ai-term--default-size-respects-custom-values ()
  "Boundary: the helper returns the customized values, not the literals."
  (let ((cj/ai-term-laptop-height 0.6)
        (cj/ai-term-desktop-width 0.33))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () t)))
      (should (= (cj/--ai-term-default-size) 0.6)))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
      (should (= (cj/--ai-term-default-size) 0.33)))))

(provide 'test-ai-term--default-geometry)
;;; test-ai-term--default-geometry.el ends here
