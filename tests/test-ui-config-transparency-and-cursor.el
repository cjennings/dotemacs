;;; test-ui-config-transparency-and-cursor.el --- Tests for transparency + cursor-type helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Companions to `test-ui-config--buffer-cursor-state.el'.  Covers the
;; transparency apply/toggle pair and the cursor-type setter, which
;; were untested before.  Frame-mutating primitives are stubbed so the
;; tests run in batch without a graphical frame.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq load-prefer-newer t)
(require 'ui-config)

;;; cj/apply-transparency

(ert-deftest test-ui-config-apply-transparency-enabled-uses-level-as-alpha ()
  "Normal: when enabled, the alpha cons matches `cj/transparency-level'."
  (let ((cj/enable-transparency t)
        (cj/transparency-level 70)
        (default-frame-alist nil)
        (applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame param value)
                 (when (eq param 'alpha) (setq applied value)))))
      (cj/apply-transparency))
    (should (equal applied '(70 . 70)))
    (should (equal (alist-get 'alpha default-frame-alist) '(70 . 70)))))

(ert-deftest test-ui-config-apply-transparency-disabled-uses-opaque ()
  "Normal: when disabled, alpha resets to (100 . 100) (fully opaque)."
  (let ((cj/enable-transparency nil)
        (cj/transparency-level 50)
        (default-frame-alist '((alpha . (50 . 50))))
        (applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame param value)
                 (when (eq param 'alpha) (setq applied value)))))
      (cj/apply-transparency))
    (should (equal applied '(100 . 100)))
    (should (equal (alist-get 'alpha default-frame-alist) '(100 . 100)))))

(ert-deftest test-ui-config-apply-transparency-skips-frame-call-on-terminal ()
  "Boundary: on a terminal frame, skip `set-frame-parameter' but still update
the default-frame-alist so a future graphical frame would pick it up."
  (let ((cj/enable-transparency t)
        (cj/transparency-level 60)
        (default-frame-alist nil)
        (set-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (&rest _) (setq set-called t))))
      (cj/apply-transparency))
    (should-not set-called)
    (should (equal (alist-get 'alpha default-frame-alist) '(60 . 60)))))

(ert-deftest test-ui-config-apply-transparency-error-message-on-failure ()
  "Error: a `set-frame-parameter' error is captured by `condition-case' and
surfaced via `message'; the default-alist update still happens."
  (let ((cj/enable-transparency t)
        (cj/transparency-level 60)
        (default-frame-alist nil)
        (msg nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/apply-transparency))
    (should (string-match-p "Failed to set transparency" msg))
    (should (equal (alist-get 'alpha default-frame-alist) '(60 . 60)))))

;;; cj/toggle-transparency

(ert-deftest test-ui-config-toggle-transparency-flips-and-applies ()
  "Normal: `cj/toggle-transparency' flips `cj/enable-transparency' and re-applies."
  (let ((cj/enable-transparency nil)
        (cj/transparency-level 80)
        (default-frame-alist nil)
        (applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame param value)
                 (when (eq param 'alpha) (setq applied value))))
              ((symbol-function 'message) #'ignore))
      (cj/toggle-transparency))
    (should cj/enable-transparency)
    (should (equal applied '(80 . 80)))))

(ert-deftest test-ui-config-toggle-transparency-double-toggle-restores-state ()
  "Boundary: two toggles in a row return `cj/enable-transparency' to its start."
  (let ((cj/enable-transparency t)
        (cj/transparency-level 90)
        (default-frame-alist nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'set-frame-parameter) #'ignore)
              ((symbol-function 'message) #'ignore))
      (cj/toggle-transparency)
      (cj/toggle-transparency))
    (should (eq cj/enable-transparency t))))

;;; cj/set-cursor-type

(ert-deftest test-ui-config-set-cursor-type-passes-symbol-to-frame ()
  "Normal: `cj/set-cursor-type' hands NEW-CURSOR-TYPE to `modify-frame-parameters'."
  (let ((received nil))
    (cl-letf (((symbol-function 'modify-frame-parameters)
               (lambda (_frame alist) (setq received alist))))
      (cj/set-cursor-type 'bar))
    (should (equal received '((cursor-type . bar))))))

(ert-deftest test-ui-config-set-cursor-type-accepts-nil ()
  "Boundary: nil is a valid cursor type (means \"invisible\")."
  (let ((received nil))
    (cl-letf (((symbol-function 'modify-frame-parameters)
               (lambda (_frame alist) (setq received alist))))
      (cj/set-cursor-type nil))
    (should (equal received '((cursor-type . nil))))))

(provide 'test-ui-config-transparency-and-cursor)
;;; test-ui-config-transparency-and-cursor.el ends here
