;;; test-ui-buffer-status-colors.el --- Tests for buffer-status faces -*- lexical-binding: t; -*-

;;; Commentary:
;; The buffer-status state classifier (`cj/buffer-status-state'), the state->face
;; map (`cj/buffer-status-faces'), and the resolver (`cj/buffer-status-color')
;; drive both the cursor color and the modeline buffer-name color, kept in sync.
;; Theme faces (error / warning / success) replace the old hard-coded hexes so
;; the colors follow whatever theme is loaded.

;;; Code:

(require 'ert)
(require 'user-constants)
(require 'ui-config)
(require 'modeline-config)

;;; State -> face map

(ert-deftest test-buffer-status-faces-has-all-states ()
  "Normal: every buffer state is mapped to a face."
  (dolist (state '(read-only overwrite modified unmodified))
    (should (alist-get state cj/buffer-status-faces))))

(ert-deftest test-buffer-status-faces-values-are-real-faces ()
  "Normal: every mapped value is an existing face."
  (dolist (entry cj/buffer-status-faces)
    (should (facep (cdr entry)))))

(ert-deftest test-buffer-status-faces-mapping ()
  "Normal: read-only->error, overwrite/modified->warning, unmodified->success."
  (should (eq (alist-get 'read-only cj/buffer-status-faces) 'error))
  (should (eq (alist-get 'overwrite cj/buffer-status-faces) 'warning))
  (should (eq (alist-get 'modified cj/buffer-status-faces) 'warning))
  (should (eq (alist-get 'unmodified cj/buffer-status-faces) 'success)))

;;; State classifier (the shared function, exercised directly)

(ert-deftest test-buffer-status-state-read-only ()
  "Normal: a read-only buffer reports `read-only'."
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (eq (cj/buffer-status-state) 'read-only))))

(ert-deftest test-buffer-status-state-overwrite-wins-over-modified ()
  "Boundary: overwrite-mode takes priority over the modified state."
  (with-temp-buffer
    (insert "x")
    (overwrite-mode 1)
    (should (eq (cj/buffer-status-state) 'overwrite))))

(ert-deftest test-buffer-status-state-modified ()
  "Normal: a writeable buffer with unsaved changes reports `modified'."
  (with-temp-buffer
    (insert "x")
    (should (eq (cj/buffer-status-state) 'modified))))

(ert-deftest test-buffer-status-state-unmodified ()
  "Normal: a clean writeable buffer reports `unmodified'."
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (should (eq (cj/buffer-status-state) 'unmodified))))

(ert-deftest test-buffer-status-state-read-only-wins-over-modified ()
  "Boundary: read-only takes priority over modified."
  (with-temp-buffer
    (insert "x")
    (set-buffer-modified-p t)
    (setq buffer-read-only t)
    (should (eq (cj/buffer-status-state) 'read-only))))

;;; Resolver

(ert-deftest test-buffer-status-color-resolves-through-the-face ()
  "Normal: the color is the mapped face's foreground."
  (let ((orig (face-attribute 'error :foreground nil t)))
    (unwind-protect
        (progn
          (set-face-foreground 'error "#abcdef")
          (should (equal (cj/buffer-status-color 'read-only) "#abcdef")))
      (when (stringp orig) (set-face-foreground 'error orig)))))

(ert-deftest test-buffer-status-color-nil-for-unknown-state ()
  "Error: an unknown state has no face, so no color."
  (should-not (cj/buffer-status-color 'nonexistent)))

;;; Modeline integration

(ert-deftest test-modeline-buffer-name-variable-exists ()
  "Normal: the modeline buffer-name construct is defined."
  (should (boundp 'cj/modeline-buffer-name)))

(ert-deftest test-modeline-buffer-name-is-mode-line-construct ()
  "Normal: it is an :eval mode-line construct."
  (should (listp cj/modeline-buffer-name))
  (should (eq (car cj/modeline-buffer-name) :eval)))

(provide 'test-ui-buffer-status-colors)
;;; test-ui-buffer-status-colors.el ends here
