;;; test-dirvish-config-drill.el --- Tests for the dirvish org-drill command -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/dirvish-drill-file' (bound to `S' in `dirvish-mode-map') opens the
;; `.org' file at point and starts an `org-drill' session on it.  These tests
;; mock `dired-get-filename', `find-file', and `cj/drill-this-file' and check
;; the happy path plus the rejection paths (no file, directory, non-`.org').
;;
;; `cj/drill-this-file' lives in org-drill-config.el; it's stubbed here so the
;; dirvish command can be exercised without loading the org-drill stack.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))

(unless (fboundp 'cj/drill-this-file)
  (defun cj/drill-this-file (&rest _)
    "Test stub; the real definition lives in org-drill-config.el."
    nil))

(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(ert-deftest test-dirvish-drill-file-opens-and-drills-an-org-file ()
  "Normal: an `.org' file at point is opened and drilled."
  (let (opened (drilled 0))
    (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) "/tmp/decks/cards.org"))
              ((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'cj/drill-this-file) (lambda (&rest _) (cl-incf drilled))))
      (cj/dirvish-drill-file))
    (should (equal "/tmp/decks/cards.org" opened))
    (should (= 1 drilled))))

(ert-deftest test-dirvish-drill-file-accepts-uppercase-extension ()
  "Boundary: the `.org' check ignores case."
  (let (opened)
    (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) "/tmp/decks/CARDS.ORG"))
              ((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'cj/drill-this-file) #'ignore))
      (cj/dirvish-drill-file))
    (should (equal "/tmp/decks/CARDS.ORG" opened))))

(ert-deftest test-dirvish-drill-file-rejects-non-org-file ()
  "Error: a non-`.org' file is refused and nothing is opened or drilled."
  (let ((opened nil) (drilled 0))
    (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) "/tmp/decks/notes.txt"))
              ((symbol-function 'find-file) (lambda (&rest _) (setq opened t)))
              ((symbol-function 'cj/drill-this-file) (lambda (&rest _) (cl-incf drilled))))
      (should-error (cj/dirvish-drill-file) :type 'user-error))
    (should-not opened)
    (should (= 0 drilled))))

(ert-deftest test-dirvish-drill-file-rejects-no-file-at-point ()
  "Error: with no file at point it raises a `user-error'."
  (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) nil))
            ((symbol-function 'find-file) (lambda (&rest _) (error "should not open")))
            ((symbol-function 'cj/drill-this-file) (lambda (&rest _) (error "should not drill"))))
    (should-error (cj/dirvish-drill-file) :type 'user-error)))

(ert-deftest test-dirvish-drill-file-rejects-a-directory ()
  "Boundary: a directory at point is refused even when its name ends in `.org'."
  (let* ((parent (make-temp-file "cj-drill-dir" t))
         (dir (expand-file-name "deck.org" parent)))
    (make-directory dir)
    (unwind-protect
        (cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) dir))
                  ((symbol-function 'find-file) (lambda (&rest _) (error "should not open")))
                  ((symbol-function 'cj/drill-this-file) (lambda (&rest _) (error "should not drill"))))
          (should-error (cj/dirvish-drill-file) :type 'user-error))
      (delete-directory parent t))))

(ert-deftest test-dirvish-drill-file-keymap-binding ()
  "Normal: `S' in `dirvish-mode-map' runs the drill command."
  (should (eq (keymap-lookup dirvish-mode-map "S") #'cj/dirvish-drill-file)))

(provide 'test-dirvish-config-drill)
;;; test-dirvish-config-drill.el ends here
