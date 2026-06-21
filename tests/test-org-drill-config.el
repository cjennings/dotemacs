;;; test-org-drill-config.el --- Tests for org-drill navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the file/directory picking in `modules/org-drill-config.el':
;;   - `cj/--drill-files-in'   (pure: list drill Org files in a dir)
;;   - `cj/--drill-pick-dir'   (pure: default `drill-dir' vs prompted dir)
;;   - `cj/--drill-pick-file'  (prompt -> absolute path, `completing-read' stubbed)
;;   - `cj/drill-this-file'    (drill the current Org buffer / refuse otherwise)
;;   - `cj/drill-start'        (pick + `find-file' + `org-drill', boundaries stubbed)
;;   - the `cj/drill-map' bindings
;;
;; The defuns live inside the `use-package org-drill' `:config' block, which
;; runs once `org' and `org-capture' are loaded -- so those are required first.
;; `cj/custom-keymap' and `drill-dir' are stubbed the way the other module
;; tests stub the constants/keymaps their modules expect from the init layer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)
(package-initialize)
(require 'org)
(require 'org-capture)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub custom keymap for org-drill-config tests.")
(defvar drill-dir (file-name-as-directory
                   (expand-file-name "cj-drill-default" temporary-file-directory))
  "Stub `drill-dir' for org-drill-config tests.")

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-drill-config)

;; -------------------------------- temp-dir help ------------------------------

(defmacro test-org-drill--with-dir (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "cj-drill-test" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun test-org-drill--touch (dir &rest names)
  "Create empty files NAMES inside DIR."
  (dolist (n names)
    (with-temp-file (expand-file-name n dir))))

;; ----------------------------- cj/--drill-files-in ---------------------------

(ert-deftest test-org-drill--drill-files-in-lists-org-files ()
  "Normal: returns the `.org' file names in the directory, sorted."
  (test-org-drill--with-dir dir
    (test-org-drill--touch dir "b.org" "a.org")
    (should (equal '("a.org" "b.org") (cj/--drill-files-in dir)))))

(ert-deftest test-org-drill--drill-files-in-empty-dir ()
  "Boundary: an empty directory yields nil."
  (test-org-drill--with-dir dir
    (should (null (cj/--drill-files-in dir)))))

(ert-deftest test-org-drill--drill-files-in-skips-dotfiles-and-non-org ()
  "Boundary: leading-dot files and non-`.org' files are excluded."
  (test-org-drill--with-dir dir
    (test-org-drill--touch dir ".hidden.org" "notes.txt" "cards.org" "deck.org.bak")
    (should (equal '("cards.org") (cj/--drill-files-in dir)))))

(ert-deftest test-org-drill--drill-files-in-missing-dir-signals ()
  "Error: a directory that does not exist signals."
  (should-error (cj/--drill-files-in "/no/such/cj-drill/dir/")))

;; ----------------------------- cj/--drill-pick-dir ---------------------------

(ert-deftest test-org-drill--drill-pick-dir-defaults-to-drill-dir ()
  "Normal: with no prefix arg the picker uses `drill-dir'."
  (should (equal drill-dir (cj/--drill-pick-dir nil))))

(ert-deftest test-org-drill--drill-pick-dir-prompts-with-prefix ()
  "Normal: with a prefix arg the picker prompts for a directory."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) "/tmp/other-decks/")))
    (should (equal "/tmp/other-decks/" (cj/--drill-pick-dir t)))))

;; ----------------------------- cj/--drill-pick-file --------------------------

(ert-deftest test-org-drill--drill-pick-file-returns-absolute-path ()
  "Normal: the chosen name is expanded against the directory."
  (test-org-drill--with-dir dir
    (test-org-drill--touch dir "spanish.org")
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "spanish.org")))
      (should (equal (expand-file-name "spanish.org" dir)
                     (cj/--drill-pick-file dir))))))

;; ----------------------------- cj/drill-this-file ----------------------------

(ert-deftest test-org-drill-this-file-drills-an-org-buffer ()
  "Normal: in an Org buffer it starts a drill session."
  (let ((called 0))
    (cl-letf (((symbol-function 'org-drill) (lambda (&rest _) (cl-incf called))))
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (cj/drill-this-file)))
    (should (= 1 called))))

(ert-deftest test-org-drill-this-file-refuses-a-non-org-buffer ()
  "Error: outside an Org buffer it raises a `user-error' and drills nothing."
  (let ((called 0))
    (cl-letf (((symbol-function 'org-drill) (lambda (&rest _) (cl-incf called))))
      (with-temp-buffer
        (fundamental-mode)
        (should-error (cj/drill-this-file) :type 'user-error)))
    (should (= 0 called))))

;; ------------------------------- cj/drill-start ------------------------------

(ert-deftest test-org-drill-start-opens-the-pick-and-drills ()
  "Normal: opens the picked file, then starts a drill session."
  (let (opened (drilled 0))
    (cl-letf (((symbol-function 'cj/--drill-pick-file)
               (lambda (_dir) "/decks/french.org"))
              ((symbol-function 'find-file) (lambda (f &rest _) (setq opened f)))
              ((symbol-function 'org-drill) (lambda (&rest _) (cl-incf drilled))))
      (cj/drill-start))
    (should (equal "/decks/french.org" opened))
    (should (= 1 drilled))))

(ert-deftest test-org-drill-start-with-prefix-uses-the-prompted-dir ()
  "Normal: a prefix arg routes the file pick through the prompted directory."
  (test-org-drill--with-dir dir
    (test-org-drill--touch dir "latin.org")
    (let (opened)
      (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) dir))
                ((symbol-function 'completing-read) (lambda (&rest _) "latin.org"))
                ((symbol-function 'find-file) (lambda (f &rest _) (setq opened f)))
                ((symbol-function 'org-drill) #'ignore))
        (cj/drill-start t))
      (should (equal (expand-file-name "latin.org" dir) opened)))))

;; -------------------------------- cj/drill-map -------------------------------

(ert-deftest test-org-drill-map-bindings ()
  "Normal: the drill keymap exposes the documented commands."
  (dolist (b '(("s" . cj/drill-start)
               ("f" . cj/drill-this-file)
               ("e" . cj/drill-edit)
               ("c" . cj/drill-capture)
               ("r" . cj/drill-refile)
               ("R" . org-drill-resume)))
    (should (eq (cdr b) (keymap-lookup cj/drill-map (car b))))))

(provide 'test-org-drill-config)
;;; test-org-drill-config.el ends here
