;;; test-dirvish-config-resolve-display-path.el --- Tests for the path-resolution helper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--dired-resolve-display-path' is the pure logic underneath
;; `cj/dired-copy-path-as-kill'.  Given the absolute FILE, the active
;; PROJECT-ROOT (or nil), the user's HOME-DIR, and a FORCE-ABSOLUTE
;; flag, the helper returns a (PATH . PATH-TYPE) cons describing what
;; to copy and how to label it.  The interactive wrapper does the
;; clipboard write and the user-visible message.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(ert-deftest test-cj--dired-resolve-display-path-project-relative ()
  "Normal: file inside a project returns a project-relative path."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me/code/proj/src/main.el"
                  "/home/me/code/proj/"
                  "/home/me")
                 (cons "src/main.el" "project-relative"))))

(ert-deftest test-cj--dired-resolve-display-path-home-relative ()
  "Normal: file under home but not in any project returns a ~/-prefixed path."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me/notes/today.org"
                  nil
                  "/home/me")
                 (cons "~/notes/today.org" "home-relative"))))

(ert-deftest test-cj--dired-resolve-display-path-home-itself ()
  "Boundary: file IS the home dir -> path is the bare ~ glyph."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me"
                  nil
                  "/home/me")
                 (cons "~" "home-relative"))))

(ert-deftest test-cj--dired-resolve-display-path-absolute-fallback ()
  "Boundary: file outside home and no project -> absolute path."
  (should (equal (cj/--dired-resolve-display-path
                  "/etc/hosts"
                  nil
                  "/home/me")
                 (cons "/etc/hosts" "absolute"))))

(ert-deftest test-cj--dired-resolve-display-path-force-absolute-overrides-project ()
  "Normal: FORCE-ABSOLUTE wins over an active project root."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me/code/proj/src/main.el"
                  "/home/me/code/proj/"
                  "/home/me"
                  t)
                 (cons "/home/me/code/proj/src/main.el" "absolute"))))

(ert-deftest test-cj--dired-resolve-display-path-force-absolute-overrides-home ()
  "Normal: FORCE-ABSOLUTE wins over a home-prefix match."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me/notes/today.org"
                  nil
                  "/home/me"
                  t)
                 (cons "/home/me/notes/today.org" "absolute"))))

(ert-deftest test-cj--dired-resolve-display-path-project-precedes-home ()
  "Normal: when a file is BOTH inside a project and under home, the project
view wins.  This matches the original code -- projects are usually under
home and the project-relative form reads more usefully."
  (should (equal (cj/--dired-resolve-display-path
                  "/home/me/code/proj/src/main.el"
                  "/home/me/code/proj/"
                  "/home/me")
                 (cons "src/main.el" "project-relative"))))

(provide 'test-dirvish-config-resolve-display-path)
;;; test-dirvish-config-resolve-display-path.el ends here
