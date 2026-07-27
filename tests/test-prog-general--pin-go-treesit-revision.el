;;; test-prog-general--pin-go-treesit-revision.el --- Go grammar pinning -*- lexical-binding: t; -*-

;;; Commentary:
;; The treesit-auto Go recipe is pinned for compatibility with Emacs 30.2.
;; Keep the mutation independent of cl-defstruct's compile-time setter
;; expansion: prog-general is loaded before treesit-auto defines that setter
;; during a normal startup.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Deliberately put `revision' at a different offset from treesit-auto's real
;; struct.  The production helper must discover the slot rather than hard-code
;; the package's current vector layout.
(cl-defstruct treesit-auto-recipe lang revision url)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

(ert-deftest test-prog-general-pin-go-treesit-revision-updates-go ()
  "Normal: the Go recipe receives the supported grammar revision."
  (let* ((go (make-treesit-auto-recipe
              :lang 'go :revision "main" :url "https://example.test/go"))
         (python (make-treesit-auto-recipe
                  :lang 'python :revision "main"
                  :url "https://example.test/python")))
    (cj/treesit-auto-pin-go-revision (list python go))
    (should (equal (treesit-auto-recipe-revision go) "v0.19.1"))
    (should (equal (treesit-auto-recipe-revision python) "main"))))

(ert-deftest test-prog-general-pin-go-treesit-revision-no-go-is-no-op ()
  "Boundary: a recipe list without Go remains unchanged."
  (let ((python (make-treesit-auto-recipe
                 :lang 'python :revision "main"
                 :url "https://example.test/python")))
    (should-not (cj/treesit-auto-pin-go-revision (list python)))
    (should (equal (treesit-auto-recipe-revision python) "main"))))

(ert-deftest test-prog-general-pin-go-treesit-revision-empty-is-no-op ()
  "Boundary: an empty recipe list does not signal an error."
  (should-not (cj/treesit-auto-pin-go-revision nil)))

(ert-deftest test-prog-general-pin-go-treesit-revision-malformed-recipe-errors ()
  "Error: a malformed recipe signals instead of silently skipping the pin."
  (should-error (cj/treesit-auto-pin-go-revision '(not-a-recipe))
                :type 'wrong-type-argument))

(provide 'test-prog-general--pin-go-treesit-revision)
;;; test-prog-general--pin-go-treesit-revision.el ends here
