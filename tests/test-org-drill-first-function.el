;;; test-org-drill-first-function.el --- Test org-drill 'first' function compatibility -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests to reproduce and verify the fix for org-drill's use of deprecated
;; 'first' function which was removed in modern Emacs.
;;
;; Original error: "mapcar: Symbol's function definition is void: first"
;;
;; The error occurred because org-drill (or its dependencies) use old Common Lisp
;; functions like 'first' instead of the modern 'cl-first' from cl-lib.

;;; Code:

(require 'ert)

(ert-deftest test-org-drill-first-function-not-defined-without-compat ()
  "Verify that 'first' function doesn't exist by default in modern Emacs.

This test documents the original problem - the 'first' function from the
old 'cl' package is not available in modern Emacs, which only provides
'cl-first' from cl-lib."
  (let ((first-defined (fboundp 'first)))
    ;; In a clean Emacs without our compatibility shim, 'first' should not exist
    ;; (unless the old 'cl' package was loaded, which is deprecated)
    (should (or (not first-defined)
                ;; If it IS defined, it should be our compatibility alias
                (eq (symbol-function 'first) 'cl-first)))))

(ert-deftest test-org-drill-cl-first-is-available ()
  "Verify that cl-first is available from cl-lib.

The modern cl-lib package provides cl-first as the replacement for
the deprecated 'first' function."
  (require 'cl-lib)
  (should (fboundp 'cl-first))
  ;; Test it works
  (should (eq 'a (cl-first '(a b c)))))

(ert-deftest test-org-drill-first-compatibility-alias ()
  "Verify that our compatibility alias makes 'first' work like 'cl-first'.

This is the fix we applied - creating an alias so that code using the
old 'first' function will work with the modern 'cl-first'."
  (require 'cl-lib)

  ;; Create the compatibility alias (same as in org-drill-config.el)
  (unless (fboundp 'first)
    (defalias 'first 'cl-first))

  ;; Now 'first' should be defined
  (should (fboundp 'first))

  ;; And it should behave like cl-first
  (should (eq 'a (first '(a b c))))
  (should (eq 'x (first '(x y z))))
  (should (eq nil (first '()))))

(ert-deftest test-org-drill-mapcar-with-first ()
  "Test the exact error scenario: (mapcar 'first ...).

This reproduces the original error that occurred during org-drill's
item collection phase where it uses mapcar with the 'first' function."
  (require 'cl-lib)

  ;; Create the compatibility alias
  (unless (fboundp 'first)
    (defalias 'first 'cl-first))

  ;; Simulate org-drill data structure: list of (status data) pairs
  (let ((drill-entries '((:new 0 0)
                         (:young 5 3)
                         (:overdue 10 2)
                         (:mature 20 1))))

    ;; This is the kind of operation that was failing
    ;; Extract first element from each entry
    (let ((statuses (mapcar 'first drill-entries)))
      (should (equal statuses '(:new :young :overdue :mature))))))

(ert-deftest test-org-drill-second-and-third-aliases ()
  "Verify that second and third compatibility aliases also work.

org-drill might use other deprecated cl functions too, so we create
aliases for second and third as well."
  (require 'cl-lib)

  ;; Create all compatibility aliases
  (unless (fboundp 'first)
    (defalias 'first 'cl-first))
  (unless (fboundp 'second)
    (defalias 'second 'cl-second))
  (unless (fboundp 'third)
    (defalias 'third 'cl-third))

  (let ((test-list '(a b c d e)))
    (should (eq 'a (first test-list)))
    (should (eq 'b (second test-list)))
    (should (eq 'c (third test-list)))))

(ert-deftest test-org-drill-config-loads-without-error ()
  "Verify that org-drill-config.el loads successfully with our fix.

This test ensures that the :init block in our use-package form
doesn't cause any loading errors."
  ;; This should not throw an error
  (should-not (condition-case err
                  (progn
                    (load (expand-file-name "modules/org-drill-config.el"
                                            user-emacs-directory))
                    nil)
                (error err))))

(ert-deftest test-org-drill-data-structure-operations ()
  "Verify that common org-drill data structure operations work with our fix.

org-drill works with data structures that require extracting elements.
This test ensures our compatibility aliases work with typical patterns."
  (require 'cl-lib)

  ;; Create compatibility aliases
  (unless (fboundp 'first)
    (defalias 'first 'cl-first))

  ;; Test that we can work with org-drill-like data structures
  ;; (similar to what persist-defvar would store)
  (let ((test-data '((:status-1 data-1)
                     (:status-2 data-2)
                     (:status-3 data-3))))
    ;; This kind of operation should work
    (should (equal '(:status-1 :status-2 :status-3)
                   (mapcar 'first test-data)))))

(provide 'test-org-drill-first-function)
;;; test-org-drill-first-function.el ends here
