;;; test-org-contacts-config-find.el --- Tests for cj/org-contacts-find -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/org-contacts-find collects contact headings (name, position, and the
;; EMAIL/PHONE annotation) before prompting, then jumps to the selected
;; heading's stored position.  These tests pin the collector helper and a
;; command-level smoke test: the jump must land on the heading, never on a
;; body line that merely mentions the name.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-contacts-config)

;;; cj/--org-contacts-collect

(ert-deftest test-org-contacts-collect-returns-name-position-info ()
  "Normal: collector returns heading name, heading position, and EMAIL/PHONE."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice\n:PROPERTIES:\n:EMAIL: alice@example.com\n:END:\n"
            "some body text mentioning Bob\n"
            "* Bob\n:PROPERTIES:\n:PHONE: 555-1234\n:END:\n")
    (let* ((alist (cj/--org-contacts-collect (current-buffer)))
           (alice (assoc "Alice" alist))
           (bob (assoc "Bob" alist)))
      (should (equal (length alist) 2))
      (should (equal (nth 2 alice) "alice@example.com"))
      (should (equal (nth 2 bob) "555-1234"))
      ;; positions land on the headings, not the body line that mentions Bob
      (should (save-excursion (goto-char (nth 1 alice)) (looking-at "\\* Alice")))
      (should (save-excursion (goto-char (nth 1 bob)) (looking-at "\\* Bob"))))))

(ert-deftest test-org-contacts-collect-empty-buffer-returns-nil ()
  "Boundary: a buffer with no headings yields an empty alist."
  (with-temp-buffer
    (org-mode)
    (should (null (cj/--org-contacts-collect (current-buffer))))))

(ert-deftest test-org-contacts-collect-heading-without-props-has-nil-info ()
  "Error: a heading with no EMAIL or PHONE yields nil info."
  (with-temp-buffer
    (org-mode)
    (insert "* Carol\n")
    (let ((alist (cj/--org-contacts-collect (current-buffer))))
      (should (equal (nth 2 (assoc "Carol" alist)) nil)))))

;;; cj/org-contacts-find

(ert-deftest test-org-contacts-find-jumps-to-heading-not-body ()
  "Normal: selecting a contact jumps to its heading, not a body mention."
  (let ((contacts-file (make-temp-file "org-contacts-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file contacts-file
            (insert "* Alice\n:PROPERTIES:\n:EMAIL: alice@example.com\n:END:\n"
                    "note: call Bob about Alice\n"
                    "* Bob\n:PROPERTIES:\n:PHONE: 555-1234\n:END:\n"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Bob"))
                    ((symbol-function 'org-fold-show-entry) #'ignore)
                    ((symbol-function 'org-reveal) #'ignore))
            (cj/org-contacts-find)
            (should (looking-at "\\* Bob"))))
      (delete-file contacts-file))))

(provide 'test-org-contacts-config-find)
;;; test-org-contacts-config-find.el ends here
