;;; test-org-noter-config--generate-notes-template.el --- Tests for cj/org-noter--generate-notes-template -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/org-noter--generate-notes-template'. The function
;; produces an org-roam-shaped buffer template with a generated UUID,
;; ROAM_REFS / NOTER_DOCUMENT pointing at the document path, and the
;; title in #+title and #+CATEGORY. `org-id-uuid' is mocked at the
;; boundary so the test is deterministic.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'org-noter-config)

(defmacro test-org-noter-config--with-uuid (uuid &rest body)
  "Run BODY with `org-id-uuid' returning UUID."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'org-id-uuid) (lambda () ,uuid)))
     ,@body))

(ert-deftest test-org-noter-config-generate-template-includes-id-property ()
  "Normal: template includes the supplied UUID in the :ID: property."
  (test-org-noter-config--with-uuid "deadbeef-1234"
    (let ((text (cj/org-noter--generate-notes-template
                 "The Pragmatic Programmer" "/books/pragmatic.pdf")))
      (should (string-match-p ":ID: deadbeef-1234" text)))))

(ert-deftest test-org-noter-config-generate-template-roam-refs-and-noter-document ()
  "Normal: ROAM_REFS and NOTER_DOCUMENT both point at the document path."
  (test-org-noter-config--with-uuid "x"
    (let ((text (cj/org-noter--generate-notes-template
                 "Title" "/books/file.pdf")))
      (should (string-match-p ":ROAM_REFS: /books/file\\.pdf" text))
      (should (string-match-p ":NOTER_DOCUMENT: /books/file\\.pdf" text)))))

(ert-deftest test-org-noter-config-generate-template-title-and-category ()
  "Normal: #+title shows the title with prefix; #+CATEGORY uses the title."
  (test-org-noter-config--with-uuid "x"
    (let ((text (cj/org-noter--generate-notes-template
                 "Anaphora Studies" "/x")))
      (should (string-match-p "^#\\+title: Notes on Anaphora Studies$" text))
      (should (string-match-p "^#\\+CATEGORY: Anaphora Studies$" text)))))

(ert-deftest test-org-noter-config-generate-template-includes-readingnotes-tag ()
  "Boundary: the FILETAGS line carries the :ReadingNotes: tag."
  (test-org-noter-config--with-uuid "x"
    (let ((text (cj/org-noter--generate-notes-template "T" "/x")))
      (should (string-match-p "^#\\+FILETAGS: :ReadingNotes:$" text)))))

(ert-deftest test-org-noter-config-generate-template-ends-with-notes-heading ()
  "Boundary: the template ends with a top-level Notes heading."
  (test-org-noter-config--with-uuid "x"
    (let ((text (cj/org-noter--generate-notes-template "T" "/x")))
      (should (string-match-p "\\* Notes\n\\'" text)))))

(provide 'test-org-noter-config--generate-notes-template)
;;; test-org-noter-config--generate-notes-template.el ends here
