;;; test-org-noter-config--predicates.el --- Tests for the org-noter-config predicates -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the predicate helpers in org-noter-config.el:
;; cj/org-noter--in-document-p, cj/org-noter--in-notes-file-p,
;; cj/org-noter--session-active-p, and cj/org-noter--get-document-path
;; (mode-dispatch helper).
;;
;; Mocks are at the boundary: derived-mode-p for mode dispatch,
;; org-entry-get for the NOTER_DOCUMENT lookup. The tests don't load
;; pdf-view or nov; they only need the symbols recognised by
;; derived-mode-p.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'org-noter-config)

;; Forward-declare and initialise so cl-letf on symbol-value works.
(defvar org-noter--session nil)
(defvar nov-file-name nil)

(defmacro test-org-noter-config--with-mode (mode &rest body)
  "Run BODY with `derived-mode-p' returning MODE for any matching arg.
MODE may be a symbol or nil; nil means \"not derived from anything we ask\"."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'derived-mode-p)
              (lambda (&rest modes) (and ,mode (memq ,mode modes)))))
     ,@body))

;;; cj/org-noter--in-document-p

(ert-deftest test-org-noter-config-in-document-p-true-in-pdf-view ()
  "Normal: pdf-view-mode buffer counts as a document."
  (test-org-noter-config--with-mode 'pdf-view-mode
    (should (cj/org-noter--in-document-p))))

(ert-deftest test-org-noter-config-in-document-p-true-in-nov ()
  "Normal: nov-mode buffer counts as a document."
  (test-org-noter-config--with-mode 'nov-mode
    (should (cj/org-noter--in-document-p))))

(ert-deftest test-org-noter-config-in-document-p-false-in-org ()
  "Boundary: org-mode buffer is not a document."
  (test-org-noter-config--with-mode 'org-mode
    (should-not (cj/org-noter--in-document-p))))

(ert-deftest test-org-noter-config-in-document-p-false-in-fundamental ()
  "Boundary: a fundamental-mode buffer is not a document."
  (test-org-noter-config--with-mode nil
    (should-not (cj/org-noter--in-document-p))))

;;; cj/org-noter--in-notes-file-p

(ert-deftest test-org-noter-config-in-notes-file-p-true-when-noter-document-set ()
  "Normal: org-mode + NOTER_DOCUMENT property → notes file."
  (test-org-noter-config--with-mode 'org-mode
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (_pos prop)
                 (when (equal prop "NOTER_DOCUMENT") "/some/doc.pdf"))))
      (should (cj/org-noter--in-notes-file-p)))))

(ert-deftest test-org-noter-config-in-notes-file-p-false-when-no-property ()
  "Boundary: org-mode without NOTER_DOCUMENT property → not a notes file."
  (test-org-noter-config--with-mode 'org-mode
    (cl-letf (((symbol-function 'org-entry-get) (lambda (&rest _) nil)))
      (should-not (cj/org-noter--in-notes-file-p)))))

(ert-deftest test-org-noter-config-in-notes-file-p-false-outside-org ()
  "Boundary: non-org buffers are never notes files even with the property."
  (test-org-noter-config--with-mode 'pdf-view-mode
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (_pos prop)
                 (when (equal prop "NOTER_DOCUMENT") "/some/doc.pdf"))))
      (should-not (cj/org-noter--in-notes-file-p)))))

;;; cj/org-noter--session-active-p

(ert-deftest test-org-noter-config-session-active-p-true-when-session-set ()
  "Normal: session-active when org-noter--session is non-nil."
  (cl-letf (((symbol-value 'org-noter--session) 'fake-session))
    (should (cj/org-noter--session-active-p))))

(ert-deftest test-org-noter-config-session-active-p-false-when-nil ()
  "Boundary: session-active is nil when org-noter--session is nil."
  (cl-letf (((symbol-value 'org-noter--session) nil))
    (should-not (cj/org-noter--session-active-p))))

;;; cj/org-noter--get-document-path

(ert-deftest test-org-noter-config-get-document-path-pdf-uses-buffer-file-name ()
  "Normal: PDF mode returns `buffer-file-name'."
  (test-org-noter-config--with-mode 'pdf-view-mode
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&optional _) "/books/file.pdf")))
      (should (equal "/books/file.pdf" (cj/org-noter--get-document-path))))))

(ert-deftest test-org-noter-config-get-document-path-nov-uses-nov-file-name ()
  "Normal: nov-mode returns `nov-file-name'."
  (test-org-noter-config--with-mode 'nov-mode
    (cl-letf (((symbol-value 'nov-file-name) "/books/book.epub"))
      (should (equal "/books/book.epub" (cj/org-noter--get-document-path))))))

(ert-deftest test-org-noter-config-get-document-path-other-mode-returns-nil ()
  "Boundary: a non-document mode returns nil."
  (test-org-noter-config--with-mode 'org-mode
    (should-not (cj/org-noter--get-document-path))))

;;; cj/org-noter--extract-document-title

(ert-deftest test-org-noter-config-extract-document-title-strips-extension ()
  "Normal: extract-document-title returns the basename without extension."
  (test-org-noter-config--with-mode 'pdf-view-mode
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&optional _) "/books/the-pragmatic-programmer.pdf")))
      (should (equal "the-pragmatic-programmer"
                     (cj/org-noter--extract-document-title))))))

(provide 'test-org-noter-config--predicates)
;;; test-org-noter-config--predicates.el ends here
