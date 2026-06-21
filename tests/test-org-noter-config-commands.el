;;; test-org-noter-config-commands.el --- Tests for org-noter-config command + helper functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the preferred-split helper, the title-to-slug
;; helper, the notes-template generator, and the in-document /
;; in-notes-file predicates.  This file fills in:
;;
;;   cj/org-noter--get-document-path
;;   cj/org-noter--extract-document-title
;;   cj/org-noter--find-notes-file
;;   cj/org-noter--create-notes-file
;;   cj/org-noter--session-active-p
;;   cj/org-noter--toggle-notes-window
;;   cj/org-noter-start
;;   cj/org-noter-insert-note-dwim
;;
;; org-noter / pdf-view / nov primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-noter-config)

;; Top-level dynamic vars some of the helpers reference.
(defvar org-noter--session nil
  "Stub for `org-noter--session' (org-noter not loaded here).")
(defvar nov-file-name nil
  "Stub for `nov-file-name' (nov.el not loaded here).")
(defvar pdf-view-display-size nil
  "Stub for `pdf-view-display-size'.")

;;; cj/org-noter--get-document-path

(ert-deftest test-org-noter-get-document-path-pdf-mode-uses-buffer-file ()
  "Normal: in pdf-view-mode the buffer-file-name is the doc path."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest modes) (memq 'pdf-view-mode modes))))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/whitepaper.pdf")
      (should (equal (cj/org-noter--get-document-path) "/tmp/whitepaper.pdf"))
      (setq buffer-file-name nil))))

(ert-deftest test-org-noter-get-document-path-nov-mode-uses-nov-file-name ()
  "Normal: in nov-mode the variable `nov-file-name' is the doc path."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest modes) (memq 'nov-mode modes))))
    (let ((nov-file-name "/tmp/book.epub"))
      (should (equal (cj/org-noter--get-document-path) "/tmp/book.epub")))))

(ert-deftest test-org-noter-get-document-path-unknown-mode-returns-nil ()
  "Boundary: an unrelated major mode returns nil."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
    (should-not (cj/org-noter--get-document-path))))

;;; cj/org-noter--extract-document-title

(ert-deftest test-org-noter-extract-document-title-strips-extension ()
  "Normal: the title is the basename without extension."
  (cl-letf (((symbol-function 'cj/org-noter--get-document-path)
             (lambda () "/tmp/papers/quantum-2024.pdf")))
    (should (equal (cj/org-noter--extract-document-title) "quantum-2024"))))

;;; cj/org-noter--find-notes-file

(ert-deftest test-org-noter-find-notes-file-returns-matching-file ()
  "Normal: a notes file whose body mentions the doc-path is returned."
  (let* ((dir (make-temp-file "test-org-noter-find-" t))
         (notes (expand-file-name "notes-on-foo.org" dir))
         (doc "/tmp/papers/foo.pdf")
         (cj/org-noter-notes-directory dir))
    (unwind-protect
        (progn
          (with-temp-file notes
            (insert ":PROPERTIES:\n:NOTER_DOCUMENT: " doc "\n:END:\n"))
          (cl-letf (((symbol-function 'cj/org-noter--get-document-path)
                     (lambda () doc)))
            (should (equal (cj/org-noter--find-notes-file) notes))))
      (delete-directory dir t))))

(ert-deftest test-org-noter-find-notes-file-nil-when-no-doc-path ()
  "Boundary: with no current document, find returns nil."
  (cl-letf (((symbol-function 'cj/org-noter--get-document-path)
             (lambda () nil)))
    (should-not (cj/org-noter--find-notes-file))))

(ert-deftest test-org-noter-find-notes-file-nil-when-no-match ()
  "Boundary: a directory with notes files that don't match returns nil."
  (let* ((dir (make-temp-file "test-org-noter-find-" t))
         (cj/org-noter-notes-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "unrelated.org" dir)
            (insert "* Notes about something else\n"))
          (cl-letf (((symbol-function 'cj/org-noter--get-document-path)
                     (lambda () "/tmp/different.pdf")))
            (should-not (cj/org-noter--find-notes-file))))
      (delete-directory dir t))))

;;; cj/org-noter--create-notes-file

(ert-deftest test-org-noter-create-notes-file-writes-template-when-absent ()
  "Normal: a new doc gets a new notes file with the template content."
  (let* ((dir (make-temp-file "test-org-noter-create-" t))
         (cj/org-noter-notes-directory dir)
         (doc "/tmp/papers/great-book.pdf"))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/org-noter--get-document-path)
                   (lambda () doc))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "great-book"))
                  ;; The template uses `org-id-uuid' (org-id not loaded here).
                  ((symbol-function 'org-id-uuid)
                   (lambda () "00000000-0000-0000-0000-000000000000"))
                  ((symbol-function 'find-file-noselect)
                   (lambda (f &rest _) (get-buffer-create (concat "*test-" f "*")))))
          (let ((path (cj/org-noter--create-notes-file)))
            (should (file-exists-p path))
            (with-temp-buffer
              (insert-file-contents path)
              (should (string-match-p (regexp-quote doc) (buffer-string))))))
      (delete-directory dir t))))

;;; cj/org-noter--session-active-p

(ert-deftest test-org-noter-session-active-p-nil-when-unbound ()
  "Boundary: with `org-noter--session' nil, predicate returns nil."
  (let ((org-noter--session nil))
    (should-not (cj/org-noter--session-active-p))))

(ert-deftest test-org-noter-session-active-p-non-nil-when-set ()
  "Normal: with `org-noter--session' set, predicate returns non-nil."
  (let ((org-noter--session 'active-session))
    (should (cj/org-noter--session-active-p))))

;;; cj/org-noter--toggle-notes-window

(ert-deftest test-org-noter-toggle-notes-window-closes-when-visible ()
  "Normal: a visible notes window gets deleted."
  (let ((deleted nil))
    (cl-letf (((symbol-function 'org-noter--get-notes-window)
               (lambda (&rest _) 'fake-window))
              ((symbol-function 'delete-window)
               (lambda (w) (setq deleted w)))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
      (cj/org-noter--toggle-notes-window))
    (should (eq deleted 'fake-window))))

(ert-deftest test-org-noter-toggle-notes-window-opens-when-hidden ()
  "Normal: when no notes window exists, `start' is requested."
  (let ((requested-mode nil))
    (cl-letf (((symbol-function 'org-noter--get-notes-window)
               (lambda (&optional flag)
                 (if flag (setq requested-mode flag) nil)))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
      (cj/org-noter--toggle-notes-window))
    (should (eq requested-mode 'start))))

;;; cj/org-noter-start

(ert-deftest test-org-noter-start-in-document-with-session-toggles-notes ()
  "Normal: in a document with an active session, start toggles the notes window."
  (let ((toggled nil))
    (cl-letf (((symbol-function 'cj/org-noter--in-document-p)
               (lambda () t))
              ((symbol-function 'cj/org-noter--in-notes-file-p)
               (lambda () nil))
              ((symbol-function 'cj/org-noter--session-active-p)
               (lambda () t))
              ((symbol-function 'cj/org-noter--toggle-notes-window)
               (lambda () (setq toggled t))))
      (cj/org-noter-start))
    (should toggled)))

(ert-deftest test-org-noter-start-in-notes-file-with-session-switches-window ()
  "Normal: in a notes file with active session, switch to doc window."
  (let ((selected nil))
    (cl-letf (((symbol-function 'cj/org-noter--in-document-p)
               (lambda () nil))
              ((symbol-function 'cj/org-noter--in-notes-file-p)
               (lambda () t))
              ((symbol-function 'cj/org-noter--session-active-p)
               (lambda () t))
              ((symbol-function 'org-noter--get-doc-window)
               (lambda () 'doc-win))
              ((symbol-function 'select-window)
               (lambda (w &rest _) (setq selected w))))
      (cj/org-noter-start))
    (should (eq selected 'doc-win))))

(ert-deftest test-org-noter-start-elsewhere-messages ()
  "Boundary: outside any noter context, the function messages."
  (let ((msg nil))
    (cl-letf (((symbol-function 'cj/org-noter--in-document-p)
               (lambda () nil))
              ((symbol-function 'cj/org-noter--in-notes-file-p)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/org-noter-start))
    (should (string-match-p "Not in a document" msg))))

;;; cj/org-noter-insert-note-dwim

(ert-deftest test-org-noter-insert-note-dwim-inserts-when-session-active ()
  "Normal: with an active session, insert-note is called directly."
  (let ((inserted nil)
        (started nil))
    (cl-letf (((symbol-function 'cj/org-noter--session-active-p)
               (lambda () t))
              ((symbol-function 'cj/org-noter-start)
               (lambda () (setq started t)))
              ((symbol-function 'org-noter-insert-note)
               (lambda () (setq inserted t))))
      (cj/org-noter-insert-note-dwim))
    (should inserted)
    ;; Doesn't try to start a session when one's already active.
    (should-not started)))

(ert-deftest test-org-noter-insert-note-dwim-starts-then-inserts-when-no-session ()
  "Normal: without a session, the function starts one and then inserts."
  (let ((session-state nil)  ; flipped to t when start "succeeds"
        (inserted nil)
        (selected nil))
    (cl-letf (((symbol-function 'cj/org-noter--session-active-p)
               (lambda () session-state))
              ((symbol-function 'cj/org-noter-start)
               (lambda () (setq session-state t)))
              ((symbol-function 'org-noter--get-doc-window)
               (lambda () 'doc-win))
              ((symbol-function 'select-window)
               (lambda (w &rest _) (setq selected w)))
              ((symbol-function 'org-noter-insert-note)
               (lambda () (setq inserted t))))
      (cj/org-noter-insert-note-dwim))
    (should (eq selected 'doc-win))
    (should inserted)))

(provide 'test-org-noter-config-commands)
;;; test-org-noter-config-commands.el ends here
