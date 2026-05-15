;;; test-custom-buffer-file-copy-buffer-source.el --- Tests for cj/copy-buffer-source-as-kill -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the dispatch behavior of `cj/copy-buffer-source-as-kill', the
;; major-mode-aware extension of the old
;; `cj/copy-path-to-buffer-file-as-kill' (which is kept as a
;; `defalias' for backwards compat).
;;
;; Dispatch tiers:
;;   eww-mode         -> (eww-current-url)
;;   elfeed-show-mode -> (elfeed-entry-link elfeed-show-entry)
;;   dired-mode       -> (dired-get-filename nil t)
;;   dirvish-mode     -> (dired-get-filename nil t)
;;   doc-view-mode    -> buffer-file-name
;;   pdf-view-mode    -> buffer-file-name
;;   anything else    -> buffer-file-name fallback (errors if nil)

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

;; Top-level defvars so let-bindings reach the dynamic var under
;; lexical scope (and stand-ins for vars defined in modes we don't
;; require here).
(defvar elfeed-show-entry nil)

;;; Fallback to buffer-file-name

(ert-deftest test-copy-buffer-source-falls-back-to-buffer-file-name ()
  "Normal: a file-visiting buffer with no special dispatch falls back
to `buffer-file-name'."
  (let (kill-ring)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/notes.txt")
      (fundamental-mode)
      (cl-letf (((symbol-function 'message) #'ignore))
        (cj/copy-buffer-source-as-kill))
      (should (equal (car kill-ring) "/tmp/notes.txt")))))

(ert-deftest test-copy-buffer-source-errors-on-nil-source ()
  "Error: a buffer with no file and no mode dispatch signals a user-error."
  (with-temp-buffer
    (fundamental-mode)
    (setq buffer-file-name nil)
    (should-error (cj/copy-buffer-source-as-kill) :type 'user-error)))

;;; eww-mode dispatch

(ert-deftest test-copy-buffer-source-eww-copies-current-url ()
  "Normal: in eww-mode, copy `(eww-current-url)'."
  (let (kill-ring)
    (cl-letf (((symbol-function 'eww-current-url)
               (lambda () "https://example.org/article")))
      (with-temp-buffer
        (setq major-mode 'eww-mode)
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "https://example.org/article"))))))

;;; elfeed-show-mode dispatch

(ert-deftest test-copy-buffer-source-elfeed-copies-entry-link ()
  "Normal: in elfeed-show-mode, copy the entry's link."
  (let ((kill-ring nil)
        (elfeed-show-entry (list :link "https://feed.test/post/42")))
    (cl-letf (((symbol-function 'elfeed-entry-link)
               (lambda (entry) (plist-get entry :link))))
      (with-temp-buffer
        (setq major-mode 'elfeed-show-mode)
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "https://feed.test/post/42"))))))

;;; dired-mode dispatch

(ert-deftest test-copy-buffer-source-dired-copies-file-at-point ()
  "Normal: in dired-mode, copy the absolute path of the file at point
(not the dired buffer's `default-directory')."
  (let (kill-ring)
    (cl-letf (((symbol-function 'dired-get-filename)
               (lambda (&optional _localp _no-error)
                 "/home/u/projects/notes.org")))
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "/home/u/projects/notes.org"))))))

(ert-deftest test-copy-buffer-source-dirvish-copies-file-at-point ()
  "Normal: dirvish-mode dispatches identically to dired-mode."
  (let (kill-ring)
    (cl-letf (((symbol-function 'dired-get-filename)
               (lambda (&optional _localp _no-error)
                 "/home/u/books/the-book.epub")))
      (with-temp-buffer
        (setq major-mode 'dirvish-mode)
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "/home/u/books/the-book.epub"))))))

;;; doc-view / pdf-view dispatch

(ert-deftest test-copy-buffer-source-doc-view-copies-buffer-file ()
  "Normal: doc-view-mode copies the underlying file path
(`buffer-file-name')."
  (let (kill-ring)
    (with-temp-buffer
      (setq buffer-file-name "/home/u/papers/paper.pdf"
            major-mode 'doc-view-mode)
      (cl-letf (((symbol-function 'message) #'ignore))
        (cj/copy-buffer-source-as-kill))
      (should (equal (car kill-ring) "/home/u/papers/paper.pdf")))))

(ert-deftest test-copy-buffer-source-pdf-view-copies-buffer-file ()
  "Normal: pdf-view-mode copies the underlying file path."
  (let (kill-ring)
    (with-temp-buffer
      (setq buffer-file-name "/home/u/books/manual.pdf"
            major-mode 'pdf-view-mode)
      (cl-letf (((symbol-function 'message) #'ignore))
        (cj/copy-buffer-source-as-kill))
      (should (equal (car kill-ring) "/home/u/books/manual.pdf")))))

;;; mu4e-view-mode dispatch

(ert-deftest test-copy-buffer-source-mu4e-view-copies-msgid-link ()
  "Normal: in mu4e-view-mode, copy a `mu4e:msgid:<id>' link form.
The URL-shaped string pastes into org as a clickable link and
matches mu4e's own org-protocol handler."
  (let (kill-ring)
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () (list :message-id "abc123@example.test"
                                :subject "Re: lunch"))))
      (with-temp-buffer
        (setq major-mode 'mu4e-view-mode)
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "mu4e:msgid:abc123@example.test"))))))

(ert-deftest test-copy-buffer-source-mu4e-view-without-message-falls-through ()
  "Boundary: when `mu4e-message-at-point' returns nil (e.g. point
isn't on a real message), the dispatcher falls back to
`buffer-file-name' rather than erroring."
  (let (kill-ring)
    (cl-letf (((symbol-function 'mu4e-message-at-point) (lambda () nil)))
      (with-temp-buffer
        (setq major-mode 'mu4e-view-mode
              buffer-file-name "/tmp/fallback.eml")
        (cl-letf (((symbol-function 'message) #'ignore))
          (cj/copy-buffer-source-as-kill))
        (should (equal (car kill-ring) "/tmp/fallback.eml"))))))

;;; Info-mode dispatch

(ert-deftest test-copy-buffer-source-info-mode-formats-as-org-info-link ()
  "Normal: in Info-mode on a compressed manual, return the org
bracket link form `[[info:(manual)node][(manual) node]]'.  Pasting
into notes lands a labeled, clickable link rather than a bare
target string."
  (let (kill-ring)
    (with-temp-buffer
      (setq major-mode 'Info-mode)
      (setq-local Info-current-file "/usr/share/info/emacs.info.gz")
      (setq-local Info-current-node "Buffers")
      (cl-letf (((symbol-function 'message) #'ignore))
        (cj/copy-buffer-source-as-kill))
      (should (equal (car kill-ring)
                     "[[info:(emacs)Buffers][(emacs) Buffers]]")))))

(ert-deftest test-copy-buffer-source-info-mode-handles-uncompressed-info-file ()
  "Boundary: uncompressed `.info' files still strip the suffix and
emit the same bracket link form.  Guards the `string-suffix-p
\".info\"' branch in the dispatcher."
  (let (kill-ring)
    (with-temp-buffer
      (setq major-mode 'Info-mode)
      (setq-local Info-current-file "/usr/local/share/info/elisp.info")
      (setq-local Info-current-node "Functions")
      (cl-letf (((symbol-function 'message) #'ignore))
        (cj/copy-buffer-source-as-kill))
      (should (equal (car kill-ring)
                     "[[info:(elisp)Functions][(elisp) Functions]]")))))

(ert-deftest test-copy-buffer-source-info-mode-without-context-falls-through ()
  "Boundary: when Info hasn't populated `Info-current-file' or
`Info-current-node' (uninitialized), the dispatcher falls through
to `buffer-file-name' (here nil → user-error)."
  (with-temp-buffer
    (setq major-mode 'Info-mode)
    (setq-local Info-current-file nil)
    (setq-local Info-current-node nil)
    (should-error (cj/copy-buffer-source-as-kill) :type 'user-error)))

;;; Backwards-compat alias

(ert-deftest test-copy-path-old-name-aliases-new-command ()
  "Backwards compat: `cj/copy-path-to-buffer-file-as-kill' still
resolves to the new `cj/copy-buffer-source-as-kill' via defalias."
  (should (eq (symbol-function 'cj/copy-path-to-buffer-file-as-kill)
              'cj/copy-buffer-source-as-kill)))

;;; Keybinding

(ert-deftest test-copy-buffer-source-bound-on-b-p ()
  "Normal: `p' in `cj/buffer-and-file-map' invokes the new command."
  (should (eq (keymap-lookup cj/buffer-and-file-map "p")
              #'cj/copy-buffer-source-as-kill)))

(provide 'test-custom-buffer-file-copy-buffer-source)
;;; test-custom-buffer-file-copy-buffer-source.el ends here
