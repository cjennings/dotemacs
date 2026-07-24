;;; test-help-utils--arch-wiki-search.el --- Tests for the ArchWiki search guard -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--arch-wiki-topics and the cj/local-arch-wiki-search command.
;;
;; The bug: the command read "/usr/share/doc/arch-wiki/html/en" with
;; `directory-files' before checking the directory existed.  On a machine
;; without arch-wiki-docs -- the exact state the command's own error text is
;; written for -- that signaled file-missing on the first line, so the friendly
;; "Is arch-wiki-docs installed?" message below it was unreachable.  The user
;; got a raw Lisp error naming a path instead of the install hint.
;;
;; Two things had to change to make this testable.  The directory was
;; hardcoded inside the command, so a test could only ever exercise whatever
;; the developer's own machine happened to have installed; it is now
;; `cj/arch-wiki-html-dir'.  And the directory read is now the pure helper
;; cj/--arch-wiki-topics, which takes a directory and returns an alist, so the
;; interesting cases are driven with real temporary directories instead of
;; mocking `directory-files'.
;;
;; Test organization:
;; - Normal Cases: topics found and returned; the command opens the choice
;; - Boundary Cases: empty dir, single topic, a name matching no topic
;; - Error Cases: missing dir returns nil and reports the install hint
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'help-utils)

(defmacro test-arch-wiki--with-topics (dir-var topics &rest body)
  "Bind DIR-VAR to a temp dir holding TOPICS (a list of html basenames).
The directory is removed after BODY."
  (declare (indent 2))
  `(let ((,dir-var (make-temp-file "arch-wiki-test" t)))
     (unwind-protect
         (progn
           (dolist (name ,topics)
             (write-region "" nil (expand-file-name (concat name ".html") ,dir-var)))
           ,@body)
       (delete-directory ,dir-var t))))

;;; Normal Cases — the pure helper

(ert-deftest test-help-utils-arch-wiki-topics-lists-html-basenames ()
  "Normal: each .html file becomes a (basename . fullpath) pair."
  (test-arch-wiki--with-topics dir '("Systemd" "Pacman")
    (let ((topics (cj/--arch-wiki-topics dir)))
      (should (equal '("Pacman" "Systemd") (sort (mapcar #'car topics) #'string<)))
      (should (string-suffix-p "Systemd.html" (cdr (assoc "Systemd" topics)))))))

(ert-deftest test-help-utils-arch-wiki-topics-ignores-non-html ()
  "Normal: files without the .html extension are not topics."
  (test-arch-wiki--with-topics dir '("Systemd")
    (write-region "" nil (expand-file-name "README.txt" dir))
    (should (equal '("Systemd") (mapcar #'car (cj/--arch-wiki-topics dir))))))

;;; Boundary Cases

(ert-deftest test-help-utils-arch-wiki-topics-empty-dir-is-nil ()
  "Boundary: an existing but empty directory yields no topics."
  (test-arch-wiki--with-topics dir '()
    (should (null (cj/--arch-wiki-topics dir)))))

(ert-deftest test-help-utils-arch-wiki-topics-single-topic ()
  "Boundary: one topic returns a one-element alist."
  (test-arch-wiki--with-topics dir '("Systemd")
    (should (= 1 (length (cj/--arch-wiki-topics dir))))))

;;; Error Cases — the missing-install path

(ert-deftest test-help-utils-arch-wiki-topics-missing-dir-returns-nil ()
  "Error: an absent directory returns nil rather than signaling file-missing."
  (let ((missing (expand-file-name "definitely-absent-arch-wiki"
                                   temporary-file-directory)))
    (should-not (file-directory-p missing))
    (should (null (cj/--arch-wiki-topics missing)))))

(ert-deftest test-help-utils-arch-wiki-search-missing-dir-reports-hint ()
  "Error: the command reports the install hint and opens nothing."
  (let ((cj/arch-wiki-html-dir (expand-file-name "definitely-absent-arch-wiki"
                                                 temporary-file-directory))
        (said nil)
        (opened nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)) nil))
              ((symbol-function 'eww-browse-url)
               (lambda (url &rest _) (setq opened url))))
      ;; Must not signal: this is the case that used to raise file-missing.
      (cj/local-arch-wiki-search))
    (should-not opened)
    (should (string-match-p "arch-wiki-docs" said))))

;;; Normal Cases — the command

(ert-deftest test-help-utils-arch-wiki-search-opens-chosen-topic ()
  "Normal: the chosen topic is opened as a file URL in EWW."
  (test-arch-wiki--with-topics dir '("Systemd")
    (let ((cj/arch-wiki-html-dir dir)
          (opened nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Systemd"))
                ((symbol-function 'eww-browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (cj/local-arch-wiki-search))
      (should (string-prefix-p "file://" opened))
      (should (string-suffix-p "Systemd.html" opened))
      ;; The opened path is the one in the temp dir, not a system copy.
      (should (string-match-p (regexp-quote dir) opened)))))

(ert-deftest test-help-utils-arch-wiki-search-unknown-topic-opens-nothing ()
  "Boundary: a name matching no topic reports rather than opening."
  (test-arch-wiki--with-topics dir '("Systemd")
    (let ((cj/arch-wiki-html-dir dir)
          (opened nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "NotATopic"))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'eww-browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (cj/local-arch-wiki-search))
      (should-not opened))))

(provide 'test-help-utils--arch-wiki-search)
;;; test-help-utils--arch-wiki-search.el ends here
