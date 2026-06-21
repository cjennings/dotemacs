;;; test-dirvish-config-print.el --- Tests for the dirvish print command -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/dirvish-print-file' (bound to `P' in `dirvish-mode-map') sends the
;; file at point to the default printer via CUPS (`lp', falling back to
;; `lpr').  It refuses directories and file types outside
;; `cj/dirvish-print-extensions'.  These tests cover the pure predicates
;; (`cj/--printable-file-p', `cj/--print-program') directly and the command
;; with `dired-get-filename', `y-or-n-p', and `call-process' mocked.

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

;;; --------------------------- cj/--printable-file-p --------------------------

(ert-deftest test-dirvish-print-printable-file-p-known-extensions ()
  "Normal: PDF, text, and org files are printable."
  (should (cj/--printable-file-p "/tmp/report.pdf"))
  (should (cj/--printable-file-p "/tmp/notes.txt"))
  (should (cj/--printable-file-p "/tmp/agenda.org")))

(ert-deftest test-dirvish-print-printable-file-p-case-insensitive ()
  "Boundary: extension matching ignores case."
  (should (cj/--printable-file-p "/tmp/REPORT.PDF"))
  (should (cj/--printable-file-p "/tmp/Notes.Txt")))

(ert-deftest test-dirvish-print-printable-file-p-no-extension ()
  "Boundary: a file with no extension is not printable."
  (should-not (cj/--printable-file-p "/tmp/README"))
  (should-not (cj/--printable-file-p "/home/cj/.bashrc")))

(ert-deftest test-dirvish-print-printable-file-p-non-printable-extension ()
  "Error: a video or archive is not printable."
  (should-not (cj/--printable-file-p "/tmp/clip.mp4"))
  (should-not (cj/--printable-file-p "/tmp/archive.tar.gz")))

;;; ----------------------------- cj/--print-program ---------------------------

(ert-deftest test-dirvish-print-program-prefers-lp ()
  "Normal: `lp' is used when available."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (when (equal cmd "lp") "/usr/bin/lp"))))
    (should (equal (cj/--print-program) "/usr/bin/lp"))))

(ert-deftest test-dirvish-print-program-falls-back-to-lpr ()
  "Boundary: `lpr' is used when `lp' is missing."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (when (equal cmd "lpr") "/usr/bin/lpr"))))
    (should (equal (cj/--print-program) "/usr/bin/lpr"))))

(ert-deftest test-dirvish-print-program-none-available ()
  "Error: nil when neither `lp' nor `lpr' is on PATH."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd &rest _) nil)))
    (should-not (cj/--print-program))))

;;; ---------------------------- cj/dirvish-print-file -------------------------

(defmacro test-dirvish-print--with-file-at-point (file &rest body)
  "Run BODY with `dired-get-filename' returning FILE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'dired-get-filename) (lambda (&rest _) ,file)))
     ,@body))

(ert-deftest test-dirvish-print-file-sends-printable-file-to-printer ()
  "Normal: confirming the prompt runs the print program on the file."
  (let (called)
    (test-dirvish-print--with-file-at-point "/tmp/report.pdf"
      (cl-letf (((symbol-function 'cj/--print-program) (lambda () "/usr/bin/lp"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'call-process)
                 (lambda (program _infile _dest _display &rest args)
                   (setq called (cons program args))
                   0))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (cj/dirvish-print-file)
        (should (equal called '("/usr/bin/lp" "/tmp/report.pdf")))))))

(ert-deftest test-dirvish-print-file-declining-the-prompt-prints-nothing ()
  "Boundary: answering no to the prompt does not invoke the print program."
  (let (called)
    (test-dirvish-print--with-file-at-point "/tmp/report.pdf"
      (cl-letf (((symbol-function 'cj/--print-program) (lambda () "/usr/bin/lp"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (setq called t) 0)))
        (cj/dirvish-print-file)
        (should-not called)))))

(ert-deftest test-dirvish-print-file-errors-without-file ()
  "Error: no file at point fails clearly."
  (test-dirvish-print--with-file-at-point nil
    (should-error (cj/dirvish-print-file) :type 'user-error)))

(ert-deftest test-dirvish-print-file-errors-on-directory ()
  "Error: a directory is not printable."
  (test-dirvish-print--with-file-at-point temporary-file-directory
    (should-error (cj/dirvish-print-file) :type 'user-error)))

(ert-deftest test-dirvish-print-file-errors-on-non-printable-type ()
  "Error: a non-printable file type is refused before any prompt."
  (test-dirvish-print--with-file-at-point "/tmp/clip.mp4"
    (should-error (cj/dirvish-print-file) :type 'user-error)))

(ert-deftest test-dirvish-print-file-errors-without-print-program ()
  "Error: no `lp'/`lpr' on PATH fails clearly."
  (test-dirvish-print--with-file-at-point "/tmp/report.pdf"
    (cl-letf (((symbol-function 'cj/--print-program) (lambda () nil)))
      (should-error (cj/dirvish-print-file) :type 'user-error))))

(ert-deftest test-dirvish-print-file-errors-when-print-command-fails ()
  "Error: a non-zero print exit code surfaces as a `user-error'."
  (test-dirvish-print--with-file-at-point "/tmp/report.pdf"
    (cl-letf (((symbol-function 'cj/--print-program) (lambda () "/usr/bin/lp"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'call-process) (lambda (&rest _) 1)))
      (should-error (cj/dirvish-print-file) :type 'user-error))))

(ert-deftest test-dirvish-print-file-bound-to-uppercase-p ()
  "Normal: `P' in `dirvish-mode-map' runs the print command."
  (should (eq (keymap-lookup dirvish-mode-map "P") #'cj/dirvish-print-file)))

(provide 'test-dirvish-config-print)
;;; test-dirvish-config-print.el ends here
