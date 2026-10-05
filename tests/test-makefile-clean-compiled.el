;;; test-makefile-clean-compiled.el --- Tests for the clean-compiled make target -*- lexical-binding: t -*-

;;; Commentary:
;; `make clean-compiled' (and `make clean', which depends on it) removes the
;; config's own compiled output.  It must leave elpa/ alone: package.el owns
;; that bytecode, nothing in this repo regenerates it, and without it Emacs
;; loads every package from source -- slower, no native-comp JIT, and on
;; Emacs 31 a "Missing `lexical-binding' cookie" warning per cookie-less file.
;;
;; Each test runs the real target against a throwaway EMACS_HOME.  Before the
;; real run, a dry run (make -n) confirms the override reached the recipe, so a
;; broken override can never sweep the developer's live tree.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst test-makefile-cc--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root holding the Makefile under test.")

(defun test-makefile-cc--touch (root rel)
  "Create an empty file REL under ROOT, making parent dirs."
  (let ((path (expand-file-name rel root)))
    (make-directory (file-name-directory path) t)
    (write-region "" nil path)
    path))

(defun test-makefile-cc--make (home &optional dry-run)
  "Run the clean-compiled target with EMACS_HOME=HOME.
With DRY-RUN, pass -n.  Return (EXIT . OUTPUT)."
  (with-temp-buffer
    (let* ((process-environment
            (cl-remove-if (lambda (e) (string-match-p "\\`MAKE\\(FLAGS\\|LEVEL\\)=" e))
                          process-environment))
           (args (append (and dry-run '("-n"))
                         (list "--no-print-directory"
                               "-C" test-makefile-cc--repo "clean-compiled"
                               (concat "EMACS_HOME=" (directory-file-name home)))))
           (exit (apply #'call-process "make" nil t nil args)))
      (cons exit (buffer-string)))))

(defmacro test-makefile-cc--with-home (files &rest body)
  "Create a temp EMACS_HOME holding FILES, bind it to HOME, run BODY.
Refuses to run the real target unless a dry run shows the override applied."
  (declare (indent 1))
  `(let ((home (file-name-as-directory (make-temp-file "test-makefile-cc-" t))))
     (unwind-protect
         (progn
           (dolist (f ,files) (test-makefile-cc--touch home f))
           (let ((dry (test-makefile-cc--make home t)))
             (should (= 0 (car dry)))
             (should (string-match-p (regexp-quote (directory-file-name home))
                                     (cdr dry)))
             (should-not (string-match-p
                          (regexp-quote (expand-file-name "~/.emacs.d"))
                          (cdr dry))))
           ,@body)
       (delete-directory home t))))

(defun test-makefile-cc--exists (home rel)
  "Return non-nil when REL exists under HOME."
  (file-exists-p (expand-file-name rel home)))

(ert-deftest test-makefile-clean-compiled-keeps-elpa-bytecode ()
  "Normal: package bytecode under elpa/ survives; the config's own is removed."
  (test-makefile-cc--with-home '("modules/foo.elc"
                                 "init.elc"
                                 "elpa/pkg-1.0/pkg.el"
                                 "elpa/pkg-1.0/pkg.elc"
                                 "elpa/pkg-1.0/sub/inner.elc")
    (should (= 0 (car (test-makefile-cc--make home))))
    (should (test-makefile-cc--exists home "elpa/pkg-1.0/pkg.elc"))
    (should (test-makefile-cc--exists home "elpa/pkg-1.0/sub/inner.elc"))
    (should (test-makefile-cc--exists home "elpa/pkg-1.0/pkg.el"))
    (should-not (test-makefile-cc--exists home "modules/foo.elc"))
    (should-not (test-makefile-cc--exists home "init.elc"))))

(ert-deftest test-makefile-clean-compiled-still-removes-native-code ()
  "Normal: .eln outside elpa/ is still removed, as before."
  (test-makefile-cc--with-home '("eln-cache/31.1-abc/foo.eln"
                                 "modules/foo.elc")
    (should (= 0 (car (test-makefile-cc--make home))))
    (should-not (test-makefile-cc--exists home "eln-cache/31.1-abc/foo.eln"))
    (should-not (test-makefile-cc--exists home "modules/foo.elc"))))

(ert-deftest test-makefile-clean-compiled-no-elpa-dir ()
  "Boundary: a home with no elpa/ at all still cleans and succeeds."
  (test-makefile-cc--with-home '("modules/foo.elc")
    (should (= 0 (car (test-makefile-cc--make home))))
    (should-not (test-makefile-cc--exists home "modules/foo.elc"))))

(ert-deftest test-makefile-clean-compiled-only-top-level-elpa-spared ()
  "Boundary: only the top-level elpa/ is spared, not look-alike paths."
  (test-makefile-cc--with-home '(".elpa-mirror/x.elc"
                                 "tests/fixtures/elpa/y.elc"
                                 "elpa/pkg-1.0/pkg.elc")
    (should (= 0 (car (test-makefile-cc--make home))))
    (should-not (test-makefile-cc--exists home ".elpa-mirror/x.elc"))
    (should-not (test-makefile-cc--exists home "tests/fixtures/elpa/y.elc"))
    (should (test-makefile-cc--exists home "elpa/pkg-1.0/pkg.elc"))))

(ert-deftest test-makefile-clean-compiled-missing-home-fails ()
  "Error: a nonexistent EMACS_HOME makes the target fail, not silently pass."
  (let* ((parent (make-temp-file "test-makefile-cc-missing-" t))
         (home (expand-file-name "absent/" parent)))
    (unwind-protect
        (progn
          (should-not (file-exists-p home))
          (let ((dry (cdr (test-makefile-cc--make home t))))
            (should (string-match-p (regexp-quote (directory-file-name home)) dry))
            (should-not (string-match-p
                         (regexp-quote (expand-file-name "~/.emacs.d")) dry)))
          (should-not (= 0 (car (test-makefile-cc--make home)))))
      (delete-directory parent t))))

(provide 'test-makefile-clean-compiled)
;;; test-makefile-clean-compiled.el ends here
