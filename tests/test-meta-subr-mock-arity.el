;;; test-meta-subr-mock-arity.el --- Guard against arity-narrow subr mocks -*- lexical-binding: t; -*-

;;; Commentary:
;; A meta-test: it tests the other tests.  Native compilation routes a
;; redefined C primitive (subr) through a trampoline that calls the
;; replacement with the primitive's FULL arity, filling optionals with nil.
;; So a fixed-arity mock that is narrower than the primitive throws
;; `wrong-number-of-arguments' the moment native-comp has compiled that
;; trampoline -- a failure that appears intermittently as the eln-cache fills.
;;
;; The rule this enforces is NOT "never mock a subr" (the suite mocks subrs
;; like `message' and `completing-read' hundreds of times, all fine).  It is:
;; a mock of a C primitive must be able to accept the primitive's maximum
;; arity -- in practice, use (lambda (&rest _) ...).  This test scans every
;; file under tests/ for `cl-letf' / `setf' / `fset' redefinitions of a
;; `symbol-function', and fails listing any whose replacement is too narrow.
;;
;; It is deterministic: a pure static read of the test sources plus
;; `func-arity', with no dependence on whether native-comp happens to have
;; built the trampoline yet.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(defconst test-meta-subr--test-dir
  (expand-file-name "tests" (or (getenv "EMACS_CONFIG_ROOT") default-directory))
  "Directory whose .el files are scanned for subr mocks.")

(defun test-meta-subr--replacement-arglist (repl)
  "Return the formal arglist of REPL, or the symbol `unknown'.
Handles (lambda ARGS ...) and (function (lambda ARGS ...)); returns `variadic'
for forms known to accept any arity (`ignore', `always'), and `unknown' for
anything whose arity can't be read statically (a bare variable, a call)."
  (pcase repl
    (`(lambda ,args . ,_) args)
    (`(function (lambda ,args . ,_)) args)
    (`(quote ,(or 'ignore 'always)) 'variadic)
    (`(function ,(or 'ignore 'always)) 'variadic)
    (_ 'unknown)))

(defun test-meta-subr--accepts-p (arglist subr-max)
  "Non-nil if a lambda with ARGLIST can be called with SUBR-MAX positional args.
ARGLIST may also be `variadic' or `unknown' (both treated as acceptable)."
  (cond
   ((memq arglist '(variadic unknown)) t)
   ((memq '&rest arglist) t)
   ((eq subr-max 'many) nil)            ; only &rest accepts unbounded arity
   ((integerp subr-max)
    (>= (length (seq-remove (lambda (s) (memq s '(&optional &rest &key)))
                            arglist))
        subr-max))
   (t t)))

(defun test-meta-subr--quoted-symbol (form)
  "If FORM is 'SYM or #'SYM, return SYM, else nil."
  (pcase form
    (`(quote ,(and s (guard (symbolp s)))) s)
    (`(function ,(and s (guard (symbolp s)))) s)))

(defun test-meta-subr--collect (form acc)
  "Walk FORM, pushing (SYM . REPLACEMENT) for each symbol-function redefinition.
Covers `cl-letf'/`setf' binding shape ((symbol-function 'SYM) REPL) and
\(fset 'SYM REPL)."
  (when (consp form)
    ;; (fset 'SYM REPL)
    (when (eq (car-safe form) 'fset)
      (let ((s (test-meta-subr--quoted-symbol (nth 1 form))))
        (when s (push (cons s (nth 2 form)) acc))))
    ;; binding element ((symbol-function 'SYM) REPL) -- cl-letf, cl-letf*, setf
    (when (and (consp (car-safe form))
               (eq (car-safe (car form)) 'symbol-function))
      (let ((s (test-meta-subr--quoted-symbol (nth 1 (car form)))))
        (when s (push (cons s (nth 1 form)) acc))))
    (dolist (sub form) (setq acc (test-meta-subr--collect sub acc))))
  acc)

(defun test-meta-subr--violations ()
  "Return a list of human-readable violation strings across the test files."
  (let ((violations '()))
    (dolist (file (directory-files-recursively test-meta-subr--test-dir "\\.el\\'"))
      ;; Don't scan this meta-test itself (its examples would self-trip).
      (unless (string-suffix-p "test-meta-subr-mock-arity.el" file)
        (let ((mocks '()))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (condition-case nil
                (while t (setq mocks (test-meta-subr--collect (read (current-buffer)) mocks)))
              (error nil)))
          (pcase-dolist (`(,sym . ,repl) (nreverse mocks))
            (when (and (fboundp sym)
                       (condition-case nil (subrp (symbol-function sym)) (error nil)))
              (let ((subr-max (cdr (func-arity sym)))
                    (arglist (test-meta-subr--replacement-arglist repl)))
                (unless (test-meta-subr--accepts-p arglist subr-max)
                  (push (format "%s: mock of subr `%s' (arity max %s) takes %S -- use (&rest _)"
                                (file-name-nondirectory file) sym subr-max arglist)
                        violations))))))))
    (nreverse violations)))

(ert-deftest test-meta-no-arity-narrow-subr-mocks ()
  "No test mocks a C primitive with a lambda too narrow for its arity.
Such a mock breaks under native-comp's subr trampoline (it calls the mock with
the primitive's full arity).  Fix by making the mock variadic: (lambda (&rest _)
...).  See this file's commentary."
  (let ((violations (test-meta-subr--violations)))
    (should (null violations))))

(provide 'test-meta-subr-mock-arity)
;;; test-meta-subr-mock-arity.el ends here
