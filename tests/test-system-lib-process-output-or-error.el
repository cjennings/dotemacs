;;; test-system-lib-process-output-or-error.el --- Tests for cj/process-output-or-error and cj/git-output-or-error -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/process-output-or-error' is the generic argv-based runner: take
;; a program name and its argument list, run it via `process-file', and
;; return stdout on success or signal a clear `user-error' on non-zero
;; exit (with program/status/output in the message).
;;
;; `cj/git-output-or-error' is the thin wrapper that supplies "git" as
;; the program.  Both helpers stay deterministic in tests by stubbing
;; `process-file'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-lib)

(defmacro test-system-lib-runner--with-stub (status output &rest body)
  "Run BODY with `process-file' stubbed to return STATUS and write OUTPUT."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-file)
              (lambda (_program _infile destination _display &rest _args)
                (when destination
                  (let ((buf (cond
                              ((eq destination t) (current-buffer))
                              ((bufferp destination) destination)
                              ((consp destination) (car destination)))))
                    (when (bufferp buf)
                      (with-current-buffer buf
                        (insert ,output)))))
                ,status)))
     ,@body))

(ert-deftest test-cj-process-output-or-error-success-returns-stdout ()
  "Normal: zero exit returns stdout."
  (test-system-lib-runner--with-stub 0 "hello world\n"
    (should (equal (cj/process-output-or-error "echo" "hello" "world")
                   "hello world\n"))))

(ert-deftest test-cj-process-output-or-error-no-args-runs ()
  "Boundary: no args is a valid invocation."
  (test-system-lib-runner--with-stub 0 "ok\n"
    (should (equal (cj/process-output-or-error "true") "ok\n"))))

(ert-deftest test-cj-process-output-or-error-non-zero-signals-user-error ()
  "Error: non-zero exit signals user-error."
  (test-system-lib-runner--with-stub 1 "boom\n"
    (should-error (cj/process-output-or-error "false")
                  :type 'user-error)))

(ert-deftest test-cj-process-output-or-error-message-names-program-and-status ()
  "Error: the user-error message names the program, the exit status,
and the (trimmed) output so the user can see what went wrong."
  (test-system-lib-runner--with-stub 128 "fatal: not a git repo\n"
    (condition-case err
        (cj/process-output-or-error "git" "status")
      (user-error
       (let ((message (error-message-string err)))
         (should (string-match-p "git" message))
         (should (string-match-p "128" message))
         (should (string-match-p "fatal: not a git repo" message)))))))

(ert-deftest test-cj-git-output-or-error-uses-git-program ()
  "Normal: git wrapper passes its args through with `git' as the program."
  (let (captured-program captured-args)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile destination _display &rest args)
                 (setq captured-program program
                       captured-args args)
                 (when destination
                   (let ((buf (cond ((eq destination t) (current-buffer))
                                    ((bufferp destination) destination)
                                    ((consp destination) (car destination)))))
                     (when (bufferp buf)
                       (with-current-buffer buf (insert "abcdef\n")))))
                 0)))
      (should (equal (cj/git-output-or-error "rev-parse" "HEAD") "abcdef\n"))
      (should (equal captured-program "git"))
      (should (equal captured-args '("rev-parse" "HEAD"))))))

(ert-deftest test-cj-git-output-or-error-non-zero-signals-user-error ()
  "Error: git wrapper raises the same user-error shape on failure."
  (test-system-lib-runner--with-stub 1 "fatal: bad ref\n"
    (should-error (cj/git-output-or-error "rev-parse" "bogus")
                  :type 'user-error)))

(provide 'test-system-lib-process-output-or-error)
;;; test-system-lib-process-output-or-error.el ends here
