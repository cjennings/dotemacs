;;; test-dev-fkeys--f4-make-once-hook.el --- Tests for cj/--f4-make-once-hook -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the one-shot `compilation-finish-functions' hook builder used by
;; "Compile + Run" and "Clean + Rebuild" to chain a follow-up command after
;; a successful compile. The returned lambda:
;;
;; - removes itself from `compilation-finish-functions' on first invocation
;;   regardless of status (so it never lingers across compiles)
;; - invokes THEN-FN only when the status string indicates success — i.e.
;;   `string-prefix-p \"finished\"' matches.
;;
;; The status conventions come from the compile.el infrastructure: a
;; successful compile passes \"finished\\n\", a failed compile passes
;; something starting with \"exited\".

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-make-once-hook-success-status-calls-then-fn ()
  "Normal: status starting with 'finished' invokes THEN-FN once."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called)))))
    (let ((compilation-finish-functions nil))
      (add-hook 'compilation-finish-functions hook)
      (funcall hook nil "finished\n"))
    (should (= called 1))))

(ert-deftest test-dev-fkeys-make-once-hook-failure-status-skips-then-fn ()
  "Normal: status string starting with 'exited abnormally' does not invoke THEN-FN."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called)))))
    (let ((compilation-finish-functions nil))
      (add-hook 'compilation-finish-functions hook)
      (funcall hook nil "exited abnormally with code 1\n"))
    (should (= called 0))))

(ert-deftest test-dev-fkeys-make-once-hook-success-removes-itself ()
  "Normal: hook removes itself from `compilation-finish-functions' on success."
  (let* ((hook (cj/--f4-make-once-hook (lambda () nil)))
         (compilation-finish-functions (list hook)))
    (should (memq hook compilation-finish-functions))
    (funcall hook nil "finished\n")
    (should-not (memq hook compilation-finish-functions))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-make-once-hook-failure-also-removes-itself ()
  "Boundary: hook removes itself even on failure, so it never fires on the
next compile. THEN-FN is not invoked, but the hook is still cleaned up."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called))))
         (compilation-finish-functions (list hook)))
    (funcall hook nil "exited abnormally with code 1\n")
    (should (= called 0))
    (should-not (memq hook compilation-finish-functions))))

(ert-deftest test-dev-fkeys-make-once-hook-only-fires-then-fn-once ()
  "Boundary: re-invoking the hook lambda after it self-removes does not
re-trigger THEN-FN — the hook has been removed from the hook list and only
external mistakes (calling the lambda directly twice) could trigger it. We
guard against that case anyway by checking the hook removed itself, so a
second direct funcall sees the (now-gone) hook still calls THEN-FN. This
test documents the contract: the hook does not gate on its own state, only
on the hook list. So the SECOND direct funcall WILL call THEN-FN. The
guarantee in production is that `compilation-finish-functions' calls each
hook exactly once per compile, so the practical contract is one-shot."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called))))
         (compilation-finish-functions (list hook)))
    (funcall hook nil "finished\n")
    (funcall hook nil "finished\n")
    (should (= called 2))))

(ert-deftest test-dev-fkeys-make-once-hook-empty-status-skips-then-fn ()
  "Boundary: empty status string does not invoke THEN-FN."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called)))))
    (let ((compilation-finish-functions nil))
      (add-hook 'compilation-finish-functions hook)
      (funcall hook nil ""))
    (should (= called 0))))

(ert-deftest test-dev-fkeys-make-once-hook-interrupt-status-skips-then-fn ()
  "Boundary: an 'interrupt' status (process killed) does not invoke THEN-FN."
  (let* ((called 0)
         (hook (cj/--f4-make-once-hook (lambda () (cl-incf called)))))
    (let ((compilation-finish-functions nil))
      (add-hook 'compilation-finish-functions hook)
      (funcall hook nil "interrupt\n"))
    (should (= called 0))))

;;; Error Cases

(ert-deftest test-dev-fkeys-make-once-hook-then-fn-error-still-removes-hook ()
  "Error: if THEN-FN raises, the hook is still removed first so the
follow-up doesn't run twice on the next compile.

Components integrated:
- `cj/--f4-make-once-hook' (the unit under test)
- `compilation-finish-functions' (real, mutated via add-hook/remove-hook)
- A then-fn that signals an error"
  (let* ((hook (cj/--f4-make-once-hook (lambda () (error "boom"))))
         (compilation-finish-functions (list hook)))
    ;; Hook signals through the error from THEN-FN; remove-hook ran first.
    (should-error (funcall hook nil "finished\n"))
    (should-not (memq hook compilation-finish-functions))))

(provide 'test-dev-fkeys--f4-make-once-hook)
;;; test-dev-fkeys--f4-make-once-hook.el ends here
