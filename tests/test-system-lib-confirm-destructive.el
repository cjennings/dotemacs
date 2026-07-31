;;; test-system-lib-confirm-destructive.el --- Tests for cj/confirm-destructive -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `cj/confirm-destructive', the confirmation used for
;; irreversible actions: file destruction, overwrites, power-off.
;;
;; The contract has two halves, and they pull against each other:
;;
;; 1. One keystroke.  This replaced a typed-"yes" prompt on 2026-07-31, after
;;    one became unanswerable -- a second agent session held the selected
;;    window while the prompt waited in another frame, so keystrokes went to a
;;    terminal and the Emacs session had to be killed with buffers unsaved.  A
;;    long-form prompt is only as safe as it is answerable.
;;
;; 2. No default.  Only y and n answer.  A stray RET or space re-prompts
;;    instead of confirming, which is the accidental-confirm protection the
;;    long form existed for, and it survives the change.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'system-lib)

;;; Normal Cases

(ert-deftest test-system-lib-confirm-destructive-returns-t-on-y ()
  "Normal: a y answer confirms."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
    (should (eq (cj/confirm-destructive "Really? ") t))))

(ert-deftest test-system-lib-confirm-destructive-returns-nil-on-n ()
  "Normal: an n answer declines."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
    (should (eq (cj/confirm-destructive "Really? ") nil))))

;;; Boundary Cases

(ert-deftest test-system-lib-confirm-destructive-accepts-uppercase ()
  "Boundary: a capital Y confirms and a capital N declines, so the answer
does not depend on the shift key or caps lock."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?Y)))
    (should (eq (cj/confirm-destructive "Really? ") t)))
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?N)))
    (should (eq (cj/confirm-destructive "Really? ") nil))))

(ert-deftest test-system-lib-confirm-destructive-offers-only-y-and-n ()
  "Boundary: RET, space and every other key are refused.

This is the protection that justified the old typed-\"yes\" form, kept now
that the answer is a single keystroke.  Asserted on the character set handed
to `read-char-choice', which is what actually decides, rather than on the
prompt text, which only describes."
  (let (chars)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt cs &rest _) (setq chars cs) ?n)))
      (cj/confirm-destructive "Really? "))
    (should (equal (sort (copy-sequence chars) #'<) '(?N ?Y ?n ?y)))
    (should-not (memq ?\r chars))
    (should-not (memq ?\n chars))
    (should-not (memq ?\s chars))))

(ert-deftest test-system-lib-confirm-destructive-is-single-keystroke ()
  "Boundary: it never routes through `yes-or-no-p'.

The regression this file guards.  Binding `use-short-answers' to nil for the
call, which is what the old implementation did, is exactly the shape that
produced an unanswerable prompt."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
            ((symbol-function 'yes-or-no-p)
             (lambda (&rest _) (error "must not demand a typed yes"))))
    (should (eq (cj/confirm-destructive "Really? ") t))))

(ert-deftest test-system-lib-confirm-destructive-ignores-short-answer-setting ()
  "Boundary: the answer is one key whether or not `use-short-answers' is set.
The global default is t, but nothing about this prompt should depend on it."
  (dolist (setting '(t nil))
    (let ((use-short-answers setting))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "must not demand a typed yes"))))
        (should (eq (cj/confirm-destructive "Really? ") t))))))

(ert-deftest test-system-lib-confirm-destructive-discards-type-ahead ()
  "Boundary: pending input is discarded before the key is read.

The regression that made this necessary.  `read-char-choice' reads the input
queue, so a key typed before the prompt appeared would answer it: a queued y
would confirm a shutdown or a file deletion instantly.  The typed-\"yes\"
form absorbed such a key harmlessly, so dropping it without this guard would
have traded a rare annoyance for a rare catastrophe.

Asserted on ordering, since that is the whole property: the discard has to
happen before the read, not merely somewhere in the function."
  (let ((events '()))
    (cl-letf (((symbol-function 'discard-input)
               (lambda (&rest _) (push 'discard events) nil))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (push 'read events) ?y)))
      (should (eq (cj/confirm-destructive "Really? ") t)))
    (should (equal (nreverse events) '(discard read)))))

;;; Error Cases

(ert-deftest test-system-lib-confirm-destructive-declines-on-other-char ()
  "Error: anything that is not y or n declines rather than confirming.

`read-char-choice' normally loops until it gets a listed character, but it
can return outside the set -- `read-char-from-minibuffer' yields RET when the
minibuffer result is empty.  On an irreversible action the safe reading of an
unexpected answer is no."
  (dolist (ch (list ?\r ?\n ?\s ?q ?\C-m))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ch)))
      (should-not (cj/confirm-destructive "Really? ")))))

(ert-deftest test-system-lib-confirm-destructive-propagates-quit ()
  "Error: C-g aborts the caller rather than reading as a confirmation.
Swallowing the quit here would turn an abort into a yes on an irreversible
action."
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (&rest _) (signal 'quit nil))))
    ;; `should-error' cannot express this: quit is not an error, so ERT lets
    ;; it through and reports the test as QUIT rather than passed.  Catching
    ;; it here is what makes the assertion a real pass or fail.
    (should (eq 'quit (condition-case nil
                          (progn (cj/confirm-destructive "Really? ")
                                 'returned-normally)
                        (quit 'quit))))))

(ert-deftest test-system-lib-confirm-destructive-prompt-shows-choices ()
  "Error: the prompt names the keys that answer, so an unfamiliar prompt is
not a guessing game."
  (let (prompt)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (p &rest _) (setq prompt p) ?n)))
      (cj/confirm-destructive "Delete everything? "))
    (should (string-match-p "Delete everything\\?" prompt))
    (should (string-match-p "y or n" prompt))))

(provide 'test-system-lib-confirm-destructive)
;;; test-system-lib-confirm-destructive.el ends here
