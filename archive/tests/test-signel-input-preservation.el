;;; test-signel-input-preservation.el --- Regression for signel #2 input clobber -*- lexical-binding: t; -*-

;;; Commentary:
;; signel-chat-mode buffers have an editable prompt area starting at
;; `signel--input-marker'.  Before this fix, both `signel--insert-msg' (the
;; receive path) and `signel--insert-system-msg' (the RPC-error path)
;; called `(delete-region (point) (point-max))' to clear the old prompt
;; before redrawing it, which destroyed any text the user was mid-typing.
;;
;; These tests lock the preservation contract: a small `signel--pending-input'
;; helper captures the in-progress input from the marker to `point-max', and
;; both inserters restore it after the freshly drawn prompt.  The chat-mode
;; buffer is constructed in a temp buffer; `signel--insert-msg' is steered
;; to it via a stub on `signel--get-buffer'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(defmacro test-signel--with-chat-buffer (&rest body)
  "Set up a temp signel-chat-mode buffer with prompt drawn and run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (signel-chat-mode)
     (setq signel--chat-id "+15555550100")
     (signel--draw-prompt)
     ,@body))

(ert-deftest test-signel-input-pending-returns-typed-text ()
  "Normal: with text after the prompt marker, `signel--pending-input'
returns the captured text."
  (test-signel--with-chat-buffer
    (insert "halfwritten")
    (should (equal (signel--pending-input) "halfwritten"))))

(ert-deftest test-signel-input-pending-returns-nil-when-empty ()
  "Boundary: an empty input area returns nil so callers don't restore an
empty string after the prompt."
  (test-signel--with-chat-buffer
    (should-not (signel--pending-input))))

(ert-deftest test-signel-input-system-msg-preserves-pending-input ()
  "Regression for #2: `signel--insert-system-msg' redraws the prompt
without clobbering text the user was mid-typing."
  (test-signel--with-chat-buffer
    (insert "halfwritten")
    (signel--insert-system-msg "An error happened" 'signel-error-face)
    (should (string-match-p "An error happened" (buffer-string)))
    (should (equal (signel--pending-input) "halfwritten"))))

(ert-deftest test-signel-input-msg-preserves-pending-input ()
  "Regression for #2: `signel--insert-msg' (the receive path) redraws the
prompt without clobbering the user's in-progress input."
  (test-signel--with-chat-buffer
    (insert "halfwritten")
    (cl-letf (((symbol-function 'signel--get-buffer)
               (lambda (_) (current-buffer))))
      (signel--insert-msg "+15555550100" "Alice" "Hi there" nil nil nil))
    (should (string-match-p "Hi there" (buffer-string)))
    (should (equal (signel--pending-input) "halfwritten"))))

(provide 'test-signel-input-preservation)
;;; test-signel-input-preservation.el ends here
