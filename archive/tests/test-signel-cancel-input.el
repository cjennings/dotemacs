;;; test-signel-cancel-input.el --- Cancel-input contract for the signel fork -*- lexical-binding: t; -*-

;;; Commentary:
;; `signel--cancel-input' is the C-c C-k handler in `signel-chat-mode'.
;; Its contract: clear any in-progress input between `signel--input-marker'
;; and `point-max' (so the prompt is fresh on next visit), then dismiss
;; the window via `quit-window' (the buffer stays alive so chat history
;; survives revisits).  These tests lock the contract; the binding test
;; locks the keymap entry.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(defmacro test-signel-cancel--with-chat-buffer (&rest body)
  "Set up a temp signel-chat-mode buffer with prompt drawn and run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (signel-chat-mode)
     (setq signel--chat-id "+15555550100")
     (signel--draw-prompt)
     ,@body))

(ert-deftest test-signel-cancel-input-clears-pending-text ()
  "Normal: pending input from input-marker to point-max is cleared."
  (test-signel-cancel--with-chat-buffer
    (insert "abandoned-draft")
    (cl-letf (((symbol-function 'quit-window) (lambda (&rest _) nil)))
      (signel--cancel-input))
    (should-not (signel--pending-input))))

(ert-deftest test-signel-cancel-input-empty-input-area-is-a-noop ()
  "Boundary: cancelling with no in-progress input is harmless."
  (test-signel-cancel--with-chat-buffer
    (cl-letf (((symbol-function 'quit-window) (lambda (&rest _) nil)))
      (signel--cancel-input))
    (should-not (signel--pending-input))))

(ert-deftest test-signel-cancel-input-calls-quit-window ()
  "Normal: cancel dismisses the window via `quit-window'."
  (test-signel-cancel--with-chat-buffer
    (insert "abandoned-draft")
    (let ((called nil))
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _) (setq called t))))
        (signel--cancel-input))
      (should called))))

(ert-deftest test-signel-cancel-input-preserves-buffer ()
  "Normal: cancel does not kill the buffer; chat history (prompt + prior
content above the input marker) survives so reopening the contact lands
in the same buffer."
  (test-signel-cancel--with-chat-buffer
    (insert "abandoned-draft")
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _) nil)))
        (signel--cancel-input))
      (should (buffer-live-p buf)))))

(ert-deftest test-signel-chat-mode-binds-c-c-c-k-to-cancel ()
  "Normal: `signel-chat-mode' binds C-c C-k to `signel--cancel-input' so
the documented cancel gesture reaches the handler."
  (with-temp-buffer
    (signel-chat-mode)
    (should (eq (lookup-key (current-local-map) (kbd "C-c C-k"))
                #'signel--cancel-input))))

(provide 'test-signel-cancel-input)
;;; test-signel-cancel-input.el ends here
